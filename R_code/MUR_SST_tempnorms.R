rm(list=ls())

# Load packages
library(tidyverse)
library(ncdf4)
library(here)
library(terra)
library(sf)
library(units)
library(FishStatsUtils)

# Set GGplot auto theme
theme_set(theme(panel.grid.major = element_line(color='lightgray'),
                panel.grid.minor = element_blank(),
                panel.background = element_blank(),
                panel.border = element_rect(color='black', size=1, fill=NA),
                legend.position = "bottom",
                axis.text.x=element_text(size=11),
                axis.text.y=element_text(size=11),
                axis.title.x=element_text(size=12),
                axis.title.y=element_text(size=12, angle=90, vjust=2),
                plot.title=element_text(size=14, hjust = 0, vjust = 1.2),
                plot.caption=element_text(hjust=0, face='italic', size=12)))

# Vector of dates to run
datevec <- seq.Date(from = as.Date('2026-01-02'),
                    to = as.Date('2026-01-10'),
                    by='1 days')

# Blank list of length datevec to save
datlist <- vector(mode='list', length = length(datevec))

# One-time bathymetry pull
total <- read.csv(here('Data/Clean_Data/Bathy/Gridded_CascoBay_Bathymetry_for_MURSST.csv'))

# Loop
for(i in 1:length(datevec)){
  print(i)
  # Open NC
  nc_data  <- nc_open(paste0(
    'C:/Users/Katie/mur_sst_download/mur_sst_subset/',
    datevec[i],
    '.nc'))
  
  # Extract variables (e.g., a variable named 'temp', and its dimensions)
  dt <- ncvar_get(nc_data, "analysed_sst")
  dt <- as.data.frame(dt)
  lon <- ncvar_get(nc_data, "lon")
  lat <- ncvar_get(nc_data, "lat")
  colnames(dt) <- lat
  rownames(dt) <- lon
  
  # Create dataframe for SST
  newdat <- dt %>% 
    mutate(lon = rownames(dt)) %>% 
    mutate(lon = as.numeric(lon)) %>% 
    pivot_longer(cols = 1:(ncol(dt)-1),
                 names_to = 'lat',
                 values_to = 'K') %>% 
    mutate(K = as.numeric(K),
           lat = as.numeric(lat)) %>% 
    mutate(sst = K - 273.15) %>% 
    dplyr::select(lon, lat, sst)
  
  # Make ID list
  newdat$ID <- 1:nrow(newdat)
  
  # Filter to only Casco Bay
  newdat <- newdat %>% 
    filter(ID %in% total$ID) %>% 
    dplyr::select(lon, lat, sst, ID) %>% 
    mutate(date = datevec[i])
  
  # Close NC
  nc_close(nc_data)
  
  # Summary stats in holder datalist
  datlist[[i]] <- data.frame(
    date = newdat$date[1],
    mean.sst = mean(newdat$sst, na.rm=T),
    min.sst = min(newdat$sst, na.rm=T),
    max.sst = max(newdat$sst, na.rm=T),
    sd = sd(newdat$sst, na.rm=T),
    ten = quantile(newdat$sst, 0.1, na.rm=T),
    nine = quantile(newdat$sst, 0.9, na.rm=T)
  )
  
  rm(newdat, dt, lon, lat)
  
}

# Rebind
daily.recs <- do.call(rbind, datlist)

# Date values
daily.recs$month <- month(daily.recs$date)
daily.recs$year <- year(daily.recs$date)
daily.recs$doy <- yday(daily.recs$date)

daily.recs %>% group_by(year) %>% 
  summarise(hot = doy[mean.sst==max(mean.sst)],
            sst=mea[max.sst == max(max.sst)]) %>% 
  as.data.frame()

# Plot
ggplot(data=daily.recs) +
  geom_line(aes(x=doy, y=mean.sst, col=year, group=year),
             alpha=1) +
  facet_wrap(vars(year)) +
  scale_color_viridis_c() +
  theme(legend.position = 'n')
