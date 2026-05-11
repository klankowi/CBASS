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
datevec <- seq.Date(from = as.Date('2002-06-01'),
                    to = as.Date('2020-05-31'),
                    by='7 days')

# Blank list of length datevec to save
datlist <- vector(mode='list', length = length(datevec))

# One-time bathymetry pull
total <- read.csv(here('Data/Clean_Data/Bathy/Gridded_CascoBay_Bathymetry_for_MURSST.csv'))

# Jenks natural breaks to define four depth regions
total$dr <- 'Nearshore'
total$dr[total$bathy>11.5065913 & total$bathy<=22.2923262] <- 'Shallow'
total$dr[total$bathy>22.2923262 & total$bathy<=35.5607464 ] <- 'Deep'
total$dr[total$bathy>35.5607464] <- 'Offshore'

for(i in 1:length(datevec)){
  print(i)
  nc_data  <- nc_open(paste0(
    'C:/Users/Katie/mur_sst_download/mur_sst_subset/',
    datevec[i],
    '.nc'))
  
  # Extract variables (e.g., a variable named 'temp', and its dimensions)
  dt <- ncvar_get(nc_data, "analysed_sst")
  dm <- ncvar_get(nc_data, 'mask')
  dt <- as.data.frame(dt)
  lon <- ncvar_get(nc_data, "lon")
  lat <- ncvar_get(nc_data, "lat")
  colnames(dt) <- lat
  rownames(dt) <- lon
  
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
  
  dm <- as.data.frame(dm)
  colnames(dm) <- lat; rownames(dm) <- lon
  
  newdm <- dm %>% 
    mutate(lon = rownames(dm)) %>% 
    mutate(lon = as.numeric(lon)) %>% 
    pivot_longer(cols = 1:(ncol(dt)-1),
                 names_to = 'lat',
                 values_to = 'mask') %>% 
    mutate(lat = as.numeric(lat)) %>% 
    dplyr::select(lon, lat, mask)
  
  newdat <- left_join(newdat, newdm,
                      by=c('lon', 'lat'))
  
  # Make ID list
  newdat$ID <- 1:nrow(newdat)
  
  newdat <- newdat %>% 
    filter(ID %in% total$ID) %>% 
    dplyr::select(lon, lat, sst, ID) %>% 
    mutate(date = datevec[i])
  
  newdat <- left_join(newdat,
                       dplyr::select(total, ID, bathy),
                       by=c('ID'))
  
  nc_close(nc_data)
  
  datlist[[i]] <- newdat
  
  rm(newdat, dt, lon, lat)
  
}
shit <- do.call(rbind, datlist)
shit$month <- month(shit$date)
shit$year <- year(shit$date)
shit$doy <- yday(shit$date)

shit$bathy <- shit$bathy * -1

shit2 <- shit %>% 
  group_by(doy, year) %>% 
  summarise(grad = coef(lm(sst~lon))[2],
            sst = mean(sst, na.rm=T)) %>% 
  arrange(year, doy)

ggplot(data=shit) +
  geom_point(aes(x=bathy, y=sst)) +
  theme(legend.position = 'n')

ggplot(data=shit2[shit2$year>2002 & shit2$year<2020,]) +
  geom_smooth(aes(x=doy, y=sst, col=year, group=year),
              method = 'gam', se=F) +
  scale_color_viridis_c() +
  facet_wrap(vars(year)) +
  theme(legend.position = 'n')

ggplot(data=shit2[shit2$year >2002 & shit2$year<2020,]) +
  geom_smooth(aes(x=doy, y=grad, col=as.factor(year), group = year),
              method = 'loess', se=F) +
  scale_color_viridis_d() +
  geom_hline(yintercept = 0, 
             lty=2) +
  geom_vline(xintercept = c(152, 272), 
             lty=2) +
  labs(x='Day of year', y='Bathymetry-based thermal gradient',
       color='Year') +
  facet_wrap(vars(year)) +
  theme(legend.position = 'n')

tgam <- gam(sst ~
              s(lon, lat) +
              s(bathy, bs='cs') + 
              s(doy, bs='cs') +
              s(year, bs='cs'),
            method = 'REML', 
            select=TRUE,
            data=shit)

summary(tgam)
gam.check(tgam)

plot(tgam,
     residuals=TRUE)
