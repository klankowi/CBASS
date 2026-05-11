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
datevec <- seq.Date(from = as.Date('2002-12-01'),
                    to = as.Date('2025-11-30'),
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
  datlist[[i]] <- newdat
  
  rm(newdat, dt, lon, lat, nc_data)
  
}

# Rebind
daily.recs <- do.call(rbind, datlist)

#### Stopped here

# Date values
daily.recs$month <- month(daily.recs$date)
daily.recs$year <- year(daily.recs$date)
daily.recs$doy <- yday(daily.recs$date)

# Split by ID
datlist <- split(daily.recs, f=daily.recs$ID)

for(i in 1:length(datlist)){
  print(i)
  # GAM to find norms by cell
  tgam <- gam(data= datlist[[i]][datlist[[i]]$year<=2020,],
              sst ~ s(year, bs='cs', k=19) + s(doy, bs='cs'),
              method = 'REML', select=T)
  
  # Predict wthout effect of year
  daily.smooth <- data.frame(
    doy= seq(1, 366, 1),
    year = 2020
  )
  
  np <- mgcv::predict.gam(tgam, daily.smooth,
                          exclude = "s(year)",
                          se.fit=T)
  
  daily.smooth$smooth.daily <- np$fit
  daily.smooth$smooth.upper <- np$fit + np$se.fit
  daily.smooth$smooth.lower <- np$fit - np$se.fit
  
  datlist[[i]] <- left_join(datlist[[i]],
                            dplyr::select(daily.smooth, -year),
                            by=c('doy'))
  
  rm(tgam, daily.smooth, np)
  
}
# Rebind
daily.recs <- do.call(rbind, datlist)
rownames(daily.recs) <- NULL

# Anomalies
daily.recs$anomaly <- daily.recs$sst - daily.recs$smooth.daily

# Weekly
weekly.recs <- daily.recs %>% 
  mutate(week = week(date)) %>% 
  group_by(week, year, ID) %>% 
  mutate(week.anomaly = mean(anomaly, na.rm=T),
         week.sst = mean(sst, na.rm=T)) %>% 
  dplyr::select(lon, lat, ID, month, year, week, week.anomaly, 
                week.sst) %>%
  unique() %>% 
  as.data.frame()

weekly.recs <- left_join(weekly.recs,
                         dplyr::select(total, ID, bathy),
                         by=c("ID"))

weekly.recs <- weekly.recs %>% 
  mutate(bathy = bathy * -1) %>% 
  arrange(bathy, year, week)

order.ID <- unique(weekly.recs$ID)

weekly.recs$ID <- factor(weekly.recs$ID, levels = order.ID)

weekly.recs$lab <- as.Date(paste(2025, 
                                 weekly.recs$week, 
                                 1, sep="-"), "%Y-%U-%u")

# Time Plot
ggplot(data=weekly.recs[weekly.recs$year %in% seq(2014, 2025, 1),]) +
  geom_line(aes(x=lab, y=week.sst, col=ID, group=ID),
             alpha=1) +
  scale_x_date(date_breaks = '2 months',
               date_labels = "%b") +
  scale_color_viridis_d() +
  facet_wrap(vars(year)) +
  #geom_hline(yintercept = 0, col='red', lty=2) +
  labs(x='Week', y='Weekly SST anomaly') +
  theme(legend.position = 'n')


# Space plots
cb <- st_read(here('GIS/CascoBay_Polygon.shp'))
cb <- st_transform(cb, crs="EPSG:4326")

# 24-25
space <- weekly.recs[weekly.recs$year == 2025 & weekly.recs$week<=48,]
space <- rbind(weekly.recs[weekly.recs$year == 2024 & weekly.recs$week>=49,],
               space)
space <- space[!is.na(space$lab),]

#space <- st_as_sf(space, coords=c('lon', 'lat'), crs="EPSG:4326")

space$lab <- as.numeric(paste0(space$year, '.', 
                               str_pad(space$week, 2, 'left', '0')))

anom25 <- ggplot() +
  geom_tile(data = space,
          aes(x=lon, y=lat, fill=week.anomaly)) +
  scale_fill_gradient2(midpoint = 0,
                       na.value = 'transparent',
                       low = scales::muted('blue'), 
                       high = scales::muted('red'),
                       limits=c(-3.3, 3.97))+
  facet_wrap(vars(lab)) +
  geom_sf(data=cb, fill=NA) +
  ggtitle('Weekly SST Anomalies Dec 2024-Nov 2025') +
  labs(x='', y='', fill='Weekly SST anomaly') +
  theme(axis.text.x = element_blank(),
        axis.text.y = element_blank(),
        legend.margin = margin(0,0,0,0),
        axis.ticks.x = element_blank(),
        axis.ticks.y = element_blank())

sst25 <- ggplot() +
  geom_tile(data = space,
            aes(x=lon, y=lat, fill=week.sst)) +
  scale_fill_viridis_c(limits=c(0.3, 20.8)) +
  facet_wrap(vars(lab)) +
  geom_sf(data=cb, fill=NA) +
  ggtitle('Weekly SST Dec 2024-Nov 2025') +
  labs(x='', y='', fill='Weekly SST') +
  theme(axis.text.x = element_blank(),
        axis.text.y = element_blank(),
        legend.margin = margin(0,0,0,0),
        axis.ticks.x = element_blank(),
        axis.ticks.y = element_blank())

# 21-22
space <- weekly.recs[weekly.recs$year == 2022 & weekly.recs$week<=48,]
space <- rbind(weekly.recs[weekly.recs$year == 2021 & weekly.recs$week>=49,],
               space)
space <- space[!is.na(space$lab),]

#space <- st_as_sf(space, coords=c('lon', 'lat'), crs="EPSG:4326")

space$lab <- as.numeric(paste0(space$year, '.', 
                               str_pad(space$week, 2, 'left', '0')))

anom22 <- ggplot() +
  geom_tile(data = space,
            aes(x=lon, y=lat, fill=week.anomaly)) +
  scale_fill_gradient2(midpoint = 0,
                       na.value = 'transparent',
                       low = scales::muted('blue'), 
                       high = scales::muted('red'),
                       limits=c(-3.3, 3.97))+
  facet_wrap(vars(lab)) +
  geom_sf(data=cb, fill=NA) +
  labs(x='', y='', fill='Weekly SST anomaly') +
  ggtitle('Weekly SST Anomalies Dec 2021-Nov 2022') +
  theme(axis.text.x = element_blank(),
        axis.text.y = element_blank(),
        legend.margin = margin(0,0,0,0),
        axis.ticks.x = element_blank(),
        axis.ticks.y = element_blank())

sst22 <- ggplot() +
  geom_tile(data = space,
            aes(x=lon, y=lat, fill=week.sst)) +
  scale_fill_viridis_c(limits=c(0.3, 20.8)) +
  facet_wrap(vars(lab)) +
  geom_sf(data=cb, fill=NA) +
  ggtitle('Weekly SST Dec 2021-Nov 2022') +
  labs(x='', y='', fill='Weekly SST') +
  theme(axis.text.x = element_blank(),
        axis.text.y = element_blank(),
        legend.margin = margin(0,0,0,0),
        axis.ticks.x = element_blank(),
        axis.ticks.y = element_blank())

ggsave(anom22, filename = here('State of the Bay/Plots/SSTAnoms_22.png'),
       width = 7, height = 8, units='in')
ggsave(anom25, filename = here('State of the Bay/Plots/SSTAnoms_25.png'),
       width = 7, height = 8, units='in')

ggsave(sst22, filename = here('State of the Bay/Plots/SSTWeek_22.png'),
       width = 7, height = 8, units='in')
ggsave(sst25, filename = here('State of the Bay/Plots/SSTWeek_25.png'),
       width = 7, height = 8, units='in')

baywide$lab <- as.Date(paste(baywide$year, 
                                 baywide$week, 
                                 1, sep="-"), "%Y-%U-%u")

baywide$rollmonth <- zoo::rollmean(baywide$baywide.anomaly, 
                                   12, 
                                   fill = list(NA, NULL, NA), 
                                   align = "left")

baywide <- weekly.recs %>% 
  filter(!is.na(lab)) %>% 
  group_by(year, week) %>% 
  mutate(baywide.anomaly = mean(week.anomaly, na.rm =T),
         baywide.sst = mean(week.sst, na.rm=T)) %>% 
  dplyr::select(-ID, -week.anomaly, -week.sst, -bathy, -lon, -lat) %>%
  arrange(year, week) %>% 
  mutate()
  unique() %>% as.data.frame()

ggplot(data=baywide[baywide$year>=2014,]) +
  geom_line(aes(x=lab, y=rollmonth), alpha=1) +
  scale_x_date(date_breaks = '6 months',
               date_labels = '%b-%y') +
  geom_hline(yintercept = 0, col='red', lty=2, alpha=0.8) +
  labs(x='Time', y='Baywide SST Anomaly') +
  theme(axis.text.x = element_text(angle = 15))

# Monthly
monthly.recs <- daily.recs %>%  
  group_by(month, year) %>% 
  mutate(month.anomaly = mean(anomaly, na.rm=T),
         month.sst = mean(sst, na.rm=T)) %>% 
  dplyr::select(month, year, month.anomaly, 
                month.sst) %>%
  mutate(lab = as.Date(paste(year, month, 1, sep='-'),
                       format = '%Y-%m-%d')) %>% 
  unique() %>% 
  as.data.frame()

ggplot(data=monthly.recs[monthly.recs$year>=2013 &
                           monthly.recs$month %in% c(12) &
                           monthly.recs$lab != '2013-06-01',]) +
  geom_point(aes(x=year+1, y=month.anomaly)) +
  geom_line(aes(x=year+1, y=month.anomaly)) +
  #scale_x_continuous(labels = seq(2014, 2026, 1)) +
  geom_hline(yintercept = 0, col='red', lty=2, alpha=0.8) +
  theme(axis.text.x = element_text(angle = 15))
