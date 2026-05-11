# Compare HOBO to Portland Harbor Temps

rm(list=ls())

# Packages
library(here)
library(tidyverse)
library(sf)
library(noaaoceans)

# Set GGplot auto theme
theme_set(theme(panel.grid.major = element_line(color='lightgray'),
                panel.grid.minor = element_blank(),
                panel.background = element_blank(),
                panel.border = element_rect(color='black', linewidth=1, fill=NA),
                legend.position = "bottom",
                axis.text.x=element_text(size=12),
                axis.text.y=element_text(size=12),
                axis.title.x=element_text(size=14),
                axis.title.y=element_text(size=14, angle=90, vjust=2),
                plot.title=element_text(size=14, hjust = 0, vjust = 1.2),
                plot.caption=element_text(hjust=0, face='italic', size=12)))

# Negate function
'%notin%' <- function(x,y)!('%in%'(x,y))

#### Download Portland data ####
# Set years to pull (Portland station reliable data record 2003-today)
years <- 2025

# Set months
months <- seq(1, 12, 1)
for(i in 1:length(months)){
  months[i] <- str_pad(months[i], 2, 'left', '0')
}

# Create string of 'starting dates.' can only pull a month at a time.
sdates <- NA

for(i in years){
  for(j in months){
    temp <- paste0(i,j,'01')
    sdates <- c(sdates, temp)
    rm(temp)
  }
}

sdates <- sdates[!is.na(sdates)]

# Create string of 'end dates.'
edates <- NA

for(i in years){
  for(j in (months)){
    if(j %in% c('01', '03', '05', '07', '08', '10', '12')){
      temp <- paste0(i,j,31)
    }
    if(j %in% c('04', '06', '09', '11')){
      temp <- paste0(i,j,30)
    }
    if(leap_year(i)==TRUE & j=='02'){
      temp <- paste0(i,j,29)
    }
    if(leap_year(i)==FALSE & j=='02'){
      temp <- paste0(i,j,28)
    }
    edates <- c(edates, temp)
    rm(temp)
  }
}

## Fix Nov 2025 and Fix May 2025
edates[edates == '20251231'] <- '20251204'

# Remove NA end dates
edates <- edates[!is.na(edates)]

# Pull inital water temp query
wat_temp <- query_coops_data(
  station_id = 8418150,
  start_date=sdates[1],
  end_date = edates[1],
  data_product = "water_temperature",
  units='metric',
  time_zone = 'lst_ldt'
)

# Pull remaining monthly queries
for(i in 2:length(sdates)){
  quer <- query_coops_data(
    station_id = 8418150,
    start_date=sdates[i],
    end_date = edates[i],
    data_product = "water_temperature",
    units='metric',
    time_zone = 'lst_ldt'
  )
  # Bind to initial
  wat_temp <- rbind(wat_temp, quer)
  rm(quer)
  print(i)
}

# Repeat for water level
wat_lvl <- query_coops_data(
  station_id = 8418150,
  start_date=sdates[1],
  end_date = edates[1],
  data_product = "water_level",
  datum='mllw',
  units='metric',
  time_zone = 'lst_ldt'
)

# Pull remaining monthly queries
for(i in 2:length(sdates)){
  quer <- query_coops_data(
    station_id = 8418150,
    start_date=sdates[i],
    end_date = edates[i],
    data_product = "water_level",
    datum='mllw',
    units='metric',
    time_zone = 'lst_ldt'
  )
  # Bind to initial
  wat_lvl <- rbind(wat_lvl, quer)
  rm(quer)
  print(i)
}

# Repeat for air temp
air_temp <- query_coops_data(
  station_id = 8418150,
  start_date=sdates[1],
  end_date = edates[1],
  data_product = "air_temperature",
  units='metric',
  time_zone = 'lst_ldt'
)

# Pull remaining monthly queries
for(i in 2:length(sdates)){
  quer <- query_coops_data(
    station_id = 8418150,
    start_date=sdates[i],
    end_date = edates[i],
    data_product = "air_temperature",
    units='metric',
    time_zone = 'lst_ldt'
  )
  # Bind to initial
  air_temp <- rbind(air_temp, quer)
  rm(quer)
  print(i)
}

# Remove intermediates
rm(years, sdates, edates, months, i, j)

# Rename columns, discard unnecessary data
wat_temp <- wat_temp %>% 
  dplyr::select(t, v) %>% 
  rename(timestamp = t,
         sst = v) %>% 
  mutate(timestamp = as.POSIXct(timestamp,
                                format = '%Y-%m-%d %H:%M',
                                tz='America/New_York')) %>% 
  mutate(sst = as.numeric(sst))

wat_lvl <- wat_lvl %>% 
  dplyr::select(t, v) %>% 
  rename(timestamp = t,
         lvl = v) %>% 
  mutate(timestamp = as.POSIXct(timestamp,
                                format = '%Y-%m-%d %H:%M',
                                tz='America/New_York')) %>% 
  mutate(lvl = as.numeric(lvl))

air_temp <- air_temp %>% 
  dplyr::select(t, v) %>% 
  rename(timestamp = t,
         air = v) %>% 
  mutate(timestamp = as.POSIXct(timestamp,
                                format = '%Y-%m-%d %H:%M',
                                tz='America/New_York')) %>% 
  mutate(air = as.numeric(air))

# Determine tidal stage
stage <- wat_lvl
stage$dif <- NA
stage$stage <- 'rising'
for(i in 2:nrow(stage)){
  stage$dif[i] <- stage$lvl[(i)] - stage$lvl[(i-1)]
}
stage$stage[stage$dif <0] <- 'falling'

# Merge
m1 <- left_join(wat_lvl, wat_temp, by=c('timestamp'))
m2 <- left_join(m1, air_temp, by=c('timestamp'))
m2 <- left_join(m2, 
                dplyr::select(stage, timestamp, stage), 
                by=c('timestamp'))

# Sun condition
sun <- suncalc::getSunlightTimes(date = unique(as.Date(m2$timestamp)),
                                 lat=43.658033, 
                                 lon =-70.244212,
                                 tz="America/New_York",
                                 kee=c('sunrise', 'sunset'))
# Merge with sunlight
m2$date <- as.Date(m2$timestamp)

m3 <- left_join(m2, 
                sun,
                by=c('date'))

# Determine timestamp sun condition
for(i in 1:nrow(m3)){
  print(i)
  m3$condition[i] <- ifelse(m3$timestamp[i] >= m3$sunrise[i] & 
                            m3$timestamp[i] <= m3$sunset[i],
                            "Day", "Night")
}

# Save
port <- m3 %>% 
  dplyr::select(-date, -lat, -lon, -sunrise, -sunset) %>% 
  mutate(month = month(timestamp))

# Rm intermediates
rm(air_temp, m1, m2, m3, sun, wat_lvl, wat_temp)
port <- port %>% 
  filter(!is.na(timestamp))

port$doy <- day(port$timestamp)

# Check
ggplot(data=port) +
  geom_point(aes(x=lvl, y=sst, col=doy),
             alpha=0.2) +
  scale_color_viridis_c() +
  facet_wrap(vars(month), nrow=3) +
  labs(x='Water level above MLLW (m)', y='SST (\u00B0C)')
#### Load QBC HOBO data ####
qbc <- read.csv(here('Data/Clean_Data/Loggers/QBC_Hobos.csv'))
qbc <- qbc %>% 
  rename(timestamp = date,
         sst = temp) %>% 
  dplyr::select(-light, -dif, -doy) %>% 
  mutate(timestamp = as.POSIXct(timestamp,
                                format = '%Y-%m-%d %H:%M:%S',
                                tz= 'America/New_York')) %>% 
  mutate(month =  month(timestamp))

qbc <- qbc %>% rename(date = timestamp,
                      temp = sst)

#### Load GMRI HOBO data ####
hobo <- read.csv(here(
  'Data/Raw_Data/Raw_HOBO_2025.csv'
))

hobo <- hobo %>% 
  mutate(date = as.POSIXct(date,
                           format = '%Y-%m-%d %H:%M:%S',
                           tz='America/New_York')) %>% 
  mutate(date = lubridate::round_date(date, unit = 'minute')) %>% 
  mutate(temp = as.numeric(temp)) %>% 
  mutate(month = mont(date))

# Merge
hobo <- rbind(qbc, hobo)

# Clean HOBO
hobo <- hobo %>% 
  filter(date > as.POSIXct('2025-05-29 00:00:00', tz='America/New_York')) %>% 
  filter(date < as.POSIXct('2025-11-13 00:00:00', tz='America/New_York')) %>% 
  filter(!is.na(temp)) %>% 
  mutate(month = month(date))

# Split by site
hobo <- split(hobo, f=hobo$site)

# Portland trimmed to HOBO record
hport <- port[port$timestamp >= as.POSIXct('2025-05-29 00:00:00') &
              port$timestamp <= as.POSIXct('2025-11-12 23:59:59'),]

# Interpolate to six-minute intervals (per harbor gauge)
for(i in 1:length(hobo)){
  hxts <- xts::xts(hobo[[i]]$temp, order.by = hobo[[i]]$date)
  
  empty <- xts::xts(rep(NA, length(hport$timestamp)), 
                    order.by = hport$timestamp)
  
  merged_xts <- merge(hxts, empty)
  
  interpolated_xts <- zoo::na.approx(merged_xts)
  
  temp <- fortify(interpolated_xts)
  colnames(temp) <- c('timestamp', 'sst', 'empty')
  temp <- temp %>% 
    dplyr::select(-empty) %>% 
    filter(timestamp %in% hport$timestamp) %>% 
    mutate(site = hobo[[i]]$site[1])
  
  hobo[[i]] <- temp
  rm(hxts, empty, merged_xts, interpolated_xts, temp)
}

hobo <- do.call(rbind, hobo)
rownames(hobo) <- NULL

port <- port %>% rename(port.sst = sst)

test <- left_join(hobo, port, by=c('timestamp', 'month'))
test$sst.dif <- test$port.sst - test$sst

# Identify dewatering events
# Probably at negative tides or >1ft 
hobo$month <- month(hobo$timestamp)
test$site <- factor(test$site,
                    levels=c(
                      'Presumpscot', 'Skittery',
                      'Audubon', 'MackNorth',
                      'MackBeach', 'Mussel',
                      'Brothers',
                      'BackCove', 'GDI', 
                      'SMCC', 'Cushing',
                      'Alewife', 
                      
                      'Garrison', 'Lowell',
                      'LongPt', 'SnowIs', 'Orrs'
                    ))
shitlist <- split(test, f=test$month)

for(i in 1:length(shitlist)){
  gpl <- 
  ggplot() +
    geom_point(data=shitlist[[i]],
               aes(x=lvl, y=sst.dif, col=condition),
               alpha=0.5) +
    scale_color_viridis_d(end=0.85, direction = -1) +
    geom_hline(yintercept = 0, col='red', lty=2) +
    geom_vline(xintercept = 0, col='red', lty=2) +
    facet_wrap(vars(site)) +
    ylim(-12,12) +
    ggtitle(paste0(lubridate::month(shitlist[[i]]$month[1],
                                    label=TRUE))) +
    annotate("text", x = 1.5, y = 5, label = "Portland Hotter", 
             color = "red", size = 4) +
    annotate("text", x = 1.5, y = -5, label = "HOBO Hotter", 
             color = "cadetblue", size = 4) +
    labs(x='Water level (m)', y='SST dif, Portland - Site',
         color='Sun condition') +
    theme(plot.margin = margin(3,0,0,3),
          legend.box.margin = margin(0,0,0,0))
  print(gpl)
  # ggsave(plot=gpl,
  #        here(paste0('Plots/HOBO_Temps/',
  #                    str_pad(i, 2, 'left', '0'),
  #                    '-',
  #                    lubridate::month(shitlist[[i]]$month[1], label=T),
  #                    '.png')),
  #        width = 11, height=7, units='in')
  
  
  # identify problems
  shitlist[[i]] <- split(shitlist[[i]], f=shitlist[[i]]$site)
  for(j in 1:length(shitlist[[i]])){
    if(j%in% c(3, 6)){next()}
    shitlist[[i]][[j]]$oow[shitlist[[i]][[j]]$lvl <=0] <- 'Out'
    shitlist[[i]][[j]]$oow[shitlist[[i]][[j]]$lvl >0] <- 'In'
    print(
    ggplot(data=shitlist[[i]][[j]]) +
      geom_histogram(aes(x=sst.dif, fill=condition)) +
      ggtitle(names(shitlist[[i]][j])) +
      facet_wrap(vars(as.factor(round(lvl,1)))) +
      geom_vline(xintercept = 0, lty=2) +
      theme(legend.position = 'n')
    )

  }
  
}

# Alewife good, Cushing good, GDI good, BackCove good, Mussel good, Audubon good
# SMCC remove below 0
# Brothers remove below 0.5
# Mack Beach remove below 0.5
# Skittery remove below 0.5
# Mack North remove below 0.5

#### Clean ####
test$sst[test$site == 'SMCC' & test$lvl<=0] <- NA
test$sst[test$site == 'Brothers' & test$lvl<=0.5] <- NA
test$sst[test$site == 'MackBeach' & test$lvl<=0.5] <- NA
test$sst[test$site == 'MackNorth' & test$lvl<=0.5] <- NA
test$sst[test$site == 'Skittery' & test$lvl<=0.5] <- NA
test$sst.dif <- test$port.sst - test$sst


#### Average ####
test$doy <- yday(test$timestamp)
ggplot(data=test) +
  geom_point(aes(x=doy, y=sst.dif)) +
  facet_wrap(vars(site))

daily <- test %>% 
  group_by(site, doy) %>% 
  summarise(sst = mean(sst, na.rm=T),
            port.sst = mean(port.sst, na.rm=T),
            sst.dif = mean(sst.dif, na.rm=T))

ggplot(data=daily) +
  geom_line(aes(x=doy, y=sst.dif)) +
  facet_wrap(vars(site)) +
  geom_hline(yintercept = 0, lty=2) +
  annotate("text", x = 175, y = 2.5, label = "Portland Hotter", 
           color = "salmon", size = 5) +
  annotate("text", x = 275, y = -5, label = "HOBO Hotter", 
           color = "cadetblue", size = 5) +
  labs(x='Water level (m)', y='SST dif, Portland - Site',
       color='Sun condition') +
  theme(plot.margin = margin(3,0,0,3),
        legend.box.margin = margin(0,0,0,0))


#### Plot, animate ####
# Data
sites <- read.csv(here('Data/Clean_Data/Seine/sites_cleaned.csv'))
sites <- sites %>% 
  filter(site_name %notin% c('The Brothers - South',
                             'Mill Brook',
                             'Presumpscot Falls', 
                             'Cedar Beach')) %>% 
  st_as_sf(coords=c('longitude', 'latitude'),
           crs="EPSG:4326")

sites.sf <- st_transform(sites, crs="EPSG:2803")

# Call coastline shapefile
coast <- st_read(here("GIS/CBASS_with_rivers.shp"), quiet=T)
coast <- st_transform(coast, st_crs(sites.sf))
mb <- st_read(here('GIS/Mill_Brook.shp'), quiet=T)
mb <- st_transform(mb, st_crs(sites.sf))

cities <- read.csv(here('Data/Clean_Data/CascoBay_City_Coordinates.csv'))
cities <- st_as_sf(cities, coords=c('lon', 'lat'), crs="EPSG:4326")
cities <- st_transform(cities, st_crs(sites.sf))

phtg <- st_as_sf(data.frame(x=43.658031,
                            y=-70.244253),
                 coords=c('y', 'x'), crs="EPSG:4326")
phtg <- st_transform(phtg, st_crs(sites.sf))

# Merge
sn <- sites.sf %>% 
  sfheaders::sf_to_df(fill=T) %>% 
  dplyr::select(site_name, abbrev)
sn$site <- c('Presumpscot', 'Skittery', 'Audubon', 'MackNorth', 'MackBeach',
             'Musel', 'Brothers', 'BackCove', 'GDI', 'SMCC', 'Cushing', 
             'Alewife', 'Orrs', 'SnowIs', 'Lowell', 'Garrison', 'LongPt', 
             'Stovers')
test <- left_join(test, 
                  dplyr::select(sn, site, abbrev),
                  by=c('site'))

test <- merge(dplyr::select(sites.sf, abbrev, geometry),
              test,
                  by=c('abbrev'))

daily <- test %>% 
  mutate(date = as.Date(timestamp)) %>% 
  group_by(site, date) %>% 
  mutate(sst = mean(sst.dif, na.rm=T)) %>% 
  dplyr::select(date, site, abbrev, sst, geometry) %>% 
  arrange(date, site) %>% 
  unique()

# Plot Casco Bay
cascobay <- ggplot() +
  geom_sf(data=coast, fill='gray', col='gray30',
          lwd=0.1) +
  geom_sf(data=mb, col='gray30', lwd=0.5) +
  geom_sf(data=sites.sf, pch=21) +
  geom_sf(data=phtg) +
  coord_sf(xlim=c(-70.29, -69.85),
           ylim=c(43.57, 43.87), crs="EPSG:4326") +
  labs(x='', y='') +
  theme(axis.text.x = element_blank(),
        axis.ticks = element_blank(),
        axis.text.y = element_blank(),
        panel.grid.major = element_blank(),
        axis.title=element_blank(),
        panel.border = element_rect(color='black', linewidth=0.5, fill=NA),
        plot.margin = margin(2,0,0,-10, 'mm'))
cascobay


# Yeehaw
dailylist <- split(daily, f=daily$date)

for(i in 1:length(dailylist)){
  sites.sf <- dailylist[[i]]
  datlab <- paste0(lubridate::month(sites.sf$date[1], label = T),
                   ' 2025')
  pic <- 
  ggplot() +
    geom_sf(data=coast, fill='gray', col='gray30',
            lwd=0.1) +
    geom_sf(data=mb, col='gray30', lwd=0.5) +
    geom_sf(data=sites.sf, pch=21,
            aes(fill=sst),
            stroke=0.4,
            cex=4) +
    scale_fill_gradient2(limits=c(-7.1, 3.1),
                          midpoint=0) +
    geom_sf(data=phtg, pch=8) +
    coord_sf(xlim=c(-70.29, -69.85),
             ylim=c(43.57, 43.87), crs="EPSG:4326") +
    labs(x='', y='', fill='SST difference (\u00B0C)\nPortland - Site') +
    theme(axis.text.x = element_blank(),
          axis.ticks = element_blank(),
          axis.text.y = element_blank(),
          panel.grid.major = element_blank(),
          axis.title=element_blank(),
          panel.border = element_rect(color='black', linewidth=0.5, fill=NA),
          plot.margin = margin(2,0,0,-8, 'mm')) +
    ggtitle(datlab)
  pic
  
  ggsave(plot=pic,
         here(paste0('Documentation/EIR_2025/TempGif/',
                     str_pad(i,3, 'left', '0'),
                     '.png')),
         width = 8, height = 8, units='in')
}

# List files in given directory
setwd('C:/Users/Katie/Documents/GitHub/CBASS/Documentation/EIR_2025/TempGif')
files  <- list.files()
files <- paste0(getwd(), "/", files)

# Set GIF save location
setwd("C:/Users/Katie/Documents/GitHub/CBASS/Documentation/EIR_2025/")

# Convert PNGs to GIF
gifski(files, "temperature_comparison.gif", loop = FALSE, delay = 0.12)
