## Retrieve NOAA station meteorological data ##
rm(list=ls())
library(tidyverse)
library(noaaoceans)

years <- seq(2003, 2023)
months <- seq(1, 12)
for(i in months){
  months[i] <- str_pad(months[i], 2, 'left', '0')
}

sdates <- NA

for(i in years){
  for(j in months){
    temp <- paste0(i,j,'01')
    sdates <- c(sdates, temp)
    rm(temp)
  }
}

sdates <- sdates[!is.na(sdates)]

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

## NOAA buoy did not collect water temperature May 5-Jul5 2011
sdates <- sdates[sdates != '20110601']
edates <- edates[edates != '20110630']

## December 2023 has not happened yet
sdates <- sdates[sdates != '20231201']
edates <- edates[edates != '20231231']

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

# Remove intermediates
rm(years, sdates, edates, months, i, j)

# Save raw copy
raw.data <- wat_temp

# Rename columns, discard unecessary data
wat_temp <- raw.data %>% 
  dplyr::select(t, v) %>% 
  rename(timestamp = t,
         sst_c = v)

# Create time stamp
wat_temp$timestamp <- as.POSIXct(wat_temp$timestamp,
                                 format='%Y-%m-%d %H:%M')

# Create hourly collector
wat_temp$ts <- paste0(lubridate::year(wat_temp$timestamp), "-", 
                      lubridate::month(wat_temp$timestamp), '-',
                      lubridate::day(wat_temp$timestamp), '_',
                      lubridate::hour(wat_temp$timestamp))

# Force to numeric
wat_temp$sst_c <- as.numeric(wat_temp$sst_c)

# Remove outliers
wat_temp$sst_c[wat_temp$sst_c > 30] <- NA
summary(wat_temp$sst_c)

# Calculate mean hourly temp
wat_temp <- wat_temp %>% 
  group_by(ts) %>% 
  summarise(hourly.temp = mean(sst_c, na.rm=TRUE))

# Split timestamp back out
wat_temp <- wat_temp %>% 
  separate(ts, into=c('year', 'month', 'day'), '-')
wat_temp <- wat_temp %>% 
  separate(day, into=c('day', 'hour'), '_')

# Append leading zeros, combine
wat_temp$timestamp <- paste0(
  wat_temp$year, '-',
  str_pad(wat_temp$month, 2, 'left', '0'), '-',
  str_pad(wat_temp$day, 2, 'left', '0'), ' ',
  str_pad(wat_temp$hour, 2, 'left', '0'), ':00'
)

# Force back to timestamp
wat_temp$timestamp <- as.POSIXct(wat_temp$timestamp,
                                 format="%Y-%m-%d %H:%M")

# Remove intermediates
wat_temp <- wat_temp %>% 
  dplyr::select(timestamp, hourly.temp)
wat_temp <- wat_temp[with(wat_temp, order(timestamp)),]
rownames(wat_temp) <- NULL

# Detrend
wat_temp$doy <- lubridate::yday(wat_temp$timestamp)
norms <- wat_temp %>% 
  group_by(doy) %>% 
  summarise(mean.daily = mean(hourly.temp, na.rm=TRUE))
norms <- norms[!is.na(norms$mean.daily),]

# Smooth
p <- ggplot(data=norms) +
  geom_smooth(aes(x=doy, y=mean.daily),
              n=366)
dat <- ggplot_build(p)$data[[1]]
norms$smooth.daily <- dat$y
norms$smooth.upper <- dat$ymax
norms$smooth.lower <- dat$ymin

tp <- wat_temp[wat_temp$timestamp > as.POSIXct('2014-01-01 00:00:00'),]
tp$year <- lubridate::year(tp$timestamp)
tp <- tp[!is.na(tp$timestamp),]

ggplot() +
  geom_point(data=tp,
            aes(x=doy, y=hourly.temp, col=as.factor(year)),
            alpha=0.1, stroke=NA) +
  scale_color_viridis_d(option='viridis', 'Year')+
  geom_line(data=norms,
            aes(x=doy, y=smooth.daily),
            col='blue', lwd=0.7) +
  geom_ribbon(data=norms,
              aes(x=doy, ymin=smooth.lower, ymax=smooth.upper),
              fill='blue', alpha=0.3) +
  xlab('Day of year') + ylab('SST (C)')+
  guides(colour = guide_legend(override.aes = list(alpha = 1))) +
  geom_vline(xintercept=151, col='red', lty=2) +
  geom_vline(xintercept=273, col='red', lty=2)

wat_temp$dailyts <- ymd(wat_temp$timestamp)
wat_temp$dailyts <- zoo::na.locf(wat_temp$dailyts)

daily.temps <- wat_temp %>% 
  group_by(dailyts) %>% 
  summarise(daily.c = mean(hourly.temp, na.rm=TRUE))

wat_temp <- left_join(wat_temp, daily.temps, by=c('dailyts'))

wat_temp <- left_join(wat_temp, dplyr::select(norms, doy, smooth.daily,
                                              smooth.upper, smooth.lower),
                      by=c('doy'))
wat_temp$anomaly <- wat_temp$daily.c - wat_temp$smooth.daily
wat_temp$year <- lubridate::year(wat_temp$timestamp)

tp <- wat_temp[wat_temp$timestamp > as.POSIXct('2014-01-01 00:00:00'),]
tp$year <- lubridate::year(tp$timestamp)
tp <- tp[!is.na(tp$timestamp),]

tp$updif <- tp$smooth.upper - tp$smooth.daily
tp$lowdif <- tp$smooth.lower - tp$smooth.daily

ggplot() +
  geom_point(data=unique(dplyr::select(tp, 
                                       doy, daily.c, anomaly, year,
                                       updif, lowdif)),
             aes(x=doy, y=anomaly, col=as.factor(year)),
             alpha=0.5, stroke=NA) +
  scale_color_viridis_d(option='viridis', 'Year') +
  geom_ribbon(data=tp,
              aes(x=doy, ymin=lowdif, ymax=updif),
              fill='blue', alpha=0.3) +
  xlab('Day of year') + ylab('SST Anomaly (C)')+
  guides(colour = guide_legend(override.aes = list(alpha = 1))) +
  geom_vline(xintercept=151, col='red', lty=2) +
  geom_vline(xintercept=273, col='red', lty=2)

tp %>% group_by(year) %>% summarise(mean.anom = mean(anomaly, na.rm=TRUE))

tp$catper <- NA
tp$catper[tp$year %in% c(2014, 2015, 2017, 2018, 2019)] <- 'cold'
tp$catper[tp$year %in% c(2016, 2020, 2021, 2022, 2023)] <- 'hot'
tp$catper <- factor(tp$catper, levels = c('hot', 'cold'))

ggplot() +
  geom_point(data=unique(dplyr::select(tp, 
                                       doy, daily.c, anomaly, year,
                                       updif, lowdif, catper)),
             aes(x=doy, y=anomaly, col=as.factor(year)),
             alpha=0.8, stroke=NA) +
  scale_color_viridis_d(option='viridis', 'Year') +
  geom_ribbon(data=tp,
              aes(x=doy, ymin=lowdif, ymax=updif),
              fill='blue', alpha=0.3) +
  xlab('Day of year') + ylab('SST Anomaly (C)')+
  guides(colour = guide_legend(override.aes = list(alpha = 1))) +
  facet_wrap(vars(catper)) +
  geom_vline(xintercept=151, col='red', lty=2) +
  geom_vline(xintercept=273, col='red', lty=2)


write.csv(tp, row.names = F,
          here('Clean_Data/Meteorological/Portland_watertemp.csv'))
