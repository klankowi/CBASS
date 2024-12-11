## Retrieve NOAA station meteorological data ##
rm(list=ls())
library(tidyverse)
library(noaaoceans)
library(here)

# Negate function
'%notin%' <- function(x,y)!('%in%'(x,y))

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

# Set years to pull (Portland station reliable data record 2003-today)
years <- seq(2003, 2024)

# Set months
months <- seq(1, 12)
for(i in months){
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

## NOAA buoy did not collect water temperature May 5-Jul5 2011
sdates <- sdates[sdates != '20110601']
edates <- edates[edates != '20110630']

## November 2024 has not finished yet
#sdates <- sdates[sdates != '20241201']
#edates <- edates[edates != '20241231']
edates[edates == '20241231'] <- '20241202'

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

# Rename columns, discard unnecessary data
wat_temp <- raw.data %>% 
  dplyr::select(t, v) %>% 
  rename(timestamp = t,
         sst_c = v)

# Create time stamp
wat_temp$timestamp <- as.POSIXct(wat_temp$timestamp,
                                 format='%Y-%m-%d %H:%M')

# Create date columns
wat_temp$month <- month(wat_temp$timestamp)
wat_temp$month <- str_pad(wat_temp$month, 2, 'left', '0')
wat_temp$year <- year(wat_temp$timestamp)

# Remove instances of missing timestamp
wat_temp <- wat_temp[!is.na(wat_temp$timestamp),]


# Remove bad  - invalid water temps (pulled by eye)
wat_temp$sst_c[wat_temp$timestamp >= as.POSIXct('2003-03-26 13:48:00') &
               wat_temp$timestamp <= as.POSIXct('2003-03-28 11:42:00')] <- NA

wat_temp$sst_c[wat_temp$timestamp >= as.POSIXct('2004-08-23 13:18:00') &
                 wat_temp$timestamp <= as.POSIXct('2004-08-23 13:42:00')] <- NA

wat_temp$sst_c[wat_temp$timestamp >= as.POSIXct('2005-02-17 14:00:00') &
                 wat_temp$timestamp <= as.POSIXct('2005-02-17 14:06:00')] <- NA

wat_temp$sst_c[wat_temp$timestamp >= as.POSIXct('2005-08-10 15:00:00') &
                 wat_temp$timestamp <= as.POSIXct('2005-08-10 15:06:00')] <- NA

wat_temp$sst_c[wat_temp$timestamp == as.POSIXct('2007-01-05 20:24:00')] <- NA

wat_temp$sst_c[wat_temp$timestamp >= as.POSIXct('2007-06-05 20:00:00') &
                 wat_temp$timestamp <= as.POSIXct('2007-06-05 20:42:00')] <- NA

wat_temp$sst_c[wat_temp$timestamp >= as.POSIXct('2009-03-13 14:54:00') &
                 wat_temp$timestamp <= as.POSIXct('2009-03-13 15:00:00')] <- NA

wat_temp$sst_c[wat_temp$timestamp >= as.POSIXct('2011-05-04 02:48:00') &
                 wat_temp$timestamp <= as.POSIXct('2011-05-04 03:30:00')] <- NA

wat_temp$sst_c[wat_temp$timestamp >= as.POSIXct('2011-05-04 04:48:00') &
                 wat_temp$timestamp <= as.POSIXct('2011-05-04 05:36:00')] <- NA

wat_temp$sst_c[wat_temp$timestamp >= as.POSIXct('2011-05-04 21:42:00') &
                 wat_temp$timestamp <= as.POSIXct('2011-05-04 23:48:00')] <- NA

wat_temp$sst_c[wat_temp$timestamp >= as.POSIXct('2011-05-05 05:48:00') &
                 wat_temp$timestamp <= as.POSIXct('2011-05-05 06:06:00')] <- NA

wat_temp$sst_c[wat_temp$timestamp >= as.POSIXct('2012-12-07 17:42:00') &
                 wat_temp$timestamp <= as.POSIXct('2012-12-07 19:06:00')] <- NA

wat_temp$sst_c[wat_temp$timestamp >= as.POSIXct('2012-12-08 23:54:00') &
                 wat_temp$timestamp <= as.POSIXct('2012-12-09 00:42:00')] <- NA

wat_temp$sst_c[wat_temp$timestamp >= as.POSIXct('2012-12-09 09:30:00') &
                 wat_temp$timestamp <= as.POSIXct('2012-12-10 14:18:00')] <- NA

wat_temp$sst_c[wat_temp$timestamp >= as.POSIXct('2012-12-22 09:24:00') &
                 wat_temp$timestamp <= as.POSIXct('2012-12-22 13:18:00')] <- NA

wat_temp$sst_c[wat_temp$timestamp >= as.POSIXct('2012-12-22 18:00:00') &
                 wat_temp$timestamp <= as.POSIXct('2012-12-22 19:42:00')] <- NA

wat_temp$sst_c[wat_temp$timestamp >= as.POSIXct('2013-01-21 06:54:00') &
                 wat_temp$timestamp <= as.POSIXct('2013-01-21 10:00:00')] <- NA

wat_temp$sst_c[wat_temp$timestamp >= as.POSIXct('2013-01-25 11:36:00') &
                 wat_temp$timestamp <= as.POSIXct('2013-01-25 12:48:00')] <- NA

wat_temp$sst_c[wat_temp$timestamp >= as.POSIXct('2013-03-04 13:30:00') &
                 wat_temp$timestamp <= as.POSIXct('2013-03-04 13:54:00')] <- NA

wat_temp$sst_c[wat_temp$timestamp >= as.POSIXct('2013-05-07 19:12:00') &
                 wat_temp$timestamp <= as.POSIXct('2013-05-07 19:36:00')] <- NA

wat_temp$sst_c[wat_temp$timestamp >= as.POSIXct('2013-05-11 12:18:00') &
                 wat_temp$timestamp <= as.POSIXct('2013-05-11 19:30:00')] <- NA

wat_temp$sst_c[wat_temp$timestamp >= as.POSIXct('2013-05-20 00:48:00') &
                 wat_temp$timestamp <= as.POSIXct('2013-05-20 01:24:00')] <- NA

wat_temp$sst_c[wat_temp$timestamp >= as.POSIXct('2019-02-18 00:00:00') &
                 wat_temp$timestamp <= as.POSIXct('2019-02-25 01:24:00') &
                 wat_temp$sst_c <=-0.5] <- NA

wat_temp$sst_c[wat_temp$timestamp >= as.POSIXct('2019-01-21 00:00:00') &
                 wat_temp$timestamp <= as.POSIXct('2019-01-28 01:24:00') &
                 wat_temp$sst_c <=-1.5] <- NA

# Create hourly collector
wat_temp$ts <- paste0(lubridate::year(wat_temp$timestamp), "-", 
                      lubridate::month(wat_temp$timestamp), '-',
                      lubridate::day(wat_temp$timestamp), '_',
                      lubridate::hour(wat_temp$timestamp))

# Force to numeric
wat_temp$sst_c <- as.numeric(wat_temp$sst_c)

# Check for outliers
hist(wat_temp$sst_c)
table(round(wat_temp$sst_c))
summary(wat_temp$sst_c)

# Save intermediate
clean.data <- wat_temp

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

# More date columns
wat_temp$doy <- lubridate::yday(wat_temp$timestamp)
wat_temp$year <- year(wat_temp$timestamp)
wat_temp$collector <- paste0(wat_temp$year, "_",
                             str_pad(wat_temp$doy, 3, 'left', '0'))

# Find "normal" daily temperature
# Period of reference 2003-2020
# Warming report uses 1991-2020, currently
norms <- wat_temp %>% 
  filter(year <=2020) %>% 
  group_by(collector) %>% 
  summarise(mean.daily = mean(hourly.temp, na.rm=TRUE),
            doy=mean(doy),
            year=mean(year))

# Smooth mean daily temperature using GAM
p <- ggplot(data=norms) +
  geom_smooth(aes(x=doy, y=mean.daily),
              method='gam', 
              se=T, 
              na.rm=TRUE,
              n=366, 
              method.args=list(method='REML')
              )

p + geom_point(data=norms, aes(x=doy, y=mean.daily),
               alpha=0.1)

tgam <- mgcv::gam(mean.daily ~ s(doy, bs='cs'), data=norms,
                  method='REML')
summary(tgam)
mgcv::gam.check(tgam)
mgcv::plot.gam(tgam, select=1, scheme=1, rug=T)
# This looks great

# Pull data from plot, append to new df
dat <- ggplot_build(p)$data[[1]]

daily.smooth <- data.frame(
  doy= seq(1, 366, 1)
)

daily.smooth$smooth.daily <- dat$y
daily.smooth$smooth.upper <- dat$ymax
daily.smooth$smooth.lower <- dat$ymin

# Timeperiod we care about: Nov 2013 onward
tp <- wat_temp[wat_temp$timestamp >= as.POSIXct('2013-11-01 00:00:00'),]
tp$year <- lubridate::year(tp$timestamp)
tp <- tp[!is.na(tp$timestamp),]

# Plot temperature as compared to norm
ggplot() +
  geom_point(data=tp,
            aes(x=doy, y=hourly.temp, col=as.factor(year)),
            alpha=0.1, stroke=NA) +
  scale_color_viridis_d(option='viridis', 'Year')+
  geom_ribbon(data=daily.smooth,
              aes(x=doy, ymin=smooth.lower, ymax=smooth.upper),
              fill='blue', alpha=0.3) +
  xlab('Day of year') + ylab('SST (C)')+
  guides(colour = guide_legend(override.aes = list(alpha = 1))) +
  geom_vline(xintercept=151, col='red', lty=2) +
  geom_vline(xintercept=273, col='red', lty=2)

# More date columns
wat_temp$dailyts <- ymd(wat_temp$timestamp)
wat_temp$dailyts <- zoo::na.locf(wat_temp$dailyts)

# Mean daily temperature
daily.temps <- wat_temp %>% 
  group_by(dailyts) %>% 
  summarise(daily.c = mean(hourly.temp, na.rm=TRUE))

# Join mean daily temp and norms
wat_temp <- left_join(wat_temp, daily.temps, by=c('dailyts'))

wat_temp <- left_join(wat_temp, dplyr::select(daily.smooth, doy, smooth.daily,
                                              smooth.upper, smooth.lower),
                      by=c('doy'))

# Calculate temperature anomaly
wat_temp$anomaly <- wat_temp$daily.c - wat_temp$smooth.daily
wat_temp$year <- lubridate::year(wat_temp$timestamp)

# Extract time period we care about
tp <- wat_temp[wat_temp$timestamp > as.POSIXct('2013-11-01 00:00:00'),]
tp$year <- lubridate::year(tp$timestamp)
tp <- tp[!is.na(tp$timestamp),]

tp$updif <- tp$smooth.upper - tp$smooth.daily
tp$lowdif <- tp$smooth.lower - tp$smooth.daily

# Plot
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

# Identify mean annual anomaly
tp$month <- month(tp$timestamp)
tp$season <- NA
tp$season[tp$month %in% c(3,4,5)] <- 'spring'
tp$season[tp$month %in% c(6,7,8)] <- 'summer'
tp$season[tp$month %in% c(9, 10, 11)] <- 'fall'
tp$season[tp$month %in% c(12, 1, 2)] <- 'winter'

tp$yearseason <- NA
tp$yearseason[tp$season %in% c('spring', 'summer', 'fall')] <- 
  tp$year[tp$season %in% c('spring', 'summer', 'fall')]
tp$yearseason[tp$month == 12] <- tp$year[tp$month == 12]
tp$yearseason[tp$month %in% c(1, 2)] <- 
  tp$year[tp$month %in% c(1, 2)] - 1

m.a.t <- tp %>% 
  group_by(yearseason, season) %>% 
  summarise(mean.anom = mean(anomaly, na.rm=TRUE)) %>% 
  as.data.frame() %>% 
  mutate(season = factor(season, levels=c('spring', 'summer', 'fall', 'winter')))

m.a.t$anom <- NA
m.a.t$anom[m.a.t$mean.anom>0] <- 'Above CRP'
m.a.t$anom[m.a.t$mean.anom<=0] <- 'Below CRP'

m.a.t <- m.a.t[-1,]
m.a.t <- m.a.t[with(m.a.t, order(mean.anom, decreasing = T)),]
rownames(m.a.t) <- NULL
m.a.t

ggplot(data=m.a.t) +
  geom_point(aes(x=yearseason, y=mean.anom, col=anom)) +
  facet_wrap(vars(season)) +
  coord_cartesian(ylim=c(-2, 2)) +
  labs(x='Year', y='Mean anomaly (C)', col='Deviation') +
  scale_x_continuous(breaks = scales::breaks_pretty())


# Assign temperature category by mean anomaly
tp$catper <- NA
tp$catper[tp$year %in% c(2014, 2015, 2017, 2018, 2019)] <- 'cold'
tp$catper[tp$year %in% c(2016, 2020, 2021, 2022, 2023, 2024)] <- 'hot'
tp$catper <- factor(tp$catper, levels = c('hot', 'cold'))

# Plot
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

# Save output
write.csv(tp, row.names = F,
          here('Clean_Data/Meteorological/Portland_watertemp.csv'))
