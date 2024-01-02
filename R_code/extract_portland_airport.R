## Retrieve Portland airport meteorological data ##
rm(list=ls())

# Load packages
library(pmetar)
library(here)
library(tidyverse)

# Pull historical METAR data from PWM (Portland Jetport)
port.report <- metar_get_historical(
  airport="PWM",
  start_date = "1991-01-01",
  end_date = "2023-12-14",
  from="iastate"
)

# Decode text file
port.weather <- metar_decode(port.report,
                             metric=TRUE,
                             altimeter=TRUE,
                             check=TRUE)

# Select pertinent variables, rename
port.weather <- port.weather %>% 
  dplyr::select(METAR_Date, Wind_speed, Gust, Wind_direction,
                Temperature, Dew_point, Pressure, Visibility) %>% 
  rename(date = METAR_Date,
         ws = Wind_speed,
         gust = Gust,
         wd = Wind_direction,
         airtemp = Temperature,
         dp = Dew_point,
         pres = Pressure,
         vis = Visibility)

# Force to timestamp and local time
port.weather$date <- as.POSIXct(port.weather$date,
                                format=c('%Y-%m-%d %H:%M:%S'))
tz(port.weather$date) <- "UTC"
port.weather$localtime <- with_tz(port.weather$date, "America/New_York")
port.weather$localdate <- as.Date(port.weather$localtime)

# Force to numeric
port.weather <- port.weather %>% 
  mutate_at(c('wd', 'vis'), as.numeric)

# Add date columns for easier picking later
port.weather$month <- month(port.weather$date)
port.weather$month <- str_pad(port.weather$month, 2, 'left', '0')
port.weather$year <- year(port.weather$date)
port.weather$collector <- paste0(port.weather$year, "_",
                                 port.weather$month)

# Remove rows with invalid date information
port.weather <- port.weather[!is.na(port.weather$date),]
port.weather <- port.weather[port.weather$localtime >=
                               as.POSIXct('1991-01-01 00:00:00'),]

# Switch to using local time over GMT
port.weather <- port.weather %>% 
  dplyr::select(-date, -gust) %>% 
  rename(date=localtime)

# Save raw
raw.data <- port.weather

# Highlight possible bad values
port.weather$next.airtemp <- c(port.weather$airtemp[2:length(port.weather$airtemp)],
                               NA)
port.weather$dif <- abs(port.weather$next.airtemp -
                          port.weather$airtemp)

# Pull 3 days of data centered on bad points
badvals <- port.weather$date[port.weather$dif >=9]
badvals <- badvals[!is.na(badvals)]

badlist <- vector(mode='list', length=length(badvals))
for(i in 1:length(badlist)){
  badlist[[i]] <- badvals[i]
}

for(i in 1:length(badlist)
    #12
    ){
  
  start = badlist[[i]] - hours(48)
  end = badlist[[i]] + hours(48)
  
  badlist[[i]] <- 
    port.weather[port.weather$date >= start &
                 port.weather$date <= end,]
  
  print(
    ggplot(data=badlist[[i]])+
      geom_point(aes(x=date, y=airtemp)) +
      geom_vline(xintercept=badlist[[i]]$date[badlist[[i]]$dif >=6],
                 col='red') +
      ggtitle(paste0(badvals[i], ', ',
                     badlist[[i]]$dif[badlist[[i]]$date == 
                                        badvals[i]],
                     ' degree dif bt hours'))
  )
  
  rm(start, end)

}

port.weather$airtemp[as.Date(port.weather$date) == '2008-12-21' &
                     port.weather$airtemp >10] <- NA

port.weather$airtemp[as.Date(port.weather$date) == '2013-05-20' &
                       port.weather$airtemp < 0] <- NA

port.weather$airtemp[as.Date(port.weather$date) == '2020-03-31' &
                       port.weather$airtemp < -5] <- NA

port.weather$airtemp[as.Date(port.weather$date) == '2011-06-07' &
                       port.weather$airtemp > 30] <- NA

port.weather$airtemp[as.Date(port.weather$date) == '2006-07-02' &
                       port.weather$airtemp < 10] <- NA

port.weather$airtemp[port.weather$date >= as.POSIXct('1999-06-07 04:51:00')&
                     port.weather$date <= as.POSIXct('1999-06-08 16:00:00')] <- 
  NA

port.weather$airtemp[port.weather$date >= as.POSIXct('1993-05-09 08:00:00')&
                       port.weather$date <= as.POSIXct('1993-05-09 17:00:00')] <- 
  NA
#########
# Check for outliers
hist(port.weather$airtemp)
table(round(port.weather$airtemp))
boxplot(port.weather$airtemp)
summary(port.weather$airtemp)

# Save intermediate
clean.data <- port.weather

# Calculate mean hourly temp
port.weather$ts <- paste0(port.weather$localdate,
                          '_',
                          str_pad(hour(port.weather$date), 2, 'left', '0'))

port.weather <- port.weather %>% 
  group_by(ts) %>% 
  summarise(hourly.temp = mean(airtemp, na.rm=TRUE))

# Split timestamp back out
port.weather <- port.weather %>% 
  separate(ts, into=c('year', 'month', 'day'), '-')
port.weather <- port.weather %>% 
  separate(day, into=c('day', 'hour'), '_')

# Append leading zeros, combine
port.weather$timestamp <- paste0(
  port.weather$year, '-',
  str_pad(port.weather$month, 2, 'left', '0'), '-',
  str_pad(port.weather$day, 2, 'left', '0'), ' ',
  str_pad(port.weather$hour, 2, 'left', '0'), ':00'
)

# Force back to timestamp
port.weather$timestamp <- as.POSIXct(port.weather$timestamp,
                                 format="%Y-%m-%d %H:%M")

# Remove intermediates
port.weather <- port.weather %>% 
  dplyr::select(timestamp, hourly.temp)
port.weather <- port.weather[with(port.weather, order(timestamp)),]
rownames(port.weather) <- NULL

# More date columns
port.weather$doy <- lubridate::yday(port.weather$timestamp)
port.weather$year <- year(port.weather$timestamp)
port.weather$collector <- paste0(port.weather$year, "_",
                             str_pad(port.weather$doy, 3, 'left', '0'))

# Find "normal" daily temperature
# Period of reference 2003-2020
# Warming report uses 1991-2020, currently
norms <- port.weather %>% 
  filter(year <=2020) %>% 
  group_by(collector) %>% 
  summarise(mean.daily = mean(hourly.temp, na.rm=TRUE),
            doy=mean(doy),
            year=mean(year))

# Smooth mean daily temperature using GAM
p <- ggplot(data=norms) +
  geom_smooth(aes(x=doy, y=mean.daily),
              method='gam', se=T, na.rm=TRUE,
              n=366)

tgam <- mgcv::gam(mean.daily ~ s(doy, bs='cs'), data=norms)
summary(tgam)
gam.check(tgam)
# This looks great

# Pull data from plot, append to new df
dat <- ggplot_build(p)$data[[1]]

daily.smooth <- data.frame(
  doy= seq(1, 366, 1)
)

daily.smooth$smooth.daily <- dat$y
daily.smooth$smooth.upper <- dat$ymax
daily.smooth$smooth.lower <- dat$ymin

# Timeperiod we care about: 2014 onward
tp <- port.weather[port.weather$timestamp >= as.POSIXct('2014-01-01 00:00:00'),]
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
port.weather$dailyts <- ymd(port.weather$timestamp)
port.weather$dailyts <- zoo::na.locf(port.weather$dailyts)

# Mean daily temperature
daily.temps <- port.weather %>% 
  group_by(dailyts) %>% 
  summarise(daily.c = mean(hourly.temp, na.rm=TRUE))

# Join mean daily temp and norms
port.weather <- left_join(port.weather, daily.temps, by=c('dailyts'))

port.weather <- left_join(port.weather, dplyr::select(daily.smooth, doy, smooth.daily,
                                              smooth.upper, smooth.lower),
                      by=c('doy'))

# Calculate temperature anomaly
port.weather$anomaly <- port.weather$daily.c - port.weather$smooth.daily
port.weather$year <- lubridate::year(port.weather$timestamp)

# Extract time period we care about
tp <- port.weather[port.weather$timestamp > as.POSIXct('2014-01-01 00:00:00'),]
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
m.a.t <- tp %>% group_by(year) %>% summarise(mean.anom = mean(anomaly, na.rm=TRUE))
m.a.t <- m.a.t[with(m.a.t, order(mean.anom, decreasing = T)),]
rownames(m.a.t) <- NULL
m.a.t

# Assign temperature category by mean anomaly
tp$catper <- NA
tp$catper[tp$year %in% c(2014, 2015, 2017, 2018, 2019)] <- 'cold'
tp$catper[tp$year %in% c(2016, 2020, 2021, 2022, 2023)] <- 'hot'
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


write.csv(tp, 
          here('Clean_data/Meteorological/Portland_Met.csv'),
          row.names = F)
