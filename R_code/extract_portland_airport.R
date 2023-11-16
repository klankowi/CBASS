## Retrieve Portland airport meteorological data ##
rm(list=ls())

# Load packages
library(pmetar)
library(here)
library(tidyverse)

# Pull historical METAR data from PWM (Portland Jetport)
port.report <- metar_get_historical(
  airport="PWM",
  start_date = "2003-01-01",
  end_date = "2023-11-14",
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

# Summarize daily values
port.dailyweather <- port.weather %>% 
  group_by(localdate) %>% 
  summarise(ws = mean(ws, na.rm=TRUE),
            gust = mean(gust, na.rm=TRUE),
            wd = mean(wd, na.rm=TRUE),
            airtemp = mean(airtemp, na.rm=TRUE),
            dp = mean(dp, na.rm=TRUE),
            pres = mean(pres, na.rm=TRUE),
            vis = mean(vis, na.rm=TRUE))
port.dailyweather <- port.dailyweather[!is.na(port.dailyweather$localdate),]

port.dailyweather$year <- lubridate::year(port.dailyweather$localdate)
port.dailyweather$doy <- lubridate::yday(port.dailyweather$localdate)

# Detrend
norms <- port.dailyweather %>% 
  group_by(doy) %>% 
  summarise(mean.daily = mean(airtemp, na.rm=TRUE))
norms <- norms[!is.na(norms$mean.daily),]

# Smooth
p <- ggplot(data=norms) +
  geom_smooth(aes(x=doy, y=mean.daily),
              n=366)
dat <- ggplot_build(p)$data[[1]]
norms$smooth.daily <- dat$y
norms$smooth.upper <- dat$ymax
norms$smooth.lower <- dat$ymin

tp <- port.dailyweather[port.dailyweather$localdate > as.POSIXct('2014-01-01 00:00:00'),]
tp$year <- lubridate::year(tp$localdate)
tp <- tp[!is.na(tp$localdate),]

ggplot() +
  geom_point(data=tp,
             aes(x=doy, y=airtemp, col=as.factor(year)),
             alpha=0.1, stroke=NA) +
  scale_color_viridis_d(option='viridis', 'Year')+
  geom_line(data=norms,
            aes(x=doy, y=smooth.daily),
            col='blue', lwd=0.7) +
  geom_ribbon(data=norms,
              aes(x=doy, ymin=smooth.lower, ymax=smooth.upper),
              fill='blue', alpha=0.3) +
  xlab('Day of year') + ylab('Air Temp (C)')+
  guides(colour = guide_legend(override.aes = list(alpha = 1))) +
  geom_vline(xintercept=151, col='red', lty=2) +
  geom_vline(xintercept=273, col='red', lty=2)

port.dailyweather <- left_join(port.dailyweather, 
                               dplyr::select(norms, doy, smooth.daily,
                                              smooth.upper, smooth.lower),
                      by=c('doy'))
port.dailyweather$anomaly <- port.dailyweather$airtemp - port.dailyweather$smooth.daily

tp <- port.dailyweather[port.dailyweather$localdate > as.POSIXct('2014-01-01 00:00:00'),]
tp <- tp[!is.na(tp$localdate),]

tp$updif <- tp$smooth.upper - tp$smooth.daily
tp$lowdif <- tp$smooth.lower - tp$smooth.daily

ggplot() +
  geom_point(data=unique(dplyr::select(tp, 
                                       doy, airtemp, anomaly, year,
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
# This exactly matches.

tp$catper <- NA
tp$catper[tp$year %in% c(2014, 2015, 2017, 2018, 2019)] <- 'cold'
tp$catper[tp$year %in% c(2016, 2020, 2021, 2022, 2023)] <- 'hot'
tp$catper <- factor(tp$catper, levels = c('hot', 'cold'))

ggplot() +
  geom_point(data=unique(dplyr::select(tp, 
                                       doy, airtemp, anomaly, year,
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
