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
years <- seq(2003, 2025)

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

## September 2025 has not finished yet
#sdates <- sdates[sdates != '20241201']
#edates <- edates[edates != '20241231']
edates[edates == '20250930'] <- '20250910'

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
tgam <- mgcv::gam(mean.daily ~ s(doy, bs='cs') + s(year, bs='cs'), 
                  data=norms,
                  method='REML')
summary(tgam)
mgcv::gam.check(tgam)
mgcv::plot.gam(tgam, select=1, scheme=1, rug=T, residuals = T)

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
tp <- wat_temp[wat_temp$timestamp > as.POSIXct('2013-12-01 00:00:00'),]
tp$year <- lubridate::year(tp$timestamp)
tp <- tp[!is.na(tp$timestamp),]

tp$updif <- tp$smooth.upper - tp$smooth.daily
tp$lowdif <- tp$smooth.lower - tp$smooth.daily

#tp <- tp %>% 
#  filter(year != 2025)

# Plot
ggplot() +
  geom_line(data=unique(dplyr::select(tp, 
                                       doy, year, smooth.daily)),
             aes(x=doy, y=smooth.daily),
             alpha=0.5) +
  
  geom_point(data=unique(dplyr::select(tp, 
                                       doy, daily.c, anomaly, year,
                                       updif, lowdif)),
             aes(x=doy, y=daily.c, col=as.factor(year)),
             alpha=0.5, stroke=NA) +
  scale_color_viridis_d(option='viridis', 'Year') +
  geom_ribbon(data=tp,
              aes(x=doy, ymin=smooth.lower, ymax=smooth.upper),
              fill='blue', alpha=0.3) +
  xlab('Day of year') + ylab('SST Anomaly (C)')+
  guides(colour = guide_legend(override.aes = list(alpha = 1))) +
  geom_vline(xintercept=151, col='red', lty=2) +
  geom_vline(xintercept=273, col='red', lty=2)

# Calculate mean average anomaly
tp$month <- month(tp$timestamp)
tp$day <- day(tp$timestamp)

tp$yearshift <- NA
tp$yearshift[tp$month %in% seq(1, 11)] <- tp$year[tp$month %in% seq(1, 11)]
tp$yearshift[tp$month ==12] <- tp$year[tp$month ==12]+1

tp$season[tp$month %in% seq(3, 5)] <- 'Spring'
tp$season[tp$month %in% seq(6, 8)] <- 'Summer'
tp$season[tp$month %in% seq(9, 11)] <- 'Fall'
tp$season[tp$month %in% c(12, 1, 2)] <- 'Winter'

m.a.t <- tp %>% 
  group_by(yearshift) %>% 
  summarise(mean.anom = mean(anomaly, na.rm=TRUE)) %>% 
  as.data.frame()

m.a.t$anom <- NA
m.a.t$anom[m.a.t$mean.anom>0] <- 'Above CRP'
m.a.t$anom[m.a.t$mean.anom<=0] <- 'Below CRP'

m.a.t <- m.a.t[m.a.t$yearshift <=2025 & m.a.t$yearshift >=2014,]
m.a.t <- m.a.t[with(m.a.t, order(mean.anom, decreasing = T)),]
rownames(m.a.t) <- NULL
m.a.t

m.a.t$ts <- as.Date(paste0(m.a.t$yearshift, '-06-02'))

ptp <- tp %>% 
  dplyr::select(dailyts, anomaly, smooth.daily) %>% 
  unique() %>% 
  as.data.frame()

ggplot() +
  geom_line(data=tp,
            aes(x=dailyts, y=smooth.daily),
            alpha=0.2) +
  geom_point(data = tp, 
             aes(x = dailyts, y=daily.c), 
             pch = 20,
             col='gray40', fill='gray40',
             alpha=0.2
             ) +
  geom_point(data=m.a.t, 
             aes(x=ts, y=mean.anom, fill=anom),
             cex=2,
             pch=21) +
  #coord_cartesian(ylim=c(-2, 2)) +
  labs(x='Year', y='Mean anomaly (C)', col='Deviation', 
       fill='Average Annual\nDeviation') +
  scale_x_date(breaks = c(as.Date('2013-12-01'), 
                          as.Date('2014-12-01'),
                          as.Date('2015-12-01'), 
                          as.Date('2016-12-01'),
                          as.Date('2017-12-01'), 
                          as.Date('2018-12-01'),
                          as.Date('2019-12-01'), 
                          as.Date('2020-12-01'),
                          as.Date('2021-12-01'), 
                          as.Date('2022-12-01'),
                          as.Date('2023-12-01'), 
                          as.Date('2024-12-01')
                          ), 
               date_labels = c('2014', '2015', '2016', '2017','2018','2019',
                               '2020', '2021', '2022', '2023', '2024', '2025')) +
  theme(strip.background = element_rect(fill='lightgray'),
        legend.box.margin = margin(-10, -10, -10, -10))

ggsave(plot = avg.t.anom,
       filename = here('Documentation/MEPS/Figures/Annual temperature anomalies.png'),
       width=169, height = 84.5, units='mm')

# Calc mean average summer anomaly


s.a.t <- tp %>% 
  #filter(month %in% seq(6, 8)) %>% 
  group_by(yearshift, season) %>% 
  summarise(mean.anom = mean(anomaly, na.rm=TRUE)) %>% 
  as.data.frame()

s.a.t$anom <- NA
s.a.t$anom[s.a.t$mean.anom>0] <- 'Above CRP'
s.a.t$anom[s.a.t$mean.anom<=0] <- 'Below CRP'

s.a.t <- s.a.t[s.a.t$yearshift >=2014,]
s.a.t <- s.a.t[with(s.a.t, order(mean.anom, decreasing = T)),]
rownames(s.a.t) <- NULL
s.a.t

s.a.t$season <- factor(s.a.t$season, levels = c('Winter', 'Spring', 'Summer', 'Fall'))

ggplot(data=s.a.t) +
  geom_point(aes(x=yearshift, y=mean.anom, col=anom)) +
  facet_wrap(vars(season)) +
  coord_cartesian(ylim=c(-2, 2)) +
  labs(x='Year', y='Mean anomaly (C)', col='Deviation') +
  scale_x_continuous(breaks = scales::breaks_pretty())

# Assign temperature category by mean anomaly
tp$catper <- NA
tp$catper[tp$year %in% c(2014, 2015, 2017, 2018, 2019)] <- 'cold'
tp$catper[tp$year %in% c(2016, 2020, 2021, 2022, 2023, 2024, 2025)] <- 'hot'
tp$catper <- factor(tp$catper, levels = c('hot', 'cold'))
# Five hottest years would exclude 2016

tp <- tp[tp$year != 2013,]

# Plot
ggplot() +
  geom_line(data=unique(dplyr::select(tp, 
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
          'C:/Users/Katie/Documents/GitHub/QBC_Monitoring_Reports/Portland_watertemp_Sep10.csv')
