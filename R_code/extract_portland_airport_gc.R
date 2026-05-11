## Retrieve Portland airport meteorological data ##
rm(list=ls())

# Load packages
library(pmetar)
library(here)
library(tidyverse)

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


# Pull historical METAR data from PWM (Portland Jetport)
port.report <- metar_get_historical(
  airport="PWM",
  start_date = "1991-01-01",
  end_date = "2026-01-11",
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
                Temperature) %>% 
  rename(date = METAR_Date,
         ws = Wind_speed,
         gust = Gust,
         wd = Wind_direction,
         airtemp = Temperature) %>% 
  mutate(station = 'Portland')

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
  rename(date=localtime) %>% 
  dplyr::select(-localdate, -collector)

# Save raw
raw.data <- port.weather

# Highlight possible bad values
port.weather <- split(port.weather, f=port.weather$station)

for(k in 1:length(port.weather)){
  port.weather[[k]]$dif <- NA
  message(port.weather[[k]]$station[1])
  for(i in 2:nrow(port.weather[[k]])){
    if(i %in% seq(1000, 1134000, 1000)){print(i)}
    port.weather[[k]]$dif[i] <- port.weather[[k]]$airtemp[(i)] - port.weather[[k]]$airtemp[(i-1)]
  }
}
port.weather <- do.call(rbind, port.weather)
#port.weather$next.airtemp <- c(port.weather$airtemp[2:length(port.weather$airtemp)],
#                               NA)
#port.weather$dif <- abs(port.weather$next.airtemp -
#                          port.weather$airtemp)

# Pull 3 days of data centered on bad points
badvals <- port.weather$date[abs(port.weather$dif) >=9&
                               port.weather$station == 'Portland']
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
      geom_point(aes(x=date, y=airtemp, col=station)) +
      #geom_vline(xintercept=badlist[[i]]$date[abs(badlist[[i]]$dif) >=6],
      #           col='black') +
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

port.weather$airtemp[as.Date(port.weather$date) == '2024-01-20' &
                       port.weather$airtemp < -1] <- NA

port.weather$airtemp[as.Date(port.weather$date) == '2025-11-15' &
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

# Clean
port.weather <- port.weather %>% 
  mutate(year = year(date), month = month(date), 
         doy = yday(date)) %>% 
  filter(year>=1991) %>% 
  dplyr::select(date, year, month, doy, airtemp, ws, wd) %>% 
  mutate(wd = as.numeric(wd)) %>% 
  group_by(year, doy) %>% 
  mutate(daily.mean = mean(airtemp, na.rm=T),
         daily.min = min(airtemp, na.rm=T),
         daily.wd = mean(wd, na.rm=T),
         daily.ws = mean(ws, na.rm=T))

norms <- port.weather %>% 
  dplyr::select(year, doy, daily.mean, daily.wd, daily.ws) %>% 
  unique() %>% as.data.frame()

# Find "normal" daily temperature
# Period of reference 2003-2020
# Warming report uses 1991-2020, currently
# Smooth mean daily temperature using GAM
tgam <- mgcv::gam(daily.mean ~ 
                    s(doy, bs='cs') +
                    s(daily.wd, bs='cs') +
                    s(daily.ws, bs='cs') +
                    s(year, bs='cs', k=30),
                  data=norms[norms$year<=2020,],
                  select=T, method = 'REML')
summary(tgam)
plot(tgam, select=1, rug=T, residuals=T, scheme=1)
mgcv::gam.check(tgam)
# This looks great

# Pull data from plot, append to new df
daily.smooth <- data.frame(
  doy= seq(1, 366, 1),
  year = 2020,
  daily.wd = 0, daily.ws = 0
)

np <- mgcv::predict.gam(tgam, daily.smooth,
                        exclude = c("s(year)",
                                    "s(daily.wd)",
                                    "s(daily.ws)"),
                        se.fit=T)

daily.smooth$smooth.daily <- np$fit
daily.smooth$smooth.upper <- np$fit + np$se.fit
daily.smooth$smooth.lower <- np$fit - np$se.fit

# Merge
wat_temp <- left_join(port.weather, 
                      dplyr::select(daily.smooth, -year, -daily.wd, 
                                    -daily.ws), 
                      by=c('doy'))

wat_temp <- wat_temp %>% 
  dplyr::select(year, doy, daily.mean, daily.min,
                smooth.daily, 
                smooth.upper, smooth.lower) %>% 
  mutate(anomaly = daily.mean - smooth.daily) %>% 
  unique() %>% as.data.frame()

# Timeperiod we care about: 2014 onward
tp <- wat_temp

# Plot temperature as compared to norm
ggplot() +
  geom_line(data=tp[tp$year>=2014 & tp$year<=2025,],
             aes(x=doy, y=daily.mean, col=as.factor(year),
                 group = as.factor(year)),
             alpha=1, stroke=NA) +
  scale_color_viridis_d(option='viridis', 'Year')+
  geom_ribbon(data=tp[tp$year>=2014 & tp$year<=2025,],
              aes(x=doy, ymin=smooth.lower, ymax=smooth.upper),
              fill='blue', alpha=0.3) +
  xlab('Day of year') + ylab('Air Temp (C)')+
  guides(colour = guide_legend(override.aes = list(alpha = 1))) +
  geom_vline(xintercept=151, col='red', lty=2) +
  geom_vline(xintercept=273, col='red', lty=2) +
  theme(legend.position =  'right')


# Calculate temperature anomaly bounds
tp$updif <- tp$smooth.upper - tp$smooth.daily
tp$lowdif <- tp$smooth.lower - tp$smooth.daily

# Plot
ggplot() +
  geom_point(data=tp[tp$year>=2014 & tp$year<=2025,],
             aes(x=doy, y=anomaly, col=as.factor(year)),
             alpha=0.5, stroke=NA) +
  scale_color_viridis_d(option='viridis', 'Year') +
  geom_ribbon(data=tp[tp$year>=2014 & tp$year<=2025,],
              aes(x=doy, ymin=lowdif, ymax=updif),
              fill='blue', alpha=0.3) +
  xlab('Day of year') + ylab('Air Temp Anomaly (C)')+
  guides(colour = guide_legend(override.aes = list(alpha = 1))) +
  geom_vline(xintercept=151, col='red', lty=2) +
  geom_vline(xintercept=273, col='red', lty=2)

# Remove 2026
tp$date <-as.Date(tp$doy - 1, 
                  origin = paste0(tp$year, "-01-01"))
tp <- tp[tp$date <= as.Date('2025-11-30') &
           tp$date >= as.Date('1991-12-01'),]

# Days under -2C
tp$cold <- 'No'
tp$cold[tp$daily.mean<=-2] <- 'Yes'

# Shift to seasonal perspective
tp$month <- month(tp$date)
tp$season <- NA
tp$season[tp$month %in% c(3,4,5)] <- 'spring'
tp$season[tp$month %in% c(6,7,8)] <- 'summer'
tp$season[tp$month %in% c(9, 10, 11)] <- 'fall'
tp$season[tp$month %in% c(12, 1, 2)] <- 'winter'

tp$season <- factor(tp$season, levels=c('winter', 'spring',
                                        'summer', 'fall'))

tp$yearseason <- NA
tp$yearseason[tp$season %in% c('spring', 'summer', 'fall')] <- 
  tp$year[tp$season %in% c('spring', 'summer', 'fall')]
tp$yearseason[tp$month == 12] <- tp$year[tp$month == 12] + 1
tp$yearseason[tp$month %in% c(1, 2)] <- 
  tp$year[tp$month %in% c(1, 2)]

tpsplit <- split(tp, f=tp$yearseason)
for(i in 1:length(tpsplit)){
  tpsplit[[i]]$season.day <- NA
  tpsplit[[i]] <- tpsplit[[i]] %>% 
    arrange(date)
  tpsplit[[i]]$season.day <- seq(1, nrow(tpsplit[[i]]), 1)
}
tp <- do.call(rbind, tpsplit)
rownames(tp) <- NULL
tp <- tp %>% 
  arrange(date)

ggplot(data=tp[tp$yearseason>=2014 & tp$yearseason<=2025 &
                 tp$season.day<=130,]) +
  geom_tile(aes(x=season.day, 
                  y=yearseason), col=NA, fill=NA,
              alpha=1) +
  scale_fill_viridis_c() +
  geom_tile(data=tp[tp$yearseason>=2014 & tp$yearseason<=2025 &
                      tp$season.day<=130 & tp$cold == 'Yes',],
            aes(x=season.day, 
                y=yearseason, fill=daily.mean),
            alpha=1) +
  #scale_color_manual(values=c('gray80', 'royalblue')) +
  geom_vline(xintercept = c(90), lty=2, col='red') +
  #coord_cartesian(xlim=c(0, 130)) +
  labs(x='Day of year (Year starting Dec 1)', y='Year')

# Identify mean annual anomaly

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
  coord_cartesian(ylim=c(-3, 3)) +
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


write.csv(tp, 
          here('Clean_data/Meteorological/Portland_Met.csv'),
          row.names = F)
