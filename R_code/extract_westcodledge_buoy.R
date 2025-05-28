rm(list=ls())

library(tidyverse)

# Negate function
'%notin%' <- function(x,y)!('%in%'(x,y))

years <- seq(2014, 2023, 1)

# Initialize
url <- "https://www.ndbc.noaa.gov/data/historical/stdmet/44007h2014.txt.gz"
tmp <- tempfile()
##
download.file(url,tmp)
##
data <- read.csv(
  gzfile(tmp),
  sep="",
  header=TRUE,
  stringsAsFactors=FALSE)
# data$mm <- 0
# data$WDIR <- NA
# data$PRES <- NA

for(i in years){
  print(i)
  # Next year
  url <- paste0("https://www.ndbc.noaa.gov/data/historical/stdmet/44007h",
                i, ".txt.gz")
  tmp <- tempfile()
  ##
  download.file(url,tmp)
  ##
  data2 <- read.csv(
    gzfile(tmp),
    sep="",
    header=TRUE,
    stringsAsFactors=FALSE)
  
  if(i == 2004){data2$mm <- 0}
  
  data <- rbind(data, data2)
  rm(url, tmp, data2)
}

qcmonths <- seq(1, 9, 1)
for(i in qcmonths){
  print(i)
  # Next year
  url <- paste0("https://www.ndbc.noaa.gov/data/stdmet/",
                month.abb[i], "/44007",
                i, "2024.txt.gz")
  tmp <- tempfile()
  ##
  download.file(url,tmp)
  ##
  data2 <- read.csv(
    gzfile(tmp),
    sep="",
    header=TRUE,
    stringsAsFactors=FALSE)
  
  data <- rbind(data, data2)
  rm(url, tmp, data2)
}

# October
url <- paste0("https://www.ndbc.noaa.gov/data/stdmet/",
              'Oct', "/44007",
              'a', "2024.txt.gz")
tmp <- tempfile()
##
download.file(url,tmp)
##
data2 <- read.csv(
  gzfile(tmp),
  sep="",
  header=TRUE,
  stringsAsFactors=FALSE)

data <- rbind(data, data2)

# November
url <- paste0("https://www.ndbc.noaa.gov/data/stdmet/",
              "Nov", "/44007",
              'b', "2024.txt.gz")
tmp <- tempfile()
##
download.file(url,tmp)
##
data2 <- read.csv(
  gzfile(tmp),
  sep="",
  header=TRUE,
  stringsAsFactors=FALSE)

data <- rbind(data, data2)

# December
url <- paste0("https://www.ndbc.noaa.gov/data/stdmet/",
              'Dec', "/44007",
              ".txt")
tmp <- tempfile()
##
download.file(url,tmp)
##
data2 <- read.csv(
  gzfile(tmp),
  sep="",
  header=TRUE,
  stringsAsFactors=FALSE)

data <- rbind(data, data2)

rm(data2, i, qcmonths, tmp, url, years)

data.units <- data[1:2,]

data <- data[-1,]

data <- data %>% 
  dplyr::select(X.YY, MM, DD, hh, mm, WTMP) %>% 
  mutate(WTMP = as.numeric(WTMP))
data$WTMP[data$WTMP == 999] <- NA

# 2003
url <- "https://www.ndbc.noaa.gov/data/historical/stdmet/44007h2003.txt.gz"
tmp <- tempfile()
download.file(url,tmp)

old <- read.csv(
  gzfile(tmp),
  sep="",
  header=TRUE,
  stringsAsFactors=FALSE)
old <- old %>% 
  dplyr::select(YYYY, MM, DD, hh, WTMP) %>% 
  mutate(mm = 0) %>% 
  rename(X.YY = YYYY)
data <- rbind(data, old)
rm(url, tmp, old)

# 2004
url <- "https://www.ndbc.noaa.gov/data/historical/stdmet/44007h2004.txt.gz"
tmp <- tempfile()
download.file(url,tmp)

old <- read.csv(
  gzfile(tmp),
  sep="",
  header=TRUE,
  stringsAsFactors=FALSE)
old <- old %>% 
  dplyr::select(YYYY, MM, DD, hh, WTMP) %>% 
  mutate(mm = 0) %>% 
  rename(X.YY = YYYY)
data <- rbind(data, old)
rm(url, tmp, old)

# 2005 - 2006
years <- seq(2005, 2006, 1)
for(i in years){
  print(i)
  # Next year
  url <- paste0("https://www.ndbc.noaa.gov/data/historical/stdmet/44007h",
                i, ".txt.gz")
  tmp <- tempfile()
  download.file(url,tmp)
  
  old <- read.csv(
    gzfile(tmp),
    sep="",
    header=TRUE,
    stringsAsFactors=FALSE)
  old <- old %>% 
    dplyr::select(YYYY, MM, DD, hh, mm, WTMP) %>% 
    rename(X.YY = YYYY)
  data <- rbind(data, old)
  rm(url, tmp, old)
}

# 2007 - 2013
years <- seq(2007, 2013, 1)
for(i in years){
  print(i)
  # Next year
  url <- paste0("https://www.ndbc.noaa.gov/data/historical/stdmet/44007h",
                i, ".txt.gz")
  tmp <- tempfile()
  download.file(url,tmp)
  
  old <- read.csv(
    gzfile(tmp),
    sep="",
    header=TRUE,
    stringsAsFactors=FALSE)
  old <- old %>% 
    dplyr::select(X.YY, MM, DD, hh, mm, WTMP)
  data <- rbind(data, old)
  rm(url, tmp, old)
}

data <- data %>% 
  mutate(ts = as.POSIXct(paste0(data$X.YY, '-', 
                                data$MM, '-',
                                data$DD, ' ',
                                data$hh, ':',
                                data$mm, ':00'),
                         format = '%Y-%m-%d %H:%M:%S')) %>% 
  mutate(WTMP = as.numeric(WTMP))

data$WTMP[data$WTMP == 999] <- NA

data <- data[with(data, order(ts)),]
rownames(data) <- NULL

data <- data %>% 
  rename(year = X.YY,
         month = MM,
         day = DD,
         hour = hh,
         min = mm,
         sst  = WTMP)

# Create time stamp
wat_temp <- data %>% 
  rename(timestamp = ts) %>% 
  filter(year != '#yr')

# Remove instances of missing timestamp
wat_temp <- wat_temp[!is.na(wat_temp$timestamp),]

# Create hourly collector
wat_temp$ts <- paste0(wat_temp$year, '-',
                      str_pad(wat_temp$month,2, 'left', '0'), '-',
                      str_pad(wat_temp$day, 2, 'left', '0'), ' ',
                      str_pad(wat_temp$hour, 2, 'left', '0'), 
                      ':00')

# Force to numeric
wat_temp$sst_c <- as.numeric(wat_temp$sst)

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
  separate(day, into=c('day', 'hour'), ' ')

# Append leading zeros, combine
wat_temp$timestamp <- paste0(
  wat_temp$year, '-',
  str_pad(wat_temp$month, 2, 'left', '0'), '-',
  str_pad(wat_temp$day, 2, 'left', '0'), ' ',
  wat_temp$hour
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

# More date columns
wat_temp$dailyts <- as.Date(wat_temp$timestamp - hours(5))

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

tp <- tp %>% 
  filter(year != 2025 & year != 2013)

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

m.a.t <- tp %>% 
  group_by(year) %>% 
  summarise(mean.anom = mean(anomaly, na.rm=TRUE)) %>% 
  as.data.frame()

m.a.t$anom <- NA
m.a.t$anom[m.a.t$mean.anom>0] <- 'Above CRP'
m.a.t$anom[m.a.t$mean.anom<=0] <- 'Below CRP'

m.a.t <- m.a.t[-1,]
m.a.t <- m.a.t[with(m.a.t, order(mean.anom, decreasing = T)),]
rownames(m.a.t) <- NULL
m.a.t

ggplot(data=m.a.t) +
  geom_point(aes(x=year, y=mean.anom, col=anom)) +
  coord_cartesian(ylim=c(-2, 2)) +
  labs(x='Year', y='Mean anomaly (C)', col='Deviation') +
  scale_x_continuous(breaks = scales::breaks_pretty())

# Assign temperature category by mean anomaly
tp$catper <- NA
tp$catper[tp$year %in% c(2017, 2014, 2019)] <- 'cold'
tp$catper[tp$year %in% c(2015, 2016, 2018, 2020:2024)] <- 'hot'
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
          here('Clean_Data/Meteorological/WCL_watertemp.csv'))
