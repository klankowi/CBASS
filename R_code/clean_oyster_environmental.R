# Relating environmental factors to oyster index data

rm(list=ls())

# Load libraries
library(tidyverse)
library(here)
library(pmetar)

# Load oyster condition index data
oci <- read.csv(here('Clean_Data/OysterCI_CI.csv'))

# View
head(oci)

# Strip
oci <- select(oci,
              Date, Site, Index_1, Index._3)
colnames(oci) <- c('date', 'site', 'index_1', 'index_3')

# Clean
oci$date <- as.POSIXct(oci$date,
                       format = "%m/%d/%Y")
oci$site[oci$site == "Snow Island "] <- 'Snow'
oci$site[oci$site != "Snow"] <- 'Dogs'
oci$site <- as.factor(oci$site)

oci$index_1 <- as.numeric(oci$index_1)
summary(oci)
oci <- oci[!is.na(oci$index_1),]

# Load oyster environmental data
oenv <- read.csv(here('Clean_Data/OysterCI_Environment.csv'))

# View
head(oenv)

# Strip
oenv <- select(oenv, -Weather_condition, -Notes)
colnames(oenv) <- tolower(colnames(oenv))

# Clean
oenv$date <- as.POSIXct(oenv$date,
                        format="%m/%d/%Y")
oenv$site[oenv$site == 'Snow Island'] <- 'Snow'
oenv$site[oenv$site != 'Snow'] <- 'Dogs'
oenv$site <- as.factor(oenv$site)

summary(oenv)

# Merge
oysters <- merge(oci, oenv, by=c('date', 'site'))

# remove intermediates
rm(oci, oenv)

# Load water temperature data
wat.temp <- read.csv(here('Clean_Data/Portland_watertemp.csv'))

# View
head(wat.temp)

# Clean
wat.temp$date <- paste0(wat.temp$date, " ", wat.temp$time.gmt, ":00")
wat.temp$time.gmt <- NULL
wat.temp$date <- as.POSIXct(wat.temp$date,
                            format="%m/%d/%Y %H:%M:%S")
wat.temp$date <- wat.temp$date - (5 * 60 * 60)
wat.temp$temp.f <- as.numeric(wat.temp$temp.f)
wat.temp$temp.f[is.na(wat.temp$temp.f)][1] <- 66.1
wat.temp$temp.f[is.na(wat.temp$temp.f)][1] <- 65.9
wat.temp$wat.temp.c <- (5/9) * (wat.temp$temp.f - 32)
wat.temp$temp.f <- NULL
str(wat.temp)

# Group by day, average
wat.temp$day <- as.Date(wat.temp$date)
daily.wat.temp <- wat.temp %>% 
  group_by(wat.temp$day) %>% 
  summarise(daily.wat.temp = mean(wat.temp.c))
daily.wat.temp <- as.data.frame(daily.wat.temp)
colnames(daily.wat.temp) <- c('day', 'daily.wattemp')

# Load precipitation data
precip <- read.csv(here('Clean_Data/Harpswell_Precipitation.csv'))

# View
head(precip)

# Clean
precip <- select(precip, Date, Gauge.mm)
colnames(precip) <- tolower(colnames(precip))
precip$gauge.mm[precip$gauge.mm == "  T  "] <- 0.01
precip$gauge.mm <- as.numeric(precip$gauge.mm)
precip$date <- as.POSIXct(precip$date,
                          format='%m/%d/%Y')

# Load Wiscasset airport met data
dm <- metar_get_historical("KIWI", 
                           start_date = "2023-05-30", 
                           end_date = "2023-08-10", 
                           from = "iastate")

# View
met.data <- metar_decode(dm)
head(met.data)

# Strip
met.data <- as.data.frame(met.data)
colnames(met.data) <- tolower(colnames(met.data))
met.data <- select(met.data,
                   metar_date, wind_speed, wind_direction,
                   temperature, pressure, visibility)
colnames(met.data)[1] <- 'date'
str(met.data)
summary(met.data)

# Clean
met.data$date <- as.POSIXct(met.data$date,
                            format="%Y-%m-%d %H:%M:%S")
met.data <- met.data[!is.na(met.data$date),]

for(i in 1:nrow(met.data)){
  if(nchar(met.data$wind_direction[i]) > 3){
    met.data$wind_direction[i] <- substr(met.data$wind_direction[i],
                                         start=1, stop=4)
  }
}
summary(met.data$wind_direction)

met.data$wind_direction[met.data$wind_direction == 'Vari'] <- NA 
met.data2 <- met.data %>% 
  separate(wind_direction, into=c('wind_direction', 'trash'), ';')
met.data2 <- dplyr::select(met.data2, date, wind_direction)

met.data$wind_direction <- met.data2$wind_direction
rm(met.data2)
met.data$wind_direction <- as.numeric(met.data$wind_direction)
met.data$visibility <- as.numeric(met.data$visibility)
str(met.data)
summary(met.data)

# Daily average
met.data$day <- as.Date(met.data$date)
met.daily <- met.data %>% 
  group_by(day) %>% 
  summarise(daily.ws = mean(wind_speed, na.rm=T),
            daily.wd = mean(wind_direction, na.rm=T),
            daily.airtemp = mean(temperature, na.rm=T),
            daily.pres = mean(pressure, na.rm = T),
            daily.vis = mean(visibility, na.rm=T))

# Merge dailies
daily.environ <- merge(daily.wat.temp, met.daily, by=c('day'))
precip$day <- as.Date(precip$date)
precip$date <- NULL
daily.environ <- merge(daily.environ, precip, by=c('day'))
summary(daily.environ)

# Find rolling 7 day avg
weekly.environ <- daily.environ %>% 
  mutate(week.wattemp = zoo::rollapply(daily.wattemp, 7, fill=NA, na.rm=T, FUN=mean),
         week.ws = zoo::rollapply(daily.ws, 7, fill=NA, na.rm=T, FUN=mean),
         week.wd = zoo::rollapply(daily.wd, 7, fill=NA, na.rm=T, FUN=mean),
         week.airtemp = zoo::rollapply(daily.airtemp, 7, fill=NA, na.rm=T, FUN=mean),
         week.pres = zoo::rollapply(daily.pres, 7, fill=NA, na.rm=T, FUN=mean),
         week.vis = zoo::rollapply(daily.vis, 7, fill=NA, na.rm=T, FUN=mean),
         week.rain = zoo::rollapply(gauge.mm, 7, fill=NA, na.rm=T, FUN=mean))

# Save for Dora
write.csv(oysters,
          here('Clean_Data/merged_oyster_data.csv'),
          row.names = F)
write.csv(weekly.environ,
          here('Clean_Data/weekly_environment.csv'),
          row.names = F)
write.csv(daily.environ,
          here('Clean_Data/daily_environment.csv'),
          row.names = F)
