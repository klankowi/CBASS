rm(list=ls())

# Libraries
library(mgcv)
library(here)
library(tidyverse)
library(sf)

# Negate function
'%notin%' <- function(x,y)!('%in%'(x,y))

# Data
dat <- read.csv(here('Clean_Data/Diversity_Index_Calculation_Tides.csv'))

# View
head(dat)
glimpse(dat)

# Trim
dat <- dplyr::select(dat, 
                     date, site_name, bay_location,
                     week, month, year,
                     green.crab, tomcod, winter.flounder,
                     sandlance, mummichog, atlantic.silverside,
                     atlantic.herring, 
                     substrate, do_mg.l, salinity_ppt, temp_degc,
                     TideHT.m, numeric.tide,
                     shannon, simpson)

# View
str(dat)
dat$site_name <- as.factor(dat$site_name)
dat$bay_location <- as.factor(dat$bay_location)
dat$substrate <- as.factor(dat$substrate)

# Check site vs substrate (knowing it will change with tide)
table(dat$substrate, dat$site_name)

# Make decision table
substrate <- data.frame(
  site_name = c('Alewife Cove', 'Audubon', 'Back Cove','Cushing Island',
                'Great Diamond Island','Mackworth Island - Beach',
                'Mackworth Island - North', 'Mussel Cove',
                'Presumpscot Moorings','Skitterygusset', 'SMCC',
                'The Brothers - North', 'The Brothers - South'),
  substrate = c('sand/gravel', 'sand/mud','mud/gravel',
                'sand/mud','sand/gravel','sand/shell','sand/mud/gravel',
                'mud', 'mud/gravel','mud', 'sand/gravel',
                'gravel/shell', NA)
  
)

# Rebind
dat$substrate <- NULL
dat <- merge(dat, substrate, by=c('site_name'), all=T)

# Didn't save weather in diversity index 
# Load trips
trip <- read.csv(here('Clean_Data/trips_cleaned.csv'))
weather <- dplyr::select(trip, date, weather, set_time, site_name)
weather.list <- split(weather, f=weather$date)
for(i in 1:length(weather.list)){
  temp <- weather.list[[i]]
  if(length(table(temp$weather)) == 1 & anyNA(temp$weather)){
    temp$weather[is.na(temp$weather)] <- temp$weather[!is.na(temp$weather)][1]
  }
  if(length(table(temp$weather)) > 1 & anyNA(temp$weather)){
    temp$weather <- zoo::na.locf(temp$weather)
  }
  weather.list[[i]] <- temp
}
weather <- do.call(rbind, weather.list)
rownames(weather) <- NULL
weathernas <- weather[is.na(weather$weather),]

# What's left are days that have no weather observations.
# Can go back in weather record for Portland.
# Portland airport observations: 
# https://www.wunderground.com/history/daily/us/me/portland/KPWM/date/2020-9-3
# 30 June 2016: Sunny
# 20 July 2016: Partly cloudy

# 21 July 2020: Sunny
# 22 July 2020: Mostly cloudy
# 6 August 2020: Partly cloudy
# 7 August 2020: Mostly cloudy
# 19 August 2020: Mostly cloudy
# 20 August 2020: Sunny
# 3 September 2020: Cloudy

# 12 July 2022: Sunny
# 25 July 2022: Cloudy

weather$weather[weather$date == "2016-06-30"] <- 'sunny'
weather$weather[weather$date == "2016-07-20"] <- 'partly cloudy'

weather$weather[weather$date == "2020-07-21"] <- 'sunny'
weather$weather[weather$date == "2020-07-22"] <- 'overcast'
weather$weather[weather$date == "2020-08-06"] <- 'partly cloudy'
weather$weather[weather$date == "2020-08-07"] <- 'overcast'
weather$weather[weather$date == "2020-08-19"] <- 'overcast'
weather$weather[weather$date == "2020-08-20"] <- 'sunny'
weather$weather[weather$date == "2020-09-03"] <- 'cloudy'

weather$weather[weather$date == "2022-07-12"] <- 'sunny'
weather$weather[weather$date == "2022-07-25"] <- 'cloudy'

table(weather$weather)
weather[is.na(weather$weather),]

# Change 'overcast' to 'mostly cloudy'
weather$weather[weather$weather == 'overcast'] <- 'mostly cloudy'

# Remove intermediates
rm(substrate, temp, weather.list, weathernas, i)

# Adjust structure
str(weather)
weather$date <- as.Date(weather$date)
weather$weather <- as.factor(weather$weather)
weather$set_time <- as.POSIXct(weather$set_time,
                               format="%Y-%m-%d %H:%M:%S")
weather$site_name <- as.factor(weather$site_name)
summary(weather)

str(trip)
trip$date <- as.Date(trip$date)
trip$set_time <- as.POSIXct(trip$set_time,
                            format="%Y-%m-%d %H:%M:%S")
trip$site_name <- as.factor(trip$site_name)
trip$weather <- NULL

str(dat)
dat$date <- as.Date(dat$date)
dat$site_name <- as.factor(dat$site_name)

# Merge
trip <- merge(trip, weather, by=c('date', 'site_name', 'set_time'))
dat <- merge(dat, weather, by=c('date', 'site_name'))

# Save
write.csv(trip, 
          here('Clean_Data/trips_cleaned.csv'),
          row.names = F)
write.csv(dat,
          here('Clean_Data/Diversity_Index_Calculation.csv'),
          row.names = F)

