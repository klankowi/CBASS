rm(list=ls())

# Load packages
library(tidyverse)
library(here)
library(suntools)

#### Load silverside data ####
load(here('Clean_Data/Cohort_Data_2024.RData'))

# Trim data
ss <- cohort.1 %>% 
  filter(species_name == 'atlantic silverside')

rm(list=setdiff(ls(), c('ss')))

#### Load water temperature data ####
wat_temp <- read.csv(here('Clean_Data/Meteorological/Portland_watertemp.csv'))

# Clean data
wat_temp <- wat_temp %>% 
  mutate(date = as.Date(dailyts)) %>% 
  mutate(week = isoweek(date)) %>% 
  filter(month >= 5, year >=2014) %>% 
  dplyr::select(date, year, week, smooth.daily) %>% 
  unique() %>% 
  as.data.frame()

#### Load tidal data ####
# Set years to pull (Portland station reliable data record 2003-today)
years <- seq(2014, 2024)

# Set months
months <- seq(5, 9)
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

## NOAA buoy did not collect water temperature May 5-Jul5 2011
sdates <- sdates[sdates != '20110601']
edates <- edates[edates != '20110630']

## Jaunary 2025 has not finished yet
#sdates <- sdates[sdates != '20241201']
#edates <- edates[edates != '20241231']
edates[edates == '20250131'] <- '20250112'

# Remove NA end dates
edates <- edates[!is.na(edates)]

# Pull inital water temp query
tides <- query_coops_data(
  station_id = 8418150,
  start_date=sdates[1],
  end_date = edates[1],
  data_product = 'predictions',
  interval = 'hilo',
  datum = 'MLLW',
  units='metric',
  time_zone = 'lst_ldt'
)

# Pull remaining monthly queries
for(i in 2:length(sdates)){
  quer <- query_coops_data(
    station_id = 8418150,
    start_date=sdates[i],
    end_date = edates[i],
    data_product = 'predictions',
    interval = 'hilo',
    datum = 'MLLW',
    units='metric',
    time_zone = 'lst_ldt'
  )
  # Bind to initial
  tides <- rbind(tides, quer)
  rm(quer)
  print(i)
}

# Remove intermediates
rm(years, sdates, edates, months, i, j)

#### Load sun condition data ####
datevec <- 
# Sunrise: self explanatory
sunrises <- round_date(
  sunriset(
    matrix(c(-69.917007, 43.758762), nrow = 1),
    as.POSIXct(wat_temp$date, tz = "America/New_York"),
    direction = "sunrise",
    POSIXct.out = TRUE)$time,
  unit='1 minute'
)

# Sunset: self explanatory
sunsets <- round_date(
  sunriset(
    matrix(c(-69.917007, 43.758762), nrow = 1),
    as.POSIXct(wat_temp$date, tz = "America/New_York"),
    direction = "sunset",
    POSIXct.out = TRUE)$time,
  unit='1 minute'
)

light <- NA

for(i in 1:length(datevec)){
  print(i)
  temp <- seq.POSIXt(sunrises[i], sunsets[i], by="sec")
  light <- c(light, temp)
  rm(temp)
}
