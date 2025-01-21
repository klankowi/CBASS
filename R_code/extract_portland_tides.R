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

## Jaunary 2025 has not finished yet
#sdates <- sdates[sdates != '20241201']
#edates <- edates[edates != '20241231']
#edates[edates == '20250131'] <- '20250112'

# Remove NA end dates
edates <- edates[!is.na(edates)]

# Pull inital water temp query
wat_temp <- query_coops_data(
  station_id = 8418150,
  start_date=sdates[1],
  end_date = edates[1],
  data_product = "water_level",
  units='metric',
  datum = 'MLLW',
  time_zone = 'lst_ldt'
)

# Pull remaining monthly queries
for(i in 2:length(sdates)){
  quer <- query_coops_data(
    station_id = 8418150,
    start_date=sdates[i],
    end_date = edates[i],
    data_product = "water_level",
    units='metric',
    datum = 'MLLW',
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
         wat_level = v)

# Create time stamp
wat_temp$timestamp <- as.POSIXct(wat_temp$timestamp,
                                 format='%Y-%m-%d %H:%M')

# Create date columns
wat_temp$month <- month(wat_temp$timestamp)
wat_temp$month <- str_pad(wat_temp$month, 2, 'left', '0')
wat_temp$year <- year(wat_temp$timestamp)

# Remove instances of missing timestamp
wat_temp <- wat_temp[!is.na(wat_temp$timestamp),]

# Keep only sampling days
wat_temp <- wat_temp %>% 
  filter(year >= 2014 &
         year <= 2024) %>% 
  mutate(timestamp = as.POSIXct(timestamp),
         wat_level = as.numeric(wat_level))

# Create hourly collector
wat_temp$ts <- paste0(lubridate::year(wat_temp$timestamp), "-", 
                      lubridate::month(wat_temp$timestamp), '-',
                      lubridate::day(wat_temp$timestamp), '_',
                      lubridate::hour(wat_temp$timestamp))

# Check for outliers
hist(wat_temp$wat_level)
table(round(wat_temp$wat_level))
summary(wat_temp$wat_level)

# Save intermediate
clean.data <- wat_temp

write.csv(clean.data, row.names = F,
          here('Clean_Data/Meteorological/Portland_tides.csv'))
