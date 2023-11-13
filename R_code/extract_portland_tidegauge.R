## Retrieve NOAA station meterological data ##
rm(list=ls())
library(tidyverse)
library(noaaoceans)

years <- seq(2014, 2023)
months <- seq(1, 12)
for(i in months){
  months[i] <- str_pad(months[i], 2, 'left', '0')
}

sdates <- NA

for(i in years){
  for(j in months){
    temp <- paste0(i,j,'01')
    sdates <- c(sdates, temp)
    rm(temp)
  }
}

sdates <- sdates[!is.na(sdates)]

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

edates <- edates[!is.na(edates)]

wat_temp <- query_coops_data(
  station_id = 8418150,
  start_date=sdates[1],
  end_date = edates[1],
  data_product = "water_temperature",
  units='metric',
  time_zone = 'lst_ldt'
)

for(i in 2:length(sdates)){
  quer <- query_coops_data(
    station_id = 8418150,
    start_date=sdates[i],
    end_date = edates[i],
    data_product = "water_temperature",
    units='metric',
    time_zone = 'lst_ldt'
  )
  wat_temp <- rbind(wat_temp, quer)
  rm(quer)
  print(i)
}

rm(years, sdates, edates, months, i, j)

wat_temp <- wat_temp %>% 
  dplyr::select(t, v) %>% 
  rename(timestamp = t,
         sst_c = v)

wat_temp$timestamp <- as.POSIXct(wat_temp$timestamp,
                                 format='%Y-%m-%d %H:%M')
wat_temp$year <- lubridate::year(wat_temp$timestamp)
wat_temp$month <- lubridate::month(wat_temp$timestamp)

wat_temp$summer.sst[wat_temp$month %in% c(5:9)] <- 
  wat_temp$sst_c[wat_temp$month %in% c(5:9)]

wat_temp$summer.sst <- as.numeric(wat_temp$summer.sst)

ggplot() +
  geom_line(data=wat_temp,
            aes(x=timestamp, y=summer.sst))

write.csv(wat_temp, row.names = F,
          here('Clean_Data/Meteorological/Portland_watertemp.csv'))
