rm(list=ls())

library(here)
library(tidyverse)
library(chron)

#### Load all data ####
# Through 2024
# Set the name of the workbook
fname <- paste0(here('Clean_Data/seine_compiled_clean.xlsx'))
# Get info about all excel sheet names in workbook
sheets <- readxl::excel_sheets(fname)
# Read in as list item, each item is a sheet
data.all <- invisible(lapply(sheets, 
                             function(x) readxl::read_excel(fname, sheet = x)))
names(data.all) <- sheets
# Coerce list items to dataframes
data.all <- lapply(data.all, as.data.frame)

rm(fname, sheets)

# 2024 GMRI
# Set the name of the workbook
fname <- paste0(here('Raw_Data/2024_Raw_Seine_Data.xlsx'))
# Get info about all excel sheet names in workbook
sheets <- readxl::excel_sheets(fname)
# Read in as list item, each item is a sheet
data.all.g2024 <- invisible(lapply(sheets, 
                             function(x) readxl::read_excel(fname, sheet = x)))
names(data.all.g2024) <- sheets
# Coerce list items to dataframes
data.all.g2024 <- lapply(data.all.g2024, as.data.frame)

rm(fname, sheets)

# 2024 QBC
# Load 2024 qbc data
trips <- read.csv(here('QBC_Intern_Projects/2024/Wrapup/2024_qbc_trips.csv'))
abund <- read.csv('C:/Users/klankowicz/Documents/GitHub/CBASS/QBC_Intern_Projects/2024/Wrapup/2024_qbc_abundance.csv')
length <- read.csv('C:/Users/klankowicz/Documents/GitHub/CBASS/QBC_Intern_Projects/2024/Wrapup/2024_qbc_length.csv')

# Clean trips
trips <- trips %>% 
  mutate(date = as.POSIXct(date, format='%m/%d/%Y'),
         week = isoweek(date),
         site_name = trimws(site_name),
         substrate = trimws(substrate),
         weather=trimws(weather),
         tide = trimws(tide),
         hermit_crabs = trimws(hermit_crabs),
         shrimp = trimws(shrimp)) %>% 
  dplyr::select(trip_id, date, week, set_time, site_name, site_id, substrate,
                weather, tide, time_of_high_tide,
                temp_degc,do_mg.l, salinity_ppt, hermit_crabs, shrimp)

for(i in 1:nrow(trips)){
  if(nchar(trips$set_time)[i] == 4){
    trips$set_time[i] <- str_pad(trips$set_time[i], 5, 'left', '0')
  }
  trips$set_time[i] <- paste0(trips$set_time[i], ':00')
  trips$time_of_high_tide[i] <- paste0(trips$time_of_high_tide[i], ':00')
}

trips$weather[trips$weather == 'cloudly/windly'] <- 'cloudy'

trips <- trips %>% 
  mutate(set_time = chron(times. = set_time, format='h:m:s'),
         time_of_high_tide = chron(times.= time_of_high_tide, format='h:m:s'))

# Clean abundance
abund <- abund %>% 
  mutate(site_name = trimws(site_name),
         species_name = trimws(species_name),
         species_name = tolower(species_name)) %>% 
  dplyr::select(trip_id, site_name, site_number, species_name, total_catch) %>% 
  rename(site_id = site_number)

# Clean length
length <- length %>% 
  dplyr::select(trip_id, site_name, site_number, species_name, sex, field_length_mm) %>% 
  mutate(species_name = trimws(species_name),
         species_name = tolower(species_name)) %>% 
  rename(site_id = site_number)

tripsdf <- data.frame(
  trip_id = trips$trip_id,
  date = trips$date
)
tripsdf <- unique(tripsdf)

abund <- merge(abund, tripsdf, by=c('trip_id'))
length <- merge(length, tripsdf, by=c('trip_id'))

data.all.q2024 <- vector('list', 3)
data.all.q2024[[1]] <- trips
data.all.q2024[[2]] <- abund
data.all.q2024[[3]] <- length
names(data.all.q2024) <- c('trips', 'abund', 'length')
rm(abund, length, trips, tripsdf, i)

#### Clean ####
# Trips
trips <- data.all$trips
data.all.g2024$trips <- data.all.g2024$trips %>% 
  mutate(loc_id = paste0(
  year(date), '_',
  str_pad(as.numeric(as.factor(date)), 3, 'left', '0'), '_',
  str_pad(site_id, 2, 'left', '0')),
  year = year(date),
  site_id = str_pad(site_id, 2, 'left', '0')) %>% 
  separate(set_time, into=c('shit', 'set_time'), sep = ' ') %>% 
  dplyr::select(-shit, -SPC, -do_pct) %>% 
  mutate(set_time = paste0(date, ' ', set_time),
         bay_location = NA)

trips <- rbind(trips, data.all.g2024$trips)

data.all.q2024$trips <- data.all.q2024$trips %>% 
  dplyr::select(-time_of_high_tide) %>% 
  mutate(bay_location = 'north',
         notes = NA,
         year = year(date),
         loc_id = paste0(
           year(date), '_',
           str_pad(as.numeric(as.factor(date)), 3, 'left', '0'), '_',
           str_pad(site_id, 2, 'left', '0'))) %>% 
  rename(`do_mg/l` = do_mg.l) %>% 
  dplyr::select(-trip_id, -week, -tide) %>% 
  mutate(set_time = paste0(date, ' ', set_time))

trips <- rbind(trips, data.all.q2024$trips)

trips$hermit_crabs[trips$hermit_crabs == 0] <- 'absent'
trips$hermit_crabs[trips$hermit_crabs == 1] <- 'present'
trips$shrimp[trips$shrimp == 0] <- 'absent'
trips$shrimp[trips$shrimp == 1] <- 'low'
trips$shrimp[trips$shrimp == 2] <- 'medium'
trips$shrimp[trips$shrimp == 3] <- 'high'

# Abundance
tripdf <- unique(dplyr::select(trips, loc_id, date, site_name, site_id))

abund <- data.all$abund

data.all.g2024$abund <- merge(data.all.g2024$abund,
                              tripdf, by=c('date', 'site_name'))
data.all.g2024$abund$bay_location <- NA
data.all.g2024$abund$notes <- NULL

abund <- rbind(abund, data.all.g2024$abund)

data.all.q2024$abund <- merge(data.all.q2024$abund,
                              tripdf, by=c('date', 'site_name', 'site_id'))

data.all.q2024$abund <- data.all.q2024$abund %>% 
  dplyr::select(-trip_id) %>% 
  rename(catch = total_catch) %>% 
  mutate(bay_location = 'north')

abund <- rbind(abund, data.all.q2024$abund)


# Lengths
length <- data.all$bio

data.all.g2024$bio <- merge(data.all.g2024$bio, tripdf,
                            by=c('date', 'site_name'))

data.all.g2024$bio <- data.all.g2024$bio %>% 
  mutate(bay_location = NA)

length <- rbind(length, data.all.g2024$bio)

data.all.q2024$length <- merge(data.all.q2024$length,
                               tripdf, by=c('date', 'site_name', 'site_id'))

data.all.q2024$length <- data.all.q2024$length %>% 
  mutate(notes = NA,
         bay_location = 'north') %>% 
  rename(length_mm = field_length_mm) %>% 
  dplyr::select(-trip_id)

length <- rbind(length, data.all.q2024$length)

rm(list=setdiff(ls(), c('abund', 'length', 'trips')))

# Fine tune
# Negate function
'%notin%' <- function(x,y)!('%in%'(x,y))
#### Join similar species, lengths ####
length <- length[
  length$species_name %notin% c('periwinkle',
                                      'horseshoe crab'),]

length$species_name[
  length$species_name %in% c('american eel',
                                   'glass eel elver')
] <- 'american eel'

length$species_name[
  length$species_name %in% c('hake')
] <- 'hake spp'

length$species_name[
  length$species_name %in% c('shortnose sturgeon',
                                   'unID sturgeon',
                             'sturgeon')
] <- 'sturgeon spp'

length$species_name[
  length$species_name %in% c( 'sculpin',
                                   'striped sculpin',
                                   'unID sculpin')
] <- 'sculpin spp'

length$species_name[
  length$species_name %in% c('atlantic herring',
                                   'herring')
] <- 'atlantic herring'

length$species_name[
  length$species_name %in% c('banded killifish',
                                   'killifish')
] <- 'killifish spp'

length$species_name[
  length$species_name %in% c('northern pipefish',
                                   'pipefish')
] <- 'northern pipefish'

length$species_name[
  length$species_name %in% c('white mullet',
                                   'mullet')
] <- 'white mullet'

length$species_name[
  length$species_name %in% c('emerald shiner',
                                   'golden shiner',
                                   'unID shiner')
] <- 'shiner spp'

length$species_name[
  length$species_name %in% c('rock gunnel',
                                   'unID gunnel')
] <- 'rock gunnel'

length$species_name[
  length$species_name %in% c(
                                   'unID stickelback')
] <- 'stickleback spp'

length$species_name[
  length$species_name %in% c('atlantic tomcod',
                            'tomcod')
] <- 'atlantic tomcod'

length$species_name[
  length$species_name %in% c('nine-spine stickleback',
                             'ninespine stickleback')
] <- 'ninespine stickleback'

length$species_name[
  length$species_name %in% c('three-spine stickleback',
                            'threespine stickleback')
] <- 'threespine stickleback'

#### Join similar species, abundance ####
abund <- abund[
  abund$species_name %notin% c('periwinkle',
                                        'horseshoe crab'),]

abund$species_name[
  abund$species_name %in% c('american eel',
                                     'glass eel elver')
] <- 'american eel'

abund$species_name[
  abund$species_name %in% c('hake')
] <- 'hake spp'

abund$species_name[
  abund$species_name %in% c('shortnose sturgeon',
                                     'unID sturgeon',
                            'sturgeon')
] <- 'sturgeon spp'

abund$species_name[
  abund$species_name %in% c('sculpin',
                                     'striped sculpin',
                                     'unID sculpin')
] <- 'sculpin spp'

abund$species_name[
  abund$species_name %in% c('atlantic herring',
                                     'herring')
] <- 'atlantic herring'

abund$species_name[
  abund$species_name %in% c('banded killifish',
                                     'killifish')
] <- 'killifish spp'

abund$species_name[
  abund$species_name %in% c('northern pipefish',
                                     'pipefish')
] <- 'northern pipefish'

abund$species_name[
  abund$species_name %in% c('white mullet',
                                     'mullet')
] <- 'white mullet'

abund$species_name[
  abund$species_name %in% c('emerald shiner',
                                     'golden shiner',
                                     'unID shiner')
] <- 'shiner spp'

abund$species_name[
  abund$species_name %in% c('rock gunnel',
                                     'unID gunnel')
] <- 'rock gunnel'

abund$species_name[
  abund$species_name %in% c(
                                     'unID stickelback')
] <- 'stickleback spp'

abund$species_name[
  abund$species_name %in% c('atlantic tomcod',
                            'tomcod')
] <- 'atlantic tomcod'

abund$species_name[
  abund$species_name %in% c('nine-spine stickleback',
                             'ninespine stickleback')
] <- 'ninespine stickleback'

abund$species_name[
  abund$species_name %in% c('three-spine stickleback',
                            'threespine stickleback')
] <- 'threespine stickleback'

table(abund$species_name)
table(length$species_name)

# Save output
write.csv(abund, here('Clean_Data/Seine/abund_through_2024.csv'), row.names = F)
write.csv(length, here('Clean_Data/Seine/lengths_through_2024.csv'), row.names = F)
write.csv(trips, here('Clean_Data/Seine/trips_through_2024.csv'), row.names = F)
