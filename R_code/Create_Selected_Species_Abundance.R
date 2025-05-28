# Create data for Indices of Abundance

rm(list=ls())

library(tidyverse)
library(here)
library(mgcv)

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

#### Trip data ####
trips <- read.csv(here('Clean_Data/Seine/trips_through_2024.csv'))

# Clean
trips <- trips %>% 
  # Remove QBC
  filter(site_id < 20) %>% 
  # Format date, add time IDs
  mutate(date = as.Date(date, format = '%m/%d/%Y')) %>% 
  mutate(month = month(date)) %>% 
  mutate(week = isoweek(date)) %>% 
  mutate(day = day(date)) %>% 
  mutate(doy = yday(date)) %>% 
  # Remove 2019 and shoulder weeks
  filter(year != 2019) %>% 
  filter(week >=24) %>% 
  filter(week<=39)

# Remove instances where they measured by "scoops"
#trips <- trips[trips$loc_id != '2016_036_04',]
#trips <- trips[trips$loc_id != '2014_005_09',]

# Get rid of "bad" seines tho-- noted bad sets
trips <- trips %>% 
  filter(notes %notin% c(
    'very low tide, seine taken in about 1 ft of water',
    'site assumed, not indicated on sheet',
    'bad set',
    'tide moving too fast, seine set was flipped',
    'net snagged, probably released most of catch',
    'tide moving too fast, no fish, had to walk in seine net',
    'bad set, net did not get a chance to open up, mummichog outside of net',
    'no fish, bag tangled in low water'
  ))

# Fix substrate
sitesub <- data.frame(
  site_name = c('Alewife Cove', 
                'Audubon',
                'Back Cove', 
                'Cushing Island',
                'Great Diamond Island', 
                'Mackworth Island - Beach', 
                'Mackworth Island - North',
                'Mussel Cove', 
                'Presumpscot Moorings',
                'Skitterygusset',
                'SMCC',
                'The Brothers - North'),
  
  substrate = c('sand/gravel', # Alewife
                'sand', 
                'mud',         # Back Cove
                'sand', 
                'sand/gravel', # GDI
                'sand',
                'sand/gravel', # Mack North
                'mud', 
                'mud',         # Presumpscot
                'mud',
                'sand/gravel', # SMCC
                'sand/gravel'  # Brothers
                )
)
trips$substrate <- NULL
trips <- left_join(trips, sitesub, by=c('site_name'))
trips$substrate <- factor(trips$substrate,
                          levels=c('gravel', 'sand/gravel',
                                   'sand', 'mud/sand', 
                                   'mud/gravel',
                                   'mud'))

trips$site_name <- factor(trips$site_name,
                          levels=c('Presumpscot Moorings',
                                   'Skitterygusset',
                                   'Audubon',
                                   'Mussel Cove',
                                   'The Brothers - North',
                                   'Mackworth Island - North',
                                   'Mackworth Island - Beach',
                                   'Back Cove', 
                                   'Great Diamond Island',
                                   'Cushing Island',
                                   'SMCC',
                                   'Alewife Cove'))

# Fix weather
table(trips$weather)
trips$weather[trips$weather %in% c('cloudy', 'foggy',
                                   'mostly cloudy')] <- 'overcast'
trips$weather[trips$weather %in% c('mostly sunny',
                                   'partly cloudy')] <- 'partly cloudy'

badweather <- trips[trips$weather %in% c(NA, 'not recorded',
                                         'windy'),]
badweather <- trips[trips$date %in% badweather$date,]
table(badweather$date, badweather$weather)

trips$weather[trips$date == as.Date('2014-08-12')] <- 'overcast'
trips$weather[trips$date == as.Date('2022-07-12')] <- 'partly cloudy'
trips$weather[trips$date == as.Date('2022-07-25')] <- 'rain'

trips$weather[trips$date == as.Date('2015-08-25')] <- 'rain'
trips$weather[trips$date == as.Date('2015-09-01')] <- 'sunny'
trips$weather[trips$date == as.Date('2016-06-30')] <- 'sunny'
trips$weather[trips$date == as.Date('2016-09-07')] <- 'overcast'
trips$weather[trips$date == as.Date('2017-06-22')] <- 'sunny'
trips$weather[trips$date == as.Date('2020-06-25')] <- 'sunny'
trips$weather[trips$date == as.Date('2024-06-11')] <- 'partly cloudy'
trips$weather[trips$date == as.Date('2024-06-18')] <- 'partly cloudy'
trips$weather[trips$date == as.Date('2024-06-28')] <- 'sunny'
trips$weather[trips$date == as.Date('2024-07-02')] <- 'sunny'
trips$weather[trips$date == as.Date('2024-07-30')] <- 'overcast'
trips$weather[trips$date == as.Date('2024-10-09')] <- 'partly cloudy'

# There is no recorded weather for a few dates
# Will pull from historic records via weather underground
trips$weather[trips$date == as.Date('2016-07-20')] <- 'partly cloudy'

trips$weather[trips$date == as.Date('2018-08-21')] <- 'sunny'

trips$weather[trips$date == as.Date('2020-07-21')] <- 'sunny'
trips$weather[trips$date == as.Date('2020-07-22')] <- 'overcast'
trips$weather[trips$date == as.Date('2020-08-06')] <- 'partly cloudy'
trips$weather[trips$date == as.Date('2020-08-07')] <- 'partly cloudy'
trips$weather[trips$date == as.Date('2020-08-19')] <- 'partly cloudy'
trips$weather[trips$date == as.Date('2020-08-20')] <- 'overcast'
trips$weather[trips$date == as.Date('2020-09-03')] <- 'overcast'

trips$weather[trips$date == as.Date('2023-08-11')] <- 'partly cloudy'

# Foggy and rain = overcast
trips$weather[trips$weather %in% c('mostly sunny')] <- 'partly cloudy'
table(trips$weather)
trips[is.na(trips$weather),]

# Trim
trips <- trips %>% 
  mutate(weather = factor(weather, levels = c('sunny',
                                              'partly cloudy', 
                                              'overcast',
                                              'rain')))

# Has to have valid time set
trips$set_time_local <- as.POSIXct(trips$set_time_local,
                                   format = "%m/%d/%Y %H:%M")
trips <- trips %>% 
  filter(!is.na(set_time_local))

# Must have valid temperature
trips <- trips %>% 
  filter(!is.na(temp_degc))

#### Minute-based tides ####
tides <- read.csv(here('Raw_Data/Portland_tides_byminute.csv'))
tides <- tides %>% 
  mutate(timestamp = as.POSIXct(TimeLocal,
                                format = "%Y-%m-%d %H:%M:%S")) %>% 
  mutate(year = year(timestamp))

tides <- tides %>% 
  filter(year %in% trips$year) %>% 
  filter(month %in% trips$month) %>% 
  filter(day %in% day(trips$set_time_local)) %>% 
  rename(set_time = timestamp)

trips <- trips %>% 
  rename(set_time = set_time_local)

mistime <- trips[is.na(trips$set_time),]

trips <- left_join(trips,
                   dplyr::select(tides, set_time,
                                 TideHT.m, stage),
                   by=c('set_time'))

# Trim
trips <- trips %>% 
  dplyr::select(-site_id, -bay_location, 
                -set_time,
                -hermit_crabs,
                -shrimp)

#### Abundance ####
abund <- read.csv(here('Clean_Data/Seine/abund_through_2024.csv'))

# Clean
abund <- abund %>% 
  filter(loc_id %in% trips$loc_id) %>% 
  filter(species_name %in% c('atlantic herring',
                             #'atlantic tomcod',
                             'atlantic silverside'#,
                             #'winter flounder',
                             #'mummichog'
                             )) %>% 
  dplyr::select(loc_id, site_name, species_name, catch) %>% 
  pivot_wider(names_from = species_name,
              values_from = catch) %>% 
  as.data.frame()
abund[is.na(abund)] <- 0
colnames(abund) <- c('loc_id', 'site_name', 'silver', 'herring')

#### Join abundance and trips ####
dat <- merge(trips, abund, 
             by=c('loc_id', 'site_name'),
             all=T)
dat$silver[is.na(dat$silver)] <- 0
dat$herring[is.na(dat$herring)] <- 0

# Clean
dat <- dat %>% 
  dplyr::select(loc_id,
                year, month, week, date, site_name, 
                substrate, weather, stage,
                TideHT.m, temp_degc, salinity_ppt, do_mg.l,
                silver, herring,
                notes)

dat <- dat %>% 
  pivot_longer(cols = c(silver, herring),
               names_to = "species_name", 
               values_to = "catch") %>% 
  as.data.frame()

dat <- dat %>% 
  mutate(stage = factor(stage, levels =c('rising', 'falling')),
         month = factor(month),
         year = factor(year))

#### Final cleaning to ensure representative dataset ####
table(dat$year)
table(dat$month)
table(dat$month, dat$year)

# # Remove May, September, October and 2018, 2019, 2024 (too few)
# dat <- dat %>% 
#   filter(month %notin% c(5, 10) & 
#            year %notin% c(2019))
# 
# dat <- droplevels(dat)
# 
# table(dat$year, dat$week)
# 
# dat <- dat %>% 
#   filter(week <= 37)
# 
# dat <- droplevels(dat)
# 
# # Remove clearly broken salinity meter values
# hisal <- dat[!is.na(dat$salinity_ppt) & dat$salinity_ppt > 35,]
# 
# dat$salinity_ppt[!is.na(dat$salinity_ppt) & dat$salinity_ppt > 35] <- NA
# 
# # Remove clearly broken DO values
# dat$do_mg.l[dat$do_mg.l > 26] <- NA
# 
# # Check values of continuous numerics
# boxplot(dat$temp_degc[dat$species_name == 'herring'])
# boxplot(dat$salinity_ppt[dat$species_name == 'herring'])
# boxplot(dat$TideHT.m[dat$species_name == 'herring'])
# 
# # Check hi temps
# hitemp <- dat[dat$temp_degc > 26,]

# Clean workspace
rm(badweather, sitesub, tides)

write.csv(dat, 
          here('Clean_Data/selected_species_abundance2.csv'),
          row.names = F)
