# Identify month-site groups with similar community composition
rm(list=ls())

# Random seed for reproducibility
set.seed(123)

library(here)
library(tidyverse)
library(vegan)
library(sf)

# Negate function
'%notin%' <- function(x,y)!('%in%'(x,y))

# Set GGplot auto theme
theme_set(theme(panel.grid.major = element_line(color='lightgray'),
                panel.grid.minor = element_blank(),
                panel.background = element_blank(),
                panel.border = element_rect(color='black', 
                                            linewidth=1, fill=NA),
                legend.position = "bottom",
                axis.text.x=element_text(size=12),
                axis.text.y=element_text(size=12),
                axis.title.x=element_text(size=12),
                axis.title.y=element_text(size=12, angle=90, 
                                          vjust=2),
                plot.title=element_text(size=14, hjust = 0, 
                                        vjust = 1.2),
                plot.caption=element_text(hjust=0, 
                                          face='italic', size=12)))

#Load abundance and trip data
abund <- read.csv(here('Clean_Data/Seine/abund_through_2024.csv'))
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

# Save outcome-- number trips conducted in each year
ntrips <- trips %>% 
  group_by(year) %>% 
  summarise(ntrips = n()) %>% 
  as.data.frame()
summary(ntrips)
# 41 to 87 samples to characterize yearly spatiotemp chars.

# Save environmental characteristics
schar <- trips %>% 
  group_by(year) %>% 
  summarise(temp = mean(temp_degc, na.rm=T),
            sal = mean(salinity_ppt, na.rm=T))
# No salinity samples in 2020 or 2023 due to instrument failure

# Clean
abund <- abund %>% 
  # Remove QBC
  filter(site_id < 20) %>% 
  # Format date, keep only 'good' trips
  mutate(date = as.Date(date, format='%m/%d/%Y')) %>% 
  filter(loc_id %in% trips$loc_id) %>% 
  mutate(month = month(date)) %>% 
  filter(loc_id %in% trips$loc_id)

# Get rid of other grouped (unidentified) species
abund <- abund %>% 
  filter(species_name %notin% c('hake spp', 
                                'killifish spp',
                                'river herring', 
                                'shiner spp', 
                                'stickleback spp',
                                'sturgeon spp', 
                                'sculpin spp',
                                'slimy sculpin'
  ))

# Add year back
abund <- abund %>% 
  mutate(year = year(date))

# Find most-caught species (by proportion of total catch)
comcat <- abund %>% 
  group_by(species_name) %>% 
  summarise(totcatch = sum(catch)) %>% 
  mutate(propcatch = (totcatch / sum(totcatch))* 100) %>% 
  arrange(propcatch) %>% as.data.frame()
tail(comcat, 10)

# Find most-encountered species
fishenc  <- as.data.frame(table(abund$species_name))
colnames(fishenc) <- c('species_name', 'encounters')
fishenc <- fishenc %>% 
  mutate(prop_enc = (fishenc$encounters /  nrow(trips))*100) %>% 
  arrange(prop_enc) %>% as.data.frame()
tail(fishenc, 10)

# Find fish encountered every year
fishyr <- abund %>% 
  dplyr::select(species_name, year) %>% 
  unique() %>% 
  group_by(species_name) %>% 
  summarise(nyear = n()) %>% 
  arrange(nyear) %>% as.data.frame()
tail(fishyr, 15)

# 10 species make up >99% of catch
# And are each caught in more than 1% of all encounters (7 ore more hauls)
# And are seen in 8/10 sampled years
usepec <- unique(
  abund$species_name[#abund$species_name %in% 
    #   comcat$species_name[comcat$propcatch >= 0.05] &
    abund$species_name %in% 
      fishenc$species_name[fishenc$prop_enc >= 1] &
      abund$species_name %in% 
      fishyr$species_name[fishyr$nyear >=8]
  ])
sum(comcat$propcatch[comcat$species_name %in% usepec])
usepec

# Others are excluded
abund <- abund[abund$species_name %in%
                 usepec,]

# Find total catch per species per site per period
abund <- abund%>% 
  group_by(species_name, year) %>% 
  summarise(catch = sum(catch)) %>% 
  arrange(species_name, year) %>% as.data.frame()

abund <- unique(abund)

# Calcualte CPUE
abund <- left_join(abund, ntrips, by=c('year'))
abund$cpue <- abund$catch / abund$ntrips
rm(comcat, fishenc, ntrips, usepec)

# Save
write.csv(abund,
          here('Clean_Data/Seine/CPUE_Top10.csv'),
          row.names = F)
