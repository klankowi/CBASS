# Identify month-site groups with similar community composition
rm(list=ls())

# Random seed for reproducibility
set.seed(123)

library(here)
library(tidyverse)

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
  # Remove 2019, 2024, and shoulder weeks
  filter(year != 2019) %>% 
  filter(week >=24) %>% 
  filter(week<=39)

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
                                'slimy sculpin',
                                'common sucker'
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

# Find most-encountered species
fishenc  <- as.data.frame(table(abund$species_name))
colnames(fishenc) <- c('species_name', 'encounters')
fishenc <- fishenc %>% 
  mutate(prop_enc = (fishenc$encounters /  nrow(trips))*100) %>% 
  arrange(prop_enc) %>% as.data.frame()

# Find fish encountered every year
fishyr <- abund %>% 
  dplyr::select(species_name, year) %>% 
  unique() %>% 
  group_by(species_name) %>% 
  summarise(nyear = n()) %>% 
  arrange(nyear) %>% as.data.frame()

sumcatch <- left_join(comcat, fishenc, by=c('species_name'))
sumcatch <- left_join(sumcatch, fishyr, by=c('species_name'))

sumcatch <- sumcatch %>% 
  mutate(propcatch = round(propcatch, 3),
         prop_enc = round(prop_enc, 2))

head(sumcatch)

colnames(sumcatch) <- c('Common name', 'Individuals caught', '% of total catch',
                        'Encounters', '% of encounters',
                        'Years encountered')

sumcatch$`Common name`[sumcatch$`Common name` == 'common dab'] <- 'american plaice'
sumcatch$`Common name`[sumcatch$`Common name` == 'eastern silver minnow'] <- 'eastern silvery minnow'
sumcatch$`Common name`[sumcatch$`Common name` == 'butterfish'] <- 'atlantic butterfish'
sumcatch$`Common name`[sumcatch$`Common name` == 'smelt'] <- 'rainbow smelt'

dat <- read.csv(here('Clean_Data/seine_encountered_species.csv'))
dat <- dat %>% 
  mutate(`Scientific name` = paste0(Genus, ' ', species)) %>% 
  rename(`Common name` = Common) %>% 
  dplyr::select(`Common name`, `Scientific name`)

sumcatch <- left_join(sumcatch, dat, by=c('Common name'))

write.csv(sumcatch, here('Documentation/MEPS/Catch Table.csv'), row.names = F)
