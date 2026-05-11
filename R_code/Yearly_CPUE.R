# Total CPUE per year

rm(list=ls())

# Packges
library(here)
library(tidyverse)

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

# Negate function
'%notin%' <- function(x,y)!('%in%'(x,y))

# Load trips
trips <- read.csv(here('Data/Clean_Data/Seine/trips_through_2025.csv'))

# Clean trips
trips <- trips %>% 
  mutate(date = as.Date(date, format = '%m/%d/%Y')) %>% 
  mutate(site_name = trimws(site_name, 'both')) %>% 
  mutate(site_id = as.numeric(as.factor(site_name))) %>% 
  mutate(notes = trimws(notes, 'both')) %>% 
  filter(notes %notin% c('very low tide, seine taken in about 1 ft of water',
                         'tide moving too fast, seine set was flipped',
                         'tide moving too fast, no fish, had to walk in seine net',
                         'short set',
                         'not normal site, set on the point',
                         'no fish; inverted set',
                         'no fish, bag tangled in low water',
                         'net snagged, probably released most of catch',
                         'low tide, set not ideal',
                         'low tide, walked seine in',
                         'had to wait after plankton tow for tide to rise enough to reach the beach, net came in folded - lots of fish escaping, alewife in ziploc #1',
                         'had to hand haul the net',
                         'failed set',
                         'caught jellyfish, bag flipped on rock',
                         'bad set, net did not get a chance to open up, mummichog outside of net',
                         'bad set no fish',
                         'bad set'
  )) %>% 
  # filter(site_name %notin% c('Orrs Island','Snow Island',
  #                            'Long Point Cove', 'Stovers Point',
  #                            'Garrison Cove', 'Cedar Beach',
  #                            'Lowell Cove')) %>% 
  mutate(week = isoweek(date)) %>% 
  dplyr::select(year, week, site_name, date) %>% 
  #filter(year %notin% c(2019)) %>% 
  filter(week >=24) %>% 
  filter(week<=39)

# Trips per year
tpy <- trips %>% 
  group_by(year) %>% 
  summarise(ntrips = n())

# Load abund
abund <- read.csv(here('Data/Clean_Data/Seine/abund_through_2025.csv'))

# Fix species names
abund$species_name[abund$species_name %in%
                     c('blueback herring', 'river herring')] <- 'river herring'

abund$species_name[abund$species_name %in%
                     c('fourspine stickleback', 'ninespine stickleback',
                       'sticleback spp', 'threespine stickleback')] <- 
  'stickleback spp'

abund$species_name[abund$species_name %in%
                     c('grubby sculpin', 'slimy sculpin',
                       'longhorn sculpin', 'shorthorn sculpin',
                       'sculpin spp', 'sculpin')] <- 'sculpin spp'

abund$species_name[abund$species_name %in%
                     c('emerald shiner', 'golden shiner',
                       'killifish spp', 'eastern silver minnow')] <- 
  'freshwater minnows'

abund$species_name[abund$species_name %in%
                     c('hake', 'hake spp', 'red hake',
                       'white hake', 'silver hake', 'spotted hake')] <- 
  'hake spp'

abund$species_name[abund$species_name %in%
                     c('mummichog', 'striped mummichog')] <- 'mummichog'

abund$species_name[abund$species_name %in%
                     c('sturgeon', 'sturgeon spp')] <- 'sturgeon spp'

abund$species_name[abund$species_name %in%
                     c('rainbow smelt', 'smelt')] <- 'smelt'

abund$species_name[abund$species_name %in%
                     c('sandlance', 'sand lance')] <- 'sand lance'

# Group into top 10 and "Other"
abund$species_name[abund$species_name %notin%
                     c('alewife', 'atlantic herring',
                       'atlantic silverside', 'atlantic tomcod',
                       'northern pipefish', 'green crab',
                       'mummichog', 'bluefish',
                       'scupin spp', 'sand lance', 
                       'winter flounder')] <- 'Other'

# Clean abund
abund <- abund %>% 
  mutate(date = as.Date(date, format = '%m/%d/%Y')) %>% 
  mutate(site_name = trimws(site_name, 'both')) %>% 
  mutate(species_name = trimws(species_name, 'both')) %>% 
  mutate(week = isoweek(date),
         year = year(date)) %>%  
  # filter(site_name %notin% c('Orrs Island','Snow Island',
  #                            'Long Point Cove', 'Stovers Point',
  #                            'Garrison Cove', 'Cedar Beach',
  #                            'Lowell Cove')) %>% 
  #filter(year != 2019) %>% 
  filter(week>=24 & week<=39) %>% 
  mutate(catch = as.numeric(catch)) %>% 
  dplyr::select(year, week, site_name, species_name, catch, date) %>% 
  group_by(year, week, site_name, date, species_name) %>% 
  summarise(tcatch = sum(catch, na.rm =T))

# Merge
total <- left_join(trips, abund, by=c('year', 'week', 'date', 'site_name'))

# NA catch to 0
total$tcatch[is.na(total$tcatch)] <- 0

# Per year
py <- total %>% 
  group_by(year, species_name) %>% 
  summarise(scatch = sum(tcatch, na.rm=T))

py <- left_join(py, tpy, by=c('year'))

py$cpue <- py$scatch / py$ntrips

py$species_name <- str_to_sentence(py$species_name)

py <- py[!is.na(py$species_name),]

py$species_name <- factor(py$species_name,
                          levels=c('Atlantic herring',
                                   'Northern pipefish',
                                   'Green crab',
                                   'Bluefish',
                                   'Alewife',
                                   'Mummichog',
                                   'Winter flounder',
                                   'Atlantic tomcod',
                                   'Sand lance',
                                   'Atlantic silverside'))

ycpue <- ggplot(data=py) +
  geom_bar(aes(x=year, y=cpue, fill=species_name),
           stat='identity', col='gray90', lwd=0.05) +
  scale_fill_viridis_d(direction=-1,
                       na.value='gray50',
                       labels=c('Atlantic herring',
                                'Northern pipefish',
                                'Green crab',
                                'Bluefish',
                                'Alewife',
                                'Mummichog',
                                'Winter flounder',
                                'Atlantic tomcod',
                                'Sand lance',
                                'Atlantic silverside',
                                'Other')) +
  scale_x_continuous(limits=c(2013.5, 2025.5),
                     breaks=seq(2014, 2025, 1)) +
  labs(x='Year', y='CPUE', fill='Species') +
  ggtitle('Yearly total catch per unit effort') +
  theme(legend.position = 'right')

ggsave(ycpue,
       filename = here('Documentation/EIR_2025/Yearly_CPUE.png'),
       width = 2325, height = 1500, units='px')
