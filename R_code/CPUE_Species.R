#### Green crab CPUE ####
rm(list=ls())

# Packages
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

# Load data
abun <- read.csv(here('Data/Clean_Data/Seine/abund_through_2025.csv'))
trips <- read.csv(here('Data/Clean_Data/Seine/trips_through_2025.csv'))

# Clean
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

# Clean
abun <- abun %>% 
  mutate(date = as.Date(date, format = '%m/%d/%Y')) %>% 
  mutate(site_name = trimws(site_name, 'both')) %>% 
  mutate(species_name = trimws(species_name, 'both')) %>% 
  mutate(week = isoweek(date),
         year = year(date)) %>% 
  filter(species_name == 'green crab') %>% 
  # filter(site_name %notin% c('Orrs Island','Snow Island',
  #                            'Long Point Cove', 'Stovers Point',
  #                            'Garrison Cove', 'Cedar Beach',
  #                            'Lowell Cove')) %>% 
  #filter(year != 2019) %>% 
  filter(week>=24 & week<=39) %>% 
  dplyr::select(year, week, site_name, catch, date)

# Merge
total <- left_join(trips, abun, by=c('year', 'week', 'date', 'site_name'))

# Weekly 
weekly <- total %>% 
  group_by(year, week) %>% 
  summarise(sets = n(),
            catch = sum(catch, na.rm = T))

# Yearly CPUE
cpue <- weekly %>% 
  group_by(year) %>% 
  summarise(sets = sum(sets),
            catch = sum(catch, na.rm=T)) %>% 
  mutate(cpue = catch/ sets)

ggplot(data = cpue) +
  geom_line(aes(x=year, y=cpue)) +
  geom_point(aes(x=year, y=cpue)) +
  labs(x='Year', y='CPUE (individuals/seine)')

write.csv(cpue,
          here('Data/Clean_Data/Seine/greencrab_cpue.csv'),
          row.names = F)
  