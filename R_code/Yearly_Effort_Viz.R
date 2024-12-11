rm(list=ls())

library(tidyverse)
library(here)

# Negate function
'%notin%' <- function(x,y)!('%in%'(x,y))

abund <- read.csv(here('Clean_Data/Seine/abund_through_2024.csv'))
trips <- read.csv(here('Clean_Data/Seine/trips_through_2024.csv'))
bio <- read.csv(here('Clean_Data/Seine/lengths_through_2024.csv'))

# Clean
abund <- abund %>% 
  mutate(month = month(date),
         year = year(date),
         week = isoweek(date))
abund$collector[abund$site_id >20] <- 'QBC'
abund$collector[abund$site_id <20] <- 'GMRI'

trips <- trips %>% 
  mutate(month = month(date),
         year = year(date),
         week = isoweek(date))
trips$collector <- NA
trips$collector[trips$site_id >20] <- 'QBC'
trips$collector[trips$site_id <20] <- 'GMRI'

bio <- bio %>% 
  mutate(month = month(date),
         year = year(date),
         week = isoweek(date))
bio$collector[bio$site_id >20] <- 'QBC'
bio$collector[bio$site_id <20] <- 'GMRI'

#### Yearly effort ####
effort <- trips %>% 
  group_by(collector, year, week) %>% 
  summarise(ntrips = n())

fulleff <- data.frame(
  year = rep(seq(2014, 2024, 1), 20)
)
fulleff <- as.data.frame(fulleff[with(fulleff, order(year)),])
colnames(fulleff) <- 'year'
fulleff$week <- rep(seq(22, 41, 1), 11)
fulleff$ntrips <- 0

fulleff <- rbind(fulleff, fulleff)
fulleff$collector[1:220] <- 'GMRI'
fulleff$collector[221:440] <- 'QBC'

fulleff$id <- paste0(fulleff$collector, '-', fulleff$year, '-', fulleff$week)


effort$id <- paste0(effort$collector, '-', effort$year, '-', effort$week)

fulleff <- fulleff[fulleff$id %notin% effort$id,]

effort <- rbind(effort, fulleff)

ggplot(data=effort) +
  geom_line(aes(x=week, y=ntrips, col=collector)) +
  facet_wrap(vars(year)) +
  labs(x='Week of year', y='Sites sampled') +
  scale_y_continuous(breaks = scales::breaks_pretty()) +
  ggtitle('Yearly effort')

rm(effort, fulleff)
