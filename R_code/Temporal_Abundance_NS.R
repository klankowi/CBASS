
rm(list=ls())

library(here)
library(tidyverse)
library(shades)

# Negate function
'%notin%' <- function(x,y)!('%in%'(x,y))
# Set GGplot auto theme
theme_set(theme(panel.grid.major = element_line(color='lightgray'),
                panel.grid.minor = element_blank(),
                panel.background = element_blank(),
                panel.border = element_rect(color='black', linewidth=1, fill=NA),
                legend.position = "bottom",
                axis.text.x=element_text(size=12),
                axis.text.y=element_text(size=12),
                axis.title.x=element_text(size=14),
                axis.title.y=element_text(size=14, angle=90, vjust=2),
                plot.title=element_text(size=14, hjust = 0, vjust = 1.2),
                plot.caption=element_text(hjust=0, face='italic', size=12)))

# Load data
trips <- read.csv(here('Clean_Data/Seine/trips_through_2024.csv'))
trips <- trips %>% 
  mutate(date = as.Date(date, format = '%m/%d/%Y')) %>% 
  mutate(week = isoweek(date),
         year = year(date))
abund <- read.csv(here('Clean_Data/Seine/abund_through_2024.csv'))
abund$species_name[abund$species_name %in% c('threespine stickleback',
                                             'fourspine stickleback', 
                                             'ninespine stickleback', 
                                             'stickleback spp')] <- 
  'stickleback spp'

abund$species_name[abund$species_name %in% c('grubby sculpin',
                                             'longhorn sculpin',
                                             'sculpin spp',
                                             'shorthorn sculpin',
                                             'slimy sculpin')] <- 
  'sculpin spp'

abund$species_name[abund$species_name %in% c('hake spp',
                                             'red hake', 
                                             'white hake',
                                             'spotted hake')] <- 
  'hake spp'

# Determine which species have been caught
# Create a table that shows number of weeks in which each species was seen
abund <- abund %>% 
  mutate(date = as.Date(date, format = '%m/%d/%Y'),
         week = isoweek(date),
         year = year(date))

table(abund$species_name)

# Only use species we care about
species <- abund %>% 
  group_by(species_name, week, year) %>% 
  summarise(ntimes = n()) %>% 
  filter(ntimes > 1)

species <- unique(species$species_name)

abund <- abund[abund$species_name %in% species,]
abund <- unique(abund)
abund$date <- as.Date(abund$date, format='%m/%d/%Y')
abund$week <- isoweek(abund$date)
abund$year <- year(abund$date)

abund <- abund %>% 
  group_by(species_name, week, year) %>% 
  summarise(catch = sum(catch))

effort <- trips %>% 
  dplyr::select(week, year) %>% 
  group_by(week, year) %>% 
  summarise(ntrips = n())

poss <- rep(seq(2014, 2024, 1), 20)
poss <- poss[order(poss)]
poss <- as.data.frame(poss)
poss$week <- rep(seq(22, 41, 1), 11)
colnames(poss) <- c('year', 'week')
poss$ntrips <- NA

poss$id <- paste0(poss$year, poss$week)
effort$id <- paste0(effort$year, effort$week)

poss <- poss[poss$id %notin% effort$id,]

effort <- rbind(effort, poss)
effort <- effort[with(effort, order(year, week)),]
effort$id <- NULL

blank <- data.frame(species_name = NA, week = NA, year = NA, catch = NA,
                    ntrips=NA)

for(i in 1:length(species)){
  spec <- abund[abund$species_name == species[i],]
  specef <- effort %>% mutate(species_name = species[i])
  spec <- merge(spec, specef, by=c('year', 'week', 'species_name'), all=T)
  spec$catch[!is.na(spec$ntrips) & is.na(spec$catch)] <- 0
  
  blank <- rbind(blank, spec)
}
abundancedata <- blank[!is.na(blank$week) & !is.na(blank$species_name),]

fish.species <- species$species_name

yeartemps <- data.frame(
  year = seq(2014, 2024, 1),
  temp = c(rep('Cold', 2),
           'Hot',
           rep('Cold', 3),
           rep('Hot',5))
)

month.week <- data.frame(
  week= c(22.5, 26.5, 30.5, 34.5, 38.5),
  month=c('June','July','Aug','Sept', 'Oct')
)

for(i in 1:
    length(species)
    ){
  input.fish <- paste0(species[i])
  
  abun <- abundancedata[abundancedata$species == input.fish,]
  #abun$catch[is.na(abun$catch)] <- 0
  abun$logcatch <- log(abun$catch + 1)
  
  a <- ifelse(unique(abun$year) %in% c(2016, seq(2020,2024)), "red", 
              "blue")
  
  resras <- 
    ggplot() +
    
    geom_raster(data=abun,
                aes(y=as.factor(year),
                    x=week,
                    fill=logcatch#,
                    #alpha=as.factor(logcatch)
                )) +
    
    scale_fill_gradientn(colours = c(alpha('lightgray', 0.85),
                                     viridis::viridis(begin=0,
                                                      end=1,
                                                      n=999)),
                         na.value = 'transparent',
                         n.breaks=2,
                         breaks=c(0, max(abun$logcatch, na.rm=T)),
                         labels = c('Low', 'Hi')) +
    
    
    labs(x = 'Week of year',
         y = 'Year',
         fill= "Relative abundance") +
    
    ggtitle(paste0(str_to_sentence(input.fish), ' abundance through time')) +
    theme(axis.text.y = element_text(colour = a)) +
    scale_x_continuous(breaks=month.week$week + 0.5,
                       labels=month.week$month)
  
  print(resras)
  
  ggsave(resras,
         filename=paste0(here(), "/2024_Rasters/", input.fish, '.png'))
}

