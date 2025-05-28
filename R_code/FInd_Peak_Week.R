rm(list=ls())

# Packages
library(rfishbase)
library(here)
library(tidyverse)

# Negate function
'%notin%' <- function(x,y)!('%in%'(x,y))

# Random seed for reproducibility
set.seed(123)

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
  filter(week >=24)

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

# Split into approx monthly increments
trips$datper <- trips$week

trips <- droplevels(trips)

# Save outcome-- number trips conducted at each site in each month
ntrips <- trips %>% 
  group_by(datper, year) %>% 
  summarise(ntrips = n()) %>% 
  as.data.frame()
summary(ntrips)
# 10 to 25 samples to characterize seasonal spatiotemp chars.

# Clean
abund <- abund %>% 
  # Remove QBC
  filter(site_id < 20) %>% 
  # Format date, keep only 'good' trips
  mutate(date = as.Date(date, format='%m/%d/%Y')) %>% 
  filter(loc_id %in% trips$loc_id) %>% 
  mutate(month = month(date)) %>% 
  filter(loc_id %in% trips$loc_id)

abund <- left_join(abund, dplyr::select(trips, loc_id, datper),
                   by=c('loc_id'))

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

# Find total catch per species per site per period
abund <- abund%>% 
  group_by(species_name, datper, year) %>% 
  summarise(catch = sum(catch)) %>% 
  arrange(datper, species_name) %>% as.data.frame()

abund <- unique(abund)

# Calcualte CPUE
abund <- left_join(abund, ntrips, by=c('datper', 'year'))
abund$cpue <- abund$catch / abund$ntrips
rm(trips, ntrips)

# Load data
dat <- read.csv(here('Clean_Data/seine_encountered_species.csv'))

# Clean
dat <- dat %>% 
  # Remove freshwater species
  filter(Salinity %notin% c('freshwater', 'freshwater-brackish')) %>% 
  # Remove non-fish
  filter(Common %notin% c('green crab', 'shoftfin squid'))

dat$Group[dat$Temperature %in% c('subpolar to polar',
                                 'temperate to polar',
                                 'temperate to subpolar')] <- 'Cold-adapted'
  
dat$Group[dat$Temperature %in% c('temperate')] <- 'Temperate'

dat$Group[dat$Temperature %in% c('subtropical to temperate',
                                 'tropical to subtropical',
                                 'tropical to temperate')] <- 'Warm-adapted'

dat$Group[dat$Temperature == 'subtropical to subpolar'] <- 'Temperate'

# Merge data
abund <- abund[abund$species_name %in% dat$Common,]
dat <- dat %>% 
  rename(species_name = Common) %>% 
  dplyr::select(species_name, Group)

abund <- left_join(abund, dat, by=c('species_name')) 

# Find peak week
abund <- split(abund, f=abund$species_name)

dat$Peakweek <- NA

newdat <- 
  data.frame(
    datper=seq(24, 41, by=1),
    year=2016
  )

for(i in 1:length(abund)){
  #if(nrow(abund[[i]])<6){next()}
  if(length(unique(abund[[i]]$datper))<6){next()}
  gam1 <- mgcv::gam(cpue ~ 
              s(datper, k=5) +
              s(year, k=5),
            data=abund[[i]])
  
  gam2 <- mgcv::predict.gam(gam1,
                            newdata=newdat,
                            exclude="s(year)",
                            type='link',
                            se.fit=TRUE)
  
  gam2 <- data.frame(
    fit = gam2$fit,
    se = gam2$se.fit,
    week = newdat$datper
  )
  
  peak <- gam2$week[gam2$fit == max(gam2$fit)]
  dat$Peakweek[dat$species_name == abund[[i]]$species_name[1]] <- peak
  
  rm(gam1, gam2, peak)
}
