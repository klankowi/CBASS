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

# Geospatial data (seine sites and shoreline polygon)
sites <- read.csv(here('Clean_Data/Seine/sites_cleaned.csv'))
sites <- st_as_sf(sites, coords = c('longitude', 'latitude'),
                  crs= "EPSG:4326")
# Remove QBC
sites <- sites[sites$site_number < 20 & 
                 sites$bay_location != 'Fresh',]

shoreline <- st_read(here('GIS/us_medium_shoreline_poly.shp'),
                     quiet=T)
shoreline <- st_transform(shoreline, st_crs(sites))

## Load data
dat <- read.csv(here('Clean_Data/selected_species_abundance2.csv'))

# Remove instances of abnormal sampling
dat <-  dat %>% 
  filter(notes %notin%
           c('site assumed, not indicated on sheet'
             ))

#Load abundance and trip data
abund <- read.csv(here('Clean_Data/Seine/abund_through_2024.csv'))
trips <- read.csv(here('Clean_Data/Seine/trips_through_2024.csv'))

# Clean
trips <- trips %>% 
  # Remove QBC
  filter(site_id < 20) %>% 
  # Format date
  mutate(date = as.Date(date, format = '%m/%d/%Y')) %>% 
  mutate(month = month(date)) %>% 
  mutate(week = isoweek(date)) %>% 
  mutate(day = day(date)) %>% 
  mutate(doy = yday(date))

trips <- trips %>% 
  filter(notes %in% dat$notes) %>% 
  filter(year %in% c(2022, 2023)) %>%  #incomplete sampling
  filter(month %in% c(6,7,8))

# Split into two-week increments
trips$datper <-trips$month

# Check site coverage in each month
sitecov <- trips %>% 
  group_by(datper, site_name) %>% 
  summarise(ntrips = n())
siteeffort <- 
  ggplot(data=sitecov) +
  geom_tile(aes(x=datper, y=site_name, fill=as.factor(ntrips)),
            width = 0.9) +
    scale_fill_viridis_d() +
    labs(x='Month', y='Site', fill='Number\noftrips') +
  theme(legend.key.width = unit(0.5, 'in')) +
  ggtitle('Assessment of sampling effort')
ggsave(plot=siteeffort,
       here('Monthly effort for 2022 and 2023.png'),
       height = 7, width = 10, units='in')
rm(sitecov)


# Save outcome-- number trips conducted at each site in each month
ntrips <- trips %>% 
  group_by(site_name, datper) %>% 
  summarise(ntrips = n()) %>% 
  as.data.frame()
summary(ntrips)
# 5 to 22 samples to characterize monthly spatiotemp chars.

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

# Get rid of grouped (unidentified) species
abund <- abund %>% 
  filter(species_name %in% c('alewife'))

abund <- abund%>% 
  group_by(site_name, species_name, datper) %>% 
  summarise(catch = sum(catch)) %>% 
  arrange(site_name, datper, species_name) %>% as.data.frame()

abund <- unique(abund)

# Calculate CPUE
abund <- left_join(abund, ntrips, by=c('site_name', 'datper'))
abund$cpue <- abund$catch / abund$ntrips
rm(comcat, fishenc, ntrips, usepec)

# Append to site spatial information
sites <- sites %>%
  dplyr::select(-bay_location,
                -site_number)

sites <- left_join(sites, 
                   dplyr::select(abund,
                                 site_name, datper, cpue),
                   by=c('site_name'))  

# Remove sites that never caught alewife
sites <- sites[!is.na(sites$datper),]

# Rename months
sites$month <- NA
sites$month[sites$datper == 6] <- 'June'
sites$month[sites$datper == 7] <- 'July'
sites$month[sites$datper == 8] <- 'August'
sites$month <- factor(sites$month, levels=c('June', 'July', 'August'))

# Plot
sim <- ggplot() + 
  geom_sf(data=shoreline) +
  geom_sf(data = sites, 
          aes(size = cpue),
          alpha=0.4, col='red',
          ) +
  scale_size(
    breaks = c(1, 10, 100, 400, 800),
    range = c(1, 6),
    guide = "legend"
  ) +
  coord_sf(xlim=c(-70.3, -70.15),
           ylim=c(43.55, 43.75)) +
  facet_wrap(vars(month)) +
  labs(size='Average\nCPUE') +
  theme(legend.position = 'right',
        axis.text.x = element_blank(),
        axis.text.y=element_blank(),
        legend.margin = margin(0,0,0,0))

# View
sim

# Save
ggsave(plot=sim,
       here('Monthly map of alewife CPUE.png'),
       width = 7, height = 4, units='in')
