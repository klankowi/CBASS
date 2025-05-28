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
  # Format date
  mutate(date = as.Date(date, format = '%m/%d/%Y')) %>% 
  mutate(month = month(date),
         year = year(date),
         week = isoweek(date)) %>% 
  mutate(set_time = as.POSIXct(set_time,
                               format = '%m/%d/%Y %H:%M')) %>% 
  filter(!is.na(set_time) &
           !is.na(temp_degc))

# Remove trips where the seine failed
trips <- trips %>% 
  filter(notes %notin% c(
    'bad set',
    'low tide, set not ideal',
    'bad set, net did not get a chance to open up, mummichog outside of net',
    'fish fleeing net, low tide and seine set on ledge',
    'net snagged, probably released most of catch',
    'no fish, bag tangled in low water',
    'no fish; inverted set',
    'tide moving too fast, seine set was flipped',
    'too much algae for effective seining',
    'very low tide, seine taken in about 1 ft of water',
    "had to wait after plankton tow for tide to rise enough to reach the beach, net came in folded - lots of fish escaping, alewife in ziploc #1" 
  ))

# Remove weeks with inconsistent sampling
trips <- trips %>% 
  filter(week >=24 & week <=34)

# Remove months with inconsistent sampling
trips <- trips %>% 
  filter(month %in% seq(6, 8, 1)) 

# Remove years with inconsistent sampling
trips <- trips %>% 
  filter(trips$year %notin% seq(2017,2020,1)) %>% 
  filter(year != 2024)

# Remove sites with inconsistent sampling
trips <- trips %>% 
  filter(site_name %notin% c('Mackworth Island - Beach',
                             'The Brothers - North',
                             'SMCC'))
# Fix substrate
sitesub <- data.frame(
  site_name = c('Alewife Cove', 'Audubon',
                'Back Cove', 'Cushing Island',
                'Great Diamond Island', 'Mackworth Island - North',
                'Mussel Cove', 'Presumpscot Moorings',
                'Skitterygusset'),
  substrate = c('sand', 'sand', 'mud', 'sand', 'sand',
                'mud', 'mud', 'mud', 'mud')
)
trips$substrate <- NULL
trips <- left_join(trips, sitesub, by=c('site_name'))
trips$substrate <- factor(trips$substrate,
                          levels=c('sand', 
                                   'mud'))

trips$site_name <- factor(trips$site_name,
                          levels=c('Presumpscot Moorings',
                                   'Skitterygusset',
                                   'Audubon',
                                   'Mussel Cove',
                                   'Mackworth Island - North',
                                   'Back Cove', 
                                   'Great Diamond Island',
                                   'Cushing Island',
                                   'Alewife Cove'))

# Fix weather
table(trips$weather)
badweather <- trips[trips$weather %in% c(NA, 'not recorded',
                                         'windy'),]
badweather <- trips[trips$date %in% badweather$date,]
table(badweather$date, badweather$weather)

trips$weather[trips$date == as.Date('2014-06-27')] <- 'sunny'
trips$weather[trips$date == as.Date('2014-08-12')] <- 'foggy'
trips$weather[trips$date == as.Date('2022-07-12')] <- 'partly cloudy'
trips$weather[trips$date == as.Date('2022-07-25')] <- 'rain'

# There is no recorded weather for two days
# Will pull from historic records via weather underground
trips$weather[trips$date == as.Date('2016-07-20')] <- 'partly cloudy'
trips$weather[trips$date == as.Date('2023-08-11')] <- 'partly cloudy'

# Foggy and rain = overcast
trips$weather[trips$weather %in% c('rain', 'foggy')] <- 'overcast'
trips$weather[trips$weather %in% c('mostly sunny')] <- 'partly cloudy'
table(trips$weather)

# Adjust seine time from UTC to local
trips$set_time <-  trips$set_time - hours(4)

# Trim
trips <- trips %>% 
  mutate(weather = factor(weather, levels = c('sunny',
                                              'partly cloudy', 
                                              'overcast'))) %>% 
  filter(!is.na(salinity_ppt))

#### Minute-based tides ####
tides <- read.csv(here('Raw_Data/Portland_tides_byminute.csv'))
tides <- tides %>% 
  mutate(timestamp = as.POSIXct(TimeLocal,
                                format = "%Y-%m-%d %H:%M:%S")) %>% 
  mutate(year = year(timestamp))

tides <- tides %>% 
  filter(year %in% trips$year) %>% 
  filter(month %in% trips$month) %>% 
  filter(day %in% day(trips$set_time)) %>% 
  rename(set_time = timestamp)

trips <- left_join(trips,
                   dplyr::select(tides, set_time,
                                 TideHT.m, stage),
                   by=c('set_time'))

# Trim
trips <- trips %>% 
  dplyr::select(-site_id, -bay_location, 
                -set_time,
                -do_mg.l, -hermit_crabs,
                -shrimp, -notes)

#### Abundance ####
abund <- read.csv(here('Clean_Data/Seine/abund_through_2024.csv'))

# Clean
abund <- abund %>% 
  filter(loc_id %in% trips$loc_id) %>% 
  filter(species_name %in% c('atlantic herring',
                             'atlantic tomcod',
                             'atlantic silverside',
                             'winter flounder',
                             'mummichog')) %>% 
  dplyr::select(loc_id, site_name, species_name, catch) %>% 
  pivot_wider(names_from = species_name,
              values_from = catch) %>% 
  as.data.frame()
abund[is.na(abund)] <- 0
colnames(abund) <- c('loc_id', 'site_name', 'tomcod', 'flounder',
                     'mummi', 'silver', 'herring')

#### Join abundance and trips ####
dat <- merge(trips, abund, 
             by=c('loc_id', 'site_name'),
             all=T)
dat[is.na(dat)] <- 0

dat <- dat %>% 
  dplyr::select(year, substrate, weathr, stage, month, site_name,
                TideHT.m, tomcod, flounder, mummi, silver, herring)

dat <- dat %>% 
  pivot_longer(cols = c(tomcod, flounder, mummi, silver, herring),
               names_to = "species_name", 
               values_to = "catch") %>% 
  as.data.frame()

dat <- dat %>% 
  mutate(stage = factor(stage, levels =c('rising', 'falling')))

#### Data explore, tomcod ####
gam.silver <- mgcv::gam(formula = log(catch + 1) ~ 
                          s(temp_degc, bs='tp') +
                          #s(TideHT.m, bs='tp') +
                          #s(salinity_ppt, bs='tp') +
                          #substrate + #weather + #stage +
                          site_name + month + year,
                         
                        data=dat[dat$species_name == 'tomcod',],
                        select = T,
                        method = 'GCV',
                        family = nb(link = 'log'))
summary(gam.silver)

p_obj <- plot(gam.silver, all=T, scheme = 1, 
              residuals = T, pages = 1)

temp <- p_obj[[1]] # just one smooth so select the first component
sm_df <- as.data.frame(temp[c("x", "se", "fit")])
sm_df$var <- 'Temperature (C)'

data_df <- as.data.frame(temp[c("raw", "p.resid")])
colnames(data_df) <- c('raw', 'p.resid')
data_df$year <- dat$year[dat$species_name == 'silver']
data_df$var <- 'Temperature (C)'

ddf <- data_df
sdf <- sm_df

## plot
ggplot(sm_df, aes(x = x, y = fit)) +
  geom_ribbon(aes(ymin = fit - se, ymax = fit + se, y = NULL),
              alpha = 0.3) +
  geom_point(data=data_df,
             aes(x=raw, y=p.resid, col=year)) +
  scale_color_viridis_c() +
  geom_line() +
  labs(x='Temperature', 
       y='Effect on Catch',
       col='Year') +
  theme(legend.position = 'right',
        legend.margin = margin(0,0,0,0))


sn <- p_obj[[2]] # just one smooth so select the first component
sm_df <- as.data.frame(sn[c("x", "se", "fit")])
sm_df$var <- 'Salinity (ppt)'

data_df <- as.data.frame(sn[c("raw", "p.resid")])
colnames(data_df) <- c('raw', 'p.resid')
data_df$year <- dat$year[dat$species_name == 'silver']
data_df$var <- 'Salinity (ppt)'

ddf <- rbind(ddf, data_df)
sdf <- rbind(sdf, sm_df)

## plot
ggplot(sm_df, aes(x = x, y = fit)) +
  geom_ribbon(aes(ymin = fit - se, ymax = fit + se, y = NULL),
              alpha = 0.3) +
  geom_point(data=data_df,
             aes(x=raw, y=p.resid, col=year)) +
  scale_color_viridis_c() +
  geom_line() +
  labs(x='Salinity', 
       y='Effect on catch per unit effort') +
  theme(legend.position = 'none',
        legend.margin = margin(0,0,0,0))

st <- p_obj[[3]] # just one smooth so select the first component
sm_df <- as.data.frame(sn[c("x", "se", "fit")])
sm_df$var <- 'Salinity (ppt)'

data_df <- as.data.frame(sn[c("raw", "p.resid")])
colnames(data_df) <- c('raw', 'p.resid')
data_df$year <- dat$year[dat$species_name == 'silver']
data_df$var <- 'Salinity (ppt)'

ddf <- rbind(ddf, data_df)
sdf <- rbind(sdf, sm_df)

## plot
ggplot(sm_df, aes(x = x, y = fit)) +
  geom_ribbon(aes(ymin = fit - se, ymax = fit + se, y = NULL),
              alpha = 0.3) +
  geom_point(data=data_df,
             aes(x=raw, y=p.resid, col=year)) +
  scale_color_viridis_c() +
  geom_line() +
  labs(x='Salinity', 
       y='Effect on catch per unit effort') +
  theme(legend.position = 'none',
        legend.margin = margin(0,0,0,0))


## plot
gamp <- ggplot() +
  geom_line(data=sdf, aes(x = x, y = fit)) +
  geom_ribbon(data=sdf,
              aes(ymin = fit - se, ymax = fit + se, x =x,
                  y=NULL),
              alpha = 0.3) +
  geom_point(data=ddf,
             aes(x=raw, y=p.resid, col=as.factor(year)),
             alpha = 0.4) +
  scale_color_viridis_d() +
  facet_wrap(vars(var), scales='free_x', ncol=1) +
  labs(
       y='Effect on catch per unit effort', 
       color='Year') +
  theme(legend.position = 'right',
        axis.title.x = element_blank(),
        legend.margin = margin(0,0,0,0)) +
  ggtitle('Environmental effects on silverside catch')
gamp

sn <- p_obj[[2]] # just one smooth so select the first component
sm_df <- as.data.frame(sn[c("x", "se", "fit")])
sm_df$var <- 'Salinity (ppt)'

data_df <- as.data.frame(sn[c("raw", "p.resid")])
colnames(data_df) <- c('raw', 'p.resid')
data_df$year <- dat$year[dat$species_name == 'silver']
data_df$var <- 'Salinity (ppt)'

ddf <- rbind(ddf, data_df)
sdf <- rbind(sdf, sm_df)

## plot
ggplot(sm_df, aes(x = x, y = fit)) +
  geom_ribbon(aes(ymin = fit - se, ymax = fit + se, y = NULL),
              alpha = 0.3) +
  geom_point(data=data_df,
             aes(x=raw, y=p.resid, col=year)) +
  scale_color_viridis_c() +
  geom_line() +
  labs(x='Salinity', 
       y='Effect on catch per unit effort') +
  theme(legend.position = 'none',
        legend.margin = margin(0,0,0,0))