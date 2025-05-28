# Run GAMS
rm(list=ls())

library(tidyverse)
library(here)
library(mgcv)
library(mgcViz)

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

## Load data
dat <- read.csv(here('Clean_Data/selected_species_abundance.csv'))

dat <-  dat %>% 
  filter(notes %notin% c('had to hand haul the net',
                         'tide going out rapidly',
                         'no fish; inverted set',
                         'had to do two net sets',
                         'Ran the seine in a circular area from the boat (not the beach)'
 )) %>% 
  mutate(site_name = factor(site_name,
                            levels = c('Presumpscot Moorings',
                                       'Skitterygusset',
                                       'Audubon',
                                       'Mussel Cove',
                                       'Mackworth Island - North',
                                       'Mackworth Island - Beach',
                                       'The Brothers - North',
                                       'Back Cove',
                                       'Great Diamond Island',
                                       'Cushing Island',
                                       'SMCC',
                                       'Alewife Cove')),
         substrate = factor(substrate, levels = c('sand/gravel',
                                                  'sand',
                                                  'mud')),
         stage = factor(stage, levels = c('rising', 'falling')),
         weather = factor(weather, levels=c('sunny','partly cloudy',
                                            'overcast', 'rain')), 
         yearf = factor(year, levels = c('2014', '2015', '2016',
                                       '2017', '2018', '2019', '2020',
                                       '2021', '2022', '2023',
                                       '2024'))) %>% 
  mutate(date = as.Date(date, format = '%Y-%m-%d')) %>% 
  filter(year %notin% c(2018, 2019)) %>% 
  filter(week >=24) %>% 
  filter(week<=34) %>% 
  mutate(monthf = factor(month, levels = c('6', '7', '8'))) %>% 
  mutate(weekf = factor(week)) %>% 
  filter(!is.na(temp_degc)) %>% 
  filter(!is.na(TideHT.m)) %>% 
  filter(temp_degc <=27,
         temp_degc>=13)

## Silverside
dat.silver <- dat %>% 
  mutate(month = as.numeric(as.character(month))) %>% 
  filter(species_name == 'silver')

gam.silver <- gam(formula = catch
                  ~ s(temp_degc, bs='ts') +
                    s(temp_degc, TideHT.m, bs='ts') +
                    #s(TideHT.m, bs='ts') +
                    #substrate +
                    weather +
                    #stage +
                    yearf +
                    s(site_name, bs='re'),
                  
                  data = dat.silver, 
                  family = nb(link = 'log'), 
                  method = "ML",
                  select = F)

summary(gam.silver)
AIC(gam.silver)

b <- getViz(gam.silver)
print(plot(b, allTerms = T))
check(b)

## Herring
dat.herring <- dat %>% 
  filter(species_name == 'herring')

gam.herring <- gam(formula = catch
                   ~ #s(temp_degc, bs='ts') +
                     #s(week, temp_degc, bs='ts') +
                     
                     #s(temp_degc, TideHT.m, bs='ts') +
                     s(TideHT.m, bs='ts') +
                     #s(week, TideHT.m, bs='ts') +
                     
                     s(week, bs='ts') +
                     
                     s(yearf, bs='re') +
                     s(site_name, bs='re') +
                     substrate +
                     weather,# +
                     #stage,# +
                     #yearf,
                  
                  data = dat.herring, 
                  family = nb(link = 'log'), 
                  method = "ML",
                  select = F)
summary(gam.herring)
AIC(gam.herring)

b <- getViz(gam.herring)
print(plot(b, allTerms = T))
check(b)

## Tomcod
dat.tomcod <- dat %>% 
  filter(species_name == 'tomcod')

gam.tomcod <- gam(formula = catch
                  ~ s(temp_degc) +
                    s(temp_degc, week) +
                    s(temp_degc, TideHT.m) +
                    s(TideHT.m) +
                    s(site_name, bs='re') +
                    substrate +
                    weather +
                    stage +
                    yearf,
                  
                   data = dat.tomcod, 
                   family = nb(link = 'log'), 
                   method = "REML",
                   select = T)
summary(gam.tomcod)

b <- getViz(gam.tomcod)
print(plot(b, allTerms = T), 
      pages = 1) # Calls print.plotGam()
check(b)

## Flounder
dat.flounder <- dat %>% 
  filter(species_name == 'flounder')

gam.flounder <- gam(formula = catch
                    ~ s(temp_degc) +
                      s(temp_degc, week) +
                      s(temp_degc, TideHT.m) +
                      s(TideHT.m) +
                      s(site_name, bs='re') +
                      substrate +
                      weather +
                      stage +
                      yearf,
                   
                   data = dat.flounder, 
                   family = nb(link = 'log'), 
                   method = "REML",
                   select = T)

summary(gam.flounder)

b <- getViz(gam.flounder)
print(plot(b, allTerms = T), 
      pages = 1) # Calls print.plotGam()
check(b)

## Mummichog
dat.mummi <- dat %>% 
  filter(species_name == 'mummi') 

gam.mummi <- gam(formula = catch
                 ~ s(temp_degc) +
                   s(temp_degc, week) +
                   s(temp_degc, TideHT.m) +
                   s(TideHT.m) +
                   s(site_name, bs='re') +
                   substrate +
                   weather +
                   stage +
                   yearf,
                 
                 data = dat.flounder, 
                 family = nb(link = 'log'), 
                 method = "REML",
                 select = T)
summary(gam.mummi)

b <- getViz(gam.mummi)
print(plot(b, allTerms = T), 
      pages = 1) 
check(b)

# Wrap:
# - M: ran(site), year,                  int(week, tide),  weather, substrate
# - T: ran(site), year, int(week, temp), int(week, tide),           substrate
# - F: ran(site), year,                  int(week, tide),  weather

# - H: ran(site), year, int(week, temp), int(week, tide), 
# - S: ran(site), year, int(week, temp),                   weather, substrate


# All used: random(site name), year
# Benthic-Migrant:  Flounder
# Benthic-Resident: Mummichog, Tomcod      
# Pelagic-Resident: Silver           
# Pelagic-Migrant:  Herring  

# Residents share: substrate
# Migrants share:  int(week, tide)

# Benthics share:  int(week, tide)
# Pelagics share:  int(week, temp)

# Nobody used: tidal stage
