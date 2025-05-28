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
         year = factor(year, levels = c('2014', '2015', '2016',
                                        '2017', '2018', '2019', '2020',
                                        '2021', '2022', '2023',
                                        '2024'))) %>% 
  mutate(date = as.Date(date, format = '%Y-%m-%d')) %>% 
  filter(month %in% seq(6, 8)) %>% 
  filter(year %notin% c(2018, 2019)) %>% 
  filter(week >=24) %>% 
  filter(week<=34) %>% 
  mutate(month = factor(month, levels = c('6', '7', '8'))) %>% 
  filter(!is.na(temp_degc)) %>% 
  filter(!is.na(TideHT.m)) %>% 
  filter(temp_degc <=27,
         temp_degc>=13) %>% 
  mutate(weekf=factor(week))

## Silverside
dat.silver <- dat %>% 
  mutate(month = as.numeric(as.character(month))) %>% 
  filter(species_name == 'silver')

gam.silver <- gam(formula = catch
                  ~ s(week, temp_degc, bs='tp') +
                    
                    s(temp_degc, bs='tp') +
                    s(TideHT.m, bs='tp') +
                    
                    year +
                    substrate + 
                    weather +
                    s(site_name, bs = "re"),
                  
                  data = dat.silver, 
                  family = nb(link = 'log'), 
                  method = "REML",
                  select = T)
AIC(gam.silver)
summary(gam.silver)

b <- getViz(gam.silver)
print(plot(b, allTerms = T), 
      pages = 1) # Calls print.plotGam()
# check(b)

## Herring
dat.herring <- dat %>% 
  filter(species_name == 'herring')

gam.herring <- gam(formula = catch
                   ~ s(week, temp_degc, bs='tp') +
 
                     s(temp_degc, bs='tp') +
                     s(TideHT.m, bs='tp') +
                     
                     year +
                     substrate + 
                     weather +
                     s(site_name, bs = "re"),
                   
                   data = dat.herring, 
                   family = nb(link = 'log'), 
                   method = "REML",
                   select = T)
AIC(gam.herring)
summary(gam.herring)

b <- getViz(gam.herring)
print(plot(b, allTerms = T), 
      pages = 1) # Calls print.plotGam()
# check(b)

## Tomcod
dat.tomcod <- dat %>% 
  filter(species_name == 'tomcod')

gam.tomcod <- gam(formula = catch
                  ~ s(week, temp_degc, bs='tp') +
                    
                    s(temp_degc, bs='tp') +
                    s(TideHT.m, bs='tp') +
                    
                    year +
                    substrate + 
                    weather +
                    s(site_name, bs = "re"),
                  
                  data = dat.tomcod, 
                  family = nb(link = 'log'), 
                  method = "REML",
                  select = T)
AIC(gam.tomcod)
summary(gam.tomcod)

b <- getViz(gam.tomcod)
print(plot(b, allTerms = T), 
      pages = 1) # Calls print.plotGam()
# check(b)

## Flounder
dat.flounder <- dat %>% 
  filter(species_name == 'flounder')

gam.flounder <- gam(formula = catch
                    ~ s(week, temp_degc, bs='tp') +
                      
                      s(temp_degc, bs='tp') +
                      s(TideHT.m, bs='tp') +
                      
                      year +
                      substrate + 
                      weather +
                      s(site_name, bs = "re"),
                    
                    data = dat.flounder, 
                    family = nb(link = 'log'), 
                    method = "REML",
                    select = T)
AIC(gam.flounder)
summary(gam.flounder)

b <- getViz(gam.flounder)
print(plot(b, allTerms = T), 
      pages = 1) # Calls print.plotGam()
# check(b)

## Mummichog
dat.mummi <- dat %>% 
  filter(species_name == 'mummi') 

gam.mummi <- gam(formula = catch
                 ~ s(week, temp_degc, bs='tp') +
                   
                   s(temp_degc, bs='tp') +
                   s(TideHT.m, bs='tp') +
                   
                   year +
                   substrate + 
                   weather +
                   s(site_name, bs = "re"),
                 
                 data = dat.mummi, 
                 family = nb(link = 'log'), 
                 method = "REML",
                 select = T)
AIC(gam.mummi)
summary(gam.mummi)

b <- getViz(gam.mummi)
print(plot(b, allTerms = T), 
      pages = 1) # Calls print.plotGam()
# check(b)
