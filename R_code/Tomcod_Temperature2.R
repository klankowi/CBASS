rm(list=ls())

library(tidyverse)
library(here)

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

abund <- read.csv(here('Clean_Data/Seine/abund_through_2024.csv'))
trips <- read.csv(here('Clean_Data/Seine/trips_through_2024.csv'))
bio <- read.csv(here('Clean_Data/Seine/lengths_through_2024.csv'))

# Clean
abund <- abund %>% 
  mutate(date = as.Date(date, format="%m/%d/%Y")) %>% 
  mutate(month = month(date),
         year = year(date),
         week = isoweek(date))
abund$collector[abund$site_id >20] <- 'QBC'
abund$collector[abund$site_id <20] <- 'GMRI'

trips <- trips %>% 
  mutate(date = as.Date(date, format="%m/%d/%Y")) %>% 
  mutate(month = month(date),
         year = year(date),
         week = isoweek(date)) %>% 
  mutate(set_time = as.POSIXct(set_time, format='%m/%d/%Y %H:%M'))

trips$collector <- NA
trips$collector[trips$site_id >20] <- 'QBC'
trips$collector[trips$site_id <20] <- 'GMRI'
trips$set_time = trips$set_time - lubridate::hours(4)

bio <- bio %>% 
  mutate(date = as.Date(date, format="%Y-%m-%d")) %>% 
  mutate(month = month(date),
         year = year(date),
         week = isoweek(date))
bio$collector[bio$site_id >20] <- 'QBC'
bio$collector[bio$site_id <20] <- 'GMRI'

#### Tomcod ####
tomcod <- abund[abund$species_name == 'atlantic tomcod',]
tomcod <- tomcod %>% 
  dplyr::select(date, year, site_name, catch)

tomcod <- merge(tomcod, trips, by=c('date', 'year','site_name'), all=T)
tomcod$catch[is.na(tomcod$catch)] <- 0

rm(abund, trips)

#### Clean ####
tomcod <- tomcod %>% 
  dplyr::select(date, year, site_name, catch, set_time,
                weather, temp_degc, do_mg.l, salinity_ppt,
                month, week, collector) %>% 
  mutate(set_time = as.POSIXct(set_time,
                               format="%m/%d/%Y %H:%M")) %>% 
  mutate(set_time = set_time - hours(4))

#### Harbor temp ####
jpm <- read.csv(here('Clean_Data/Meteorological/Portland_watertemp_POR.csv'))

jpm <- jpm %>% 
  mutate(date = as.Date(timestamp, format='%Y-%m-%d %H:%M:%S')) %>% 
  filter(date >= as.Date('2013-11-01')) %>% 
  filter(date <= as.Date('2024-10-31')) %>% 
  mutate(week = week(date),
         year = year(date),
         month = month(date))

jpm <- jpm %>% 
  dplyr::select(date, week, month, year, 
                daily.c, year) %>% 
  unique()

yjpm <- jpm %>% 
  group_by(year) %>% 
  summarise(jantemp = mean(daily.c[month ==1], na.rm=T),
            febtemp = mean(daily.c[month ==2], na.rm=T),
            martemp = mean(daily.c[month ==3], na.rm=T),
            aprtemp = mean(daily.c[month ==4], na.rm=T),
            maytemp = mean(daily.c[month ==5], na.rm=T),
            juntemp = mean(daily.c[month ==6], na.rm=T),
            jultemp = mean(daily.c[month ==7], na.rm=T),
            augtemp = mean(daily.c[month ==8], na.rm=T),
            septemp = mean(daily.c[month ==9], na.rm=T),
            octtemp = mean(daily.c[month ==10], na.rm=T),
            novtemp = mean(daily.c[month ==11], na.rm=T),
            dectemp = mean(daily.c[month ==12], na.rm=T),
            mayseptemp = mean(daily.c[month %in% 
                                        seq(5, 9, 1)],
                              na.rm=T))

#### Join phys ####
tomcod <- tomcod %>% 
  rename(timestamp = set_time)

tomcod <- left_join(tomcod,
                    dplyr::select(jpm, date, daily.c), 
                    by=c('date'))

#### Data explore ####
gmri <- tomcod

gmri <- left_join(gmri,
                    yjpm,
                    by=c('year'))

gmri <- gmri %>% 
  group_by(year, collector) %>% 
  reframe(cpue = sum(catch) / n(),
            temp_degc = mean(temp_degc, na.rm=T),
            daily.c = mean(daily.c),
          jantemp = mean(jantemp),
          febtemp = mean(febtemp),
          martemp = mean(martemp),
          aprtemp = mean(aprtemp),
          maytemp = mean(maytemp),
          juntemp = mean(juntemp),
          jultemp = mean(jultemp),
          augtemp = mean(augtemp),
          septemp = mean(septemp),
          octtemp = mean(octtemp),
          novtemp = mean(novtemp),
          dectemp = mean(dectemp),
          agtemp = mean(mayseptemp)) %>% 
  as.data.frame()

# Test correlation
# Create correlation matrix
df_cormat <- dplyr::select(gmri, -year, -temp_degc,
                           -daily.c, -collector)
model.matrix(~0+., data=df_cormat) %>% 
  cor(use="all.obs", method="spearman") %>% 
  ggcorrplot::ggcorrplot(show.diag = F, type="lower", lab=TRUE, lab_size=3)

tccatch <- ggplot(data=gmri,
                  aes(x=agtemp, y=cpue, label=year)) +
  geom_label(aes(x=agtemp, y=cpue, fill=collector),
             size = 3,
             alpha=0.4,
             col='black') +
  labs(x='Average Casco Bay temperature on sampling days (C)', y='Catch per unit effort', 
       fill='Collecting\nagency') +
  ggtitle('Atlantic tomcod catch - temperature relationship') +
  theme(legend.position = 'bottom',
        legend.margin = margin(0,0,0,0))
tccatch
ggsave(plot=tccatch, 
       here('2024_Rasters/tomcod_temp.png'))
