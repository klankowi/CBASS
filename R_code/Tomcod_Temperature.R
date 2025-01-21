rm(list=ls())

library(tidyverse)
library(here)

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
         month = month(date)) %>% 
  mutate(spawnyear = year)

jpm <- jpm %>% 
  dplyr::select(date, week, month, year, 
                daily.c, spawnyear) %>% 
  unique()

for(i in 1:nrow(jpm)){
  if(jpm$month[i] %in% c(11, 12)){jpm$spawnyear[i] <- (jpm$year[i] + 1)}
}

jpm$state[jpm$month %in% c(11, 12, 1, 2)] <- 'spawn'
jpm$state[jpm$month %in% c(3, 4, 5)] <- 'move'

mjpm <- jpm %>% 
  filter(!is.na(state)) %>% 
  group_by(spawnyear, state) %>% 
  summarise(stemp = mean(daily.c))

#### Tidal conditions ####
tides <- read.csv(here('Raw_Data/Portland_tides_byminute.csv'))
tides$date <- as.Date(substr(tides$TimeLocal, start=1, stop=10))

tides <- tides %>% 
  filter(tides$date %in% tomcod$date)

tides$TimeLocal[nchar(tides$TimeLocal) == 10] <- paste0(tides$TimeLocal[nchar(tides$TimeLocal) == 10], ' 00:00:00')

tides <- tides %>% 
  mutate(timestamp = as.POSIXct(TimeLocal, format= '%Y-%m-%d %H:%M:%S')) %>% 
  dplyr::select(-tidename, -date, -TimeLocal)

tides <- tides %>% 
  filter(!is.na(tides$timestamp)) %>% 
  unique() %>% as.data.frame()

#### Presumpscot at Westbrook ####
river <- importDVs("01064118", 
                   code="00065", 
                   stat="00003",
                   sdate="2013-11-01", edate="2022-08-01")

river <- river %>% 
  dplyr::select(-staid, -qualcode) %>% 
  mutate(date = as.Date(dates),
         val = as.numeric(val)) %>%
  rename(RiverHT.ft = val) %>% 
  dplyr::select(-dates)

#### Precip ####
rain <- read.csv(here('Raw_Data/Portland_Precip_2025.csv'))
rain <- rain %>% 
  dplyr::select(-STATION, -NAME) %>% 
  rename(date = DATE,
         precip = PRCP,
         snow = SNOW,
         snwdp = SNWD) %>% 
  mutate(date = as.Date(date))

#### Join phys ####
tomcod <- tomcod %>% 
  rename(timestamp = set_time)

tomcod <- left_join(tomcod, 
                dplyr::select(tides,timestamp, TideHT.m, tide.num2, stage),
                by=c('timestamp'))

tomcod <- left_join(tomcod,
                    dplyr::select(jpm, date, daily.c), 
                    by=c('date'))

tomcod <- left_join(tomcod,
                    river,
                    by=c('date'))
  
tomcod <- tomcod %>% 
  mutate(RiverHT.m = RiverHT.ft * 0.3048) %>% 
  dplyr::select(-RiverHT.ft)

tomcod <- left_join(tomcod,
                    rain,
                    by=c('date'))

#### Data explore ####
tomcod$totalprecip <-  tomcod$precip + tomcod$snow

gmri <- tomcod %>% filter(collector == 'GMRI')

gmri$spawnyear <- gmri$year
gmri$spawnyear[gmri$month %in% c(11, 12)] <- 
  gmri$spawnyear[gmri$month %in% c(11, 12)] +1

gmri <- gmri %>% 
  group_by(spawnyear) %>% 
  summarise(cpue = sum(catch) / n(),
            temp_degc = mean(temp_degc, na.rm=T),
            daily.c = mean(daily.c),
            RiverHT.m = mean(RiverHT.m, na.rm=T),
            precip = mean(totalprecip)) %>% 
  as.data.frame()

# Test correlation
# Create correlation matrix
df_cormat <- dplyr::select(gmri, -spawnyear)
model.matrix(~0+., data=df_cormat) %>% 
  cor(use="all.obs", method="spearman") %>% 
  ggcorrplot(show.diag = F, type="lower", lab=TRUE, lab_size=3)


tccatch <- ggplot(data=gmri) +
  geom_point(aes(x=daily.c, y=cpue, col=spawnyear),
             cex=2) + 
  scale_color_viridis_c() +
  labs(x='Daily mean temperature (C)', y='Catch per unit effort', 
       col='Spawning\nYear') +
  ggtitle('Atlantic tomcod annual catch vs. temperature') +
  theme(legend.position = 'bottom',
        legend.key.width = unit(0.8, 'in'))
tccatch
ggsave(plot=tccatch, 
       here('2024_Rasters/tomcod_temp.png'),
       width = 7.5, height = 5, units='in')
