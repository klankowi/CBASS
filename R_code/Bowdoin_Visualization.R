rm(list=ls())

library(here)
library(tidyverse)
library(scales)
library(metR)
library(sf)

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

# Make location data
buoy <- st_as_sf(
  data.frame(
    lat = 43.762488, 
    lon = -69.989032
  ),
  coords=c('lon', 'lat'),
  crs="EPSG:4326"
  )

# Load met data
dat <- read.csv(here('Clean_Data/Bowdoin_Buoy.csv'))

dat$date <- as.POSIXct(dat$date,
                       format='%m/%d/%Y %H:%M')
dat <- dat[!is.na(dat$date),]

datestring <- seq.POSIXt(from = min(dat$date, na.rm=T),
                         to = max(dat$date, na.rm=T),
                         by='30 min')

datestring <- as.data.frame(datestring)
colnames(datestring) <- 'date'

dat <- merge(datestring, dat, by=c('date'), all=T)

dat2 <- dat %>% 
  mutate(year = year(date),
         date = as.Date(date)) %>% 
  group_by(date) %>% 
  summarise(meanT = mean(temp, na.rm=T)) %>% 
  dplyr::select(date, meanT) %>% 
  unique() %>% as.data.frame()

dat2 <- dat2 %>% 
  mutate(year = year(date)) %>% 
  mutate(mon.day = as.Date(paste0('2025-',
                           month(date), '-',
                           day(date)),
                           format = '%Y-%m-%d'))

dat2$meanT[is.nan(dat2$meanT)] <- NA

#### Data validity plots ####
ggplot() +
  geom_line(data=dat2,
            aes(x=mon.day, y=meanT, 
                col=as.factor(year), group=year)) +
  scale_color_viridis_d(na.value = 'transparent') +
  scale_x_date(breaks='1 month', labels=date_format("%b")) +
  labs(col='Year', x='Time of Year', y='Daily Avg Temp') +
  guides(color=guide_legend(nrow=1)) +
  ggtitle('Annual Daily Temps, Bowdoin Buoy')

ggplot() +
  geom_line(data=dat2[month(dat2$mon.day) %in% c(1, 2, 3),],
              aes(x=mon.day, y=meanT, 
                  col=year, fill=year)) +
  facet_wrap(~year) +
  scale_color_viridis_c(na.value = 'transparent') +
  scale_x_date(breaks='1 month', labels=date_format("%b")) +
  guides(color=guide_legend(nrow=1))

ggplot() +
  geom_line(data=dat2[month(dat2$mon.day) %in% c(4, 5, 6),],
             aes(x=mon.day, y=meanT, 
                 col=year, fill=year)) +
  facet_wrap(~year) +
  scale_color_viridis_c(na.value = 'transparent') +
  scale_x_date(breaks='1 month', labels=date_format("%b")) +
  guides(color=guide_legend(nrow=1))

ggplot() +
  geom_line(data=dat2[month(dat2$mon.day) %in% c(7, 8, 9),],
             aes(x=mon.day, y=meanT, 
                 col=year, fill=year)) +
  facet_wrap(~year) +
  scale_color_viridis_c(na.value = 'transparent') +
  scale_x_date(breaks='1 month', labels=date_format("%b")) +
  guides(color=guide_legend(nrow=1))

ggplot() +
  geom_line(data=dat2[month(dat2$mon.day) %in% c(10, 11, 12),],
             aes(x=mon.day, y=meanT, 
                 col=year, fill=year)) +
  facet_wrap(~year) +
  scale_color_viridis_c(na.value = 'transparent') +
  scale_x_date(breaks='1 month', labels=date_format("%b")) +
  guides(color=guide_legend(nrow=1))

ggplot() +
  geom_line(data=dat2,
             aes(x=mon.day, y=meanT, 
                 col=as.factor(year), group=year)) +
  scale_color_viridis_d(na.value = 'transparent') +
  scale_x_date(breaks='1 month', labels=date_format("%b")) +
  labs(col='Year', x='Time of Year', y='Daily Avg Temp') +
  guides(color=guide_legend(nrow=1)) +
  ggtitle('Annual Daily Temps, Bowdoin Buoy')

ggplot() +
  geom_line(data=dat2[dat2$year %in% c(2016, 2017) &
                        month(dat2$date) %in% c(7, 8)
                      ,],
            aes(x=mon.day, y=meanT, 
                col=as.factor(year), group=year),
            lwd=1) +
  scale_color_viridis_d(na.value = 'transparent') +
  scale_x_date(breaks='1 month', labels=date_format("%b")) +
  labs(col='Year', x='Time of Year', y='Daily Avg Temp') +
  guides(color=guide_legend(nrow=1))


#### Current plots ####
coast <- st_read(here('GIS/us_medium_shoreline_poly.shp'),
                 quiet=T)
coast <- st_transform(coast, crs="EPSG:32619")
buoy <- st_transform(buoy, st_crs(coast))

current <- dat %>% 
  filter(!is.na(current.dir)) %>% 
  mutate(lat = 43.762488,
         lon = -69.989032) %>% 
  mutate(year = year(date)) %>% 
  filter(year != 2014) %>% 
  dplyr::select(date, lon, lat, year,
                current.dir, current.spd, temp, salinity) %>% 
  unique() %>% as.data.frame()

current <- st_as_sf(current,
                    coords=c('lon', 'lat'),
                    crs="EPSG:4326")
current <- st_transform(current,
                        crs=st_crs(coast))

current <- sfheaders::sf_to_df(current, fill=T)

current <- current %>% 
  dplyr::select(-sfg_id, -point_id) %>% 
  rename(lon = x, lat=y)

# Get dv and du components
deg2rad <- function(deg) {(deg * pi) / (180)}

current <- current %>% 
  mutate(rad = deg2rad(current$current.dir)) %>% 
  mutate(current.spd = current.spd/ 1000) %>% 
  mutate(du = (current.spd) * sin(rad),
         dv = (current.spd) * cos(rad))

current.small <- current %>% 
  filter(as.Date(date, tz='America/New_York') == 
           as.Date('2015-05-01', tz='America/New_York'))


#current <- st_as_sf(current, coords=c('lon', 'lat'), crs="EPSG:4326")

current$month <- month(current$date)


ggplot() +
  geom_sf(data=coast) +
  #geom_sf(data=buoy) +
  coord_sf(xlim=c(417000, 424000),
           ylim=c(4842000 , 4848500),
           crs="EPSG:32619") +
  geom_vector(data = current[current$month == 7 &
                             current$current.spd < 1,],
              aes(x=lon, y=lat,
                  dx = du, dy = dv,
                  col = log(current.spd),
                  mag = log(current.spd)
                  ),
              #mag=1,
              preserve.dir = T,
              pivot=0, alpha=0.3) +
  scale_color_viridis_c() +
  #scale_mag() +
  facet_wrap(vars(year)) +
  #ggh4x::facet_grid2(year~month) +
  labs(fill='Speed', col = 'Speed', mag='Speed')
  #scale_x_continuous(n.breaks=4)

