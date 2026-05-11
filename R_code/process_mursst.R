rm(list=ls())

# Load packages
library(tidyverse)
library(ncdf4)
library(here)
library(stars)
library(sf)
library(units)
library(FishStatsUtils)

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

datevec <- seq.Date(from = as.Date('2012-07-01'),
                    to = as.Date('2012-07-31'),
                    by='day')
tempvec <- vector(mode='numeric', length = length(datevec))
minvec <- vector(mode='numeric', length = length(datevec))
maxvec <- vector(mode='numeric', length = length(datevec))
sdvec <- vector(mode='numeric', length = length(datevec))

cb <- st_read(here('GIS/CascoBay_Polygon.shp'),
              quiet = T)
cb <- st_transform(cb, crs='EPSG:4326')
cb <- dplyr::select(cb, geometry)

land <- st_read(here('GIS/us_medium_shoreline_Poly.shp'),
                quiet=T)
land <- st_transform(land, crs='EPSG:4326')

keep <- read.csv(here('Data/Clean_Data/index_points_casco_bay.csv'))

for(i in 1:length(datevec)){
  print(i)
  nc_data  <- nc_open(paste0(
    'C:/Users/Katie/mur_sst_download/mur_sst_subset/',
    datevec[i],
    '.nc'))
  
  # Extract variables (e.g., a variable named 'temp', and its dimensions)
  dt <- ncvar_get(nc_data, "analysed_sst")
  dm <- ncvar_get(nc_data, 'mask')
  dt <- as.data.frame(dt)
  lon <- ncvar_get(nc_data, "lon")
  lat <- ncvar_get(nc_data, "lat")
  colnames(dt) <- lat
  rownames(dt) <- lon

  newdat <- dt %>% 
    mutate(lon = rownames(dt)) %>% 
    mutate(lon = as.numeric(lon)) %>% 
    pivot_longer(cols = 1:(ncol(dt)-1),
                 names_to = 'lat',
                 values_to = 'K') %>% 
    mutate(K = as.numeric(K),
           lat = as.numeric(lat)) %>% 
    mutate(sst = K - 273.15) %>% 
    dplyr::select(lon, lat, sst)
  
  dm <- as.data.frame(dm)
  colnames(dm) <- lat; rownames(dm) <- lon
  
  newdm <- dm %>% 
    mutate(lon = rownames(dm)) %>% 
    mutate(lon = as.numeric(lon)) %>% 
    pivot_longer(cols = 1:(ncol(dt)-1),
                 names_to = 'lat',
                 values_to = 'mask') %>% 
    mutate(lat = as.numeric(lat)) %>% 
    dplyr::select(lon, lat, mask)
  
  newdat <- left_join(newdat, newdm,
                      by=c('lon', 'lat'))
  
  newdat <- newdat %>% 
    filter(lon >=-70.32 & lon <=-69.83) %>% 
    filter(lat >=43.56 & lat<=43.94) %>% 
    dplyr::select(lon, lat, sst, mask)
  
  newdat <- newdat[keep$keep,]
  #newdat <- newdat[newdat$mask == 1,]
  
  nc_close(nc_data)

  #newdat <- st_as_sf(newdat, 
  #                   coords=c('lon', 'lat'),
  #                   crs="EPSG:4326")
  tplot <- 
  ggplot() +
    geom_tile(data=newdat, aes(x=lon, y=lat, col=sst,
                               fill=sst)) +
    scale_color_viridis_c(limits=c(16.3, 20.5)) +
    scale_fill_viridis_c(limits=c(16.3, 20.5)) +
    labs(x='', y='', col='SST (deg C)',
         fill='SST (deg C)') +
    geom_sf(data=land, fill='gray70') +
    theme(legend.position = 'right') +
    ggtitle(paste0(datevec[i])) +
    coord_sf(xlim=c(-70.3, -69.85),
             ylim=c(43.55, 43.9))
  
  ggsave(plot = tplot,
         filename = paste0(here('Casco_Temps/'), 
                           '/',
                           datevec[i], '.png'))
}
  
fl <- list.files(here('Casco_Temps/'))
setwd(here('Casco_Temps/'))
gifski::gifski(png_files = fl,
               delay = 0.35,
               loop=F,
               width = 1400, height = 800,
               gif_file = 'July2012_heatwave.gif')

  
  tempvec[i] <- mean(newdat$sst, na.rm=T)  
  minvec[i] <- min(newdat$sst, na.rm=T)
  maxvec[i] <- max(newdat$sst, na.rm=T)
  sdvec[i] <- sd(newdat$sst, na.rm=T)
  
  rm(newdat, dt, lon, lat)
  
}

df <- data.frame(
  date = datevec,
  sst = tempvec,
  min = minvec,
  max = maxvec,
  sd = sdvec
)

df <- df %>% 
  mutate(range = max - min) %>% 
  mutate(year = year(date),
         month = month(date)) %>% 
  mutate(hold = as.Date(paste0('2025-',
    substr(date, start = 6, stop = 10)
  )))

df$year <- as.factor(df$year)

ggplot(data=df) +
  geom_line(aes(x=hold, y=sst, col=year)) +
  geom_line(data=df[df$year == '2012',],
            aes(x=hold, y=sst), col='red')+
  scale_x_date(date_labels = "%b") +
  scale_color_viridis_d() +
  theme(legend.position = 'right')

## Find norms
# Day of year
df$doy <- yday(df$date)
df$year <- as.numeric(df$year+2001)

# Smooth mean daily temperature using GAM
tgam <- mgcv::gam(sst ~ 
                    s(doy, bs='cs') + 
                    s(year, bs='cs', k=15), 
                  data=df,
                  method='REML')
summary(tgam)
mgcv::gam.check(tgam)
mgcv::plot.gam(tgam, select=1, scheme=1, rug=T, residuals = T)

daily.smooth <- data.frame(
  doy= seq(1, 366, 1),
  year = 2020
)

np <- mgcv::predict.gam(tgam, daily.smooth,
                        exclude = "s(year)",
                        se.fit=T)

daily.smooth$smooth.daily <- np$fit

df <- left_join(df, dplyr::select(daily.smooth, -year),
                by=c('doy'))

df <- df %>% 
  group_by(doy) %>% 
  mutate(tenth = quantile(sst, 0.1),
         ninetieth = quantile(sst, 0.9))

# Smooth percentiles using gam
tgam <- mgcv::gam(tenth ~ 
                    s(doy, bs='cs') + 
                    s(year, bs='cs', k=15), 
                  data=df,
                  method='REML')
summary(tgam)
mgcv::gam.check(tgam)
mgcv::plot.gam(tgam, select=1, scheme=1, rug=T, residuals = T)

daily.smooth <- data.frame(
  doy= seq(1, 366, 1),
  year = 2020
)

np <- mgcv::predict.gam(tgam, daily.smooth,
                        exclude = "s(year)",
                        se.fit=T)

daily.smooth$smooth.tenth <- np$fit

df <- left_join(df, dplyr::select(daily.smooth, -year),
                by=c('doy'))

tgam <- mgcv::gam(ninetieth ~ 
                    s(doy, bs='cs') + 
                    s(year, bs='cs', k=15), 
                  data=df,
                  method='REML')
summary(tgam)
mgcv::gam.check(tgam)
mgcv::plot.gam(tgam, select=1, scheme=1, rug=T, residuals = T)

daily.smooth <- data.frame(
  doy= seq(1, 366, 1),
  year = 2020
)

np <- mgcv::predict.gam(tgam, daily.smooth,
                        exclude = "s(year)",
                        se.fit=T)

daily.smooth$smooth.ninetieth <- np$fit

df <- left_join(df, dplyr::select(daily.smooth, -year),
                by=c('doy'))

df$color <- 'gray80'
df$color[df$sst >= df$smooth.ninetieth] <- 'firebrick'
df$color[df$sst <= df$smooth.tenth] <- 'royalblue4'

ggplot(data=df) +
  geom_line(aes(x=doy, y=smooth.daily), col='gray50') +
  geom_line(aes(x=doy, y=smooth.tenth), col='gray80') +
  geom_line(aes(x=doy, y=smooth.ninetieth), col='gray80') +
  geom_line(aes(x=doy, y=sst, col=color, group = year),
             cex=0.4) +
  scale_color_manual(values = c("gray80" = "gray80", 
                                "firebrick" = "firebrick", 
                                "royalblue4" = "royalblue4")) +
  
  facet_wrap(vars(year)) +
  theme(legend.position = 'n')