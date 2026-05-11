# Plot NASA Global MUR SST

rm(list=ls())

library(tidyverse)
library(ncdf4)
library(data.table)

# Set GGplot auto theme
theme_set(theme(panel.grid.major = element_line(color='lightgray'),
                panel.grid.minor = element_blank(),
                panel.background = element_blank(),
                panel.border = element_rect(color='black', linewidth=1, fill=NA),
                legend.position = "bottom",
                legend.background = element_rect(fill='transparent', colour = 'transparent'),
                axis.text.x=element_text(size=10),
                axis.text.y=element_text(size=10),
                axis.title.x=element_text(size=11),
                axis.title.y=element_text(size=11, angle=90, vjust=2),
                plot.title=element_text(size=12, hjust = 0, vjust = 1.2),
                plot.caption=element_text(hjust=0, face='italic', size=12)))

dat <- nc_open('C:/Users/Katie/Downloads/20120702090000-JPL-L4_GHRSST-SSTfnd-MUR-GLOB-v02.0-fv04.1.nc')

sst <- ncvar_get(dat, "analysed_sst")
#mask <- ncvar_get(dat, 'mask')
lat <- dat$dim$lat$vals
lon <- dat$dim$lon$vals

colnames(sst) <- lat
rownames(sst) <- lon

sst <- as.data.frame(sst)
sst$lon <- rownames(sst)

test <- melt(setDT(sst), id.vars = 18000, measure.vars = 1:17999, 
     variable.name = "lat", value.name = "sst")
test <- as.data.frame(test)

test <- test %>% 
  mutate(lon = as.numeric(lon),
         lat = as.numeric(as.character(lat)))

test <- test %>% 
  filter(lon >=-70.53 & lon <=-69.8) %>% 
  filter(lat >=43.55 & lat <=43.9)

#test <- st_as_sf(test, coords=c('lon', 'lat'), crs="EPSG:4326")

coast <- st_read('C:/Users/Katie/Documents/GitHub/CBASS/GIS/us_medium_shoreline_poly.shp')
coast <- st_transform(coast, st_crs(test))

ggplot() + 
  geom_tile(data=test, aes(fill=sst, x=lon, y=lat)) + 
  scale_fill_viridis_c(na.value = 'transparent') +
  geom_sf(data=coast, fill='gray') +
  coord_sf(xlim=c(-70.3, -69.8),
           ylim=c(43.55, 43.9),
           crs="EPSG:4326")

