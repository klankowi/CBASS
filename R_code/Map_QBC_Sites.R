rm(list=ls())

library(sf)
library(here)
library(tidyverse)

# Create dataframe
sites <- data.frame(
  site_name = c("Orrs", "Snow", 'Lowells', 'Cedar', 'Garrison', 
                'LandsEnd', 'LongPt'),
  lond = c(69, 69, 69, 69, 69, 70, 69),
  lonm = c(54, 54, 58, 59, 59, 00, 56),
  lons = c(50, 34, 32, 11, 29, 04, 20),
  latd = c(43, 43, 43, 43, 43, 43, 43),
  latm = c(50, 48, 45, 44, 44, 42, 46),
  lats = c(04, 50, 33, 34, 52, 59, 29)
)

# Convert to decimal degrees
sites$longitude <- sites$lond + 
  (sites$lonm / 60) +
  (sites$lons / 3600)
sites$longitude <- sites$longitude * -1

sites$latitude <- sites$latd + 
  (sites$latm / 60) +
  (sites$lats / 3600)

# Add QBC dock
dock <- data.frame(
  site_name="QBC_Dock",
  longitude = -69.913146,
  latitude = 43.795690
)

# Convert to sf
qbc.sites <- dplyr::select(sites,
                           site_name, longitude, latitude)
qbc.sites
qbc.sites <- st_as_sf(qbc.sites,
                      coords=c('longitude', 'latitude'))
st_crs(qbc.sites) <- "EPSG:4326"

dock <- st_as_sf(dock,
                 coords=c('longitude', 'latitude'))
st_crs(dock) <- "EPSG:4326"

# Call coastline shapefile
coast <- st_read(here("GIS/us_medium_shoreline_poly.shp"), quiet=T)
coast <- st_transform(coast, st_crs(qbc.sites))

# Plot
p <- ggplot() +
  geom_sf(data=coast, fill='gray') +
  geom_sf(data=qbc.sites,
          aes(col=site_name), 
          cex=1.5) +
  geom_sf(data=dock, col='black') +
  coord_sf(xlim=c(-70.05, -69.85),
           ylim=c(43.68, 43.9)) +
  theme(axis.text.x = element_text(angle = 45, vjust=0.6)) +
  labs(caption='QBC Seining sites. QBC dock is depicted as the black dot.')

p <- p + guides(col=guide_legend(title="Seine Sites"))

p

ggsave(p, 
       filename=here('QBC_Seine_Sites.png'),
       device="png")
