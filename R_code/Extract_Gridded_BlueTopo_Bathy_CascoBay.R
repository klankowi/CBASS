## One-time identification of bathymetry within 1km (gridded) of 
## MURSST locations

rm(list=ls())

# Load packages
library(tidyverse)
library(ncdf4)
library(here)
library(terra)
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

# Vector of dates to run
datevec <- seq.Date(from = as.Date('2002-06-01'),
                    to = as.Date('2002-06-07'),
                    by='day')

# Blank list of length datevec to save
datlist <- vector(mode='list', length = length(datevec))

# Pull BlueTopo bathymetry data
Bathy_Raster <- terra::rast(here(
  'Data/Clean_Data/Bathy/BlueTopo_CascoBay_bathy.TIF'
))

# Load initial day
nc_data <- nc_open(paste0('C:/Users/Katie/mur_sst_download/',
                          'mur_sst_subset/2002-06-01.nc'))

# Extract variables
dt <- ncvar_get(nc_data, "analysed_sst")
dm <- ncvar_get(nc_data, 'mask')
dt <- as.data.frame(dt)
lon <- ncvar_get(nc_data, "lon")
lat <- ncvar_get(nc_data, "lat")
colnames(dt) <- lat
rownames(dt) <- lon

# Convert to DF
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

# Remove masked (land) observations
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

# Make ID list
newdat$ID <- 1:nrow(newdat)

# newdat <- newdat %>% 
#   filter(mask==1)

# Keep just to Casco Bay
newdat <- newdat %>% 
  filter(lat >=43.55 & lat<=43.94) %>% 
  filter(lon >=-70.32 & lon<=-69.83)

# Convert to sf
dat_sf <- st_as_sf(newdat, coords=c('lon', 'lat'),
                   crs="EPSG:4326")

# Mask to Casco Bay
cb <- st_read(here('GIS/CascoBay_Polygon.shp'), quiet = T)
cb <- st_transform(cb, st_crs(dat_sf))
#dat_sf <- st_intersection(dat_sf, cb)

# Close NC
nc_close(nc_data)

# Remove intermediates
rm(newdat, dm, dt, newdm, nc_data)

# Plot to check
ggplot() +
  tidyterra::geom_spatraster(data=Bathy_Raster, aes(fill=Elevation)) +
  scale_fill_viridis_c() +
  geom_sf(data=dat_sf, aes(col=sst), cex=0.4) +
  scale_color_viridis_c(option='inferno')

# Grid
dat_grid <- st_transform(dat_sf, crs="EPSG:4326")
centr <- st_centroid(dat_grid)

# 2. Define the desired cell size (e.g., 1 unit)
cellSize <- 0.01 #meters

# 3. Extend the bounding box by half of the cell size in all directions
# This ensures your points fall in the *center* of the new grid cells
extended_bbox <- (st_bbox(centr) + cellSize / 2 * c(-1, -1, 1, 1))

# 4. Create the new grid using the extended bounding box
grid <- st_make_grid(extended_bbox, 
                     cellsize = c(cellSize, cellSize)) %>% st_sf()

# Clip to Casco Bay
grid <- st_intersection(grid, cb)

# Project
grid <- st_transform(grid, st_crs(cb))

# Plot to check
ggplot() +
  tidyterra::geom_spatraster(data=Bathy_Raster, aes(fill=Elevation)) +
  scale_fill_viridis_c() +
  geom_sf(data=grid, fill=NA) +
  geom_sf(data=dat_sf, cex=0.2)

# Join with characteristics of bathy raster
bathy <- extract(Bathy_Raster, grid, bind=T, fun='mean',
                 na.rm=TRUE)
bathy <- as.data.frame(bathy)
bathy <- bathy %>% 
  dplyr::select(Elevation) %>% 
  rename(bathy = Elevation)
grid$bathy <- bathy$bathy

# Merge with data
total <- st_intersection(dat_sf, grid)
total <- st_intersection(total, cb)

# Remove masked land
total <- total[total$mask == 1,]

# Plot to check
ggplot() +
  geom_sf(data=total, aes(fill=bathy, col=bathy)) +
  scale_fill_viridis_c() + scale_color_viridis_c()

ggplot() +
  geom_sf(data=total, aes(fill=sst, col=sst)) +
  scale_fill_viridis_c() + scale_color_viridis_c()

ggplot() +
  geom_point(data=total,
             aes(x=bathy, y=sst))

write.csv(total,
          here('Data/Clean_Data/Bathy/Gridded_CascoBay_Bathymetry_for_MURSST.csv'),
          row.names = F)

# Remove intermediates
rm(bathy, Bathy_Raster, cb, centr, dat_grid, dat_sf,
   grid, cellSize, extended_bbox)