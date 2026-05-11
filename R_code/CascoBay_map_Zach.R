
# Clear workspace
rm(list=ls())

# Load packages
library(tidyverse)
library(sf)
library(raster)

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
                plot.caption=element_text(hjust=0, face='italic', size=12),
                strip.text.x=element_text(size=12)))

# Load sites
sites <- read.csv('C:/Users/Katie/Downloads/Zach_sites.csv')

# Convert to spatial object
sites.sf <- st_as_sf(sites, coords=c('Long', 'Lat'),
# This line specifies that you are providing unprojected lon-lat data
                     crs="EPSG:4326") 

# Convert to appropriate projection (UTM 19N)
sites.sf <- st_transform(sites.sf, crs="EPSG:2803")

# Set bounding box that covers all of Casco Bay
cb.box <- cbind(c(-70.32, -70.00, -70.32, -70.00),
                c(43.50, 43.50, 43.70, 43.70))
cb.box <- as.data.frame(cb.box)
colnames(cb.box) <- c('lon', 'lat')
cb.box <- st_as_sf(cb.box, coords=c('lon', 'lat'), crs="EPSG:4326")
cb.box <- st_transform(cb.box, st_crs(sites.sf))
cb.box <- sfheaders::sf_to_df(cb.box, fill=T)
cb.box <- dplyr::select(cb.box, x, y)

# Extract limits (easier for plotting later)
cb.box.x <- c(min(cb.box$x), max(cb.box$x))
cb.box.y <- c(min(cb.box$y), max(cb.box$y))

# Turn vertices into a polygon
cb.box <- st_as_sf(cb.box, coords=c('x', 'y'), crs=st_crs(sites.sf))
cb.box$eh <- 1
cb.box <- cb.box %>% 
  group_by(eh) %>% 
  summarise() %>% 
  st_cast("POLYGON")

# Call coastline shapefile
coast <- st_read("C:/Users/Katie/Downloads/5 Visualization/us_medium_shoreline_poly.shp", quiet=T)
coast <- st_transform(coast, st_crs(sites.sf))

# Call bathymetry TIF
bathy <- raster('C:/Users/Katie/Downloads/cascobay_gebco.tif')
# Don't change these. It creates a raster that matches GEBCO's original grain.
bathy2 <- raster(ncol=219, nrow=196)
extent(bathy2) <- extent(bathy)
values(bathy2) <- getValues(bathy)

# If it's above water, exclude it.
bathy2@data@values[bathy2@data@values >= 0] <- NA

# Extent inshore (GEBCO can't get the really shallow stuff, so we interpolate)
w <- matrix(c(1,1,1,1,1,1,1,1,1), nr=3,nc=3)
bathy <- focal(bathy2, w=w, fun=mean, na.rm=TRUE) 
bathy <- focal(bathy, w=w, fun=mean, na.rm=TRUE) 
bathy <- focal(bathy, w=w, fun=mean, na.rm=TRUE) 
bathy <- focal(bathy, w=w, fun=mean, na.rm=TRUE) 
bathy <- focal(bathy, w=w, fun=mean, na.rm=TRUE) 

# Transform raster to same projection as sites and shoreline
bathy <- projectRaster(bathy, crs = "EPSG:2803")

# Convert to feet and make the scale positive
bathy <- as.data.frame(bathy, xy=TRUE)
bathy$layer <- bathy$layer * -1 * 3.28084

# Plot Casco Bay
cascobay <- ggplot() +
  # Add bathymetry raster first
  geom_raster(data=bathy,
              aes(x=x, y=y, fill=layer)) +
  # Give nice color scale, deeper is darker
  scale_fill_continuous(high = "#132B43", low = "#52A1E0") +
  # Add coastline
  geom_sf(data=coast, fill='gray', col='gray30') +
  # Add sites
  geom_sf(data=sites.sf,
          # New color for each site, can remove this
          aes(col=site_name), 
          col='black') +
  # Set zoom window
  coord_sf(xlim=cb.box.x,
           ylim=cb.box.y) +
  # ALTERNATIVE: Set zoom manually
  # coord_sf(xlim=c(value 1, value 2),
  #          ylim=c(value 1, value 2),
  #          crs="EPSG:4326") +
  # Angle bottom text
  theme(axis.text.x = element_text(angle = 45, vjust=0.6),
        # Put legend across bottom, adjust spacing
        legend.position = 'bottom',
        legend.box.spacing = unit(-10, "pt")) +
  # Remove x and y labs, name depth colorscale and specify units
  labs(x=" ", y=" ", fill='Depth (ft)')

# View
cascobay

# Check save location
getwd()

Save
ggsave(plot=cascobay,
       filename = 'Casco_Bay.png',
       dpi=300)