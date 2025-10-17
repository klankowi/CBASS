## Make map of CBASS seine sites

## Clear workspace
rm(list=ls())

## Set working directory
setwd('/Users/YOURNAME/WHERE YOU SAVED THIS FOLDER/Casco Bay Maps')

## Load packages
# tidyverse- includes ggplot for plotting
library(tidyverse)
# sf - functions for handling spatial features
library(sf)
# ggsflabel - labelling of spatial features
library(ggsflabel)
# cowplot - make side-by-side plots
library(cowplot)
# tigris - contains database of states as spatial features
library(tigris)
# here - sets root directory and shortens filenames
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
                plot.caption=element_text(hjust=0, face='italic', size=12),
                strip.text.x=element_text(size=12)))

## Load Data
# Load CBASS site locations
sites <- read.csv('sites_cleaned.csv')

# Clean data
sites <- sites %>% 
  # Remove QBC sites
  filter(collector != 'QBC') %>% 
  # Remove site we no longer sample
  filter(site_name %notin% c('The Brothers - South',
                             'Mill Brook', 
                             'Presumpscot Falls')) %>% 
  # Convert from dataframe to spatial feature
  st_as_sf(coords=c('longitude', 'latitude'), # Name of columns that include lat-lon info
           crs="EPSG:4326")                   # Code for unprojected lat-lon information

# Transform to useful Maine-specific projection
# North American Datum 1983, Maine West High-Accuracy-Reference-Network
sites.sf <- st_transform(sites, crs="EPSG:2803")

# Set bounding box
# Start by listing the lat-lon of the box corners
cb.box <- cbind(c(-70.32, -70.15, -70.15, -70.32),
                c(43.56, 43.56, 43.75, 43.75))
# Turn that into a data frame
cb.box <- as.data.frame(cb.box)
# Name the columns
colnames(cb.box) <- c('lon', 'lat')
# Convert to spatial feature
cb.box <- st_as_sf(cb.box, coords=c('lon', 'lat'), crs="EPSG:4326")
# Transform to projected lat-lon
cb.box <- st_transform(cb.box, st_crs(sites.sf))
# Back to dataframe
cb.box <- sfheaders::sf_to_df(cb.box, fill=T)
# Select useful variables
cb.box <- dplyr::select(cb.box, x, y)
# Find x limits of bounding box after transformation
cb.box.x <- c(min(cb.box$x), max(cb.box$x))
# Find y limits of bounding box after transformation
cb.box.y <- c(min(cb.box$y), max(cb.box$y))

# Call coastline shapefile
coast <- st_read("us_medium_shoreline_poly.shp", quiet=T)
# Transform to projected lat-lon
coast <- st_transform(coast, st_crs(sites.sf))

# Call cities dataframe
cities <- read.csv('CascoBay_City_Coordinates.csv')
# Convert to spatial feature
cities <- st_as_sf(cities, coords=c('lon', 'lat'), crs="EPSG:4326")
# Transform to projected lat-lon
cities <- st_transform(cities, st_crs(sites.sf))

# Plot Casco Bay
cascobay <- ggplot() +
  # Add coastline
  geom_sf(data=coast, fill='gray', col='gray30') +
  # Add sites
  geom_sf(data=sites.sf, cex=0.4,
          col='blue') +
  # Label sites
  geom_sf_label_repel(data=sites.sf, aes(label=`abbrev`),
                      min.segment.length = unit(0, 'lines'), 
                      seed=150,
                      force=5,
                      label.size=0.1,
                      fill = alpha(c("white"),0.3),
                      size=3) +
  # Set limits of the viewframe
  coord_sf(xlim=cb.box.x,
           ylim=cb.box.y) +
  # Change label names
  labs(x='', y='', col='Collector') +
  # Add city names over city locations
  geom_sf_text(data=cities[cities$name %in% c('Portland',
                                              'Harpswell'),], 
               aes(label=name), col='gray35', 
               size=2.5, angle=45) +
  # Adjust size of different fonts, etc
  theme(axis.text.x = element_text(angle = 45, vjust=0.6, size=8),
        axis.text.y = element_text(angle=0, size=8),
        axis.title=element_blank())

# View result
cascobay

# Save
ggsave(plot=cascobay,
       filename = 'Map.png',
       width = 6, height=6, 
       units = 'in')

