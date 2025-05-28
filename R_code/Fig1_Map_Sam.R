
# Clear workspace
rm(list=ls())

# Load packages
library(reprex, quietly=T, verbose=F)
suppressPackageStartupMessages(library(tidyverse, quietly=T, verbose=F))
suppressPackageStartupMessages(library(readxl, quietly=T, verbose=F))
suppressPackageStartupMessages(library(here, quietly=T, verbose=F))
suppressPackageStartupMessages(library(padr, quietly=T, verbose=F))
suppressPackageStartupMessages(library(zoo, quietly=T, verbose=F))
suppressPackageStartupMessages(library(sf, quietly=T, verbose=F))
suppressPackageStartupMessages(library(vegan, quietly=T, verbose=F))
suppressPackageStartupMessages(library(kableExtra, quietly=T, verbose=F))
suppressPackageStartupMessages(library(reshape2, quietly=T, verbose=F))
suppressPackageStartupMessages(library(ggsflabel, quietly=T, verbose=F))
suppressPackageStartupMessages(library(cowplot, quietly=T, verbose=F))
suppressPackageStartupMessages(library(smplot2, quietly=T, verbose=F))
suppressPackageStartupMessages(library(raster, quietly=T, verbose=F))
suppressPackageStartupMessages(library(tigris, quietly=T, verbose=F))

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

# Data
sites <- read.csv(here('Clean_Data/Seine/sites_cleaned.csv'))
sites <- sites %>% 
  filter(bay_location != 'East') %>%
  filter(site_name != 'The Brothers - South') %>% 
  st_as_sf(coords=c('longitude', 'latitude'),
           crs="EPSG:4326")

sites.sf <- st_transform(sites, crs="EPSG:2803")

cb.box <- cbind(c(-70.32, -70.15, -70.15, -70.32),
                c(43.56, 43.56, 43.75, 43.75))
cb.box <- as.data.frame(cb.box)
colnames(cb.box) <- c('lon', 'lat')
cb.box <- st_as_sf(cb.box, coords=c('lon', 'lat'), crs="EPSG:4326")
cb.box <- st_transform(cb.box, st_crs(sites.sf))
cb.box <- sfheaders::sf_to_df(cb.box, fill=T)
cb.box <- dplyr::select(cb.box, x, y)
cb.box.x <- c(min(cb.box$x), max(cb.box$x))
cb.box.y <- c(min(cb.box$y), max(cb.box$y))

cb.box <- st_as_sf(cb.box, coords=c('x', 'y'), crs=st_crs(sites.sf))
cb.box$eh <- 1
cb.box <- cb.box %>% 
  group_by(eh) %>% 
  summarise() %>% 
  st_cast("POLYGON")

# Call coastline shapefile
coast <- st_read(here("GIS/CBASS_with_rivers.shp"), quiet=T)
coast <- st_transform(coast, st_crs(sites.sf))
mb <- st_read(here('GIS/Mill_Brook.shp'), quiet=T)
mb <- st_transform(mb, st_crs(sites.sf))

cities <- read.csv(here('Clean_Data/CascoBay_City_Coordinates.csv'))
cities <- st_as_sf(cities, coords=c('lon', 'lat'), crs="EPSG:4326")
cities <- st_transform(cities, st_crs(sites.sf))

# Plot Casco Bay
cascobay <- ggplot() +
  geom_sf(data=coast, fill='gray', col='gray30',
          lwd=0.1) +
  geom_sf(data=mb, col='gray30', lwd=0.1) +
  geom_sf(data=sites.sf, cex=0.4) +
  geom_sf_label_repel(data=sites.sf, aes(label=`abbrev`),
                      min.segment.length = unit(0, 'lines'), 
                      seed=150,
                      force=5,
                      label.size=0.1,
                      fill = alpha(c("white"),0.3),
                      size=3) +
  coord_sf(xlim=c(-70.374, -70.15),
           ylim=c(43.790, 43.56), crs="EPSG:4326") +
  labs(x='', y='') +
  geom_sf_text(data=cities[cities$name %in% c('Portland'),], 
               aes(label=name), col='gray35', 
               size=2.5, angle=0) +
  theme(axis.text.x = element_text(angle = 45, vjust=0.6, size=8),
        axis.text.y = element_text(angle=0, size=8),
        axis.title=element_blank(),
        plot.margin = margin(2,3,0,0, 'mm'))
cascobay

# # Call state outline
# states <- states(cb = TRUE, resolution = "500k", year = NULL)
# states <- states[states$STUSPS == 'ME',]
# states <- st_transform(states, st_crs(sites.sf))
# 
# # Plot state outline
# maine_gg <- ggplot() + 
#   geom_sf(data=states, fill='gray', col='gray30') + 
#   geom_sf(data=cb.box, 
#           col='red', fill=NA, lwd=0.6) +
#   theme(panel.grid.major = element_blank(),
#         axis.text.x = element_blank(),
#         axis.text.y = element_blank(),
#         axis.ticks = element_blank(),
#         #panel.background = element_rect(fill = "white", colour = 'black'),
#         plot.background = element_rect(fill = "white", colour = 'black'),
#         panel.border = element_blank(),
#         plot.margin = margin(0,0,0,0))
# 
# # PLot together as inset
# total <- ggdraw() +
#   draw_plot(cascobay) +
#   draw_plot(maine_gg,
#             height = 0.2,
#             x = -0.16,
#             y = 0.26
#   )

# Save
ggsave(plot=cascobay,
       here('CascoBay_Sam.png'),
       width = 81, height=120, units = 'mm')

