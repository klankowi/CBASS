
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
suppressPackageStartupMessages(library(canadianmaps, quietly=T, verbose=F))

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
sites <- read.csv(here('Data/Clean_Data/Seine/sites_cleaned.csv'))
sites <- sites %>% 
  filter(bay_location != 'East') %>%
  filter(site_name %notin% c('The Brothers - South',
                             'Mill Brook',
                             'Presumpscot Falls')) %>% 
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

cities <- read.csv(here('Data/Clean_Data/CascoBay_City_Coordinates.csv'))
cities <- st_as_sf(cities, coords=c('lon', 'lat'), crs="EPSG:4326")
cities <- st_transform(cities, st_crs(sites.sf))

# Plot Casco Bay
cascobay <- ggplot() +
  geom_sf(data=coast, fill='gray', col='gray30',
          lwd=0.1) +
  geom_sf(data=mb, col='gray30', lwd=0.5) +
  geom_sf(data=sites.sf, cex=0.4) +
  geom_label(aes(x=-70.285, y=43.73, label='B')) +
  geom_sf_label_repel(data=sites.sf, aes(label=`abbrev`),
                      min.segment.length = unit(0, 'lines'), 
                      seed=150,
                      force=5,
                      label.size=0.1,
                      fill = alpha(c("white"),0.3),
                      size=3) +
  coord_sf(xlim=c(-70.29, -70.17),
           ylim=c(43.58, 43.73), crs="EPSG:4326") +
  labs(x='', y='') +
  geom_sf_text(data=cities[cities$name %in% c('Portland'),], 
               aes(label=name), col='gray35', 
               size=3.25, angle=50) +
  theme(axis.text.x = element_blank(),
        axis.ticks = element_blank(),
        axis.text.y = element_blank(),
        panel.grid.major = element_blank(),
        axis.title=element_blank(),
        plot.margin = margin(2,0,0,-10, 'mm'),
        panel.border = element_rect(color='blue', linewidth=1.4, fill=NA))
cascobay

# # Call state outline
states <- states(cb = TRUE, resolution = "500k", year = NULL)
states <- states[states$STUSPS %in% c('ME', 'NH', 'VT', 
                                      'MA', 'CT', 'RI',
                                      'NY'),]
states$abbrev <- states$STUSPS
states <- st_transform(states, st_crs(sites.sf))
states <- dplyr::select(states, abbrev, geometry)

# Call Canadian provinces
prov <- canadianmaps::PROV
prov <- prov[prov$PREABBR %in% c('N.B.', 'N.S.', 'Que.'),]
prov <- st_transform(prov, st_crs(sites.sf))
prov$abbrev <- c('QC', 'NB', 'NS')
prov <- dplyr::select(prov, abbrev, geometry)

# Combine
land <- rbind(states, prov)

landnames <- data.frame(
  abbrev  = c('ME', 'NH', 'VT',
              'MA', 'CT', 'RI',
              'NY',
              'QC', 'NB', 'NS',
              'Gulf of Maine',
              'Bay of\nFundy'),
  x = c(44.89,   43.25, 40,
        42.36,   40   , 41.70,
        40,
        45.91,   45.82, 44.10,
        42.98,
        44.88),
  y = c(-69.12, -71.41, -74,
        -71.33, -74    , -71.57,
        -74,
        -71.45, -66.21, -65.70,
        -68.77,
        -66.21)
)
landnames <- st_as_sf(landnames, coords=c('y', 'x'),
                      crs="EPSG:4326")
landnames <- st_transform(landnames, st_crs(sites.sf))

# Plot state outline
maine_gg <- ggplot() +
  geom_sf(data=land, fill='gray', col='gray30',
          lwd=0.1) +
  geom_sf(data=cb.box,
          col='blue', fill=NA, lwd=0.6) +
  geom_label(aes(x=-72.2, y=45, label='A')) +
  geom_sf_text(data=landnames,
               aes(label=abbrev), 
               col='gray35', 
              size=3.5, angle=0) +
  coord_sf(xlim=c(-72.2, -65.55),
           ylim=c(41.05, 45),
           crs="EPSG:4326") +
  scale_y_continuous(breaks=seq(41.5, 44.5, 1.0)) +
  scale_x_continuous(breaks=seq(-71.5, -65.5, 1.0)) +
  labs(x=NULL, y=NULL) +
  theme(axis.text.x = element_text(angle = 45, size=8,
                                   margin = margin(t=-30,b=20)),
        axis.text.y = element_text(angle=0, size=8, 
                                   margin = margin(l = 20, r = -30)),
        axis.ticks.length=unit(-0.2, "cm"),
        axis.ticks=element_line(color='gray45'),
        axis.title=element_blank(),
        plot.margin = margin(0,-10,0,-3, 'mm'),
        panel.grid.major = element_line(color='gray90'),
        panel.border = element_rect(color='black', linewidth=0.5, fill=NA))
#maine_gg

# PLot together as inset
# total <- ggdraw() +
#   draw_plot(cascobay) +
#   draw_plot(maine_gg,
#             height = 0.2,
#             x = -0.16,
#             y = 0.26
#   )
total <- gridExtra::grid.arrange(maine_gg, cascobay, ncol=2)

# Save
ggsave(plot=total,
       here('Documentation/MEPS/Figures/Figure_1.png'))

