
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

# Set the name of the workbook
fname <- paste0(here('Clean_Data/seine_compiled_clean.xlsx'))
# Get info about all excel sheet names in workbook
sheets <- readxl::excel_sheets(fname)
# Read in as list item, each item is a sheet
data.all <- invisible(lapply(sheets, 
                             function(x) readxl::read_excel(fname, sheet = x)))
names(data.all) <- c('sites', 'species', 'trips', 'fish', 'abundance')
# Coerce list items to dataframes
data.all <- lapply(data.all, as.data.frame)

rm(fname, sheets)

data.all$sites <- dplyr::select(data.all$sites,
                                site_name, latitude, longitude, bay_location)
data.all.goodcoords <- data.all$sites[complete.cases(data.all$sites),]
# Convert to sf (spatial features) object
sites.sf <- sf::st_as_sf(data.all.goodcoords, coords=c('longitude', 'latitude'),
                         crs="EPSG:4326")
sites.sf <- sites.sf[sites.sf$site_name != "The Brothers - South",]
sites.sf <- sites.sf[sites.sf$site_name != 'Lands End Alternate site',]
sites.sf$`Sampling location` <- ' '
# Set CRS (coordinate reference system)
colnames(sites.sf)[1:2] <- c('Site name', 'Bay location')

sites.sf <- st_transform(sites.sf, crs="EPSG:2803")

cb.box <- cbind(c(-70.32, -69.84, -70.32, -69.84),
                c(43.56, 43.56, 43.86, 43.86))
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
coast <- st_read(here("GIS/us_medium_shoreline_poly.shp"), quiet=T)
coast <- st_transform(coast, st_crs(sites.sf))

cities <- read.csv(here('Clean_Data/CascoBay_City_Coordinates.csv'))
cities <- st_as_sf(cities, coords=c('lon', 'lat'), crs="EPSG:4326")
cities <- st_transform(cities, st_crs(sites.sf))

baylabel <- data.frame(name='Casco Bay', lon=-70.04, lat=43.67)
baylabel <- st_as_sf(baylabel, coords=c('lon', 'lat'), crs="EPSG:4326")
baylabel <- st_transform(baylabel, st_crs(sites.sf))

bathy <- raster(here('Clean_Data/Bathy/cascobay_gebco.tif'))
bathy2 <- raster(ncol=219, nrow=196)
extent(bathy2) <- extent(bathy)
values(bathy2) <- getValues(bathy)
bathy2@data@values[bathy2@data@values >= 0] <- NA
w <- matrix(c(1,1,1,1,1,1,1,1,1), nr=3,nc=3)
bathy <- focal(bathy2, w=w, fun=mean, na.rm=TRUE) 
bathy <- focal(bathy, w=w, fun=mean, na.rm=TRUE) 
bathy <- focal(bathy, w=w, fun=mean, na.rm=TRUE) 
bathy <- focal(bathy, w=w, fun=mean, na.rm=TRUE) 
bathy <- focal(bathy, w=w, fun=mean, na.rm=TRUE) 

bathy <- projectRaster(bathy, crs = "EPSG:2803")

bathy <- as.data.frame(bathy, xy=TRUE)
bathy$layer <- bathy$layer * -1 * 3.28084

# Plot Casco Bay
cascobay <- ggplot() +
  geom_raster(data=bathy,
              aes(x=x, y=y, fill=layer)) +
  scale_fill_continuous(high = "#132B43", low = "#52A1E0") +
  geom_sf(data=coast, fill='gray', col='gray30') +
  geom_sf(data=sites.sf[sites.sf$`Site name` != "Lands End Alternate site",],
          aes(col=`Sampling location`)) +
  scale_color_manual(values='black') +
  coord_sf(xlim=cb.box.x,
           ylim=cb.box.y) +
  theme(axis.text.x = element_text(angle = 45, vjust=0.6),
        legend.position = 'bottom',
        legend.box.spacing = unit(-10, "pt")) +
  labs(x=" ", y=" ", fill='Depth (ft)') +
  geom_sf_text(data=cities[cities$name != 'South Portland',], 
               aes(label=name), col='gray15')+
  geom_sf_text(data=baylabel,
               aes(label=name), col='navy', size=6, angle=30)

# Call state outline
states <- states(cb = TRUE, resolution = "500k", year = NULL)
states <- states[states$STUSPS == 'ME',]
states <- st_transform(states, st_crs(sites.sf))

# Plot state outline
maine_gg <- ggplot() + 
  geom_sf(data=states, fill='gray', col='gray30') + 
  geom_sf(data=cb.box, 
          col='black', fill=NA, lwd=0.6) +
  theme(panel.grid.major = element_blank(),
        axis.text.x = element_blank(),
        axis.text.y = element_blank(),
        axis.ticks = element_blank(),
        panel.background = element_rect(fill = "white", colour = NA),
        plot.background = element_rect(fill = "white", colour = NA),
        panel.border = element_blank())

# PLot together as inset
total <- ggdraw() +
  draw_plot(cascobay) +
  draw_plot(maine_gg,
            height = 0.2,
            x = -0.24,
            y = 0.75
  )

# Save
ggsave(plot=total,
       here('Documentation/CascoBay.png'),
       dpi=300)