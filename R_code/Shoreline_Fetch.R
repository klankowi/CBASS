rm(list=ls())

library(here)
library(tidyverse)
library(sf)

# Negate function
'%notin%' <- function(x,y)!('%in%'(x,y))

# Mode function
Mode <- function(x) {
  ux <- unique(x)
  ux[which.max(tabulate(match(x, ux)))]
}

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

shor <- st_read("C:/Users/klankowicz/Downloads/Maine_Living_Shoreline/Maine_Living_Shoreline_Decision_Support_Tool_-_Total_Score.shp", 
                quiet=T)

shor <- shor[shor$REGION =='Casco Bay',]

ggplot() +
  geom_sf(data = shor,
          aes(col=FETCH_SCOR),
          alpha=0.5, cex=0.5) +
  scale_fill_viridis_c(direction = -1)

sites <- read.csv(here('Clean_Data/Seine/sites_cleaned.csv'))
sites <- st_as_sf(sites,
                  coords=c('longitude', 'latitude'),
                  crs="EPSG:4326")
sites <- st_transform(sites, st_crs(shor))

# GMRI sites
ggplot() +
  geom_sf(data = shor,
          aes(col=as.numeric(FETCH_SCOR)),
          alpha=0.5, cex=0.5) +
  scale_color_viridis_c(direction = -1) +
  geom_sf(data=sites, cex=0.75, col='red') +
  coord_sf(xlim=c(-70.28, -70.18),
           ylim=c(43.575, 43.725),
           crs="EPSG:4326")

# QBC sites
ggplot() +
  geom_sf(data = shor,
          aes(col=as.numeric(FETCH_SCOR)),
          alpha=0.5, cex=0.5) +
  scale_color_viridis_c(direction = -1) +
  geom_sf(data=sites, cex=0.75, col='red') +
  coord_sf(xlim=c(-70.025, -69.900),
          ylim=c(43.74, 43.84),
          crs="EPSG:4326")

# Find three closest fetch scores, average

hold <- shor[1,]
hold$OBJECTID <- 0
hold$site_name <- 0

for(i in 1:nrow(sites)){
  test <- nngeo::st_nn(x = sites[i,],
                    y= shor, k=3)
  test <- test[[1]]
  test <- shor[test,]
  test$site_name <- sites[i,]$site_name
  
  hold <- rbind(hold, test)
  
  rm(test)
  
}

hold <- hold %>% 
  filter(site_name != 0) %>% 
  sfheaders::sf_to_df(fill = T) %>% 
  group_by(site_name) %>% 
  summarise(fetch = Mode(FETCH_SCOR), 
            bathy = Mode(BATHY_SCOR),
            sea = Mode(SEA_SCORE),
            aspect = Mode(ASPECT_SCO),
            birds = Mode(TWWH_SCORE),
            eelgrass = Mode(EEL_SCORE),
            shells = Mode(SHELL_SCOR),
            struct = Mode(STRUCTURE_)
            ) %>% 
  as.data.frame()

write.csv(hold,
          here('Clean_Data/Seine/site_characteristics.csv'),
          row.names = F)
