
rm(list=ls())

library(here)
library(tidyverse)
library(shades)

# # Build color palette based on gmRi
# gmri_colors <- c(
#   `orange`    =  "#EA4F12",
#   `yellow`    =  "#EACA00",
#   `gmri green`=  "#ABB400",
#   `green`     =  "#407331",
#   `teal`      =  "#00736D",
#   `gmri blue` =  "#00608A",
#   `dark gray` =  "#535353"
# )
# 
# gmri_pal <- function(reverse = FALSE, ...) {
#   pal <- gmri_colors
#   
#   if (reverse) pal <- rev(pal)
#   
#   grDevices::colorRampPalette(pal, ...)
# }
# 
# scale_color_gmri <- function(reverse = FALSE, ...) {
#   pal <- gmri_pal(reverse = reverse)
#   ggplot2::scale_color_gradientn(colours = pal(256))
# }

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
                plot.caption=element_text(hjust=0, face='italic', size=12)))


abundancedata <- read.csv(here('Clean_Data/rasterdata.csv'))

# Cut to species of interest
keepers <- abundancedata[abundancedata$species %in%
                           c('atlantic tomcod',
                             'atlantic herring'),]

southern.species <- c('permit',
                      'crevalle jack',
                      'white mullet',
                      'summer flounder')

south.abund <- abundancedata[abundancedata$species %in% southern.species,]

south.abund <- south.abund %>% 
  group_by(year, week) %>% 
  summarise(catch = sum(catch))
south.abund$species <- 'Southern species'

northern.species <- c('shorthorn sculpin',
                      #'ninespine stickleback', 
                      'lumpfish',
                      'atlantic cod',
                      'rock gunnel'
)

north.abund <- abundancedata[abundancedata$species %in% northern.species,]

north.abund <- north.abund %>% 
  group_by(year, week) %>% 
  summarise(catch = sum(catch))
north.abund$species <- 'Northern species'

abundancedata <- rbind(keepers, south.abund, north.abund)

fish.species <- unique(abundancedata$species)

yeartemps <- data.frame(
  year = seq(2014, 2023, 1),
  temp = c(rep('Cold', 2),
           'Hot',
           rep('Cold', 3),
           rep('Hot',4))
)

month.week <- data.frame(
  week= c(22.5, 26.5, 30.5, 34.5),
  month=c('June','July','Aug','Sept')
)

for(i in 1:#
    #length(fish.species)
    3
    ){
  input.fish <- paste0(fish.species[i])
  
  abun <- abundancedata[abundancedata$species == input.fish,]
  #abun$catch[is.na(abun$catch)] <- 0
  abun$logcatch <- log(abun$catch + 1)
  
  a <- ifelse(unique(abun$year) %in% c(2016, seq(2020,2023)), "red", 
              "blue")
  
  resras <- 
    ggplot() +
    
    geom_raster(data=abun,
                aes(y=as.factor(year),
                    x=week,
                    fill=logcatch#,
                    #alpha=as.factor(logcatch)
                )) +
    
    scale_fill_gradientn(colours = c(alpha('lightgray', 0.85),
                                     viridis::viridis(begin=0,
                                                      end=1,
                                                      n=999)),
                         na.value = NA,
                         n.breaks=2,
                         breaks=c(0, max(abun$logcatch, na.rm=T)),
                         labels = c('Low', 'Hi')) +
    
    
    labs(x = 'Week of year',
         y = 'Year',
         fill= "Relative abundance") +
    
    ggtitle(paste0(str_to_sentence(input.fish), ' abundance through time')) +
    theme(axis.text.y = element_text(colour = a)) +
    scale_x_continuous(breaks=month.week$week + 0.5,
                       labels=month.week$month)
  
  print(resras)
  
  ggsave(resras,
         filename=paste0("C:/Users/klankowicz/Box/Program Communications",
                         "/Fisheries Ecology — Program Communications/",
                         "Ecosystem Impact Reports/Visuals/",
                         "2023_EIR_Visuals/",
                         input.fish,
                         " temporal density.png"),
         dpi=300)
}

