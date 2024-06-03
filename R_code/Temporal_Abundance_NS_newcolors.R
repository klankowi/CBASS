
rm(list=ls())

library(here)
library(tidyverse)
library(shades)
library(colorblindcheck)

# Scale viridis colors from gray instead of purple
colfunc <- colorRampPalette(c("#8F9CA6", 
                              "#4D9A90", 
                              "#57AF90", 
                              "#4FC46AFF",
                              "#84D44BFF",
                              "#C1DF24FF",
                              "#FDE725FF"))
plot(rep(1,50),col=(colfunc(50)), pch=19,cex=2)
palette_check(colfunc(50), plot=TRUE)

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

for(i in 1:length(fish.species)){
  input.fish <- paste0(fish.species[i])
  
  abun <- abundancedata[abundancedata$species == input.fish,]
  #abun$catch[is.na(abun$catch)] <- 0
  abun$logcatch <- log(abun$catch + 1)
  
  nums <- unique(abun$logcatch[!is.na(abun$logcatch)])[
    order(unique(abun$logcatch[!is.na(abun$logcatch)]))]
  
  a <- ifelse(unique(abun$year) %in% c(2016, seq(2020,2023)), "red", 
              "blue")
  
  abun$logcatch[is.na(abun$logcatch)] <- 'No sampling'
  abun$logcatch <- as.factor(abun$logcatch)
  abun$logcatch <- factor(abun$logcatch,
                          levels=c('No sampling',
                                   nums))
  tablen <- length(table(abun$logcatch)) - 2
  
  fair_cols <- c(NA,
                 alpha('lightgray', 0.9),
                 #fade_cols, mid_cols, hi_cols
                 colfunc(n=tablen)
                 )
    
  names(fair_cols) <- levels(abun$logcatch)
  
  resras <- 
    ggplot() +
    
    geom_raster(data=abun,
                aes(y=as.factor(year),
                    x=week,
                    fill=logcatch#,
                    #alpha=as.factor(logcatch)
                )) +
    
    scale_fill_manual(values = fair_cols,
                         na.value = NA,
                      guide='none'
                         # n.breaks=2,
                         # breaks=c(0, max(as.numeric(pos$logcatch), na.rm=T)),
                         # labels = c('Low', 'Hi')
                         ) +
    
    
    labs(x = 'Week of year',
         y = 'Year',
         fill= "Relative abundance") +
    
    ggtitle(paste0(str_to_sentence(input.fish), ' abundance through time')) +
    theme(axis.text.y = element_text(colour = a)) +
    scale_x_continuous(breaks=month.week$week + 0.5,
                       labels=month.week$month)
  
  print(resras)
  
  # ggsave(resras,
  #        filename=paste0(here(), "/VAST_runs/tuna10_both/proportion.info.png"),
  #        driver)
}

