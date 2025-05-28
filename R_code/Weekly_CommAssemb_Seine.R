rm(list=ls())

library(tidyverse)
library(here)

# Negate function
'%notin%' <- function(x,y)!('%in%'(x,y))

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

# Load abundance data
abund <- read.csv(here('Clean_Data/Seine/abund_through_2024.csv'))

# Clean
abund <- abund %>% 
  mutate(date = as.Date(date, format="%m/%d/%Y")) %>% 
  mutate(month = month(date),
         year = year(date),
         week = isoweek(date))

abund$species_name[abund$species_name %in% c('fourspine stickleback',
                                             'ninespine stickleback',
                                             'stickleback spp',
                                             'threespine stickleback')] <- 'stickleback'

abund$species_name[abund$species_name %in% c('hake spp',
                                             'red hake',
                                             'spotted hake',
                                             'white hake')] <- 'hake'

abund$species_name[abund$species_name %in% c('grubby sculpin',
                                             'shorthorn sculpin',
                                             'longhorn sculpin',
                                             'sculpin spp', 
                                             'slimy sculpin')] <- 'sculpin'

abund$species_name[abund$species_name == 'common dab'] <- 'winter flounder'

abund$species_name[abund$species_name == 'sturgeon spp'] <- 'sturgeon'

# Clean
abund <- abund %>% 
  filter(year == 2024) %>% 
  filter(week != 40) %>% 
  dplyr::select(date, year, month, week, site_name, 
                species_name, catch)

# Labels
specab <- as.data.frame(unique(abund$species_name))
colnames(specab) <- 'species_name'
specab$abbr <- c('g.crab', 
                 'sculpin',
                 'hrrg', 
                 'tomcod',
                 'pipefsh',
                 'pollock',
                 'w.fldr',
                 'sturgeon',
                 'alewife', 
                 'slrsd',
                 'mmchg', 
                 'stcklbk',
                 'blfsh', 
                 'fllfsh',
                 'w.mllt',
                 'am.eel',
                 'hake',
                 'moonfsh',
                 'as.crab',
                 'mkrl',
                 'sndlnce',
                 'permit',
                 'c.jack',
                 'w.perch',
                 'saury')

qta <- abund %>% 
  group_by(week) %>%
  summarise(total = sum(catch))

qabund <- merge(abund, qta, by=c('week'))
qabund <- merge(qabund, specab, by=c('species_name'), all=T)
qabund <- qabund %>% 
  group_by(week, species_name, abbr) %>% 
  summarise(total = total[1],
            scatch = sum(catch)) %>% 
  mutate(pcatch = 100* (scatch / total)) %>% 
  mutate(label = ifelse(pcatch >=10.0, abbr, NA))

qabund <- qabund[with(qabund, order(week, species_name)),]

qabund$species_name <- factor(qabund$species_name,
                              levels=c('atlantic silverside', #1
                                       'asian shore crab',
                                       'bluefish',
                                       'alewife', #4
                                       'sturgeon',
                                       'stickleback',
                                       'atlantic tomcod', #7
                                       'winter flounder',
                                       'crevalle jack',
                                       'sculpin',#10
                                       'sandlance',
                                       'white perch',
                                       'mummichog', # 13
                                       'fallfish',
                                       'northern pipefish',
                                       'american eel',#16
                                       'atlantic moonfish',
                                       'permit',
                                       'white mullet',# 19
                                       'hake',
                                       'pollock',
                                       'green crab',#22
                                       'atlantic saury',
                                       'atlantic mackerel',
                                       'atlantic herring')) # 25

qabund <- qabund[with(qabund, order(week, species_name)),]

psum <- split(qabund, f=qabund$week)
for(i in 1:length(psum)){
  psum[[i]]$psum <- NA
  psum[[i]]$ypos <- NA
  psum[[i]] <- psum[[i]][with(psum[[i]],
                              order(species_name, decreasing = T)),]
  psum[[i]]$psum[1] <- psum[[i]]$pcatch[1]
  
  for(j in 2:nrow(psum[[i]])){
    psum[[i]]$psum[j] <- psum[[i]]$psum[(j-1)] + psum[[i]]$pcatch[j]
  }
  
  for(j in 1:nrow(psum[[i]])){
    if(!is.na(psum[[i]]$label[1])){
      psum[[i]]$ypos[1] <- psum[[i]]$psum[1] / 2
    }
  }
  
  for(j in 2:nrow(psum[[i]])){
    if(!is.na(psum[[i]]$label[j])){
      psum[[i]]$ypos[j] <- psum[[i]]$psum[(j-1)] + (psum[[i]]$pcatch[j]/2)
    }
  }
}

qabund <- do.call(rbind, psum)
weeks <- rep(seq(min(qabund$week), max(qabund$week, 1)))
weeks <- weeks[order(weeks)]
years <- 2024
years <- years[order(years)]
years <- rep(years, length(unique(weeks)))

weekdf <- data.frame(
  week = weeks,
  year = years
)

weekdf <- weekdf %>% 
  mutate(Week.of = as.Date(paste(year, week, 1, sep = "-"),
                           format = "%Y-%U-%u")) %>% 
  mutate(month = month(Week.of),
         day = day(Week.of)) %>% 
  mutate(month = month.abb[month]) %>% 
  mutate(Week.of = paste0(month, ' ', day)) %>% 
  dplyr::select(-month, -day)

qabund <- merge(qabund, weekdf, by=c('week'))

commass <- ggplot(data=qabund,
                  aes(x=week, y=pcatch, col=species_name, fill=species_name,
                      label=label)) +
  geom_bar(stat='identity', position = 'stack') +
  geom_label(aes(x=week, y=ypos),
             size = 3,
             col='black',
             fill='white') +
  scale_fill_viridis_d() +
  scale_color_viridis_d() +
  labs(x='Week of', y='% of weekly catch', col='Species', fill='Species') +
  scale_x_continuous(breaks=seq(min(weekdf$week), max(weekdf$week), 1),
                     labels=weekdf$Week.of) +
  ggtitle('2024 weekly seine community assemblage') +
  theme(legend.position = 'right',
        legend.key.height = unit(0.44, 'cm'),
        axis.text.x =element_text(angle = 35)) +
  guides(fill=guide_legend(ncol=1),
         color=guide_legend(ncol=1))
commass

ggsave(plot=commass,
       '2024communityassemblage.png',
       width = 7.75, height = 5, units='in')
