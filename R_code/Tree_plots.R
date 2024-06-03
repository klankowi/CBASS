# Create tree plots (rectangle plots) of species relative abundance

rm(list=ls())

library(tidyverse)
library(here)
library(treemapify)

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
                plot.caption=element_text(hjust=0, face='italic', size=12),
                strip.text.x=element_text(size=14)))

# Set the name of the workbook
fname <- paste0(here('Clean_Data/seine_compiled_clean.xlsx'))
# Get info about all excel sheet names in workbook
sheets <- readxl::excel_sheets(fname)
# Read in as list item, each item is a sheet
data.all <- invisible(lapply(sheets, 
                             function(x) readxl::read_excel(fname, sheet = x)))
names(data.all) <- sheets
# Coerce list items to dataframes
data.all <- lapply(data.all, as.data.frame)

rm(fname, sheets)


#### Join similar species, abundance ####
data.all$abund <- data.all$abund[
  data.all$abund$species_name %notin% c('periwinkle',
                                        'horseshoe crab'),]

data.all$abund$species_name[
  data.all$abund$species_name %in% c('american eel',
                                     'glass eel elver')
] <- 'american eel'

data.all$abund$species_name[
  data.all$abund$species_name %in% c('hake', 
                                     'red hake',
                                     'spotted hake',
                                     'white hake')
] <- 'hake spp'

data.all$abund$species_name[
  data.all$abund$species_name %in% c('shortnose sturgeon',
                                     'unID sturgeon')
] <- 'sturgeon spp'

data.all$abund$species_name[
  data.all$abund$species_name %in% c('slimy sculpin',
                                     'striped sculpin',
                                     'unID sculpin')
] <- 'sculpin spp'

data.all$abund$species_name[
  data.all$abund$species_name %in% c('atlantic herring',
                                     'herring')
] <- 'atlantic herring'

data.all$abund$species_name[
  data.all$abund$species_name %in% c('banded killifish',
                                     'killifish')
] <- 'killifish spp'

data.all$abund$species_name[
  data.all$abund$species_name %in% c('northern pipefish',
                                     'pipefish')
] <- 'northern pipefish'

data.all$abund$species_name[
  data.all$abund$species_name %in% c('white mullet',
                                     'mullet')
] <- 'white mullet'

data.all$abund$species_name[
  data.all$abund$species_name %in% c('emerald shiner',
                                     'golden shiner',
                                     'unID shiner')
] <- 'shiner spp'

data.all$abund$species_name[
  data.all$abund$species_name %in% c('rock gunnel',
                                     'unID gunnel')
] <- 'rock gunnel'

data.all$abund$species_name[
  data.all$abund$species_name %in% c('unID stickelback')
] <- 'stickleback spp'

data.all$abund$species_name[data.all$abund$species_name == 'atlantic herring'] <- 
  'herring'

data.all$abund$species_name[data.all$abund$species_name == 'atlantic silverside'] <- 
  'silverside'

#### Trips per year ####
trips.year <- as.data.frame(table(data.all$trips$year))
colnames(trips.year) <- c('year', 'ntrips')
trips.year$period[trips.year$year %in% c(2014, 2015, 2017, 2018, 2019)] <- 'Coldest 5 years'
trips.year$period[trips.year$year %in% c(2016, 2020, 2021, 2022, 2023)] <- 'Warmest 5 years'
trips.period <- trips.year %>% 
  group_by(period) %>% 
  summarise(ntrips = sum(ntrips))

#### Call fish encountered on >10% trips ####
top <- data.all$abund
top <- as.data.frame(table(top$species_name))
colnames(top) <- c('Species', 'n_encounters')
top <- top[with(top, order(n_encounters, decreasing = T)),]

top$pct_encounter <- top$n_encounters / sum(trips.period$ntrips)
top$pct_encounter <- round(top$pct_encounter, 3)

top.spec <- top$Species[top$pct_encounter >=0.1]
top.spec <- droplevels(top.spec)

abund <- data.all$abund
abund$year <- lubridate::year(abund$date)

abund$period[abund$year %in% c(2014, 2015, 2017, 2018, 2019)] <- 'Coldest 5 years'
abund$period[abund$year %in% c(2016, 2020, 2021, 2022, 2023)] <- 'Warmest 5 years'

abund$period <- factor(abund$period, levels=c('Coldest 5 years', 
                                              'Warmest 5 years'))

abund$Species <- abund$species_name
abund$Species[abund$Species %notin% top.spec]  <- 'Other'

abund$Species <- factor(abund$Species,
                        levels=c('Other', 'silverside',
                                 'green crab', 'herring',
                                 'sandlance', 'winter flounder',
                                 'mummichog', 'alewife', 'tomcod'))

#### Plot ####
abund0.1 <- abund %>% 
  group_by(Species, period) %>% 
  summarise(catch=sum(catch))
abund0.1 <- merge(abund0.1, trips.period, by=c('period'))
abund0.1$catch <- abund0.1$catch / abund0.1$ntrips

tree.plot <- ggplot(data=abund0.1) +
  geom_treemap(aes(area=catch, fill=Species)) +
  scale_fill_viridis_d(option='viridis',
                       direction=-1,
                       begin=0.1, end=1) +
  geom_treemap_text(aes(area = catch, fill = Species, 
                        label = Species),
                    reflow=TRUE, grow=FALSE, min.size = 6,
                    place='centre')+
  facet_wrap(vars(period))  +
  #ggtitle('Periods, top 9 + other')+
  theme(legend.position = 'bottom',
        legend.text = element_text(size=10),
        legend.title = element_text(size=12),
        legend.key.size = unit(0.5, 'cm')) +
  labs(fill='Species')

ggsave(tree.plot,
       filename=paste0("C:/Users/klankowicz/Box/Program Communications",
                       "/Fisheries Ecology — Program Communications/",
                       "Ecosystem Impact Reports/Visuals/",
                       "2023_EIR_Visuals/treeplot_with_pictures_of_fish.png"),
       dpi=300)
