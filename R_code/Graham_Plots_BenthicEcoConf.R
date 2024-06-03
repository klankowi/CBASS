rm(list=ls())


library(tidyverse)
library(here)
library(DT)
library(pdftools)
library(patchwork)
library(ggiraph)
library(here)
library(tidyverse)
library(sf)
library(kableExtra)
library(pander)
library(pmetar)
library(treemapify)
library(data.table)
library(ggnewscale)

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

#### Join similar species, lengths ####
data.all$bio <- data.all$bio[
  data.all$bio$species_name %notin% c('periwinkle',
                                      'horseshoe crab'),]

data.all$bio$species_name[
  data.all$bio$species_name %in% c('american eel',
                                   'glass eel elver')
] <- 'american eel'

data.all$bio$species_name[
  data.all$bio$species_name %in% c('hake', 
                                   'red hake',
                                   'spotted hake',
                                   'white hake')
] <- 'hake spp'

data.all$bio$species_name[
  data.all$bio$species_name %in% c('shortnose sturgeon',
                                   'unID sturgeon')
] <- 'sturgeon spp'

data.all$bio$species_name[
  data.all$bio$species_name %in% c('grubby sculpin',
                                   'longhorn sculpin',
                                   'shorthorn sculpin',
                                   'slimy sculpin',
                                   'striped sculpin',
                                   'unID sculpin')
] <- 'sculpin spp'

data.all$bio$species_name[
  data.all$bio$species_name %in% c('atlantic herring',
                                   'herring')
] <- 'atlantic herring'

data.all$bio$species_name[
  data.all$bio$species_name %in% c('banded killifish',
                                   'killifish')
] <- 'killifish spp'

data.all$bio$species_name[
  data.all$bio$species_name %in% c('northern pipefish',
                                   'pipefish')
] <- 'northern pipefish'

data.all$bio$species_name[
  data.all$bio$species_name %in% c('white mullet',
                                   'mullet')
] <- 'white mullet'

data.all$bio$species_name[
  data.all$bio$species_name %in% c('emerald shiner',
                                   'golden shiner',
                                   'unID shiner')
] <- 'shiner spp'

data.all$bio$species_name[
  data.all$bio$species_name %in% c('rock gunnel',
                                   'unID gunnel')
] <- 'rock gunnel'

data.all$bio$species_name[
  data.all$bio$species_name %in% c('threespine stickleback',
                                   'fourspine stickleback',
                                   'ninespine stickleback',
                                   'unID stickelback')
] <- 'stickleback spp'

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
  data.all$abund$species_name %in% c('grubby sculpin',
                                     'longhorn sculpin',
                                     'shorthorn sculpin',
                                     'slimy sculpin',
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
  data.all$abund$species_name %in% c('threespine stickleback',
                                     'fourspine stickleback',
                                     'ninespine stickleback',
                                     'unID stickelback')
] <- 'stickleback spp'

load(here('Clean_Data/Cohort_Data.RData'))

# Pull lengths
lengths <- dplyr::select(data.all$bio,
                         species_name, length_mm, date)

# Remove one large mummichog
lengths <- lengths[lengths$length_mm != 385,]

# Set week
lengths$wk <- lubridate::isoweek(lengths$date)

# Remove early weeks
#lengths <- lengths[lengths$wk >=24,]

# Set year
lengths$year <- lubridate::year(lengths$date)

# Force to numeric
lengths$length_mm <- as.numeric(lengths$length_mm)

# Remove missing values
lengths <- lengths[!is.na(lengths$length_mm),]

# Fix data types
lengths <- lengths %>% 
  mutate_at('species_name', as.factor) %>% 
  mutate_at(c('length_mm', 'wk', 'year'), as.numeric)

# Find biological limits
biolims <- lengths %>% 
  group_by(species_name) %>% 
  summarise(min.length = min(length_mm),
            max.length = max(length_mm))

#### Focus on top 8 ####
lengths <- lengths[lengths$species_name %in% 
                     c('alewife', 'atlantic silverside',
                       'atlantic herring', 
                       'winter flounder', 'mummichog', 
                       'tomcod', 'sandlance'),]
biolims <- biolims[biolims$species_name %in% lengths$species_name,]
biolims <- biolims[with(biolims, order(species_name)),]
rownames(biolims) <- NULL

Linf.all <- data.frame(
  species_name = biolims$species_name,
  L.inf.cm = c(40, 45, 18, 
               15, 23.5, 38.1, 64.0),
  agemax = c(9, 25, 2,
             4, 12, 4, 14)
)
Linf.all$Linf <- Linf.all$L.inf.cm * 10
Linf.all$L.inf.cm <- NULL

biolims <- merge(biolims, Linf.all, by=c('species_name'))

lengths <- lengths[with(lengths, order(species_name, wk, length_mm)),]

#### Split by period ####
lengths$period[lengths$year %in% c(2014, 2015, 2017, 2018, 2019)] <- 'cold'
lengths$period[lengths$year %in% c(2016, 2020, 2021, 2022, 2023)] <- 'hot'
lengths$species_name <- droplevels(lengths$species_name)

biolims <- biolims[biolims$species_name %notin% 
                     c('tomcod'),]

lengths <- lengths[lengths$species_name %in% biolims$species_name,]

cohort.1$Group[cohort.1$Group == 1] <- 'A'
cohort.1$Group[cohort.1$Group == 2] <- 'B'

cohort.1 <- cohort.1 %>% 
  filter(species_name %in% c('atlantic silverside', 'atlantic herring'),
         Group == 'A') %>% 
  mutate(species_name = str_to_sentence(species_name))

growth.by.week <-     
  ggplot(data=cohort.1) +
      geom_jitter(aes(x = wk, y = length_mm, col=as.factor(year)),
                 alpha=0.04, width=0.2, stroke=NA, cex=2) +
      
      geom_smooth(
        aes(x=wk, y=length_mm, col=as.factor(year), lty=period),
        se=F, method='glm', fullrange=T) +
  
  guides(color = guide_legend(override.aes = list(lwd = 2)),
         lty = guide_legend(override.aes = list(lwd = 0.7))) +
      
      scale_color_viridis_d(option='viridis') +
      
      labs(color='Year', x='Week of year', y='Length (mm)', lty='Heat') +
      
      facet_wrap(vars(species_name), nrow = 3, scales='free_y') +
      
      theme(legend.position = 'right',
            strip.text.x = element_text(size=12),
            legend.title = element_text(size=12),
            legend.text = element_text(size=12),
            axis.title.x = element_text(size=14),
            axis.title.y= element_text(size=14),
            axis.text.x = element_text(size=12),
            axis.text.y = element_text(size=12))  

growth.by.week

    growth.est$species_name <- str_to_sentence(growth.est$species_name)
    growth.est$period <- str_to_sentence(growth.est$period)
    growth.est$period <- factor(growth.est$period,
                                levels=c('Hot', 'Cold'))
    
    sig.text <- data.frame(
      species_name = 'Atlantic silverside',
      Group = 1,
      lab='Text',
      period= as.factor('Cold'),
      growth=0.85
    )
    
significance <-     
    ggplot(data=growth.est[growth.est$species_name %in% c('Atlantic herring', 'Atlantic silverside') &
                           growth.est$Group == 1,]) +
      geom_boxplot(aes(x=period, y=growth/7,
                       fill=period)) +
      facet_wrap(vars(species_name),
                 scales = "free_x") +
      labs(y='Growth rate (mm/day)', 
           x='Cohort',
           fill='Heat period') +
      theme(legend.position = 'bottom',
            strip.text.x = element_text(size=12),
            axis.text.x = element_blank(),
            axis.title.x = element_blank(),
            axis.ticks = element_blank(),
            legend.text = element_text(size=12),
            legend.title = element_text(size=12),
            axis.text.y = element_text(size=12),
            axis.title.y = element_text(size=14))


significance <- 
  significance    + 
  geom_text(data=sig.text, label='*',
            size=15, x=1.5,
            aes(y=growth))

significance


getwd()
ggsave('herring_silverside_weekly_growth.png',
       growth.by.week,
       height = 8.5, width = 11, units='in')

ggsave('daily_growth_comparison.png',
       significance,
       height = 4.25, width = 5.5, units='in')
