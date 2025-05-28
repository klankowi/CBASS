# Identify cohorts by length
rm(list=ls())

# Load packages
library(tidyverse)
library(here)

# Duplicatability 
set.seed(109861235)

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

load(here('Clean_Data/Cohort_Data_2024.RData'))
rm(abund, bio, trips)

cohort.1 <- cohort.1[cohort.1$species_name %in% c('atlantic silverside',
                                                  'atlantic herring') &
                       cohort.1$Group ==1,]
growth.est <- growth.est[growth.est$species_name %in% c('atlantic silverside',
                                                  'atlantic herring') &
                           growth.est$Group == 1,]
model.1 <- model.1[model.1$species_name %in% c('atlantic silverside',
                                                  'atlantic herring') &
                     model.1$Group == 1,]


cohort.1$period[cohort.1$period == 'hot'] <- 'Warmer'
cohort.1$period[cohort.1$period == 'cold'] <- 'Cooler'

cohort.1$species_name <- str_to_title(cohort.1$species_name)

gro <- ggplot(data=cohort.1) +
  geom_jitter(aes(x = wk, y = length_mm, col=as.factor(year)),
              alpha=0.25, width=0.2, stroke=NA, cex=2) +
  geom_smooth(
    aes(x=wk, y=length_mm, col=as.factor(year)),
    se=F, method='glm', fullrange=T) +
  ggh4x::facet_grid2(species_name ~ period, scales='free_y') +
  scale_color_viridis_d(option='viridis') +
  labs(color='Year', x='Week of year', y='Length (mm)', lty='Heat') +
  guides(color = guide_legend(byrow = TRUE, nrow=1)) +
  theme(legend.margin = margin(0,0,0,0))
  
ggsave(plot=gro, 'growth.png')  
