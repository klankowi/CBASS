rm(list=ls())

# Packages
library(here)
library(tidyverse)

# Load data
oysters <- read.csv(here('Oyster_Condition_Index/Raw_OCI.csv'))

# Clean data
# FILTERING:  must have wet meat weight value
#             wet meat plus wet shell weight must be less than or equal to wet 
#                     whole weight
#             dry meat weight must be less than wet meat weight
#             dry shell weight must be less than wet shell weight
#             end data collection at week 22 2024
#             
oysters <- oysters %>% 
  mutate(date = as.POSIXct(date, format="%m/%d/%Y"),
         Site = factor(Site, levels=c('Dogs Head', 'Snow Island'))) %>% 
  dplyr::select(date, Site, Oyster_length_mm, Oyster_width_mm,
                Whole.oyster_weight_g, Wet_shell_weight_g, 
                Wet_meat_weight_g, Dry_shell_weight_g, 
                Dry_meat_weight_g) %>% 
  rename(Whole_oyster_weight_g = Whole.oyster_weight_g) %>% 
  mutate(Index_1 = (Dry_meat_weight_g / 
                      (Whole_oyster_weight_g - Wet_shell_weight_g)) * 100,
         Index_3 = (Dry_meat_weight_g*100) / Dry_shell_weight_g) %>% 
  filter(Dry_meat_weight_g < Wet_meat_weight_g &
         Dry_shell_weight_g < Wet_shell_weight_g &
         (Wet_meat_weight_g + Wet_shell_weight_g) <= Whole_oyster_weight_g &
         date <= as.POSIXct('2024-06-01')) %>% 
  mutate(year = year(date),
         week = week(date))

# Lowercase column names
colnames(oysters) <- tolower(colnames(oysters))

# Check
str(oysters)
summary(oysters)

# Weekly summary
sitebased <- oysters %>% 
  group_by(site, date) %>% 
  summarise(mean_1 = mean(index_1, na.rm=T),
            mean_3 = mean(index_3, na.rm=T))

ggplot(data=oysters) +
  geom_point(aes(x=date, y=index_1), alpha=0.5, stroke=NA) +
  geom_smooth(aes(x=date, y=index_1), 
              method='gam',
              formula=y ~ s(x, bs = "cs", k = 10),
              alpha=0.3) +
  facet_wrap(vars(site)) +
  labs(x='Date', y='Index 1')
oysters$week <- str_pad(oysters$week, 2, 'left', '0')
oysters$wkyr <- as.numeric(paste0(oysters$year, oysters$week))
oysters$wkyr <- as.numeric(as.factor(oysters$wkyr))

library(mgcv)
gam1 <- gam(index_1 ~ s(wkyr, bs='cs', by=site, k=10), 
    data=oysters,
    method='REML')
#summary(gam1)
plot(gam1)
gam.check(gam1)
