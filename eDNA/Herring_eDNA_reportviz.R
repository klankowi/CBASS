rm(list=ls())

# Packages
library(tidyverse)
library(here)
library(sf)

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

# Load standard curve
sc <- read.csv(here("eDNA/Clean_Data/QBC_2023_Mids_ShortPrimer.csv"))
sc <- sc %>% 
  dplyr::select(Sample, Cq) %>% 
  mutate(Sample = Sample) %>% 
  filter(Sample != 100)

ggplot(data=sc, aes(x=seq(4, 1, -1), y=Cq)) +
  ggpmisc::stat_poly_line() +
  ggpmisc::stat_poly_eq(ggpmisc::use_label(c("eq", "R2"))) +
  geom_point()
Efficiency=(10^(-1/-3.411)) - 1

standard <- lm(Cq ~ log10(Sample), data=sc)

# Load data
dna <- read.csv(here('eDNA/Clean_Data/QBC_2023_Mids.csv'))
abund <- read.csv(here('Clean_Data/Seine/abund_through_2024.csv'))
trips <- read.csv(here('Clean_Data/Seine/trips_through_2024.csv'))

# Clean dna data
dna <- dna %>% 
  mutate(run = as.numeric(as.factor(received))) %>% 
  filter(dilution != 'b') %>% 
  dplyr::select(run, content, weeklab, plotlab, site_number, date, 
                cq, filtered) %>% 
  mutate(date = as.Date(date, format = '%m/%d/%Y'),
         site_number = as.numeric(site_number),
         content = factor(content),
         weeklab = factor(weeklab, levels=c('A', 'B', 'C', 'D',
                                             'E', 'F', 'G', 'H',
                                             'I', 'J')),
         plotlab = factor(plotlab, levels=c('A', 'B', 'C', 'D', 'E',
                                            'F', 'G','H', 'I','J','K',
                                            'L'))) %>% 
  mutate(label = paste0(month.abb[month(date)], ' ', day(date)))

# Clean trip and abundance data
abund <- abund %>% 
  mutate(date = as.Date(date, format="%m/%d/%Y")) %>% 
  mutate(month = month(date),
         year = year(date),
         week = isoweek(date)) %>% 
  filter(site_id > 20 & year == 2023) %>% 
  rename(site_number = site_id) %>% 
  filter(species_name %in% c('atlantic herring')) %>% 
  dplyr::select(date, site_number, site_name, species_name, catch)
  
trips <- trips %>% 
  mutate(date = as.Date(date, format="%m/%d/%Y")) %>% 
  mutate(month = month(date),
         year = year(date),
         week = isoweek(date)) %>% 
  filter(site_id > 20 & year == 2023) %>% 
  rename(site_number = site_id) %>% 
  dplyr::select(date, site_number, site_name, substrate, weather, 
               temp_degc, do_mg.l, salinity_ppt)

# Merge dna and abundance/ trip data
dna <- left_join(dna, trips, by=c('date', 'site_number'))
dna <- left_join(dna, dplyr::select(abund, -species_name),
                 by=c('date', 'site_number', 'site_name'))

# Pearson correlation of detection
pc <- dna %>% 
  filter(content == 'Sample') %>% 
  mutate(detect.catch = 'No catch',
         detect.dna = 'No eDNA detected')
pc$detect.dna[pc$cq < 39] <- 'eDNA detected'
pc$detect.catch[pc$catch > 0] <- 'Herring caught' 

pc <- as.matrix(table(pc$detect.catch, pc$detect.dna))

psych::phi(pc)

# Estimate relative DNA concentration from standard curve
dna$rel.dna <- (dna$cq  - 31.305) / -3.411
dna$rel.dna <- 10 ^ (dna$rel.dna)

# Adjust to filtered volume
dna$rel.dna <- (1000 / dna$filtered) * dna$rel.dna

# Adjust to relative scale
dna$rel.dna <- dna$rel.dna * 10

# Broad-space temporal trends of herring eDNA abundance (short primer)
conc <- ggplot() +
  geom_boxplot(data=dna[dna$content %in% c('Sample'),],
               aes(x=plotlab, y=rel.dna, group = plotlab)) +
  geom_jitter(data=dna[dna$content %in% c('Sample', 'Field Blank'),],
              aes(x=plotlab, y=rel.dna, col=content), 
              width = 0.2, alpha=0.9, cex=2) +
  scale_x_discrete(labels = c(unique(dna$label[dna$content == 'Sample']))) +
  labs(y= 'Relative eDNA conc.', x='Sampling date', col='Site') +
  guides(color = guide_legend(nrow = 1)) +
  theme(legend.margin = margin(0,0,0,0)) #+
  #ggtitle("Herring eDNA relative concentration for 2023 QBC mid-sites")
ggsave(plot = conc, here('concPlot.png'), width = 8.5, height = 3,
       units='in')
# Site-specific temporal trends of herring eDNA abundance
seas <- ggplot(data=dna[dna$content =='Sample',]) +
  geom_line(aes(x=date, y=rel.dna), lty=2) +
  geom_point(aes(x=date, y=rel.dna)) +
  scale_x_date(limits=c(as.Date('2023-06-01'), as.Date('2023-08-31'))) +
  facet_wrap(vars(site_name)) +
  labs(y= 'Relative eDNA concentration', x='Sampling date', col='Site') +
  guides(color = guide_legend(nrow = 1)) +
  theme(legend.margin = margin(0,0,0,0)) #+
  #ggtitle("Herring eDNA relative concentration for 2023 QBC mid-sites")
ggsave(plot = seas, here('seasPlot.png'),
       width = 8.5, height = 4, units = 'in')

# Identification of problematic runs
# dna$run <- paste0('Run ', dna$run)
# ggplot() +
#   geom_boxplot(data=dna[dna$content == 'Sample',],
#                aes(x=weeklab, y=cq, group = weeklab)) +
#   geom_jitter(data=dna[dna$content %notin% c('Sample'),],
#               aes(x=weeklab, y=cq, col=content, group = cq), 
#               alpha=1, cex=2, width = 0.2) +
#   scale_y_reverse(limits = c(44, 16)) +
#   labs(y= 'Cq', x='Sample label', col='Blank/\nControl',
#        caption = 'Sample-derived Cq values for each week are shown as boxplots. Compare those to the behavior of\nfield blank, lab blank, and negative control Cq values.') +
#   facet_wrap(vars(run), scale='free_x') +
#   guides(color = guide_legend(nrow = 1)) +
#   theme(legend.margin = margin(0,0,0,0)) +
#   ggtitle("Comparison of Blank/ Control/ Sample Cq values")

# Broad-time spatial trends of herring eDNA abundance (short primer)
sites <- read.csv(here('Clean_Data/Seine/sites_cleaned.csv'))
sites <- sites %>% 
  filter(bay_location == 'East')

dna <- merge(dna, sites, by=c('site_name', 'site_number'))

dna <- st_as_sf(dna, coords=c('longitude', 'latitude'), crs='EPSG:4326')

coast <- st_read(here('GIS/us_medium_shoreline_poly.shp'), quiet=T)
dna <- st_transform(dna, st_crs(coast))
dna$catch[is.na(dna$catch) & dna$content == 'Sample'] <- 0

dna <- dna %>% 
  filter(content == 'Sample') %>% 
  group_by(site_name) %>% 
  summarise(meandna = mean(rel.dna, na.rm=T),
            meancatch = mean(catch, na.rm=T))

placelabs <- data.frame(
  name = c('Cundys\nHarbor', 'Bailey\nIsland', "Orr's\nIsland", 'Great Island',
           'Harpswell', 'Brunswick'),#, 'Freeport'),
  
  lon = c(-69.893265, -69.996590, -69.969901, -69.905662,
          -69.989258, -69.941160),#, -70.051434),
  
  lat = c(43.796197, 43.724283, 43.768987, 43.841780,
          43.79148, 43.864851)#, 43.839683)
)

placelabs <- st_transform(st_as_sf(placelabs, coords=c('lon', 'lat'), crs="EPSG:4326"),
                          st_crs(coast))

dnaplot <- 
  ggplot() +
  geom_sf(data=coast) +
  geom_sf(data=dna, aes(size = meandna),
          alpha=0.5, fill='#00BFC4', stroke=0.7,
          col='black', pch=21) +
  geom_sf_text(data=placelabs,
             aes(label = name), 
             alpha=0.7, col='gray20') +
  scale_size(range = c(2, 10),
             breaks = c(0.255, 0.338, 0.574, 1.07),
             labels = c(0.26, 0.34, 0.57, 1.10)) +
  coord_sf(xlim=c(-70.049, -69.876),
           ylim=c(43.717, 43.865)) +
  scale_y_continuous(breaks = c(43.75, 43.8, 43.85),
                     labels = c('43.75\u00B0N', '43.80\u00B0N', '43.85\u00B0N')) +
  labs(x='', y='', size = 'Mean relative\neDNA conc.') +
  theme(legend.margin = margin(0,0,0,0))

catchplot <- 
  ggplot() +
  geom_sf(data=coast) +
  geom_sf(data=dna, aes(size = meancatch),
          alpha=0.5, fill='#F8766D', stroke=0.7,
          col='black', pch=21) +
  geom_sf_text(data=placelabs,
               aes(label = name), 
               alpha=0.7, col='gray20') +
  scale_size(range = c(0.5, 8)) +
  coord_sf(xlim=c(-70.049, -69.876),
           ylim=c(43.717, 43.865)) +
  scale_y_continuous(breaks = c(43.75, 43.8, 43.85),
                     labels = c('43.75\u00B0N', '43.80\u00B0N', '43.85\u00B0N')) +
  labs(x='', y='', size = 'Mean \ncatch') +
  theme(legend.margin = margin(0,0,0,0))

ggsave(plot = dnaplot, width = 4.25, height = 4.25, units = 'in',
       here('dnaPlot.png'))
ggsave(plot = catchplot, width = 4.25, height = 4.25, units = 'in',
       here('catchPlot.png'))
