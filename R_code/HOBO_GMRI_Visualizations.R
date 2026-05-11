rm(list=ls())

library(here)
library(tidyverse)

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

trips <- read.csv(here('Data/Clean_Data/Seine/trips_through_2025.csv'))

trips <- trips %>% 
  filter(site_id <20) %>% 
  filter(year == 2025) %>% 
  dplyr::select(date, site_name) %>%
  rename(site = site_name) %>% 
  mutate(date = as.Date(date, format = '%m/%d/%Y'))

trips$site[trips$site == 'Alewife Cove'] <- 'Alewife'
trips$site[trips$site == 'Back Cove'] <- 'BackCove'
trips$site[trips$site == 'Cushing Island'] <- 'Cushing'
trips$site[trips$site == 'Great Diamond Island'] <- 'GDI'
trips$site[trips$site == 'Mackworth Island - North'] <- 'MackNorth'
trips$site[trips$site == 'Mackworth Island - Beach'] <- 'MackBeach'
trips$site[trips$site == 'Mussel Cove'] <- 'Mussel'
trips$site[trips$site == 'Presumpscot Moorings'] <- 'Presumpscot'
trips$site[trips$site == 'Skitterygusset'] <- 'Skittery'
trips$site[trips$site == 'The Brothers - North'] <- 'Brothers'

sites.EDT <- c('Audubon', 'Mussel' )

sites.EST <- c('Skittery', 'Brothers', 'Presumpscot', 'BackCove',
               'MackBeach', 'MackNorth', 'GDI',  'Cushing', 'SMCC', 'Alewife')

data <- data.frame(
  date = NA,
  temp = NA,
  site = NA
)

for(i in 1:length(sites.EST)){
  site <- read.csv(here(paste0('Data/Raw_Data/HOBOs/', 
                               sites.EST[i], '.csv')))
  
  site <- site %>% 
    rename(date = Date.Time..EDT.EST.,
           temp = Temperature.....C.) %>% 
    dplyr::select(date, temp) %>% 
    mutate(site = sites.EST[i])
  
  data <- rbind(data, site)
  
  rm(site)
}

for(i in 1:length(sites.EDT)){
  site <- read.csv(here(paste0('Data/Raw_Data/HOBOs/', 
                               sites.EDT[i], '.csv')))
  
  site <- site %>% 
    rename(date = Date.Time..EDT.,
           temp = Temperature.....C.) %>% 
    dplyr::select(date, temp) %>% 
    mutate(site = sites.EDT[i])
  
  data <- rbind(data, site)
  
  rm(site)
}
data <- data[-c(1),]

data <- data %>% 
  mutate(date = as.POSIXct(date, 
                           format = '%m/%d/%Y %H:%M:%S',
                           tz='America/New_York')) %>% 
  mutate(temp = as.numeric(temp)) %>% 
  mutate(site = factor(site,
                       levels=c('Alewife', 'Cushing', 'SMCC',
                                'GDI', 'BackCove', 'MackBeach',
                                'MackNorth', 'Brothers', 'Mussel',
                                'Audubon', 'Skittery', 'Presumpscot'))) %>% 
  mutate(month = month(date)) %>% 
  mutate(date = as.Date(date))

data <- data %>% 
  group_by(site, date, month) %>% 
  summarise(temp = mean(temp, na.rm=T))

loggers <- ggplot(data=data) +
  geom_line(aes(x=date, y=temp, col=as.factor(month)),
            alpha=0.9) +
  geom_point(data=trips,
             aes(x=date, y=8), 
             col='salmon', pch=17) +
  #scale_color_viridis_d(end = 0.85) +
  facet_wrap(vars(site)) +
  theme(legend.position = 'n') +
  labs(x='Date', y='Temperature (°C)') +
  ggtitle('HOBO data loggers 2025')

ggsave(loggers,
       filename = here('Data Loggers 2025.png'),
       width = 11, height = 8, units = 'in')
