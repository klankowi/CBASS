rm(list=ls())

bio <- read.csv(here('Data/Clean_Data/Seine/lengths_through_2025.csv'))

bio <- bio %>% 
  filter(site_name %in% c('Audubon', 'Back Cove',
                          'Mussel Cove',
                          'Mackworth Island - Beach',
                          'Skitterygusset')) %>% 
  filter(species_name == 'atlantic silverside') %>% 
  mutate(date = as.Date(date, format = '%m/%d/%Y')) %>% 
  mutate(year = year(date),
         week = isoweek(date),
         doy = yday(date)) %>% 
  mutate(length_mm = as.numeric(length_mm)) %>% 
  filter(!is.na(length_mm) & !is.na(site_name)) %>% 
  dplyr::select(-loc_id, -site_id, -bay_location, -sex, -notes) %>% 
  filter(year != 2019 & year != 2024) %>% 
  filter(doy >=185)

bio$flag <- 0
for(i in 1:nrow(bio)){
  if(bio$week[i] <=30 & bio$length_mm[i] >=80){bio$flag[i] <- 1}
  if(bio$doy[i] <=225 & bio$length_mm[i]>=100){bio$flag[i] <- 1}
  if(bio$week[i] <=29 & bio$length_mm[i]>=50){bio$flag[i] <- 1}
}

bio <- bio[bio$flag ==0,]

ggplot(data = bio)  +
  geom_point(aes(x=doy, y=length_mm, col=as.factor(year)), alpha = 0.2,
             position = position_jitter(width = 2, height = 0)) +
  geom_smooth(aes(x=doy, y=length_mm, col = as.factor(year)),
              method = 'lm',
              formula = y ~ (x),
              lwd=1.5, se = F) + 
  scale_color_viridis_d() 
