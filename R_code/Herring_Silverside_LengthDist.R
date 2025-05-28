rm(list=ls())

library(here)
library(tidyverse)
library(mgcv)

# Negate function
'%notin%' <- function(x,y)!('%in%'(x,y))

#### Make sure there's relatively equal representation across weeks ####
trips <- read.csv(here('Clean_Data/Seine/trips_through_2024.csv'))

# Identify weeks with limited sampling across years
trips <- trips %>% 
  # Add date vals
  mutate(date = as.Date(date, format = '%m/%d/%Y')) %>% 
  mutate(week = isoweek(date),
         year = year(date),
         month = month(date),
         day = day(date)) %>% 
  # Get rid of 2019
  filter(year != 2019) %>% 
# Drop weeks 40 and 41 (only happened in 2024)
# Drop weeks 22 and 23 (only happened in 2020s)
  filter(week>=24 & week<=39)

trips2 <- trips %>% 
  dplyr::select(week, year) %>% 
  group_by(week, year) %>% 
  summarise(ntrips = n())

ggplot(data = trips2) +
  geom_tile(aes(x=week, y=year, fill=ntrips),
            width = 0.9) +
  binned_scale(aesthetics = "fill",
               scale_name = "stepsn", 
               palette = function(x) c(viridis::viridis(n=4)),
               #breaks = c(1, 2, 3, 4),
               #limits = c(0, 5),
               guide = "colorsteps"
  ) +
  theme(legend.key.width = unit(0.5, 'in')) +
  ggtitle('Assessment of sampling effort')
# Keep weeks 24 through 36

#### Filter length data ####
size <- read.csv(here('Clean_Data/seine/lengths_through_2024.csv'))

size <- size %>% 
  mutate(date = as.Date(date, format = '%m/%d/%Y')) %>% 
  filter(loc_id %in% trips$loc_id) %>% 
  dplyr::select(date, species_name, length_mm) %>% 
  mutate(week = isoweek(date),
         year = year(date)) %>% 
  mutate(length_mm = as.numeric(length_mm)) %>% 
  filter(!is.na(length_mm)) #%>% 
  #filter(week >=24 & week<=36)

herr <- size %>% 
  filter(species_name == 'atlantic herring')

sil <- size %>% 
  filter(species_name == 'atlantic silverside')

#### GAMs ####
# Herring
hg <- gam(length_mm ~ s(week, bs='cs') + s(year, bs='cs'),
          method = 'REML', data = herr)
summary(hg)
#plot(hg, scheme = 1, select = 1, residuals = T)
#plot(hg, scheme = 1, select = 2, residuals = T)

wdf <- data.frame(
  week = seq(min(herr$week), 
             max(herr$week), 
             0.1),
  year = 2019
)

phg <- predict.gam(hg, wdf,
            exclude = "s(year)",
            se.fit=T)

phg <- data.frame(
  week = seq(min(herr$week), 
             max(herr$week), 
             0.1),
  fit = phg$fit,
  se.fit = phg$se.fit,
  Species = 'Herring'
)

# Silvers
sg <- gam(length_mm ~ s(week, bs='cs') + s(year, bs='cs'),
          method = 'REML', data = sil)
summary(sg)
#plot(sg, scheme = 1, select = 1, residuals = T)
#plot(sg, scheme = 1, select = 2, residuals = T)

wdf <- data.frame(
  week = seq(min(sil$week), 
             max(sil$week), 
             0.1),
  year = 2019
)

psg <- predict.gam(sg, wdf,
                   exclude = "s(year)",
                   se.fit=T)

psg <- data.frame(
  week = seq(min(sil$week), 
             max(sil$week), 
             0.1),
  fit = psg$fit,
  se.fit = psg$se.fit,
  Species = 'Silverside'
)

#### Plot ####
gherring <- ggplot(data = phg) +
  geom_jitter(data=herr,
              aes(x=week, y=length_mm),
              alpha=0.2, width = 0.3,
              pch=20, stroke=NA,
              col='gray40') +
  geom_ribbon(aes(x=week, ymin = fit-se.fit, ymax=fit+se.fit),
              fill='lightblue') +
  geom_line(aes(x=week, y=fit), col='navyblue') +
  labs(x='Week of year', y='Length (mm)') +
  facet_wrap(vars(Species)) +
  theme(axis.title.x = element_blank(), axis.text.x = element_blank()) +
  ylim(0, 215) +
  xlim(24, 39) +
  theme(axis.ticks.length.x = unit(0, "cm")) +
  theme(plot.margin = unit(c(0,0,0,0), "cm"))

gsilver <- ggplot(data = psg) +
  geom_jitter(data=sil,
              aes(x=week, y=length_mm),
              alpha=0.2, width = 0.3,
              pch=20, stroke=NA, col='gray40') +
  geom_ribbon(aes(x=week, ymin = fit-se.fit, ymax=fit+se.fit),
              fill='lightblue') +
  geom_line(aes(x=week, y=fit), col='navyblue') +
  labs(x='Week of year', y='Length (mm)') +
  facet_wrap(vars(Species)) +
  ylim(0, 215) +
  xlim(24, 39)

hs.ld <- egg::ggarrange(gherring, gsilver, nrow=2)
ggsave(plot=hs.ld,
       here('Documentation/MEPS/Figures/herring_silverside_lengthdist_week.png'),
       width=81, height = 121, units='mm')
