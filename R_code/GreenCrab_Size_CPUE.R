# Green crab LFA
rm(list=ls())

# Packages
library(here)
library(tidyverse)

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

# Load data
len <- read.csv(here('Data/Clean_Data/Seine/lengths_through_2025.csv'))
trips <- read.csv(here('Data/Clean_Data/Seine/trips_through_2025.csv'))

# Clean
trips <- trips %>% 
  mutate(date = as.Date(date, format = '%m/%d/%Y')) %>% 
  mutate(site_name = trimws(site_name, 'both')) %>% 
  mutate(site_id = as.numeric(as.factor(site_name))) %>% 
  mutate(notes = trimws(notes, 'both')) %>% 
  filter(notes %notin% c('very low tide, seine taken in about 1 ft of water',
                         'tide moving too fast, seine set was flipped',
                         'tide moving too fast, no fish, had to walk in seine net',
                         'short set',
                         'not normal site, set on the point',
                         'no fish; inverted set',
                         'no fish, bag tangled in low water',
                         'net snagged, probably released most of catch',
                         'low tide, set not ideal',
                         'low tide, walked seine in',
                         'had to wait after plankton tow for tide to rise enough to reach the beach, net came in folded - lots of fish escaping, alewife in ziploc #1',
                         'had to hand haul the net',
                         'failed set',
                         'caught jellyfish, bag flipped on rock',
                         'bad set, net did not get a chance to open up, mummichog outside of net',
                         'bad set no fish',
                         'bad set'
  )) %>% 
  # filter(site_name %notin% c('Orrs Island','Snow Island',
  #                            'Long Point Cove', 'Stovers Point',
  #                            'Garrison Cove', 'Cedar Beach',
  #                            'Lowell Cove')) %>% 
  mutate(week = isoweek(date)) %>% 
  dplyr::select(year, week, site_name, date) %>% 
  #filter(year %notin% c(2019)) %>% 
  filter(week >=24) %>% 
  filter(week<=39)

# Clean
len <- len %>% 
  mutate(date = as.Date(date, format = '%m/%d/%Y')) %>% 
  mutate(site_name = trimws(site_name, 'both')) %>% 
  mutate(species_name = trimws(species_name, 'both')) %>% 
  mutate(week = isoweek(date),
         year = year(date)) %>% 
  filter(species_name == 'green crab') %>% 
  # filter(site_name %notin% c('Orrs Island','Snow Island',
  #                            'Long Point Cove', 'Stovers Point',
  #                            'Garrison Cove', 'Cedar Beach',
  #                            'Lowell Cove')) %>% 
  #filter(year != 2019) %>% 
  filter(week>=24 & week<=39) %>% 
  dplyr::select(year, week, site_name, length_mm, date)

# Merge
total <- left_join(len, 
                   trips,
                   by=c('year', 'week', 'date', 'site_name'))

total <- total %>% 
  mutate(length_mm = as.numeric(length_mm))


# LF
all <- total %>% 
  group_by(year, week) %>% 
  summarise(total = n())

# Between August settlement and Oct 1, YOY reach 5.5mm
# Absence of exuviae until mid-May suggests cessation of growth for 7ish mos
# By next Oct 1, now Age-1 crabs reach 13-25mm

a0 <- total %>% 
  filter(length_mm <6) %>% 
  group_by(year, week) %>% 
  summarise(a0 = n())

a1 <- total %>% 
  filter(length_mm <=34) %>% 
  group_by(year, week) %>% 
  summarise(a1 = n())

a2p <- total %>% 
  filter(length_mm > 34) %>% 
  group_by(year, week) %>% 
  summarise(a2p = n())

fs <- left_join(
  all, a0,
  by=c('year', 'week')
)

fs <- left_join(
  fs, a1,
  by=c('year', 'week')
)

fs <- left_join(
  fs, a2p,
  by=c('year', 'week')
)

fs$a0[is.na(fs$a0)] <- 0
fs$a1[is.na(fs$a1)] <- 0
fs$a2p[is.na(fs$a2p)] <- 0

fs$a0.prop <- fs$a0 / fs$total
fs$a1.prop <- fs$a1 / fs$total
fs$a2p.prop <- fs$a2p / fs$total

ggplot(data=fs) +
  geom_line(aes(x=week, y=a0.prop)) +
  geom_point(aes(x=week, y=a0.prop)) +
  
  geom_line(aes(x=week, y=a1.prop), col='blue') +
  geom_point(aes(x=week, y=a1.prop), col='blue') +
  
  geom_line(aes(x=week, y=a2p.prop), col='red') +
  geom_point(aes(x=week, y=a2p.prop), col='red') +
  
  facet_wrap(vars(year))

# Weekly catch
# Load data
abun <- read.csv(here('Data/Clean_Data/Seine/abund_through_2025.csv'))

# Clean
abun <- abun %>% 
  mutate(date = as.Date(date, format = '%m/%d/%Y')) %>% 
  mutate(site_name = trimws(site_name, 'both')) %>% 
  mutate(species_name = trimws(species_name, 'both')) %>% 
  mutate(week = isoweek(date),
         year = year(date)) %>% 
  filter(species_name == 'green crab') %>% 
  # filter(site_name %notin% c('Orrs Island','Snow Island',
  #                            'Long Point Cove', 'Stovers Point',
  #                            'Garrison Cove', 'Cedar Beach',
  #                            'Lowell Cove')) %>% 
  #filter(year != 2019) %>% 
  filter(week>=24 & week<=39) %>% 
  dplyr::select(year, week, site_name, catch, date)

# Merge
total <- left_join(trips, abun, by=c('year', 'week', 'date', 'site_name'))

# Fix 0
total$catch[is.na(total$catch)] <- 0

# Weekly 
weekly <- total %>% 
  group_by(year, week) %>% 
  summarise(sets = n(),
            catch = sum(catch, na.rm = T))

# Weekly small catch estimation
weekly <- left_join(weekly, fs, by=c('year',  'week'))

# Estimate # of Age0-1 by prop of subsample
weekly$a0.est <- round(weekly$a0.prop * weekly$catch)
weekly$a1.est <- round(weekly$a1.prop * weekly$catch)
weekly$a2p.est <- round(weekly$a2p.prop * weekly$catch)

weekly$sumcheck <- weekly$a0.est + weekly$a1.est + weekly$a2p.est

# Estimate yearly size-based CPUE
yearly <- weekly %>% 
  group_by(year) %>% 
  summarise(sets = sum(sets),
            a0 = sum(a0.est),
            a1 = sum(a1.est),
            a2 = sum(a2p.est),
            catch = sum(catch)) %>% 
  mutate(a0.cpue = a0 / sets) %>% 
  mutate(a1.cpue = a1 / sets) %>% 
  mutate(a2.cpue = a2 / sets) %>% 
  mutate(all.cpue = catch / sets)

yearly <- yearly %>% 
  pivot_longer(cols=c('a0.cpue', 'a1.cpue', 'a2.cpue', 'all.cpue'),
               names_to = 'CPUE')

yearly$CPUE[yearly$CPUE == 'a0.cpue'] <- 'crabs <6mm'
yearly$CPUE[yearly$CPUE == 'a1.cpue'] <- 'crabs 0-34mm'
yearly$CPUE[yearly$CPUE == 'a2.cpue'] <- 'crabs >34mm'
yearly$CPUE[yearly$CPUE == 'all.cpue'] <- 'all crabs'

yearly$CPUE <- factor(yearly$CPUE, levels=c('crabs <6mm',
                                            'crabs 0-34mm',
                                            'crabs >34mm',
                                            'all crabs'))

ggplot(data=yearly) +
  geom_point(aes(x=year, y=value, col=CPUE),cex=1.3) +
  geom_line(aes(x=year, y=value, col=CPUE), lwd=1) +
  labs(x='Year', y='CPUE', color='Size class')

write.csv(yearly,
          here('Data/Clean_Data/Seine/GreenCrab_Size_CPUE2.csv'),
          row.names = F)

## Corrs
library(tseries)
library(forecast)
big.ts <- ts(yearly$value[yearly$CPUE == 'crabs >25mm'])
big.tsaa <- auto.arima(big.ts, seasonal=F, test="kpss")

# Check residuals
checkresiduals(big.tsaa, plot=T)
Box.test(big.tsaa$residuals, type="Lj")

small.ts <- ts(yearly$value[yearly$CPUE == 'crabs 6-25mm'])
small.tsaa <- auto.arima(small.ts, seasonal=F, test="kpss")

# Check residuals
checkresiduals(small.tsaa, plot=T)
Box.test(small.tsaa$residuals, type="Lj")

# Test correlation
cf1 <- ccf(small.tsaa$residuals, # The x time series
           big.tsaa$residuals, # The y time series
           type='correlation',
           main = paste0('Big vs. Small crab CPUE'))

vis <- data.frame(
  year = seq(2014, 2025, 1),
  big = as.numeric(big.tsaa$residuals),
  small = as.numeric(small.tsaa$residuals)
)

ggplot(data=vis) +
  geom_line(aes(x=year, y=big), col='black') +
  geom_line(aes(x=year, y=small), col='blue') +
  scale_x_continuous(breaks = seq(2014, 2025, 1))
