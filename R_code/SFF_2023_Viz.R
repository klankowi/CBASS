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

abund <- read.csv(here('Clean_Data/Seine/abund_through_2024.csv'))
trips <- read.csv(here('Clean_Data/Seine/trips_through_2024.csv'))
bio <- read.csv(here('Clean_Data/Seine/lengths_through_2024.csv'))

# Clean
abund <- abund %>% 
  mutate(date = as.Date(date, format="%m/%d/%Y")) %>% 
  mutate(month = month(date),
         year = year(date),
         week = isoweek(date))
abund$collector[abund$site_id >20] <- 'QBC'
abund$collector[abund$site_id <20] <- 'GMRI'

trips <- trips %>% 
  mutate(date = as.Date(date, format="%m/%d/%Y")) %>% 
  mutate(month = month(date),
         year = year(date),
         week = isoweek(date))
trips$collector <- NA
trips$collector[trips$site_id >20] <- 'QBC'
trips$collector[trips$site_id <20] <- 'GMRI'

bio <- bio %>% 
  mutate(date = as.Date(date, format="%Y-%m-%d")) %>% 
  mutate(month = month(date),
         year = year(date),
         week = isoweek(date))
bio$collector[bio$site_id >20] <- 'QBC'
bio$collector[bio$site_id <20] <- 'GMRI'

#### Tomcod by site ####
tomcod <- abund[abund$species_name == 'atlantic tomcod',]
tomcod <- tomcod %>% 
  dplyr::select(date, year, site_name, catch)

tomcod <- merge(tomcod, trips, by=c('date', 'year','site_name'), all=T)
tomcod$catch[is.na(tomcod$catch)] <- 0

ggplot(data=tomcod[tomcod$catch != 0,]) +
  geom_bar(aes(x=year, y=catch, fill=site_name),
           stat='identity') +
  scale_fill_viridis_d() +
  ggtitle('Tomcod catch by site and year')

#### Tomcod by month-year ####
ytom <- tomcod %>% 
  group_by(year, month) %>% 
  summarise(ntrips = n(),
            catch = sum(catch)) %>% 
  mutate(cpue = catch / ntrips) %>% 
  as.data.frame() %>% 
  mutate(date = as.Date(paste0(year, '-',
                               str_pad(month, 2, 'left', '0'),
                               '-01')))

ggplot(data=ytom) +
  geom_point(aes(x=month, y=cpue)) +
  facet_wrap(vars(year)) +
  ggtitle('Tomcod CPUE per month and year')

#### Temperature ####
datevec <- seq.Date(as.Date('2014-06-01'), 
                    as.Date('2024-10-15'), 'day')
datevec <- as.data.frame(datevec)
colnames(datevec) <- 'date'
datevec <- datevec %>% 
  mutate(month = month(date),
         year = year(date),
         day = day(date)) %>% 
  filter(month %in% c(6, 7, 8, 9, 10))
for(i in 1:length(datevec)){
  if(datevec$month[i] == 10 & datevec$day[i]>15){
    datevec$day[i] <- NA
  }
}
datevec <- datevec[!is.na(datevec$day),]

jpm <- read.csv(here('Clean_Data/Meteorological/Portland_watertemp_POR.csv'))

jpm <- jpm %>% 
  mutate(date = as.Date(timestamp, format='%Y-%m-%d %H:%M:%S')) %>% 
  filter(date >= as.Date('2008-11-01')) %>% 
  filter(date <= as.Date('2024-10-31')) %>% 
  mutate(week = week(date),
         year = year(date),
         month = month(date)) %>% 
  mutate(spawnyear = year)

jpm <- jpm %>% 
  dplyr::select(date, week, month, year, 
                daily.c, spawnyear) %>% 
  unique()

for(i in 1:nrow(jpm)){
  if(jpm$month[i] %in% c(11, 12)){jpm$spawnyear[i] <- (jpm$year[i] + 1)}
}
                
jpm$spawning <- 'No'
jpm$spawning[jpm$month %in% c(11, 12, 1, 2)] <- 'Yes'

jpm$spawn <- NA
jpm$spawn[jpm$daily.c >=-1.111 & jpm$daily.c<=6.111] <- 'Yes'
jpm$spawn[jpm$daily.c <= (-1.112)] <- 'Cold'
jpm$spawn[jpm$daily.c > 6.111] <- 'Hot'

jpm$optim <- NA
jpm$optim[jpm$daily.c >=2.7 & jpm$daily.c<=12.6] <- 'Yes'
jpm$optim[jpm$daily.c <= (2.7)] <- 'Cold'
jpm$optim[jpm$daily.c > 12.6] <- 'Hot'

jpm.spawn <- jpm

spawndates <- c(seq.Date(as.Date('2008-11-01'),
                         as.Date('2024-11-01'),
                         'year'),
                seq.Date(as.Date('2009-02-28'),
                         as.Date('2024-02-28'),
                         'year'))

movedates <- c(seq.Date(as.Date('2009-03-01'),
                        as.Date('2024-03-01'),
                        'year'),
               seq.Date(as.Date('2009-05-31'),
                        as.Date('2025-05-31'),
                        'year'))

sampdates <- c(seq.Date(as.Date('2014-05-31'),
                        as.Date('2024-05-31'),
                        'year'),
               seq.Date(as.Date('2014-10-09'),
                        as.Date('2024-10-09'),
                        'year'))

ggplot(data=jpm.spawn[jpm.spawn$spawnyear > 2013,]) +
  geom_line(aes(x=date, y=daily.c)) +
  facet_wrap(vars(spawnyear), scales='free_x') +
  # geom_ribbon(data=jpm.spawn[jpm.spawn$spawnyear > 2013,],
  #             aes(x=date, ymin=rep(-1.1, nrow(jpm.spawn[jpm.spawn$spawnyear > 2013,])),
  #                 ymax = rep(6.1), nrow(jpm.spawn[jpm.spawn$spawnyear > 2013,])),
  #             alpha=0.1, fill='springgreen') +
  geom_ribbon(data=jpm.spawn[jpm.spawn$spawnyear > 2013,],
              aes(x=date, ymin=rep(2.7, nrow(jpm.spawn[jpm.spawn$spawnyear > 2013,])),
                  ymax = rep(12.6), nrow(jpm.spawn[jpm.spawn$spawnyear > 2013,])),
              alpha=0.1, fill='blue') +
  #geom_vline(lty=2, xintercept=spawndates, col='blue') +
  #geom_vline(lty=2, xintercept=sampdates, col='red') +
  ggtitle('Daily avg temperature') + labs(x='Date', y='Daily avg. temperature (C)')

# ggplot(data=jpm.spawn[jpm.spawn$month %in% c(5, 6, 7, 8, 9, 10) &
#                       jpm.spawn$year >=2014,]) +
#   
#   geom_line(aes(x=date, y=cfp)) +
#   
#   geom_point(data=tomcod, aes(x=date, y=catch/16),
#              alpha=0.4, col='red') +
#   
#   facet_wrap(vars(spawnyear), scales = 'free_x') +
#   
#   coord_cartesian(ylim=c(-2, 3)) +
# 
#   ggtitle('Daily avg temperature')

jpm.spawn$spawnday <- sequence(rle(as.character(jpm.spawn$spawn))$lengths)
jpm.spawn$spawnday[jpm.spawn$spawn != 'Yes'] <- NA

jpm.spawn$suitday <- sequence(rle(as.character(jpm.spawn$optim))$lengths)
jpm.spawn$suitday[jpm.spawn$optim != 'Yes'] <- NA

jpm.suit <- jpm.spawn %>% 
  group_by(spawnyear) %>% 
  summarise(nspawnday = max(spawnday, na.rm=T),
            noptimday = max(suitday, na.rm=T)) %>% 
  as.data.frame()
jpm.suit

#### Tomcod by previous rolling month's temperature ####
tomcod <- tomcod %>% 
  group_by(date) %>% 
  summarise(catch= sum(catch),
            ntrips = n()) %>% 
  mutate(cpue = catch / ntrips)

tomcod <- merge(tomcod,
                dplyr::select(jpm, date, daily.c),
                all=T)

#### 2024  ####
trips <- trips %>%
  filter(year == 2023) %>% 
  dplyr::select(date, year, month, week, collector, site_name, set_time)

abund <- abund %>% 
  filter(year == 2023) %>% 
  dplyr::select(date, year, month, week, collector, site_name, 
                species_name, catch)

bio <- bio %>% 
  filter(year == 2023) %>% 
  dplyr::select(date, year, month, week, collector, site_name,
                species_name, sex, length_mm)

#### Effort ####
# For QBC
qtrips <- trips %>% 
  filter(collector == 'QBC') %>% 
  #mutate(set_time = paste0(set_time, ':00')) %>% 
  mutate(#date = as.Date(date),
         set_time = as.POSIXct(set_time,
                               format = "%m/%d/%Y %H:%M")) %>% 
  group_by(date) %>% 
  summarise(diftime = difftime(max(set_time, na.rm=T), 
                               min(set_time, na.rm=T), 
                               units='hours'))

sum(qtrips$diftime) + (nrow(qtrips) * as.difftime(0.5, units='hours'))
nrow(trips[trips$collector == 'QBC',])
length(unique(trips$week[trips$collector == 'QBC']))

gtrips <- trips %>% 
  filter(collector == 'GMRI') %>% 
  #mutate(set_time = paste0(set_time, ':00')) %>% 
  mutate(#date = as.Date(date),
         set_time = as.POSIXct(set_time,
                               format = "%m/%d/%Y %H:%M")) %>% 
  group_by(date) %>% 
  summarise(diftime = difftime(max(set_time), min(set_time), 
                               units='hours'))
sum(gtrips$diftime) + (nrow(gtrips) * as.difftime(0.5, units='hours'))
nrow(trips[trips$collector == 'GMRI',])  
length(unique(trips$week[trips$collector == 'GMRI']))

ttrips <- trips %>% 
  group_by(date, collector) %>%
  #mutate(set_time = paste0(set_time, ':00')) %>% 
  mutate(set_time = as.POSIXct(set_time,
                               format='%m/%d/%Y %H:%M')) %>% 
  summarise(diftime = difftime(max(set_time), min(set_time),
                               units='hours') +
              as.difftime(0.5, units='hours'),
            sites = length(unique(site_name)),
            week = week[1],
            month = month[1]) %>% 
  group_by(week, collector) %>% 
  summarise(diftime = sum(diftime),
            sites = sum(sites),
            month = month[1])

week.eff <- ggplot(data=ttrips) +
  geom_bar(aes(x=week, col=collector, fill=collector, y=diftime),
           stat='identity') +
  labs(x='Week of', y='Time seining (hours)', col='Agency', fill='Agency') +
  scale_x_continuous(breaks=seq(23, 41, 1),
                     labels = c('3 June', '', '', '', 
                                '1 July', '', '', '',
                                '29 July', '', '', '',
                                '26 Aug', '', '', '',
                                '23 Sep', '', '7 Oct')) +
  scale_color_manual(values = c('#00608A', '#08a654')) +
  scale_fill_manual(values = c('#00608A', '#08a654')) +
  ggtitle('Weekly CBASS seining effort')
ggsave(plot=week.eff,
       here('2024effort.png'))
#### Abundance ####
# Remove single-time catches
spec <- as.data.frame(table(abund$species_name))
onetime <- spec[spec$Freq == 1,]
multcatch <- spec[spec$Freq != 1,]
abund <- abund %>% 
  filter(species_name %in% multcatch$Var1)

specab <- as.data.frame(unique(abund$species_name))
colnames(specab) <- 'species_name'
specab$abbr <- c('g.crab', 
                 'mummi.',
                 'gunnel', 
                 'w.flndr',
                 'g.sculpin',
                 'slvrsd',
                 'herring', 
                 'tomcod', 
                 'alewife', 
                 'pipefsh',
                 'sndlnce',
                 'scuplin',
                 '3.stcklbk',
                 '9.stcklbk',
                 'w.mllt',
                 'blfsh',
                 'c.jack')

qta <- abund %>% 
  filter(week != 40) %>% 
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
                              levels=c('atlantic silverside',
                                       'crevalle jack', 
                                       'sandlance', 
                                       'alewife', 
                                       'grubby sculpin',
                                       'green crab', 
                                       'white perch',
                                       'bluefish',
                                       'rock gunnel',
                                       'atlantic tomcod',
                                       'threespine stickleback',
                                       'ninespine stickleback',
                                       'mummichog',
                                       'northern pipefish',
                                       'winter flounder',
                                       'white mullet',
                                       'atlantic herring'))
qabund <- qabund[!is.na(qabund$species_name),]

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
  scale_x_continuous(breaks=seq(23, 41, 1),
                     labels = c('3 June', '', '', '', 
                                '1 July', '', '', '',
                                '29 July', '', '', '',
                                '26 Aug', '', '', '',
                                '23 Sep', '', '7 Oct')) +
  ggtitle('2023 Weekly CBASS community assemblage') +
  theme(legend.position = 'right') +
  guides(fill=guide_legend(ncol=1),
         color=guide_legend(ncol=1))
commass

ggsave(plot=commass,
       '2023communityassemblage.png',
       width = 7.75, height = 5, units='in')

abund <- read.csv(here('Clean_Data/Seine/abund_through_2024.csv'))
abund <- abund %>% 
  mutate(date = as.Date(date, format="%Y-%m-%d")) %>% 
  mutate(month = month(date),
         year = year(date),
         week = isoweek(date))
abund$collector[abund$site_id >20] <- 'QBC'
abund$collector[abund$site_id <20] <- 'GMRI'

sum.caught <- abund %>%
  filter(year == 2024) %>% 
  rename(Species = species_name) %>%
  group_by(Species) %>% 
  summarise(Catch = sum(catch)) %>% 
  as.data.frame()

sum.caught <- sum.caught[with(sum.caught, order(Catch, decreasing = T)),]
rownames(sum.caught) <- NULL

sum.caught %>%
  kable(caption = "Species caught in 2024 CBASS",
        col.names = c('Species', 'Total catch'),
        row.names = FALSE) %>%
  row_spec(0, background="black", color="white") %>% 
  kable_styling(font_size = 9, latex_options = 'HOLD_position')

tomcod$year <- year(tomcod$date)
tomcod$month <- month(tomcod$date)
tomcod$day <- day(tomcod$date)

tomcod <- tomcod %>% 
  filter(year >=2014) %>% 
  filter(month %in% c(6, 7, 8, 9, 10))

for(i in 1:nrow(tomcod)){
  if(tomcod$month[i] == 10 & tomcod$day[i]>15){
    tomcod$day[i] <- NA
  }
}

tomcod <- tomcod[!is.na(tomcod$day),]

tc.cpue <- ggplot(data=tomcod) +
  geom_smooth(aes(x=date, y=cpue), se=F, alpha=0.4) +
  geom_point(aes(x=date, y=cpue), cex=0.75) +
  facet_wrap(vars(year), scales='free_x') +
  coord_cartesian(ylim=c(0, 11)) +
  labs(x='Date', y='Catch per Unit Effort') +
  ggtitle('Atlantic tomcod CPUE') 
ggsave(plot=tc.cpue,
       'tomcodcpue.png')
