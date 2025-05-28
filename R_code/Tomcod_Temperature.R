rm(list=ls())

library(tidyverse)
library(here)

# Negate function
'%notin%' <- function(x,y)!('%in%'(x,y))

# Convert Celsius to Fahrenheit
c_to_f <- function(celsius) {
  return((celsius * 9/5) + 32)
}

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

spawnyeardf <- data.frame(
  spawnmonth = seq(1, 12, 1),
  month = seq(1, 12, 1)
)

abund <- read.csv(here('Clean_Data/Seine/abund_through_2024.csv'))
trips <- read.csv(here('Clean_Data/Seine/trips_through_2024.csv'))

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
         week = isoweek(date)) %>% 
  mutate(set_time = as.POSIXct(set_time, format='%m/%d/%Y %H:%M'))

trips$collector <- NA
trips$collector[trips$site_id >20] <- 'QBC'
trips$collector[trips$site_id <20] <- 'GMRI'
trips$set_time = trips$set_time - lubridate::hours(4)

#### Tomcod ####
tomcod <- abund[abund$species_name == 'atlantic tomcod',]
tomcod <- tomcod %>% 
  dplyr::select(date, year, site_name, catch)

tomcod <- merge(tomcod, trips, by=c('date', 'year','site_name'), all=T)
tomcod$catch[is.na(tomcod$catch)] <- 0

tomcod <- tomcod %>% 
  dplyr::select(date, year, site_name, catch,
                weather, temp_degc, do_mg.l, salinity_ppt,
                month, week, collector)  %>% 
  filter(month %in% c(6, 7) & year != 2019) %>% 
  group_by(year, collector) %>% 
  summarise(cpue = sum(catch) / n(),
            temp_degc = mean(temp_degc, na.rm=T))

rm(abund, trips)

#### Harbor temp ####
jpm <- read.csv(here('Clean_Data/Meteorological/Portland_watertemp_POR.csv'))

jpm <- jpm %>% 
  mutate(date = as.Date(timestamp, format='%Y-%m-%d %H:%M:%S')) %>% 
  filter(date >= as.Date('2013-11-01')) %>% 
  filter(date <= as.Date('2024-12-31')) %>% 
  mutate(week = week(date),
         year = year(date),
         month = month(date)) %>% 
  mutate(spawnyear = year) %>% 
  mutate(daily.c = c_to_f(daily.c))

jpm <- left_join(jpm, spawnyeardf, by=c('month'))

jpm <- jpm %>% 
  dplyr::select(date, week, spawnmonth, spawnyear, 
                daily.c, year) %>% 
  unique()

jpm <- jpm %>% 
  group_by(spawnyear) %>% 
  summarise(temp1 = mean(daily.c[spawnmonth ==1], na.rm=T),
            temp2 = mean(daily.c[spawnmonth ==2], na.rm=T),
            temp3 = mean(daily.c[spawnmonth ==3], na.rm=T),
            temp4 = mean(daily.c[spawnmonth ==4], na.rm=T),
            temp5 = mean(daily.c[spawnmonth ==5], na.rm=T),
            temp6 = mean(daily.c[spawnmonth ==6], na.rm=T),
            temp7 = mean(daily.c[spawnmonth ==7], na.rm=T),
            temp8 = mean(daily.c[spawnmonth ==8], na.rm=T),
            temp9 = mean(daily.c[spawnmonth ==9], na.rm=T),
            temp10 = mean(daily.c[spawnmonth ==10], na.rm=T),
            temp11= mean(daily.c[spawnmonth ==11], na.rm=T),
            temp12 = mean(daily.c[spawnmonth ==12], na.rm=T),
            summtm = mean(daily.c[spawnmonth %in% c(7:9)],
                          na.rm=T))

jpm <- jpm[jpm$spawnyear %in% seq(2014, 2024),]

#### Precip ####
rain <- read.csv(here('Raw_Data/Portland_Precip_2025.csv'))
rain <- rain %>% 
  dplyr::select(-STATION, -NAME, -SNWD) %>% 
  rename(date = DATE,
         precip = PRCP,
         snow = SNOW) %>% 
  mutate(date = as.Date(date)) %>% 
  mutate(year = year(date),
         month = month(date)) %>% 
  dplyr::select(-date) %>% 
  mutate(spawnyear = year)

rain <- left_join(rain, spawnyeardf, by=c('month'))

rain <- rain %>% 
  dplyr::select(spawnmonth, spawnyear, 
                precip, snow) %>% 
  unique()

snows <- rain %>% 
  group_by(spawnyear) %>% 
  summarise(snow1 = sum(snow[spawnmonth ==1], na.rm=T),
            snow2 = sum(snow[spawnmonth ==2], na.rm=T),
            snow3 = sum(snow[spawnmonth ==3], na.rm=T),
            snow4 = sum(snow[spawnmonth ==4], na.rm=T),
            snow5 = sum(snow[spawnmonth ==5], na.rm=T),
            snow6 = sum(snow[spawnmonth ==6], na.rm=T),
            snow7 = sum(snow[spawnmonth ==7], na.rm=T),
            snow8 = sum(snow[spawnmonth ==8], na.rm=T),
            snow9 = sum(snow[spawnmonth ==9], na.rm=T),
            snow10 = sum(snow[spawnmonth ==10], na.rm=T),
            snow11 = sum(snow[spawnmonth ==11], na.rm=T),
            snow12 = sum(snow[spawnmonth ==12], na.rm=T))

rains <- rain %>% 
  group_by(spawnyear) %>% 
  summarise(precip1 = sum(precip[spawnmonth ==1], na.rm=T),
            precip2 = sum(precip[spawnmonth ==2], na.rm=T),
            precip3 = sum(precip[spawnmonth ==3], na.rm=T),
            precip4 = sum(precip[spawnmonth ==4], na.rm=T),
            precip5 = sum(precip[spawnmonth ==5], na.rm=T),
            precip6 = sum(precip[spawnmonth ==6], na.rm=T),
            precip7 = sum(precip[spawnmonth ==7], na.rm=T),
            precip8 = sum(precip[spawnmonth ==8], na.rm=T),
            precip9 = sum(precip[spawnmonth ==9], na.rm=T),
            precip10 = sum(precip[spawnmonth ==10], na.rm=T),
            precip11 = sum(precip[spawnmonth ==11], na.rm=T),
            precip12 = sum(precip[spawnmonth ==12], na.rm=T),
            wprecip = sum(precip[spawnmonth %in% seq(1, 3, 1)],
                          na.rm=T))

#### Monthly joins of phys data ####
tomcod$spawnyear <- tomcod$year
tomcod <- left_join(tomcod,
                    jpm, 
                    by=c('spawnyear'))

tomcod <- left_join(tomcod,
                    rains,
                    by=c('spawnyear'))

tomcod <- left_join(tomcod,
                    snows,
                    by=c('spawnyear'))

tomcod <- as.data.frame(tomcod)

tomcod <- tomcod %>% 
  dplyr::select(-snow6, -snow5, -snow7, -snow8, -snow9, -snow10)

rm(jpm, rain, rains, snows, spawnyeardf)

#### Data explore ####
# Test correlation
# Create correlation matrix
df_cormat <- dplyr::select(tomcod, -year, -spawnyear, -collector,
                           -summtm, -wprecip)
df_cormat <- model.matrix(~0+., data=df_cormat) %>% 
  cor(use="all.obs", method="spearman")
df_cormat <- df_cormat[,1]
df_cormat <- as.data.frame(df_cormat)
df_cormat$var <- rownames(df_cormat)
rownames(df_cormat) <- NULL
colnames(df_cormat) <- c('sp.cor', 'variable')
df_cormat <- df_cormat %>% 
  filter(variable != 'cpue') %>% 
  filter(sp.cor > 0.6 | sp.cor < -0.6)
df_cormat <- df_cormat[with(df_cormat, order(abs(sp.cor))),]
df_cormat

df_cormat <- dplyr::select(tomcod, cpue, 
                           summtm, wprecip, snow4)
model.matrix(~0+., data=df_cormat) %>% 
  cor(use="all.obs", method="spearman") %>% 
  ggcorrplot::ggcorrplot(show.diag = F, type="lower", lab=TRUE, 
                         lab_size=3)

# Summer temps: April to September
# Winter precipitation: January to March
# Snow: April

modtom <- dplyr::select(tomcod, cpue, spawnyear,
                        summtm, wprecip, snow4,
                        collector)
library(mgcv)
g1 <- gam(cpue ~ s(summtm, bs='cs', k=5)+ s(wprecip, bs='cs', k=5)+ 
            s(snow4, k=5, bs='cs'),
          data=tomcod, family=gaussian(link='identity'),
          method='REML')
summary(g1)

p_obj <- plot(g1, residuals = TRUE, pages = 1)
smmtm <- p_obj[[1]] # just one smooth so select the first component
sm_df <- as.data.frame(smmtm[c("x", "se", "fit")])
sm_df$var <- 'Average temperature, July to September (F)'

data_df <- as.data.frame(smmtm[c("raw", "p.resid")])
colnames(data_df) <- c('raw', 'p.resid')
data_df$year <- modtom$spawnyear
data_df$collector <- modtom$collector
data_df$ycol <- 'gray50'
data_df$ycol[data_df$year == 2024] <- 'black'
data_df$var <- 'Average temperature, July to September (F)'

ddf <- data_df
sdf <- sm_df

## plot
ggplot(sm_df, aes(x = x, y = fit)) +
  geom_ribbon(aes(ymin = fit - se, ymax = fit + se, y = NULL),
              alpha = 0.3) +
  geom_point(data=data_df,
             aes(x=raw, y=p.resid, col=ycol)) +
  ggrepel::geom_label_repel(data = data_df, 
             mapping = aes(x = raw, y = p.resid, fill=collector,
                           label = year, col=ycol),
             alpha=0.8,
             min.segment.length = unit(0, 'lines')) +
  scale_color_manual(values = c('black', 'gray35'),
                     guide='none') +
  geom_line() +
  ylim(-2, 2) +
  labs(x='Average Casco Bay summmer temperature (F)', 
       y='Effect on catch per unit effort', 
       fill='Collecting\nagency') +
  ggtitle('Environmental influence on Atlantic tomcod catch') +
  theme(legend.position = 'bottom',
        legend.margin = margin(0,0,0,0))


wpc <- p_obj[[2]] # just one smooth so select the first component
sm_df <- as.data.frame(wpc[c("x", "se", "fit")])
sm_df$var <- 'Total precipitation, January to March (in)'

data_df <- as.data.frame(wpc[c("raw", "p.resid")])
colnames(data_df) <- c('raw', 'p.resid')
data_df$year <- modtom$spawnyear
data_df$collector <- modtom$collector
data_df$ycol <- 'gray50'
data_df$ycol[data_df$year == 2024] <- 'black'
data_df$var <- 'Total precipitation, January to March (in)'

ddf <- rbind(ddf, data_df)
sdf <- rbind(sdf, sm_df)

## plot
ggplot(sm_df, aes(x = x, y = fit)) +
  geom_ribbon(aes(ymin = fit - se, ymax = fit + se, y = NULL),
              alpha = 0.3) +
  geom_point(data=data_df,
             aes(x=raw, y=p.resid, col=ycol)) +
  ggrepel::geom_label_repel(data = data_df, 
                            mapping = aes(x = raw, y = p.resid, fill=collector,
                                          label = year, col=ycol),
                            alpha=0.8,
                            min.segment.length = unit(0, 'lines')) +
  scale_color_manual(values = c('black', 'gray35'),
                     guide='none') +
  geom_line() +
  labs(x='Total rainfall, January to March', 
       y='Effect on catch per unit effort', 
       fill='Collecting\nagency') +
  ylim(-2, 2) +
  theme(legend.position = 'bottom',
        legend.margin = margin(0,0,0,0))


sn <- p_obj[[3]] # just one smooth so select the first component
sm_df <- as.data.frame(sn[c("x", "se", "fit")])
sm_df$var <- 'Total snowfall, April (in)'

data_df <- as.data.frame(sn[c("raw", "p.resid")])
colnames(data_df) <- c('raw', 'p.resid')
data_df$year <- modtom$spawnyear
data_df$collector <- modtom$collector
data_df$ycol <- 'gray50'
data_df$ycol[data_df$year == 2024] <- 'black'
data_df$var <- 'Total snowfall, April (in)'

ddf <- rbind(ddf, data_df)
sdf <- rbind(sdf, sm_df)

## plot
ggplot(sm_df, aes(x = x, y = fit)) +
  geom_ribbon(aes(ymin = fit - se, ymax = fit + se, y = NULL),
              alpha = 0.3) +
  geom_point(data=data_df,
             aes(x=raw, y=p.resid, col=ycol)) +
  ggrepel::geom_label_repel(data = data_df, 
                            mapping = aes(x = raw, y = p.resid, fill=collector,
                                          label = year, col=ycol),
                            alpha=0.8,
                            min.segment.length = unit(0, 'lines')) +
  scale_color_manual(values = c('black', 'gray35'),
                     guide='none') +
  geom_line() +
  ylim(-2, 2) +
  labs(x='Total snowfall, April', 
       y='Effect on catch per unit effort', 
       fill='Collecting\nagency') +
  theme(legend.position = 'bottom',
        legend.margin = margin(0,0,0,0))


## plot
gamp <- ggplot() +
  geom_line(data=sdf, aes(x = x, y = fit)) +
  geom_ribbon(data=sdf,
              aes(ymin = fit - se, ymax = fit + se, x =x,
                  y=NULL),
              alpha = 0.3) +
  geom_point(data=ddf,
             aes(x=raw, y=p.resid, col=ycol)) +
  ggrepel::geom_label_repel(data = ddf, 
                            mapping = aes(x = raw, y = p.resid, fill=collector,
                                          label = year, col=ycol),
                            alpha=0.8,
                            min.segment.length = unit(0, 'lines')) +
  scale_color_manual(values = c('black', 'gray35'),
                     guide='none') +
  ylim(-2, 2) +
  facet_wrap(vars(var), scales='free_x', ncol=1) +
  labs(
       y='Effect on catch per unit effort', 
       fill='Collecting\nagency') +
  theme(legend.position = 'bottom',
        axis.title.x = element_blank(),
        legend.margin = margin(0,0,0,0)) +
  ggtitle('Environmental effects on Atlantic tomcod catch')

ggsave(plot = gamp,
       'tomcod_gams.png', 
       height = 7.225, width = 8.5, units='in')

modtom$ycol <- 'gray35'
modtom$ycol[modtom$year == 2024] <- 'black'
modtom$var <- 'Catch per unit effort, June to July'

cpue <- ggplot() +
  geom_point(data=modtom,
             aes(x=spawnyear, y=cpue, fill=collector),
             cex=3, alpha=0.7, stroke=0.5,
             pch=21) +
  geom_smooth(data=modtom,
              aes(x=spawnyear, y=cpue),
              method='gam', col='black', lwd = 0.5,
              method.args=list(family=gaussian(link='identity'),
                               method='REML'),
              se=F) +
  facet_wrap(vars(var)) +
  labs(x='Year', 
       y='Catch per unit effort') +
  ggtitle('Atlantic tomcod catch') +
  theme(legend.position = 'n',
        legend.margin = margin(0,0,0,0))

test <- ggpubr::ggarrange(cpue, gamp, ncol=1,
                          heights = c(0.5, 2))
ggsave(plot=test, 'test2.png', height = 11, width = 8.5, units = 'in')
