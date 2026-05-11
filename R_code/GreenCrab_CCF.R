rm(list=ls())

# Packages
library(here)
library(tidyverse)
library(sf)

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
dat <- read.csv(here('Data/Clean_Data/Meteorological/CascoBay_DailyTemps_MURSST_Composite.csv'))

# Clean
dat <- dat %>% 
  mutate(date = as.Date(date, format = '%m/%d/%Y')) %>% 
  filter(date >= as.Date('2003-01-01') &
           date <= as.Date('2025-12-31')) %>% 
  mutate(holder = as.Date(doy-1, 
                          origin = "2024-01-01")) %>% 
  mutate(mean.sst = weathermetrics::celsius.to.fahrenheit(mean.sst),
         min.sst = weathermetrics::celsius.to.fahrenheit(min.sst),
         max.sst = weathermetrics::celsius.to.fahrenheit(max.sst))

dat$season <- NA     
dat$season[dat$month %in% c(1, 2, 3)] <- 'Winter'
dat$season[dat$month %in% c(4, 5, 6)] <- 'Spring'
dat$season[dat$month %in% c(7, 8, 9)] <- 'Summer'
dat$season[dat$month %in% c(10, 11, 12)] <- 'Fall'

tgam <- mgcv::gam(mean.sst ~
                    s(doy, bs='cs') +
                    s(year, bs='cs', k=18),
                  data=dat[dat$year<=2020,],
                  select=T, method = 'REML')
summary(tgam)

# Predict wthout effect of year
daily.smooth <- data.frame(
  doy= seq(1, 366, 1),
  year = 2020
)

np <- mgcv::predict.gam(tgam, daily.smooth,
                        exclude = "s(year)",
                        se.fit=T)

daily.smooth$smooth.daily <- np$fit
daily.smooth$smooth.upper <- np$fit + np$se.fit
daily.smooth$smooth.lower <- np$fit - np$se.fit

dat <- left_join(dat,
                 dplyr::select(daily.smooth, -year),
                 by=c('doy'))

dat$anom <- dat$mean.sst - dat$smooth.daily

dat$GDD <- 'NO'
dat$GDD[dat$mean.sst>=50] <- 'YES'

labs <- data.frame(
  x=c(as.Date(c('2024-02-15', '2024-05-15', '2024-08-15', '2024-11-15'))),
  y=c(rep(2026.5, 4)),
  label=c('Winter', 'Spring', 'Summer', 'Fall')
)

dat.anom <- ggplot(data=dat) +
  geom_tile(aes(x=holder, y=year, group = year,
                fill=anom)) +
  geom_point(data=dat[dat$anom<0,],
             aes(x=holder, y=year, group=year)) +
  # scale_y_continuous(limits=c(2002, 2027.5), expand = c(0,0.0),
  #                    breaks=c(seq(2005, 2025, 5)))+
  # scale_y_continuous(breaks = seq(2003, 2025, 1),
  #                    labels = c('', "", '2005', rep("", 4), '2010',
  #                               rep("", 4), '2015', rep("", 4), '2020',
  #                               rep("", 4), '2025')) +
  scale_x_date(breaks = as.Date(c('2024-01-01','2024-02-01', '2024-03-01',
                                  '2024-04-01','2024-05-01', '2024-06-01',
                                  '2024-07-01','2024-08-01', '2024-09-01',
                                  '2024-10-01','2024-11-01', '2024-12-01')),
               date_labels = '%b', expand=c(0.0,0.0)) +
  scale_fill_gradient2(low = ('#2767A9'),
                       high = ('#AC1C2F'), 
                       midpoint = 0, limits=c(-8, 8),
                       breaks=c(-8, -4, 0, 4, 8)) +
  geom_vline(xintercept = as.Date(c(#'2024-01-01',#'2024-02-01', '2024-03-01',
                                    '2024-04-01',#'2024-05-01', '2024-06-01',
                                    '2024-07-01',#'2024-08-01', '2024-09-01',
                                    '2024-10-01')),#,'2024-11-01', '2024-12-01')),
             lty=2, col='gray50')+
  geom_label(data=labs, aes(x=x, y=y, label = label)) +
  labs(y='Year', x='Month', 
       fill='SST anomaly (\u00B0F)') +
  theme(legend.margin = margin(0,0,0,0),
        panel.grid.major.x= element_blank())
dat.anom
#ggsave(dat.anom,
#      filename = here('Documentation/EIR_2025/MURSST_Daily_Anomalies.png'))


week <- dat %>% 
  mutate(week = week(date)) %>% 
  group_by(year, week) %>% 
  mutate(holder = min(holder),
         anom = mean(anom)) %>% 
  dplyr::select(year, week, holder, anom) %>% 
  unique() %>% as.data.frame()

week.anom <- ggplot(data=week[week$year>=2013,]) +
  geom_tile(aes(x=holder, y=year, group = year,
                fill=anom)) +
  scale_y_continuous(breaks = seq(2013, 2025, 1)) +
  scale_x_date(breaks = as.Date(c('2024-01-01',
                                  '2024-04-01',
                                  '2024-07-01',
                                  '2024-10-01',
                                  '2024-12-31')),
               date_labels = '%b %d') +
  scale_fill_gradient2(low = scales::muted('blue'),
                       high = scales::muted('red'), 
                       midpoint = 0) +
  geom_vline(xintercept = as.Date(c('2024-01-01',
                                    '2024-04-01',
                                    '2024-07-01',
                                    '2024-10-01', 
                                    '2024-12-31')),
             lty=2, col='gray50')+
  geom_label(data=labs, aes(x=x, y=y, label = label)) +
  labs(y='Year', x='Month', 
       fill='SST anomaly (\u00B0C)') +
  theme(legend.margin = margin(0,0,0,0),
        panel.grid.major.x= element_blank())
week.anom
# ggsave(week.anom,
#        filename = here('Documentation/EIR_2025/MURSST_Anomalies.png'))


ggplot(data=dat[dat$year>=2013,]) +
  geom_tile(aes(x=holder, y=year, group = year,
                fill=mean.sst)) +
  geom_point(data=dat[dat$year>=2013 &
                        dat$mean.sst>50,],
             aes(x=holder, y=year, group=year),
             cex=0.4, col='black') +
  scale_y_continuous(breaks = seq(2014, 2025, 1)) +
  scale_x_date(breaks = as.Date(c('2024-01-01',
                                  '2024-04-01',
                                  '2024-07-01',
                                  '2024-10-01')),
               date_labels = '%b') +
  scale_fill_gradient2(low = scales::muted('blue'),
                       high = scales::muted('red'), 
                       midpoint = 50,
                       limits=c(34, 71)) +
  geom_label(data=labs, aes(x=x, y=y, label = label)) +
  labs(y='Year', x='Day of year', 
       fill='Casco Bay SST\ndeg F') +
  ggtitle('Ideal larval development and adult feeding conditions (>50F)') +
  theme(legend.margin = margin(0,0,0,0))

# GC CPUE for year x  ~ Spring oopsies year x (kill adults) + 
#                       fall warm year x-1(grow babies)
month <- dat %>% 
  group_by(year, month) %>% 
  summarise(mean.sst = mean(mean.sst),
            min.sst = min(min.sst),
            max.sst = max(max.sst))

ggplot(data=month[month$year>=2013,]) +
  geom_line(aes(x=year, y=mean.sst)) +
  geom_point(aes(x=year, y=mean.sst)) +  
  geom_hline(yintercept = 50, lty=2, col='red') +
  facet_wrap(vars(month), scales='free_y') +
  scale_x_continuous(breaks=seq(2013, 2025, 1)) +
  theme(axis.text.x = element_text(angle = 20))

#### Environmental impacts ####
# In theory, GC CPUE for any summer x should be affected by factors acting on:
#   - adult mortality in winter-spring x
#   - adult mortality in summer-fall x-1
#   - larval mortality in fall x-1
#   - juvenile mortality fall x-1
#   - juvenile mortality winter-spring x

# Load crab cpue
crab <- read.csv(here('Data/Clean_Data/Seine/GreenCrab_Size_CPUE2.csv'))
# This is weeks 24-39 (1st week June - last week Sept)

#### Convert crab cpue to TS ####
library(tseries) 
library(forecast)
# Convert cpue to time series
dat.ts <- ts(crab$value[crab$CPUE == 'all crabs'])

# Kwiatkowski-Phillips-Schmidt-Shin test to check for stationarity
kpss.test(dat.ts)

# Use automated process to fit ARIMA model to best deal with non-stationarity
dat.tsaa <- auto.arima(dat.ts, seasonal=F, test="kpss")

# Check residuals
checkresiduals(dat.tsaa)
Box.test(dat.tsaa$residuals, type="Lj")

# Monthly
month <- dat %>% 
  group_by(year, month) %>% 
  summarise(mean.sst = mean(mean.sst),
            min.sst = min(min.sst),
            max.sst = max(max.sst))
  

#### CCF to find cors ####
for(i in 1:12){
  # Convert var to time series
  atp.ts <- ts(month$mean.sst[month$month == i & month$year %in% seq(2014, 2025, 1)])
  
  # Use automated process to fit ARIMA model to best deal with non-stationarity
  atp.tsaa <- auto.arima(atp.ts, seasonal=F, test="kpss")
  
  # Check residuals
  checkresiduals(atp.tsaa, plot=F)
  Box.test(atp.tsaa$residuals, type="Lj")
  
  # Test correlation
  cf1 <- ccf(dat.tsaa$residuals, # The x time series
             atp.tsaa$residuals, # The y time series
             type='correlation',
             main = paste0('GC CPUE vs. mean SST, ', month(i, abbr=TRUE,
                                                           label=TRUE)),
             na.action = na.exclude)
}
#### All crabs
# Mean SST Nov of last year pos corr with crabs this year
# Mean SST Jun of two years ago pos corr with crabs this year

#### Immature crabs
# Dec of four years ago neg corr with crabs this year
# Nov of last year pos corr
# June of two years ago pos corr

#### Mature crabs
# Nov of last year pos corr with crabs this year
# Aug of two years ago pos corr

dec <- month %>% 
  filter(month == 12) %>% 
  mutate(year = year + 4) %>% 
  dplyr::select(year, month, mean.sst) %>% 
  filter(year <=2025)

jun <- month %>% 
  filter(month == 6) %>% 
  mutate(year = year + 2) %>% 
  dplyr::select(year, month, mean.sst) %>% 
  filter(year <=2025)

nov <- month %>% 
  filter(month == 11) %>% 
  mutate(year = year + 1) %>% 
  dplyr::select(year, month, mean.sst) %>% 
  filter(year <=2025)

sst <- rbind(jun, nov)

sst <- sst %>% 
  pivot_wider(names_from = 'month', values_from = 'mean.sst')
colnames(sst) <- c('year', 'jun', 'nov')

ac <- crab %>% 
  filter(CPUE == 'all crabs') %>% 
  dplyr::select(year, value) %>% 
  rename(cpue = value)

df <- merge(ac, sst, by=c('year'))

library(mgcv)
tgam <- gam(cpue ~ 
              s(year, bs='ts')+
              s(jun, bs='ts')+
              s(nov, bs='ts'),# +
              #s(dec, bs='ts'),
            data=df, method = 'REML', select=T, family=gaussian(link='identity'))
summary(tgam)
AIC(tgam)
gam.check(tgam)
gam.hp::gam.hp(tgam)
plot(tgam, scheme = 1, pages = 1, all.terms = T)
library(mgcViz)
b <- getViz(tgam)

ggplot(data=df) +
  geom_label(aes(x=nov, y=cpue, label = year), alpha=0.5)+
  labs(x='Mean SST (deg F) in previous November', y='CPUE') +
  geom_smooth(aes(x=nov, y=cpue), method = 'gam', fullrange=T)

ggplot(data=df) +
  geom_label(aes(x=jun, y=cpue, label = year), alpha=0.5)+
  labs(x='Mean SST (deg F) in previous November', y='CPUE') +
  geom_smooth(aes(x=jun, y=cpue), method = 'gam', fullrange=T)

# Pretty plots - Year
p_obj <- plot(tgam, residuals = TRUE, pages = 1)
smmtm <- p_obj[[1]] # just one smooth so select the first component
sm_df <- as.data.frame(smmtm[c("x", "se", "fit")])
sm_df$var <- 'Partial Effect of Year'

data_df <- as.data.frame(smmtm[c("raw", "p.resid")])
colnames(data_df) <- c('raw', 'p.resid')
data_df$year <- seq(2014, 2025, 1)
data_df$ycol <- 'gray50'
data_df$ycol[data_df$year == 2025] <- 'black'
data_df$var <- 'Partial Effect of Year'

ddf <- data_df
sdf <- sm_df

## plot
yrp <- ggplot(sm_df, aes(x = x, y = fit)) +
  geom_ribbon(aes(ymin = fit - se, ymax = fit + se, y = NULL),
              alpha = 0.3) +
  geom_point(data=data_df,
             aes(x=raw, y=p.resid)) +
  #ggrepel::geom_label_repel(data = data_df, 
  #                          mapping = aes(x = raw, y = p.resid,
  #                                        label = year, col=ycol),
  #                          alpha=0.8,
  #                          min.segment.length = unit(0, 'lines')) +
  #scale_color_manual(values = c('black', 'gray35'),
  #                   guide='none') +
  geom_line() +
  labs(y='Effect on catch per unit effort') +
  facet_wrap(~var) +
  ggtitle('Environmental influence on Green crab CPUE') +
  theme(legend.position = 'bottom',
        axis.title.x = element_blank(),
        legend.margin = margin(0,0,0,0))
yrp


# Pretty plots - November
#p_obj <- plot(tgam, residuals = TRUE, pages = 1)
smmtm <- p_obj[[3]] # just one smooth so select the first component
sm_df <- as.data.frame(smmtm[c("x", "se", "fit")])
sm_df$var <- 'Mean SST (deg F) in previous November'

data_df <- as.data.frame(smmtm[c("raw", "p.resid")])
colnames(data_df) <- c('raw', 'p.resid')
data_df$year <- seq(2014, 2025, 1)
data_df$ycol <- 'gray50'
data_df$ycol[data_df$year == 2025] <- 'black'
data_df$var <- 'Mean SST (deg F) in previous November'

ddf <- data_df
sdf <- sm_df

## plot
nov <- ggplot(sm_df, aes(x = x, y = fit)) +
  geom_ribbon(aes(ymin = fit - se, ymax = fit + se, y = NULL),
              alpha = 0.3) +
  geom_point(data=data_df,
             aes(x=raw, y=p.resid, col=ycol)) +
  ggrepel::geom_label_repel(data = data_df, 
                            mapping = aes(x = raw, y = p.resid,
                                          label = year, col=ycol),
                            alpha=0.8,
                            min.segment.length = unit(0, 'lines')) +
  scale_color_manual(values = c('black', 'gray35'),
                     guide='none') +
  geom_line() +
  labs(y='Effect on catch per unit effort', 
       fill='Collecting\nagency') +
  facet_wrap(~var) +
  theme(legend.position = 'bottom',
        axis.title.x = element_blank(),
        legend.margin = margin(0,0,0,0))
nov

# Pretty plots - June
#p_obj <- plot(tgam, residuals = TRUE, pages = 1)
smmtm <- p_obj[[2]] # just one smooth so select the first component
sm_df <- as.data.frame(smmtm[c("x", "se", "fit")])
sm_df$var <- 'Mean SST (deg F) in June two years prior'

data_df <- as.data.frame(smmtm[c("raw", "p.resid")])
colnames(data_df) <- c('raw', 'p.resid')
data_df$year <- seq(2014, 2025, 1)
data_df$ycol <- 'gray50'
data_df$ycol[data_df$year == 2025] <- 'black'
data_df$var <- 'Mean SST (deg F) in June two years prior'

ddf <- data_df
sdf <- sm_df

## plot
june <- ggplot(sm_df, aes(x = x, y = fit)) +
  geom_ribbon(aes(ymin = fit - se, ymax = fit + se, y = NULL),
              alpha = 0.3) +
  geom_point(data=data_df,
             aes(x=raw, y=p.resid, col=ycol)) +
  ggrepel::geom_label_repel(data = data_df, 
                            mapping = aes(x = raw, y = p.resid,
                                          label = year, col=ycol),
                            alpha=0.8,
                            min.segment.length = unit(0, 'lines')) +
  scale_color_manual(values = c('black', 'gray35'),
                     guide='none') +
  geom_line() +
  labs(y='Effect on catch per unit effort', 
       fill='Collecting\nagency') +
  facet_wrap(~var) +
  theme(legend.position = 'bottom',
        axis.title.x = element_blank(),
        legend.margin = margin(0,0,0,0))
june

test <- ggpubr::ggarrange(yrp, june, nov, ncol=1,
                          heights = c(1, 1, 1))
test
ggsave(plot=test, 
       here('Documentation/EIR_2025/gc_cpue_mods.png'), 
       height = 11, width = 8.5, units = 'in')
