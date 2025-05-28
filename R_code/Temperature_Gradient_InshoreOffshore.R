rm(list=ls())

library(here)
library(tidyverse)

# Negate function
'%notin%' <- function(x,y)!('%in%'(x,y))

wcl <- read.csv(here('Clean_Data/Meteorological/WCL_watertemp.csv'))
pb <- read.csv(here('Clean_Data/Meteorological/Portland_watertemp.csv'))

# Clean
wcl <- wcl %>% 
  dplyr::select(dailyts, daily.c) %>% 
  unique() %>% as.data.frame()

pb <- pb %>% 
  dplyr::select(dailyts, daily.c) %>% 
  mutate(dailyts = as.Date(dailyts, format='%Y-%m-%d')) %>% 
  unique() %>% as.data.frame() %>% 
  filter(year(dailyts) >= 2014)

# Add missing dates
md.w <- seq(as.Date('2014-01-01'),
            as.Date('2024-12-31'),
            'day')
md.w <- md.w[md.w %notin% wcl$dailyts]
md.w <- as.data.frame(md.w)
md.w <- md.w %>% 
  rename(dailyts = md.w) %>% 
  mutate(daily.c = NA,) %>% 
  mutate(dailyts = as.character(dailyts))
wcl$dailyts <- as.character(wcl$dailyts)
wcl <- rbind(wcl, md.w)
wcl$dailyts <- as.Date(wcl$dailyts, format = '%Y-%m-%d')
wcl <- wcl[with(wcl, order(dailyts)),]
wcl <- wcl[!is.na(wcl$dailyts),]
rownames(wcl) <- NULL

# Merge
wcl <- wcl %>% 
  rename(wcl.temp = daily.c)
pb <- pb %>% 
  rename(pb.temp = daily.c)
dat <- merge(pb, wcl, by=c('dailyts'))

ggplot(data=dat) +
  geom_point(aes(x=pb.temp, y=wcl.temp)) +
  geom_abline(slope = 1, intercept = 0, col='red', lty=2)


dat$doy <- yday(dat$dailyts)
dat$grad <- dat$wcl.temp - dat$pb.temp

ggplot(data=dat) +
  geom_point(aes(x=doy, y=grad)) +
  labs(x='Day of year', y='WCL - Portland temperature gradient') +
  geom_hline(yintercept = 0, col='red', lty=2)

dat$cooler <- NA
dat$cooler[dat$pb.temp > dat$wcl.temp] <- 'WCL'
dat$cooler[dat$pb.temp < dat$wcl.temp] <- 'Portland'

ggplot(data=dat) +
  geom_point(aes(x=doy, y=grad, col=as.factor(cooler))) +
  labs(x='Day of year', y='WCL - Portland temperature gradient') +
  geom_hline(yintercept = 0, col='red', lty=2)

dat$year <- year(dat$dailyts)

labdat <- data.frame(
  x=c(182, 182),
  #x=c(136, 136),
  y=c(-2.5, 5),
  #y=c(-0.5, 0.5),
  label = c('Cooler offshore', 
            'Cooler inshore')
)
  
p <- ggplot(data=dat[dat$year != 2015,]) +
  geom_line(aes(x=doy, y=grad, #col=year, 
                group = year), 
            alpha=0.5) +
  geom_smooth(data=dat[leap_year(year(dat$dailyts)),],
    aes(x=doy, y=grad, #col=year,
                  group = year),
              alpha=0.7, method='gam',
              se=F, lwd=0.7,
              n=366) +
  geom_smooth(data=dat[!leap_year(year(dat$dailyts)) &
                         dat$year != 2015,],
              aes(x=doy, y=grad, #col=year,
                  group = year),
              alpha=0.7, method='gam',
              se=F, lwd=0.7,
              n=365) +
  #scale_color_viridis_c() +
  scale_x_continuous(breaks=c(1, 60,
                              121,
                              182, 244,
                              305,
                              366),
                     labels=c('Jan', 'Mar',
                               'May', 
                              'Jul',  'Sep',
                              'Nov',
                              'Jan')) +
  geom_hline(yintercept = 0, lty=2, col='red',
             alpha=0.5) +
  geom_text(data=labdat, 
            aes(x=x, y=y, label=label),
            alpha=0.5) +
  coord_cartesian(ylim=c(-3, 6)) +
  facet_wrap(vars(year)) +
  labs(x='Month', y='Temperature Gradient (C)\nWCL temp - PH temp') +
  theme(legend.key.width = unit(1, 'in'))
p
# Grab gams
gamdat.1 <- ggplot_build(p)$data[[2]]
gamdat.2 <- ggplot_build(p)$data[[3]]
gamdat <- rbind(gamdat.1, gamdat.2)

years <- seq(2014, 2024, 1)
years <- years[-2]

gamdat <- gamdat[with(gamdat, order(PANEL, x)),]
rownames(gamdat) <- NULL

gamdat <- gamdat %>% 
  dplyr::select(x, y, PANEL) %>% 
  mutate(year = years[PANEL]) %>% 
  dplyr::select(-PANEL) %>% 
  rename(doy = x, 
         smooth.grad = y)

table(gamdat$year[gamdat$smooth.grad < 0])

wcl <- ggplot(data=dat[dat$year != 2015,]) +
  geom_point(aes(x=doy, y=wcl.temp, #col=year, 
                group = year), stroke = NA, 
            alpha=0.1) +
  geom_smooth(data=dat[leap_year(year(dat$dailyts)),],
              aes(x=doy, y=wcl.temp, col=year,
                  group = year),
              alpha=0.7, method='gam',
              se=F, lwd=0.7,
              n=366) +
  geom_smooth(data=dat[!leap_year(year(dat$dailyts)) &
                         dat$year != 2015,],
              aes(x=doy, y=wcl.temp, col=year,
                  group = year),
              alpha=0.7, method='gam',
              se=F, lwd=0.7,
              n=365) +
  scale_color_viridis_c( )+
  coord_cartesian(ylim=c(-2, 22)) +
  ggtitle('West Cod Ledges') +
  scale_x_continuous(breaks=c(1, 60,
                              121,
                              182, 244,
                              305,
                              366),
                     labels=c('Jan', 'Mar',
                              'May', 
                              'Jul',  'Sep',
                              'Nov',
                              'Jan')) +
  labs(x='Month', y='Temperature (C)') +
  theme(legend.key.width = unit(1, 'in'))

pb <- ggplot(data=dat[dat$year != 2015,]) +
  geom_point(aes(x=doy, y=pb.temp, #col=year, 
                 group = year), stroke = NA, 
             alpha=0.1) +
  geom_smooth(data=dat[leap_year(year(dat$dailyts)),],
              aes(x=doy, y=pb.temp, col=year,
                  group = year),
              alpha=0.7, method='gam',
              se=F, lwd=0.7,
              n=366) +
  geom_smooth(data=dat[!leap_year(year(dat$dailyts)) &
                         dat$year != 2015,],
              aes(x=doy, y=pb.temp, col=year,
                  group = year),
              alpha=0.7, method='gam',
              se=F, lwd=0.7,
              n=365) +
  scale_color_viridis_c( )+
  coord_cartesian(ylim=c(-2, 22)) +
  ggtitle('Portland Harbor') +
  scale_x_continuous(breaks=c(1, 60,
                              121,
                              182, 244,
                              305,
                              366),
                     labels=c('Jan', 'Mar',
                              'May', 
                              'Jul',  'Sep',
                              'Nov',
                              'Jan')) +
  labs(x='Month', y='Temperature (C)') +
  theme(legend.key.width = unit(1, 'in'))

ggpubr::ggarrange(wcl, pb, common.legend = T,
                  legend = 'bottom')

ggplot(data=dat[dat$year != 2015,]) +
  geom_line(aes(x=doy, y=pb.temp, #col=year, 
                group = year),
            col='red',
            alpha=0.5) +
  geom_smooth(data=dat[leap_year(year(dat$dailyts)),],
              aes(x=doy, y=pb.temp, #col=year,
                  group = year), col='red',
              alpha=0.7, method='gam',
              se=F, lwd=0.7,
              n=366) +
  geom_smooth(data=dat[!leap_year(year(dat$dailyts)) &
                         dat$year != 2015,],
              aes(x=doy, y=pb.temp, #col=year,
                  group = year),
              col='red',
              alpha=0.7, method='gam',
              se=F, lwd=0.7,
              n=365) +
  
  geom_line(aes(x=doy, y=wcl.temp, #col=year, 
                group = year), 
            col='blue',
            alpha=0.5) +
  geom_smooth(data=dat[leap_year(year(dat$dailyts)),],
              aes(x=doy, y=wcl.temp, #col=year,
                  group = year), col='blue',
              alpha=0.7, method='gam',
              se=F, lwd=0.7,
              n=366) +
  geom_smooth(data=dat[!leap_year(year(dat$dailyts)) &
                         dat$year != 2015,],
              aes(x=doy, y=wcl.temp, #col=year,
                  group = year),
              col='blue',
              alpha=0.7, method='gam',
              se=F, lwd=0.7,
              n=365) +
  
  #scale_color_viridis_c() +
  scale_x_continuous(breaks=c(1, 60,
                              121,
                              182, 244,
                              305,
                              366),
                     labels=c('Jan', 'Mar',
                              'May', 
                              'Jul',  'Sep',
                              'Nov',
                              'Jan')) +
  #coord_cartesian(ylim=c(-3, 6)) +
  facet_wrap(vars(year)) +
  labs(x='Month', y='Temperature pb.tempient (C)\nWCL temp - PH temp') +
  theme(legend.key.width = unit(1, 'in'))
