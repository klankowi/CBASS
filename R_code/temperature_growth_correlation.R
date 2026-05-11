rm(list=ls())

library(here)

dat <- read.csv(here('Data/Clean_Data/temp_growth2.csv'))
dat <- dat[dat$yearshift != 2025,]

head(dat)
str(dat)

######## Visuals
#### Silverside
# Annual
ggplot(data=dat) +
  geom_point(aes(x=annual.anom, y=silver.gr))

# Summer
ggplot(data=dat) +
  geom_point(aes(x=summer.anom, y=silver.gr))

#### Herring
# Annual
ggplot(data=dat) +
  geom_point(aes(x=annual.anom, y=herr.gr))

# Summer
ggplot(data=dat) +
  geom_point(aes(x=summer.anom, y=herr.gr))

#### Both
test <- dat %>% 
  pivot_longer(cols=c('silver.gr', 'herr.gr'), names_to = 'species')
test$species[test$species == 'silver.gr'] <- 'Silverside'
test$species[test$species == 'herr.gr'] <- 'Herring'

test <- test %>% 
  pivot_longer(cols = c('annual.anom', 'summer.anom'), names_to = 'period',
               values_to = 'anomaly')
test$period[test$period == 'annual.anom'] <- 'Annual anomaly'
test$period[test$period == 'summer.anom'] <- 'Summer anomaly'

ggplot(data=test) +
  geom_point(aes(x=rank(anomaly), y=rank(value), col=species)) +
  facet_wrap(vars(period)) +
  labs(x='Temp. anomaly \u00B0C', 
       y=expression(paste("Growth rate, mm ", wk^{-1})),
       col='Species')

#ggsave(plot=growthplot,
#       here('Documentation/MEPS/Figures/Figure_2_Rev.png'),
#       width = 169, height = 100, units='mm')

######## Regression
#### Silverside
# Annual
summary(lm(silver.gr ~ annual.anom, data = dat))

# Summer
summary(lm(silver.gr ~ summer.anom, data = dat))

#### Herring
# Annual
summary(lm(herr.gr ~ annual.anom, data = dat))

# Summer
summary(lm(herr.gr ~ summer.anom, data = dat))

######## Corr
dat <- dat[dat$yearshift != 2019 & dat$yearshift != 2024,]
#### Silverside
# Annual
cor.test(dat$annual.anom, dat$silver.gr, method = 'spearman')

# Summer
cor.test(dat$summer.anom, dat$silver.gr, method = 'spearman')

# Year
cor.test(dat$yearshift, dat$silver.gr, method = 'spearman')

#### Herring
dat <- dat[dat$yearshift != 2023,]
# Annual
cor.test(dat$annual.anom, dat$herr.gr, method = 'spearman')

# Summer
cor.test(dat$summer.anom, dat$herr.gr, method = 'spearman')

# year
cor.test(dat$yearshift, dat$herr.gr, method = 'spearman')
