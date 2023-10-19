rm(list=ls())

# Libraries
library(tidyverse)
#library(here)

# Set GGplot auto theme
theme_set(theme(panel.grid.major = element_line(color='lightgray'),
                panel.grid.minor = element_blank(),
                panel.background = element_blank(),
                panel.border = element_rect(color='black', size=1, fill=NA),
                legend.position = "bottom",
                axis.text.x=element_text(size=12),
                axis.text.y=element_text(size=12),
                axis.title.x=element_text(size=14),
                axis.title.y=element_text(size=14, angle=90, vjust=2),
                plot.title=element_text(size=14, hjust = 0, vjust = 1.2),
                plot.caption=element_text(hjust=0, face='italic', size=12)))

# Load data
oysters <- read.csv(here('Clean_Data/merged_oyster_data.csv'))
weekly.env <- read.csv(here('Clean_Data/weekly_environment.csv'))

# CLean oyster data
str(oysters)
oysters$date <- as.POSIXct(oysters$date, format="%Y-%m-%d")
oysters$site <- as.factor(oysters$site)

# Clean environmental data
str(weekly.env)
weekly.env$date <- as.POSIXct(weekly.env$day,
                              format="%Y-%m-%d")

# Plot oyster condition index
oyster.index <- ggplot(data=oysters, aes(x=date, y=index_1, color=site)) +
  geom_point() + 
  geom_smooth(aes(fill=site), alpha=0.25) +
  xlab("Date") + ylab("Index 1") + labs(col='Site', fill='Site')

ggsave(plot=oyster.index,
       filename = "index_1_plot.png",
       device="png")

# Plot oyster condition index
oyster.index3 <- ggplot(data=oysters, aes(x=date, y=index_3, color=site)) +
  geom_point() + 
  geom_smooth(aes(fill=site), alpha=0.25) +
  xlab("Date") + ylab("Index 3") + labs(col='Site', fill='Site')

ggsave(plot=oyster.index3,
       filename = "index_3_plot.png",
       device="png")

# Statistical comparison of condition indices at sites
t.test(oysters$index_1[oysters$site == "Snow"],
       oysters$index_1[oysters$site == "Dogs"])

ggplot(data=oysters,
       aes(x=site, y=index_1))+
  geom_boxplot() +
  xlab('Site') + ylab('Index 1')

# Statistical comparison of condition indices at sites
t.test(oysters$index_3[oysters$site == "Snow"],
       oysters$index_3[oysters$site == "Dogs"])

ggplot(data=oysters,
       aes(x=site, y=index_3))+
  geom_boxplot() +
  xlab('Site') + ylab('Index 3')

# Plot daily water temperature
ggplot(data=weekly.env, aes(x=date, y=daily.wattemp)) +
  geom_line() +
  xlab("Date") + ylab('Daily mean water temp (C)')

# Plot weekly water temperature
ggplot(data=weekly.env, aes(x=date, y=week.wattemp)) +
  geom_line() +
  xlab("Date") + ylab('7-day average water temp (C)')

# Plot daily precipitation
ggplot(data=weekly.env, aes(x=date, y=gauge.mm)) +
  geom_line() +
  xlab("Date") + ylab('Daily precipitation (mm)')

# Plot weekly precipitation
ggplot(data=weekly.env, aes(x=date, y=week.rain)) +
  geom_line() +
  xlab("Date") + ylab('7-day average rainfall (mm)')

# Modeling just for Katie
library(mgcv)
library(ggcorrplot)
quants <- dplyr::select(oysters, -date, -site)
# Create correlation matrix
model.matrix(~0+., data=quants) %>% 
  cor(use="all.obs", method="spearman") %>% 
  ggcorrplot(show.diag = F, type="lower", lab=TRUE, lab_size=3)
# Correlated: Indices 1 and 3, water temp and salinity/ph/turbidity,
# salinity and ph/turbidity, ph and turbidity.
# Response: they all suck
# Try weekly
oyster.env <- dplyr::select(weekly.env,
                            date, week.wattemp, week.ws, week.wd, week.airtemp,
                            week.pres, week.vis, week.rain, cumlt.rain)
oyster.env <- merge(oysters, oyster.env, by=c('date'))
quants <- dplyr::select(oyster.env,
                        week.wattemp, week.ws, week.wd,
                        week.airtemp, week.pres, week.vis, week.rain,
                        cumlt.rain, index_1, index_3)
# Create correlation matrix
model.matrix(~0+., data=quants) %>% 
  cor(use="all.obs", method="spearman") %>% 
  ggcorrplot(show.diag = F, type="lower", lab=TRUE, lab_size=3)
# Correlated: water temp and airtemp/ cumlt rain, wind speed and wind direction,
# wind direction and airtemp, airtemp and cumlt rain, pressure and cumlt rain,
# visibility and weekly rain.
# Remove: cumlt rain, wind speed, air temp
quants <- dplyr::select(oyster.env,
                        week.wattemp, week.wd,
                        week.pres, week.vis, week.rain,
                        index_1, index_3)
# Create correlation matrix
model.matrix(~0+., data=quants) %>% 
  cor(use="all.obs", method="spearman") %>% 
  ggcorrplot(show.diag = F, type="lower", lab=TRUE, lab_size=3)
# Try a GAM with index_1, week.vis, week, site.
oyster.env$week <- isoweek(oyster.env$date)

# Create GAM
gam1 <- gam(index_1 ~ 
              s(week.rain, bs='cs') +
              #s(week, bs='cs', k=9) +
              site,
            data=oyster.env,
            select=TRUE,
            method='REML')
summary(gam1)
anova(gam1)
gam.check(gam1)
plot(gam1, all.terms = TRUE, scheme=1, select=1)
abline(h=0, col='blue', lty=2)
