# Exploratory GAM
# 4 August

rm(list=ls())

library(tidyverse)
library(mgcv)
library(here)
library(mgcViz)
library(rstatix)
library(ggcorrplot)
library(rcompanion)

# Load data
sites <- read.csv(here('Clean_Data/sites_cleaned.csv'))
dat <- read.csv(here('Clean_Data/Diversity_Index_Calculation.csv'))
str(dat)

# Check
dat <- dat[dat$site_name !='The Brothers - South',]

# Merge
dat <- merge(dat, sites, by=c('site_name', 'bay_location'))

# Test for collinearity
quant <- dplyr::select(dat,
                       week, year, green.crab, tomcod, winter.flounder,
                       sandlance, mummichog, atlantic.silverside, 
                       atlantic.herring, do_mg.l, salinity_ppt, temp_degc,
                       TideHT.m,
                       numeric.tide, simpson, latitude, longitude)

# Create correlation matrix
model.matrix(~0+., data=quant) %>% 
  cor(use="all.obs", method="spearman") %>% 
  ggcorrplot(show.diag = F, type="lower", lab=TRUE, lab_size=3)

# Remove collinear terms
quant <- dplyr::select(quant, -TideHT.m, -salinity_ppt)

# Quantitatively check for outliers in response var
outlier.check <- dat %>% 
  identify_outliers(simpson) %>% 
  filter(is.extreme == T)

# Pull out any extreme outliers for observation
nrow(outlier.check[outlier.check$is.extreme==TRUE,])
# No extreme outliers

# GAMs
gam1 <- gam(
  (simpson) ~
              s(green.crab, bs='cs') +
              #s(site_name, bs='re') +
              #s(do_mg.l, bs='cs') +
              s(temp_degc, bs='cs') +
              s(numeric.tide, bs='cs') +
              te(week,year, bs='cs'), #+
              #s(week, bs='cs') +
              #s(year, bs='cs', k=6) +
              #bay_location +
              #weather,# +
              #substrate,
  
  data=dat, 
            
  method='REML', 
  select=TRUE,
  family=gaussian(link='log'))

# Check model diagnostics
gam.check(gam1)

# View results
summary(gam1)
anova(gam1)

# Check AIC
gam1$aic

# Plot results
# All results, one at a time
plot(gam1, all.terms = T, scheme=1)
# All results, together
plot(gam1, all.terms=T, scheme=1, page=1)

# 3 dimensional view of tensor product smooth (interaction)
vis.gam(gam1, view = c("week", "year"),
        theta = 50, n.grid = 50, lwd = 0.4)
# Adjust rotation by changing theta

# Plot smooths one at a time using "select"
plot(gam1, select=1, scheme=1)
# Add reference line at y-intercept 0.
# When confidence interval includes 0, explanatory variable at that value (on
# x-axis) is not significantly different from the mean of all responses.
abline(h=0, col='blue')

# On one line, do the same thing (plot smooth and reference line)
plot(gam1, select=2, scheme=1);abline(h=0, col='blue')
