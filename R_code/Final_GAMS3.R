# Run GAMS
rm(list=ls())

library(tidyverse)
library(here)
library(mgcv)
library(mgcViz)
library(marginaleffects)

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

## Load data
dat <- read.csv(here('Clean_Data/selected_species_abundance2.csv'))

# Shorten names
sn <- data.frame(
  site_name = c('Presumpscot Moorings',
                'Skitterygusset',
                'Audubon',
                'Mussel Cove',
                'Mackworth Island - North',
                'Mackworth Island - Beach',
                'The Brothers - North',
                'Back Cove',
                'Great Diamond Island',
                'Cushing Island',
                'SMCC',
                'Alewife Cove'),
  site_abbrev = c('PM',
                  'SG',
                  'AD',
                  'MC',
                  'MN',
                  'MB',
                  'BN',
                  'BC',
                  'GD',
                  'CI',
                  'SM',
                  'AC')
)

dat <- left_join(dat, sn, by=c('site_name'))

dat <-  dat %>% 
  mutate(site_name = factor(site_abbrev,
                            levels = c('PM',
                                      'SG',
                                      'AD',
                                      'MC',
                                      'MN',
                                      'MB',
                                      'BN',
                                      'BC',
                                      'GD',
                                      'CI',
                                      'SM',
                                      'AC')),
         substrate = factor(substrate, levels = c('sand/gravel',
                                                  'sand',
                                                  'mud')),
         stage = factor(stage, levels = c('rising', 'falling')),
         weather = factor(weather, levels=c('sunny','partly cloudy',
                                            'overcast', 'rain')),
         year = factor(year, levels = c('2014', '2015', '2016',
                                        '2017', '2018', '2019', '2020',
                                        '2021', '2022', '2023',
                                        '2024'))) %>% 
  mutate(date = as.Date(date, format = '%Y-%m-%d')) %>% 

  filter(year != 2019) %>% 
  filter(week >=24) %>% 
  filter(week<=39) %>% 
  #mutate(month = factor(month, levels = c('6', '7', '8', '9'))) %>% 
  filter(!is.na(temp_degc)) %>% 
  filter(!is.na(TideHT.m)) #%>% 
  #mutate(weekf=factor(week))

#### hold ####

#### Silverside ####
dat.silver <- dat %>% 
  #mutate(month = as.numeric(as.character(month))) %>% 
  filter(species_name == 'silver')

gam.silver <- gam(formula = catch
                  ~ s(week, temp_degc, bs='tp') + #1
                    
                    s(temp_degc, bs='tp') +       #2
                    s(TideHT.m, bs='tp') +        #3
                    
                    s(site_name, bs='re') +      #4
                    
                    year +                        #5
                    substrate +                   #6
                    weather+                      #7
                    stage,
                    
                  
                  data = dat.silver, 
                  family = nb(link = 'log'), 
                  method = "REML",
                  select = T)
AIC(gam.silver)
summary(gam.silver)

# Significant: T1,  T4, T5, T7

b <- getViz(gam.silver)
print(plot(b, allTerms = T), pages = 1)
# check(b)
t1 <- as.data.frame(plot(b, allTerms=T)$plots[[1]]$ggObj$data)

t4 <- as.data.frame(plot(b, allTerms=T)$plots[[4]]$ggObj$data)

t5 <- as.data.frame(plot(b, allTerms=T)$plots[[5]]$ggObj$data)
t7 <- as.data.frame(plot(b, allTerms=T)$plots[[7]]$ggObj$data)

t1 <- ggplot(data=t1) +
  geom_raster(aes(x=x, y=y, fill=z)) +
  geom_contour(aes(x=x, y=y, z=z), col='gray30') +
  metR::geom_text_contour(aes(x=x, y=y, z = z)) +
  
  geom_point(data=dat.silver[dat.silver$catch >0,],
             aes(x=week, y=temp_degc, size=catch),
             pch=19, #cex=0.2, 
             alpha=0.2) +
  scale_size_binned(name = 'Catch',
                    breaks = c(100, 1000, 2000, 
                               7000),
                    range=c(1, 12)) +
  
  geom_point(data=dat.silver[dat.silver$catch == 0,],
             aes(x=week, y=temp_degc),
             pch='x', cex=3, 
             alpha=0.2) +
  
  scale_fill_gradient2(na.value = 'transparent',
                       midpoint = 0) +
  theme(legend.position = 'bottom',
        legend.margin = margin(0,0,0,0),
        legend.box='horizontal',
        legend.direction = 'horizontal',
        legend.spacing = unit(0, 'cm')) +
  guides(size = guide_legend(nrow = 1,
                             byrow=T,
                             bycol=T)) +
  labs(x='Week', y='Temp (C)', fill='s(x)', size='Catch')

# And now plot them on the outcome scale
t4 <- marginaleffects::plot_predictions(gam.silver, 
                 condition = 'site_name',
                 exclude = c('s(week, temp_degc)',
                             's(temp_degc)',
                             's(TideHT.m)',
                             'year',
                             'substrate',
                             'weather')) +
  labs(y = 're(Site name)', x='Site name') +
  theme(panel.grid.major = element_line(color='lightgray'),
        panel.grid.minor = element_blank(),
        panel.background = element_blank(),
        panel.border = element_rect(color='black', size=1, fill=NA),
        legend.position = "bottom",
        axis.text.x=element_text(size=11),
        axis.text.y=element_text(size=11),
        axis.title.x=element_text(size=12),
        axis.title.y=element_text(size=12, angle=90, vjust=2),
        plot.title=element_text(size=14, hjust = 0, vjust = 1.2),
        plot.caption=element_text(hjust=0, face='italic', size=12))

t5 <- ggplot(data=t5) +
  geom_errorbar(aes(x=x, ymin = y-(se*1.96), ymax=y+(1.96*se)),
                col='gray40', width = 0.5, lwd=0.8) +
  geom_point(aes(x=x, y=y), cex=2) +
  geom_hline(yintercept = 0, lty=2, col='red') +
  labs(x='Year', y='f(Year)')

t7$x <- ordered(t7$x, levels = c('sunny','partly cloudy', 'overcast', 'rain'))

t7 <- ggplot(data=t7) +
  geom_errorbar(aes(x=x, ymin = y-(se*1.96), ymax=y+(1.96*se)),
                col='gray40', width = 0.5, lwd=0.8) +
  geom_point(aes(x=x, y=y), cex=2) +
  geom_hline(yintercept = 0, lty=2, col='red') +
  labs(x='Weather', y='f(Weather)') 

plot.silver <- egg::ggarrange(t1, t4, t5, t7, nrow=2, ncol=2,
                                 labels=c('a)', 'b)', 'c)', 'd)'),
                              
                              label.args = list(gp=grid::gpar(font=1), 
                                                x=unit(0,"line"), 
                                                hjust=0))
rm(t1, t2, t3, t4, t5, t6, t7, b)

#### Herring ####
dat.herring <- dat %>% 
  filter(species_name == 'herring')

gam.herring <- gam(formula = catch
                   ~ s(week, temp_degc, bs='tp') + #1
                     
                     s(temp_degc, bs='tp') +       #2
                     s(TideHT.m, bs='tp') +        #3
                     
                     s(site_name, bs = "re")+      #4
                     
                     year +                        #5
                     substrate +                   #6
                     weather+                      #7
                     stage,
                   
                   data = dat.herring, 
                   family = nb(link = 'log'), 
                   method = "REML",
                   select = T)
AIC(gam.herring)
summary(gam.herring)
# Significant: T1, T2, T3, T5, T6

b <- getViz(gam.herring)
print(plot(b, allTerms = T), pages = 1)

t1 <- as.data.frame(plot(b, allTerms=T)$plots[[1]]$ggObj$data)
t2 <- as.data.frame(plot(b, allTerms=T)$plots[[2]]$ggObj$data)
t3 <- as.data.frame(plot(b, allTerms=T)$plots[[3]]$ggObj$data)
t5 <- as.data.frame(plot(b, allTerms=T)$plots[[5]]$ggObj$data)
t6 <- as.data.frame(plot(b, allTerms=T)$plots[[6]]$ggObj$data)

t1 <- ggplot(data=t1) +
  geom_raster(aes(x=x, y=y, fill=z)) +
  geom_contour(aes(x=x, y=y, z=z), col='gray30') +
  metR::geom_text_contour(aes(x=x, y=y, z = z)) +
  
  geom_point(data=dat.herring[dat.herring$catch >0,],
             aes(x=week, y=temp_degc, size=catch),
             pch=19, #cex=0.2, 
             alpha=0.2) +
  scale_size_binned(name = 'Catch',
                    breaks = c(100, 1000,
                               10000, 20000),
                    range=c(1, 12)) +

  geom_point(data=dat.herring[dat.herring$catch == 0,],
             aes(x=week, y=temp_degc),
             pch='x', cex=3, 
             alpha=0.2) +
  
  scale_fill_gradient2(na.value = 'transparent',
                       midpoint = 0) +
  theme(legend.position = 'bottom',
        legend.margin = margin(0,0,0,0),
        legend.box='horizontal',
        legend.direction = 'horizontal',
        legend.spacing = unit(0, 'cm')) +
  guides(size = guide_legend(nrow = 1,
                             byrow=T,
                             bycol=T)) +
  labs(x='Week', y='Temp (C)', fill='s(x)', size='Catch')

t2 <- ggplot(data=t2) +
  geom_ribbon(aes(x=x, ymin=y-se, ymax=y+se),
              fill='gray80') +
  geom_line(aes(x=x, y=y)) +
  geom_rug(data=dat.herring,
           aes(x=temp_degc)) +
  labs(title=NULL, x='Temp (C)', y='s(Temp (C))') + 
  theme(panel.grid.major = element_line(color='lightgray'),
        panel.grid.minor = element_blank(),
        panel.background = element_blank(),
        panel.border = element_rect(color='black', size=1, fill=NA),
        legend.position = "bottom",
        axis.text.x=element_text(size=11),
        axis.text.y=element_text(size=11),
        axis.title.x=element_text(size=12),
        axis.title.y=element_text(size=12, angle=90, vjust=2),
        plot.title=element_text(size=14, hjust = 0, vjust = 1.2),
        plot.caption=element_text(hjust=0, face='italic', size=12))

t3 <- ggplot(data=t3) +
  geom_ribbon(aes(x=x, ymin=y-se, ymax=y+se),
              fill='gray80') +
  geom_line(aes(x=x, y=y)) +
  geom_rug(data=dat.herring,
           aes(x=TideHT.m)) +
  labs(title=NULL, x='Tide Gauge Ht (m)', y='s(Tide Gauge Ht (m))') + 
  theme(panel.grid.major = element_line(color='lightgray'),
        panel.grid.minor = element_blank(),
        panel.background = element_blank(),
        panel.border = element_rect(color='black', size=1, fill=NA),
        legend.position = "bottom",
        axis.text.x=element_text(size=11),
        axis.text.y=element_text(size=11),
        axis.title.x=element_text(size=12),
        axis.title.y=element_text(size=12, angle=90, vjust=2),
        plot.title=element_text(size=14, hjust = 0, vjust = 1.2),
        plot.caption=element_text(hjust=0, face='italic', size=12)) 

t4 <- marginaleffects::plot_predictions(gam.herring, 
                                        condition = 'site_name',
                                        exclude = c('s(week, temp_degc)',
                                                    's(temp_degc)',
                                                    's(TideHT.m)',
                                                    'year',
                                                    'substrate',
                                                    'weather')) +
  labs(y = 're(Site name)', x='Site name') +
  theme(panel.grid.major = element_line(color='lightgray'),
        panel.grid.minor = element_blank(),
        panel.background = element_blank(),
        panel.border = element_rect(color='black', size=1, fill=NA),
        legend.position = "bottom",
        axis.text.x=element_text(size=11),
        axis.text.y=element_text(size=11),
        axis.title.x=element_text(size=12),
        axis.title.y=element_text(size=12, angle=90, vjust=2),
        plot.title=element_text(size=14, hjust = 0, vjust = 1.2),
        plot.caption=element_text(hjust=0, face='italic', size=12))

t5 <- ggplot(data=t5) +
  geom_errorbar(aes(x=x, ymin = y-(se*1.96), ymax=y+(1.96*se)),
                col='gray40', width = 0.5, lwd=0.8) +
  geom_point(aes(x=x, y=y), cex=2) +
  geom_hline(yintercept = 0, lty=2, col='red') +
  labs(x='Year', y='f(Year)')

t6$x <- ordered(t6$x, levels = c('sand/gravel','sand', 'mud'))

t6 <- ggplot(data=t6) +
  geom_errorbar(aes(x=x, ymin = y-(se*1.96), ymax=y+(1.96*se)),
                col='gray40', width = 0.5, lwd=0.8) +
  geom_point(aes(x=x, y=y), cex=2) +
  geom_hline(yintercept = 0, lty=2, col='red') +
  labs(x='Substrate', y='f(Substrate)')

plot.herring <- egg::ggarrange(t1, t2, t3, #t4, 
                               t5, t6, nrow=3, ncol=2,
                              labels=c('a)', 'b)', 'c)', 'd)', 'e)', 'f)'),
                              
                              label.args = list(gp=grid::gpar(font=1), 
                                                x=unit(0,"line"), 
                                                hjust=0))

rm(b, t1, t2, t3, t4, t5, t6, t7)

#### Save ####
ggsave(plot = plot.silver,
       filename = here('Documentation/MEPS/Figures/Silverside GAM2.png'),
       width = 338, 
       height = 169,
       units = 'mm')

ggsave(plot = plot.herring,
       filename = here('Documentation/MEPS/Figures/Herring GAM2.png'),
       width = 338, 
       height = 245,
       units = 'mm')
