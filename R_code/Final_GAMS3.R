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
dat <- read.csv(here('Data/Clean_Data/Seine/Intermediates/selected_species_abundance_for_GAMS.csv'))

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
performance::check_overdispersion(gam.silver)
performance::check_zeroinflation(gam.silver)
summary(gam.silver)

# Significant: T1,  T4, T5, T7

b <- getViz(gam.silver)
print(plot(b, allTerms = T), pages = 1)
# check(b)
t1 <- as.data.frame(plot(b, allTerms=T)$plots[[1]]$ggObj$data)

t4 <- as.data.frame(plot(b, allTerms=T)$plots[[4]]$ggObj$data)

t5 <- as.data.frame(plot(b, allTerms=T)$plots[[5]]$ggObj$data)
t7 <- as.data.frame(plot(b, allTerms=T)$plots[[7]]$ggObj$data)

# Make plots
labdat1 <- data.frame(
  x=23.88, y=29.6, label='A'
)
p1 <- ggplot(data=t1) +
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
  geom_label(data=labdat1, 
             aes(x=x, y=y, label=label)) +
  coord_cartesian(expand = F,
                  xlim=c(23.5, 39.5),
                  ylim=c(11.5, 30.5)) +
  theme(legend.position = 'bottom',
        legend.margin = margin(0,0,0,0),
        legend.box='horizontal',
        legend.direction = 'horizontal',
        legend.spacing = unit(0, 'cm')) +
  guides(size = guide_legend(nrow = 1,
                             byrow=T,
                             bycol=T)) +
  labs(x='Week', y='Temp (\u00B0C)', fill='s(x)', size='Catch')

# And now plot them on the outcome scale
labdat4 <- data.frame(
  x=0.8, y=13.0, label='B'
)
p4 <- marginaleffects::plot_predictions(gam.silver, 
                 condition = 'site_name',
                 exclude = c('s(week, temp_degc)',
                             's(temp_degc)',
                             's(TideHT.m)',
                             'year',
                             'substrate',
                             'weather')) +
  geom_label(data=labdat4, 
             aes(x=x, y=y, label=label)) +
  labs(y = 're(Site name)', x='Site name') +
  coord_cartesian(xlim=c(0.5, 12.5),
                  ylim=c(-7, 14),
                  expand = F) +
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

labdat5 <- data.frame(
  x=0.73, y=3.085, label='C'
)
p5 <- ggplot(data=t5) +
  geom_errorbar(aes(x=x, ymin = y-(se*1.96), ymax=y+(1.96*se)),
                col='gray40', width = 0.5, lwd=0.8) +
  geom_point(aes(x=x, y=y), cex=2) +
  geom_hline(yintercept = 0, lty=2, col='red') +
  geom_label(data=labdat5, 
             aes(x=x, y=y, label=label)) +
  labs(x='Year', y='f(Year)') +
  coord_cartesian(expand=F,
                  xlim=c(0.5, 10.5),
                  ylim=c(-0.25, 3.25))

t7$x <- ordered(t7$x, levels = c('sunny','partly cloudy', 'overcast', 'rain'))

labdat7 <- data.frame(
  x=0.6, y=1.09, label='D'
)
p7 <- ggplot(data=t7) +
  geom_errorbar(aes(x=x, ymin = y-(se*1.96), ymax=y+(1.96*se)),
                col='gray40', width = 0.5, lwd=0.8) +
  geom_point(aes(x=x, y=y), cex=2) +
  geom_hline(yintercept = 0, lty=2, col='red') +
  labs(x='Weather', y='f(Weather)')  +
  geom_label(data=labdat7, 
             aes(x=x, y=y, label=label)) +
  coord_cartesian(expand=F,
                  xlim=c(0.5, 4.5),
                  ylim=c(-1.2, 1.2))

plot.silver <- egg::ggarrange(p1, p4, p5, p7, nrow=2, ncol=2,
                              
                              label.args = list(gp=grid::gpar(font=1), 
                                                x=unit(0,"line"), 
                                                hjust=0))
ggsave(plot = plot.silver,
       filename = here('Documentation/MEPS/Figures/Figure_3.png'),
       width = 338, 
       height = 169,
       units = 'mm')

rm(p1, p4, p5, p7, b, t1, t4, t5, t7,
   labdat1, labdat4, labdat5, labdat7, dat.silver, gam.silver,
   plot.silver)

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
performance::check_overdispersion(gam.herring)
performance::check_zeroinflation(gam.herring)
summary(gam.herring)
# Significant: T1, T2, T3, T5, T6

b <- getViz(gam.herring)
print(plot(b, allTerms = T), pages = 1)

t1 <- as.data.frame(plot(b, allTerms=T)$plots[[1]]$ggObj$data)
t2 <- as.data.frame(plot(b, allTerms=T)$plots[[2]]$ggObj$data)
t3 <- as.data.frame(plot(b, allTerms=T)$plots[[3]]$ggObj$data)
t5 <- as.data.frame(plot(b, allTerms=T)$plots[[5]]$ggObj$data)
t6 <- as.data.frame(plot(b, allTerms=T)$plots[[6]]$ggObj$data)

labdat1 <- data.frame(
  x=23.88, y=29.4, label='A'
)
p1 <- ggplot(data=t1) +
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
  geom_label(data=labdat1, aes(x=x, y=y, label=label)) +
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
  coord_cartesian(expand = F,
                  xlim=c(23.5, 39.5),
                  ylim=c(11.5, 30.5)) +
  labs(x='Week', y='Temp (C)', fill='s(x)', size='Catch')

labdat2 <- data.frame(
  x=12.45, y=1.8, label='B'
)
p2 <- ggplot(data=t2) +
  geom_ribbon(aes(x=x, ymin=y-se, ymax=y+se),
              fill='gray80') +
  geom_line(aes(x=x, y=y)) +
  geom_rug(data=dat.herring,
           aes(x=temp_degc)) +
  labs(title=NULL, x='Temp (C)', y='s(Temp (C))') + 
  coord_cartesian(expand = F,
                  xlim=c(12, 30.1), 
                  ylim=c(-17, 3)) +
  geom_label(data=labdat2, aes(x=x, y=y, label=label)) +
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

labdat3 <- data.frame(
  x=-0.08, y=1.5, label='C'
)
p3 <- ggplot(data=t3) +
  geom_ribbon(aes(x=x, ymin=y-se, ymax=y+se),
              fill='gray80') +
  geom_line(aes(x=x, y=y)) +
  geom_rug(data=dat.herring,
           aes(x=TideHT.m)) +
  labs(title=NULL, x='Tide Gauge Ht (m)', y='s(Tide Gauge Ht (m))') + 
  coord_cartesian(expand = F,
                  xlim=c(-0.16, 3.28), 
                  ylim=c(-5.2, 1.9)) +
  geom_label(data=labdat3,
             aes(x=x, y=y, label=label)) +
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

labdat5 <- data.frame(
  x=0.75, y=3.2, label='E'
)
p5 <- ggplot(data=t5) +
  geom_errorbar(aes(x=x, ymin = y-(se*1.96), ymax=y+(1.96*se)),
                col='gray40', width = 0.5, lwd=0.8) +
  geom_point(aes(x=x, y=y), cex=2) +
  geom_hline(yintercept = 0, lty=2, col='red') +
  coord_cartesian(xlim=c(0.5, 10.5),
                  ylim=c(-10, 4),
                  expand = F) +
  geom_label(data=labdat5, aes(x=x, y=y, label=label)) +
  labs(x='Year', y='f(Year)')

t6$x <- ordered(t6$x, levels = c('sand/gravel','sand', 'mud'))

labdat6 <- data.frame(
  x=0.57, y=1.1, label='F'
)
p6 <- ggplot(data=t6) +
  geom_errorbar(aes(x=x, ymin = y-(se*1.96), ymax=y+(1.96*se)),
                col='gray40', width = 0.5, lwd=0.8) +
  geom_point(aes(x=x, y=y), cex=2) +
  geom_hline(yintercept = 0, lty=2, col='red') +
  coord_cartesian(expand = F,
                  xlim=c(0.5, 3.5),
                  ylim=c(-5.5, 1.5)) +
  geom_label(data=labdat6, aes(x=x, y=y, label=label)) +
  labs(x='Substrate', y='f(Substrate)')

plot.herring <- egg::ggarrange(p1, p2, p3, #t4, 
                               p5, p6, nrow=3, ncol=2,
                              
                              label.args = list(gp=grid::gpar(font=1), 
                                                x=unit(0,"line"), 
                                                hjust=0))
plot.herring

ggsave(plot = plot.herring,
       filename = here('Documentation/MEPS/Figures/Figure_4.png'),
       width = 338, 
       height = 245,
       units = 'mm')

rm(b, t1, t2, t3, t4, t5, t6, t7, 
   p1, p2, p3, p4, p5, p6, p7,
   labdat1, labdat2, labdat3,labdat4, labdat5, labdat6, labdat7,
   gam.herring, plot.herring)


