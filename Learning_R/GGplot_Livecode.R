rm(list=ls())

# Load libraries
library(palmerpenguins)
library(tidyverse)

# Load data
penguins <- palmerpenguins::penguins

# View data
head(penguins)
summary(penguins)
str(penguins)

# Clean data
penguins <- penguins %>% 
  na.omit()

#### Basic plots ####

# Basic plots: boxplot
ggplot(data=penguins,
       aes(x=species, y=flipper_length_mm)) +
  geom_boxplot()

# Basic plots: histogram
ggplot(data=penguins,
aes(x=flipper_length_mm)) +
  geom_histogram()

# Basic plots: scatterplot
ggplot(data=penguins,
       aes(x=flipper_length_mm,
           y=bill_length_mm)) +
  geom_point()

#### Facet plots ####

# Facet plots: boxplot
ggplot(data=penguins,
       aes(x=species, y=flipper_length_mm)) +
  geom_boxplot() +
  facet_wrap(vars(island))

# Facet plots: histogram
ggplot(data=penguins,
       aes(x=flipper_length_mm)) +
  geom_histogram() +
  facet_wrap(vars(island))

# Basic plots: scatterplot
ggplot(data=penguins,
       aes(x=flipper_length_mm,
           y=bill_length_mm)) +
  geom_point() +
  facet_wrap(vars(island))

#### Pretty plots ####
# Facet plots: boxplot
ggplot(data=penguins,
       aes(x=species, y=flipper_length_mm)) +
  geom_boxplot() +
  facet_wrap(vars(island))

# Add color for sex
ggplot(data=penguins,
       aes(x=species, y=flipper_length_mm, col=sex)) +
  geom_boxplot() +
  facet_wrap(vars(island))

# Adjust axis titles and plot title
ggplot(data=penguins,
       aes(x=species, y=flipper_length_mm, col=sex)) +
  geom_boxplot() +
  facet_wrap(vars(island)) +
  xlab('Species') +
  ylab('Flipper length (mm)') +
  ggtitle('Flipper length of penguins')

# Adjust axis title and plot title font size
ggplot(data=penguins,
       aes(x=species, y=flipper_length_mm, col=sex)) +
  geom_boxplot() +
  facet_wrap(vars(island)) +
  xlab('Species') +
  ylab('Flipper length (mm)') +
  ggtitle('Flipper length of penguins') +
  theme(axis.title.x = element_text(size=14),
        axis.title.y = element_text(size=14),
        plot.title = element_text(size=16))

# Adjust axis labels font size
ggplot(data=penguins,
       aes(x=species, y=flipper_length_mm, col=sex)) +
  geom_boxplot() +
  facet_wrap(vars(island)) +
  xlab('Species') +
  ylab('Flipper length (mm)') +
  ggtitle('Flipper length of penguins') +
  theme(axis.title.x = element_text(size=14),
        axis.title.y = element_text(size=14),
        plot.title = element_text(size=16),
        axis.text.x=element_text(size=12),
        axis.text.y=element_text(size=12))

# Adjust facet label size
ggplot(data=penguins,
       aes(x=species, y=flipper_length_mm, col=sex)) +
  geom_boxplot() +
  facet_wrap(vars(island)) +
  xlab('Species') +
  ylab('Flipper length (mm)') +
  ggtitle('Flipper length of penguins') +
  theme(axis.title.x = element_text(size=14),
        axis.title.y = element_text(size=14),
        plot.title = element_text(size=16),
        axis.text.x=element_text(size=12),
        axis.text.y=element_text(size=12),
        strip.text = element_text(size=12))

# Remove gray background from plot
ggplot(data=penguins,
       aes(x=species, y=flipper_length_mm, col=sex)) +
  geom_boxplot() +
  facet_wrap(vars(island)) +
  xlab('Species') +
  ylab('Flipper length (mm)') +
  ggtitle('Flipper length of penguins') +
  theme(axis.title.x = element_text(size=14),
        axis.title.y = element_text(size=14),
        plot.title = element_text(size=16),
        axis.text.x=element_text(size=12),
        axis.text.y=element_text(size=12),
        strip.text = element_text(size=12),
        panel.background = element_blank())

# Add gray grid lines to background
ggplot(data=penguins,
       aes(x=species, y=flipper_length_mm, col=sex)) +
  geom_boxplot() +
  facet_wrap(vars(island)) +
  xlab('Species') +
  ylab('Flipper length (mm)') +
  ggtitle('Flipper length of penguins') +
  theme(axis.title.x = element_text(size=14),
        axis.title.y = element_text(size=14),
        plot.title = element_text(size=16),
        axis.text.x=element_text(size=12),
        axis.text.y=element_text(size=12),
        strip.text = element_text(size=12),
        panel.background = element_blank(),
        panel.grid.major = element_line(color='lightgray'))

# Change facet strip text background color
ggplot(data=penguins,
       aes(x=species, y=flipper_length_mm, col=sex)) +
  geom_boxplot() +
  facet_wrap(vars(island)) +
  xlab('Species') +
  ylab('Flipper length (mm)') +
  ggtitle('Flipper length of penguins') +
  theme(axis.title.x = element_text(size=14),
        axis.title.y = element_text(size=14),
        plot.title = element_text(size=16),
        axis.text.x=element_text(size=12),
        axis.text.y=element_text(size=12),
        strip.text = element_text(size=12),
        panel.background = element_blank(),
        panel.grid.major = element_line(color='lightgray'),
        strip.background = element_rect(fill='lightblue'))

# Change legend title
ggplot(data=penguins,
       aes(x=species, y=flipper_length_mm, col=sex)) +
  geom_boxplot() +
  facet_wrap(vars(island)) +
  xlab('Species') +
  ylab('Flipper length (mm)') +
  ggtitle('Flipper length of penguins') +
  theme(axis.title.x = element_text(size=14),
        axis.title.y = element_text(size=14),
        plot.title = element_text(size=16),
        axis.text.x=element_text(size=12),
        axis.text.y=element_text(size=12),
        strip.text = element_text(size=12),
        panel.background = element_blank(),
        panel.grid.major = element_line(color='lightgray'),
        strip.background = element_rect(fill='lightblue')) +
  labs(col='Sex')

# Change legend item names
ggplot(data=penguins,
       aes(x=species, y=flipper_length_mm, col=sex)) +
  geom_boxplot() +
  facet_wrap(vars(island)) +
  xlab('Species') +
  ylab('Flipper length (mm)') +
  ggtitle('Flipper length of penguins') +
  theme(axis.title.x = element_text(size=14),
        axis.title.y = element_text(size=14),
        plot.title = element_text(size=16),
        axis.text.x=element_text(size=12),
        axis.text.y=element_text(size=12),
        strip.text = element_text(size=12),
        panel.background = element_blank(),
        panel.grid.major = element_line(color='lightgray'),
        strip.background = element_rect(fill='lightblue')) +
  labs(col='Sex') +
  scale_color_discrete(labels=c('Gals', 'Guys'))

# Remove legend background
ggplot(data=penguins,
       aes(x=species, y=flipper_length_mm, col=sex)) +
  geom_boxplot() +
  facet_wrap(vars(island)) +
  xlab('Species') +
  ylab('Flipper length (mm)') +
  ggtitle('Flipper length of penguins') +
  theme(axis.title.x = element_text(size=14),
        axis.title.y = element_text(size=14),
        plot.title = element_text(size=16),
        axis.text.x=element_text(size=12),
        axis.text.y=element_text(size=12),
        strip.text = element_text(size=12),
        panel.background = element_blank(),
        panel.grid.major = element_line(color='lightgray'),
        strip.background = element_rect(fill='lightblue'),
        legend.key=element_blank()) +
  labs(col='Sex') +
  scale_color_discrete(labels=c('Gals', 'Guys'))

# Move legend to bottom
ggplot(data=penguins,
       aes(x=species, y=flipper_length_mm, col=sex)) +
  geom_boxplot() +
  facet_wrap(vars(island)) +
  xlab('Species') +
  ylab('Flipper length (mm)') +
  ggtitle('Flipper length of penguins') +
  theme(axis.title.x = element_text(size=14),
        axis.title.y = element_text(size=14),
        plot.title = element_text(size=16),
        axis.text.x=element_text(size=12),
        axis.text.y=element_text(size=12),
        strip.text = element_text(size=12),
        panel.background = element_blank(),
        panel.grid.major = element_line(color='lightgray'),
        strip.background = element_rect(fill='lightblue'),
        legend.key=element_blank(),
        legend.position = 'bottom') +
  labs(col='Sex') +
  scale_color_discrete(labels=c('Gals', 'Guys'))

# Change legend colors
ggplot(data=penguins,
       aes(x=species, y=flipper_length_mm, col=sex)) +
  geom_boxplot() +
  facet_wrap(vars(island)) +
  xlab('Species') +
  ylab('Flipper length (mm)') +
  ggtitle('Flipper length of penguins') +
  theme(axis.title.x = element_text(size=14),
        axis.title.y = element_text(size=14),
        plot.title = element_text(size=16),
        axis.text.x=element_text(size=12),
        axis.text.y=element_text(size=12),
        strip.text = element_text(size=12),
        panel.background = element_blank(),
        panel.grid.major = element_line(color='lightgray'),
        strip.background = element_rect(fill='lightblue'),
        legend.key=element_blank(),
        legend.position = 'bottom') +
  labs(col='Sex') +
  scale_color_manual(labels=c('Gals', 'Guys'),
                     values=c('purple', 'gold'))
# Notice that we have to use scale_color_manual to do this, previously we were using
# scale_color_discrete.

# Rotate axis labels
ggplot(data=penguins,
       aes(x=species, y=flipper_length_mm, col=sex)) +
  geom_boxplot() +
  facet_wrap(vars(island)) +
  xlab('Species') +
  ylab('Flipper length (mm)') +
  ggtitle('Flipper length of penguins') +
  theme(axis.title.x = element_text(size=14),
        axis.title.y = element_text(size=14),
        plot.title = element_text(size=16),
        axis.text.x=element_text(size=12, angle=45, vjust=0.7),
        axis.text.y=element_text(size=12),
        strip.text = element_text(size=12),
        panel.background = element_blank(),
        panel.grid.major = element_line(color='lightgray'),
        strip.background = element_rect(fill='lightblue'),
        legend.key=element_blank(),
        legend.position = 'bottom') +
  labs(col='Sex') +
  scale_color_manual(labels=c('Gals', 'Guys'),
                     values=c('purple', 'gold'))

#### Save results ####
# When you're happy with how your plot looks, save the output as an object.
penguins.plot <- ggplot(data=penguins,
                        aes(x=species, y=flipper_length_mm, col=sex)) +
  geom_boxplot() +
  facet_wrap(vars(island)) +
  xlab('Species') +
  ylab('Flipper length (mm)') +
  ggtitle('Flipper length of penguins') +
  theme(axis.title.x = element_text(size=14),
        axis.title.y = element_text(size=14),
        plot.title = element_text(size=16),
        axis.text.x=element_text(size=12, angle=45, vjust=0.7),
        axis.text.y=element_text(size=12),
        strip.text = element_text(size=12),
        panel.background = element_blank(),
        panel.grid.major = element_line(color='lightgray'),
        strip.background = element_rect(fill='lightblue'),
        legend.key=element_blank(),
        legend.position = 'bottom') +
  labs(col='Sex') +
  scale_color_manual(labels=c('Gals', 'Guys'),
                     values=c('purple', 'gold'))

# Use ggsave to save the output to a specific place on your computer.
ggsave(plot=penguins.plot,  # Specify which plot you want to save
       filename="C:/Users/klankowicz/Desktop/penguins.png", # Specify file path and name
       # On a mac: 
       # filename="Users/klankowicz/Desktop/penguins.png",
       device="png", # Specify file type, must match your filename extension
       height=8.5,   # Specify image height
       width=11,     # Specify image width
       units="in")   # Specify units intended for image height and width