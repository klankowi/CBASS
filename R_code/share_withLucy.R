rm(list=ls())

# Load libraries
library(tidyverse)
library(here)

mp <- read.csv(here('Raw_Data/oyster_mp_data.csv'))

head(mp)
str(mp)

# Trim down dataframe
df <- select(mp,
             oyster_id, total_fibers, total_fragments,total_beads)

# Pivot into long format
df <- df %>% 
  pivot_longer(!oyster_id, names_to='type', values_to='count')

# Make stacked barplot
stacked.mp <- ggplot(df, aes(fill=type, y=count, x=oyster_id)) + 
  geom_bar(position="stack", stat="identity") +
  xlab('Oyster ID') +
  ylab('Frequency') +
  labs(fill='Type') +
  theme(axis.text.x = element_text(angle=45)) +
  scale_fill_discrete(labels=c('Beads', 'Fibers', 'Fragments'))
ggsave(plot = stacked.mp,
       filename = "stacked.mp.png",
       device="png")

# Scatterplot of particle abundance vs wet meat weight
meat.cor <- cor(mp$weight_wet_meat, mp$total_particles, use="pairwise.complete.obs")
meat.cor <- round(meat.cor, 2)

scatter.meat <- ggplot(data=mp, aes(x=weight_wet_meat, y=total_particles)) +
  geom_point() +
  geom_smooth(method='lm', se=FALSE) +
  xlab('Meat wet weight (g)') +
  ylab('Total particles') +
  annotate("text",
           label=paste0("Correlation = ", meat.cor),
           x=7.5, y=8)
ggsave(plot=scatter.meat,
       filename="scatter.meat.png",
       device='png')

# Scatterplot of particle abundance vs wet whole weight
whole.cor <- cor(mp$weight_wet_whole, mp$total_particles, use="pairwise.complete.obs")
whole.cor <- round(whole.cor, 2)

scatter.whole <- ggplot(data=mp, aes(x=weight_wet_whole, y=total_particles)) +
  geom_point() +
  geom_smooth(method='lm', se=FALSE) +
  xlab('Whole wet weight (g)') +
  ylab('Total particles') +
  annotate("text",
           label=paste0("Correlation = ", whole.cor),
           x=35, y=8)
ggsave(plot=scatter.whole,
       filename='scatter.whole.png',
       device='png')



