# Identify month-site groups with similar community composition
rm(list=ls())

# Random seed for reproducibility
set.seed(123)

library(here)
library(tidyverse)
library(vegan)
library(sf)

# Negate function
'%notin%' <- function(x,y)!('%in%'(x,y))

# Set GGplot auto theme
theme_set(theme(panel.grid.major = element_line(color='lightgray'),
                panel.grid.minor = element_blank(),
                panel.background = element_blank(),
                panel.border = element_rect(color='black', 
                                            linewidth=1, fill=NA),
                legend.position = "bottom",
                axis.text.x=element_text(size=12),
                axis.text.y=element_text(size=12),
                axis.title.x=element_text(size=12),
                axis.title.y=element_text(size=12, angle=90, 
                                          vjust=2),
                plot.title=element_text(size=14, hjust = 0, 
                                        vjust = 1.2),
                plot.caption=element_text(hjust=0, 
                                          face='italic', size=12)))

# Geospatial data (seine sites and shoreline polygon)
sites <- read.csv(here('Clean_Data/Seine/sites_cleaned.csv'))
sites <- st_as_sf(sites, coords = c('longitude', 'latitude'),
                  crs= "EPSG:4326")
# Remove QBC
sites <- sites[sites$site_number < 20,]

shoreline <- st_read(here('GIS/us_medium_shoreline_poly.shp'),
                     quiet=T)
shoreline <- st_transform(shoreline, st_crs(sites))

## Load data
dat <- read.csv(here('Clean_Data/selected_species_abundance.csv'))

# Remove instances of abnormal sampling
dat <-  dat %>% 
  filter(notes %notin%
           c('site assumed, not indicated on sheet'
             ))

#Load abundance and trip data
abund <- read.csv(here('Clean_Data/Seine/abund_through_2024.csv'))
trips <- read.csv(here('Clean_Data/Seine/trips_through_2024.csv'))

# Clean
trips <- trips %>% 
  # Remove QBC
  filter(site_id < 20) %>% 
  # Format date
  mutate(date = as.Date(date, format = '%m/%d/%Y')) %>% 
  mutate(month = month(date)) %>% 
  mutate(week = isoweek(date)) %>% 
  mutate(day = day(date)) %>% 
  mutate(doy = yday(date))

trips <- trips %>% 
  filter(notes %in% dat$notes) %>% 
  filter(year != 2019) #incomplete sampling

# Split into two-week increments
trips$datper <- NA
trips$datper[trips$month == 5] <- 'LateSpring'
trips$datper[trips$month == 6 & trips$day <= 20] <- 'LateSpring'
trips$datper[trips$month == 6 & trips$day >= 21] <- 'EarlySummer'
trips$datper[trips$month == 7 & trips$day <= 20] <- 'EarlySummer'
trips$datper[trips$month == 7 & trips$day >= 21] <- 'MidSummer'
trips$datper[trips$month == 8 & trips$day <= 20] <- 'MidSummer'
trips$datper[trips$month == 8 & trips$day >= 21] <- 'LateSummer'
trips$datper[trips$month == 9 & trips$day <= 20] <- 'LateSummer'
trips$datper[trips$month == 9 & trips$day >= 21] <- 'EarlyFall'
trips$datper[trips$month == 10] <- 'EarlyFall'

trips$datper <- factor(trips$datper,
                       levels = c('LateSpring', 'EarlySummer',
                                  'MidSummer', 'LateSummer',
                                  'EarlyFall'))

# Check site coverage in each month
sitecov <- trips %>% 
  group_by(datper, site_name) %>% 
  summarise(ntrips = n())
ggplot(data=sitecov) +
  geom_tile(aes(x=datper, y=site_name, fill=ntrips),
            width = 0.9) +
  binned_scale(aesthetics = "fill",
               scale_name = "stepsn", 
               palette = function(x) c(viridis::viridis(n=7)),
               breaks = c(2, 4, 6, 8, 10, 12, 14),
               limits = c(2, 14),
               guide = "colorsteps"
  ) +
  theme(legend.key.width = unit(0.5, 'in')) +
  ggtitle('Assessment of sampling effort')
rm(sitecov)

# Chuck early fall
trips <- trips[trips$datper != 'EarlyFall',]

trips <- droplevels(trips)

# Save outcome-- number trips conducted at each site in each month
ntrips <- trips %>% 
  group_by(site_name, datper) %>% 
  summarise(ntrips = n()) %>% 
  as.data.frame()
summary(ntrips)
# 5 to 22 samples to characterize monthly spatiotemp chars.

# Save environmental characteristics
schar <- trips %>% 
  group_by(site_name, datper) %>% 
  summarise(temp = mean(temp_degc, na.rm=T),
            sal = mean(salinity_ppt, na.rm=T))

# Clean
abund <- abund %>% 
  # Remove QBC
  filter(site_id < 20) %>% 
  # Format date, keep only 'good' trips
  mutate(date = as.Date(date, format='%m/%d/%Y')) %>% 
  filter(loc_id %in% trips$loc_id) %>% 
  mutate(month = month(date)) %>% 
  filter(loc_id %in% trips$loc_id)

abund <- left_join(abund, dplyr::select(trips, loc_id, datper),
                   by=c('loc_id'))

# Get rid of grouped (unidentified) species
abund <- abund %>% 
  filter(species_name %notin% c('hake spp', 'killifish spp',
                                'river herring', 
                                'shiner spp', 'stickleback spp',
                                'sturgeon spp', 'sculpin spp'))

# Find most-caught species (by proportion of total catch)
comcat <- abund %>% 
  group_by(species_name) %>% 
  summarise(totcatch = sum(catch)) %>% 
  mutate(propcatch = (totcatch / sum(totcatch))* 100) %>% 
  arrange(propcatch) %>% as.data.frame()
tail(comcat, 10)

# Find most-encountered species
fishenc  <- as.data.frame(table(abund$species_name))
colnames(fishenc) <- c('species_name', 'encounters')
fishenc <- fishenc %>% 
  mutate(prop_enc = (fishenc$encounters /  nrow(trips))*100) %>% 
  arrange(prop_enc) %>% as.data.frame()
tail(fishenc, 10)

# Eight species make up 99% of catch and are each encountered 
# in more than 1% of all seines
usepec <- c('atlantic herring', 'atlantic silverside',
            'mummichog', 'green crab', 'atlantic tomcod',
            'alewife', 'winter flounder', 'sandlance',
            'white mullet', 'rock gunnel', 'threespine stickleback',
            'bluefish', 'shorthorn sculpin', 'grubby sculpin',
            'northern pipefish')
sum(comcat$propcatch[comcat$species_name %in% usepec])

# Keep only fish caught in every year
abund$year <- year(abund$date)
yenc <- abund %>% 
  dplyr::select(year, species_name) %>% 
  unique() %>% 
  group_by(species_name) %>% 
  summarise(nyear = n()) %>% 
  arrange(nyear) %>% as.data.frame()

# Get rid of single catches, single encounters, less than half of yrs
abund <- abund %>% 
  filter(species_name %in% yenc$species_name[yenc$nyear >=8])

# Find total catch per species per site per month
abund <- abund%>% 
  group_by(site_name, species_name, datper) %>% 
  summarise(catch = sum(catch)) %>% 
  arrange(site_name, datper, species_name) %>% as.data.frame()

abund <- unique(abund)

# Calcualte CPUE
abund <- left_join(abund, ntrips, by=c('site_name', 'datper'))
abund$cpue <- abund$catch / abund$ntrips
rm(comcat, fishenc, ntrips, usepec)

# Make catch dataframe with all species
tab <- abund %>% 
  mutate(sitedate = paste0(site_name, '_', datper)) %>% 
  dplyr::select(sitedate, species_name, cpue) %>% 
  pivot_wider(names_from = "species_name",
              values_from = "cpue") %>% 
  as.data.frame()
# Sub 0 for NA when no fish caught
tab[is.na(tab)] <- 0

# Make site identifier row name
rownames(tab) <- tab$sitedate
tab$sitedate <- NULL

# Use nonmetric dimensional scaling to reduce from 41 to 2 dimensions
vare.mds <- metaMDS(tab,
                    distance = 'bray',
                    k = 2,
                    autotransform = T,
                    try = 500, trymax = 500)

# View stress
vare.mds$stress
stressplot(vare.mds)

# Extract scores
data.scores <- as.data.frame(scores(vare.mds)$sites)

# Add site names back in as their own column
data.scores$site <- rownames(data.scores)
rownames(data.scores) <- NULL
data.scores <- data.scores %>%
  separate(site, "_", into=c('site_name', #'month',
                             'datper'))

# Decide how many clusters to look at
n_clusters <- 20

# Initialize total within sum of squares error: wss
wss <- numeric(n_clusters)

# Look over 1 to n possible clusters
for (i in 1:n_clusters) {
  # Fit the model: km.out
  km.out <- kmeans(data.scores[,1:2], centers = i, nstart = 20)
  # Save the within cluster sum of squares
  wss[i] <- km.out$tot.withinss
}

# Produce a scree plot
wss_df <- tibble(clusters = 1:n_clusters, wss = wss)

ggplot(wss_df, aes(x = clusters, y = wss, group = 1)) +
  geom_point(size = 4)+
  geom_line() +
  scale_x_continuous(breaks = c(2, 4, 6, 8, 10)) +
  xlab('Number of clusters') +
  geom_hline(
    yintercept = wss, 
    linetype = 'dashed'
  )

# Silhouette method
factoextra::fviz_nbclust(tab, kmeans, 'silhouette')
factoextra::fviz_nbclust(tab, kmeans, 'wss')
rm(wss_df, n_clusters, i, wss)

# Assign k clustrers
km.out <- kmeans(data.scores[,1:2], centers = 3, nstart = 20)

# Append
data.scores$cluster <- km.out$cluster
rm(km.out)

data.scores$cluster[data.scores$cluster==1] <- 'mixed'
data.scores$cluster[data.scores$cluster==2] <- 'silverside'
data.scores$cluster[data.scores$cluster==3] <- 'herring'

data.scores$cluster <- factor(data.scores$cluster,
                              levels=c('herring',
                                       'silverside',
                                       'mixed'))

# Extract species similarities
spec.scores <- as.data.frame(scores(vare.mds)$species)
spec.scores$species <- rownames(spec.scores)

# Test relationship to temperature
temp.s <- ordisurf(vare.mds ~ temp, data=schar, method='REML',
                   select=TRUE)
zg <- temp.s$grid
lx <- length(zg$x)
for (i in 1:lx) {    
      result.i <- cbind(x=rep(zg$x[i], lx), y=zg$y, z=zg$z[i, ]) 
      if (i == 1) {
        result <- result.i
      }else{
        result <- rbind(result, result.i)
      }
}
temp.grad <- as.data.frame(result)
rm(zg, lx, i, result.i, result)

# Test relationship to salinity
sal.s <- ordisurf(vare.mds ~ sal, data=schar, method='REML',
                  select=TRUE)
zg <- sal.s$grid
lx <- length(zg$x)
for (i in 1:lx) {    
  result.i <- cbind(x=rep(zg$x[i], lx), y=zg$y, z=zg$z[i, ]) 
  if (i == 1) {
    result <- result.i
  }else{
    result <- rbind(result, result.i)
  }
}
sal.grad <- as.data.frame(result)
rm(zg, lx, i, result.i, result)

# Plot site clusters
ggplot() + 
  geom_tile(data=temp.grad,
            aes(x=x, y=y, fill=z),
            alpha=0.5, col=NA) +
  scale_fill_viridis_c(na.value = 'transparent',
                       'Temp (C)') +
  geom_point(data=data.scores,
             aes(x=NMDS1,y=NMDS2,
                 color=(cluster)),
             alpha=0.8, stroke=1) +
  ggnewscale::new_scale_fill() +
  geom_segment(data = spec.scores,
               aes(x = 0, y = 0, 
                   xend = NMDS1, yend = NMDS2), 
               size =1, alpha = 0.5, colour = "grey30") +
  geom_text(data = spec.scores, 
            aes(x = NMDS1, y = NMDS2, label=species), 
            colour = "gray30", alpha=0.8) +
  labs(col = 'Cluster', x='NMDS2', y='NMDS1') +
  coord_equal(xlim=c(-2, 2)) +
  theme(legend.margin = margin(0,0,0,0))

ggplot() + 
  geom_tile(data=sal.grad,
            aes(x=x, y=y, fill=z),
            alpha=0.5, col=NA) +
  scale_fill_viridis_c(na.value = 'transparent',
                       'Salinity (ppt)') +
  geom_point(data=data.scores,
             aes(x=NMDS1,y=NMDS2,
                 color=as.factor(cluster)),
             alpha=0.8, stroke=1) +
  ggnewscale::new_scale_fill() +
  geom_segment(data = spec.scores,
               aes(x = 0, y = 0, 
                   xend = NMDS1, yend = NMDS2), 
               size =1, alpha = 0.5, colour = "grey30") +
  geom_text(data = spec.scores, 
            aes(x = NMDS1, y = NMDS2, label=species), 
            colour = "gray30", alpha=0.8) +
  labs(col = 'Cluster', x='NMDS2', y='NMDS1') +
  coord_equal(xlim=c(-2, 2)) +
  theme(legend.margin = margin(0,0,0,0))

# ggsave(plot=clus.char,
#        here('Cluster_Characteristics.png'),
#        dpi=300,
#        units='in', width = 4, height = 4.5)

sites <- sites %>%
  dplyr::select(-bay_location,
                -site_number)

sites <- left_join(sites, 
                   dplyr::select(data.scores,
                                 site_name, datper, cluster),
                   by=c('site_name'))  

sites$datper <- factor(sites$datper, 
                      levels=c('LateSpring', 'EarlySummer',
                               'MidSummer', 'LateSummer'))

sites <- sites %>% filter(!is.na(cluster))

sim <- ggplot() + 
  geom_sf(data=shoreline) +
  geom_sf(data = sites, 
          aes(col = as.factor(cluster))) +
  coord_sf(xlim=c(-70.3, -70.15),
           ylim=c(43.55, 43.75)) +
  labs(col='Cluster') +
  facet_wrap(vars(datper)) +
  theme(legend.position = 'right',
        axis.text.x = element_blank(),
        axis.text.y = element_blank(),
        legend.margin = margin(0,0,0,0))
sim

# Always silverside-dominated: Presumpscot Moorings, Skitterygusset
# Always herring-dominated: none (GDI until late summer)
# Always mixed: Alewife Cove

# ggsave(plot = sim,
#        here('Monthly_CS_Similarity.png'),
#        dpi=300, width = 9, height = 5, units='in')

data.scores$sitedate <- paste0(data.scores$site_name, '_',
                               data.scores$datper)
abund$sitedate <- paste0(abund$site_name, '_', 
                         abund$datper)

abund <- left_join(abund, dplyr::select(data.scores,
                                        sitedate, cluster),
                   by=c('sitedate'))

char <- abund %>% 
  dplyr::select(-sitedate) %>% 
  group_by(cluster, species_name) %>% 
  summarise(cpue = mean(cpue))

char$species_name[char$species_name == 'atlantic herring'] <- 
  'herring'
char$species_name[char$species_name == 'atlantic silverside'] <- 
  'silverside'
char$species_name[char$species_name == 'atlantic tomcod'] <- 
  'tomcod'
char$species_name[char$species_name == 'winter flounder'] <- 
  'flounder'
char$species_name[char$species_name == 'mummichog'] <- 'mummi.'
char$species_name[char$species_name == 'northern pipefish'] <- 
  'pipefish'

char$species_name <- factor(char$species_name,
                            levels=c('bluefish', 'tomcod', 
                                     'pipefish',
                                     'flounder', 'alewife',
                                     'green crab', 'mummi.',
                                     'sandlance', 'herring', 
                                     'silverside'))

ggplot() +
  geom_tile(data=char,
              aes(x=species_name, y=cluster, 
                  fill=log(cpue)),
            width = 0.95, height = 0.98) +
  scale_fill_viridis_c(na.value = 'transparent') +
  labs(fill = 'CPUE', x='Species', y='') +
  theme(axis.text.x = element_text(angle = 90),
    legend.position = 'right')

## SIMPER
simper <- with(data.scores, simper(tab, cluster))

comparisons <- c("mixed_herring", 
                 "mixed_silverside", 
                 "herring_silverside")

simper.results <- c()

for(i in 1:length(comparisons)) {
  require(tidyverse)
  temp <- summary(simper)[as.character(comparisons[i])] %>%
    as.data.frame()
  colnames(temp) <- gsub(
    paste(comparisons[i],".", sep = ""), "", colnames(temp))
  temp <- temp %>%
    mutate(Comparison = comparisons[i],
           Position = row_number()) %>%
    rownames_to_column(var = "Species")
  simper.results <- rbind(simper.results, temp)
}

simper.results %>% filter(p < 0.05)
# Herring, silverside, mummichog, tomcod
