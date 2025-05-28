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
                panel.border = element_rect(color='black', linewidth=1, fill=NA),
                legend.position = "bottom",
                axis.text.x=element_text(size=12),
                axis.text.y=element_text(size=12),
                axis.title.x=element_text(size=12),
                axis.title.y=element_text(size=12, angle=90, vjust=2),
                plot.title=element_text(size=14, hjust = 0, vjust = 1.2),
                plot.caption=element_text(hjust=0, face='italic', size=12)))

# Geospatial data (seine sites and shoreline polygon)
sites <- read.csv(here('Clean_Data/Seine/sites_cleaned.csv'))
sites <- st_as_sf(sites, coords = c('longitude', 'latitude'),
                  crs= "EPSG:4326")
# Remove QBC
sites <- sites[sites$site_number < 20,]

shoreline <- st_read(here('GIS/us_medium_shoreline_poly.shp'),
                     quiet=T)
shoreline <- st_transform(shoreline, st_crs(sites))

#Load abundance and trip data
abund <- read.csv(here('Clean_Data/Seine/abund_through_2024.csv'))
trips <- read.csv(here('Clean_Data/Seine/trips_through_2024.csv'))

# Clean
trips <- trips %>% 
  # Remove QBC
  filter(site_id < 20) %>% 
  # Format date
  mutate(date = as.Date(date, format = '%m/%d/%Y')) %>% 
  mutate(month = month(date)) 

# Remove trips where the seine failed
trips <- trips %>% 
  filter(notes %notin% c(
    'bad set',
    'low tide, set not ideal',
    'bad set, net did not get a chance to open up, mummichog outside of net',
    'fish fleeing net, low tide and seine set on ledge',
    'net snagged, probably released most of catch',
    'no fish, bag tangled in low water',
    'no fish; inverted set',
    'tide moving too fast, seine set was flipped',
    'too much algae for effective seining',
    'very low tide, seine taken in about 1 ft of water',
    "had to wait after plankton tow for tide to rise enough to reach the beach, net came in folded - lots of fish escaping, alewife in ziploc #1" 
  ))

# Decide which months are representative
table(trips$month, trips$year)

# Remove May and Oct
trips <- trips %>% 
  filter(month %in% seq(6, 9, 1)) 

# Decide which years are representative
table(trips$site_id, trips$year)

# Remove 2019
trips <- trips %>% 
  filter(trips$year != 2019)

# Check site coverage in each month
sitecov <- trips %>% 
  group_by(year, month, site_name) %>% 
  summarise(ntrips = n())
ggplot(data=sitecov) +
  geom_tile(aes(x=year, y=site_name, fill=ntrips),
            width = 0.9) +
  binned_scale(aesthetics = "fill",
               scale_name = "stepsn", 
               palette = function(x) c(viridis::viridis(n=4)),
               breaks = c(1, 2, 3, 4),
               limits = c(0, 4),
               #show.limits = TRUE, 
               guide = "colorsteps"
  ) +
  facet_wrap(vars(month)) +
  scale_x_continuous(limits = c(2014, 2024),
                     breaks=seq(2014, 2024, 2))+
  theme(legend.key.width = unit(0.5, 'in')) +
  ggtitle('Assessment of sampling effort')
rm(sitecov)

# Right. So, should probably remove September.
trips <- trips %>% 
  filter(month != 9)

trips <- droplevels(trips)

# Save outcome-- number trips conducted at each site in each month
ntrips <- trips %>% 
  group_by(site_name, month) %>% 
  summarise(ntrips = n()) %>% 
  as.data.frame()
# 11 to 21 samples to characterize monthly spatiotemp chars.

# Save environmental characteristics
schar <- trips %>% 
  group_by(site_name, month) %>% 
  summarise(temp = mean(temp_degc, na.rm=T),
            sal = mean(salinity_ppt, na.rm=T))

# Clean
abund <- abund %>% 
  # Remove QBC
  filter(site_id < 20) %>% 
  # Format date, keep only 'good' trips
  mutate(date = as.Date(date, format='%m/%d/%Y')) %>% 
  filter(loc_id %in% trips$loc_id) %>% 
  mutate(month = month(date)) 

# Remove unidentified species
abund <- abund %>% 
  filter(species_name %notin% c('hake spp',
                                'killifish spp',
                                'stickleback spp',
                                'shiner spp'
                                ))

# Group sculpins (too many unID'd)
abund$species_name[abund$species_name %in% c('grubby sculpin',
                                             'shorthorn sculpin',
                                             'slimy sculpin',
                                             'longhorn sculpin',
                                             'sculpin spp')] <- 'sculpin spp'

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

# Nine species make up nearly 99% of catch and are each encountered 
# in more than 10% of all seines. Aggregate other minor species.
usepec <- c('atlantic herring', 'atlantic silverside',
            'mummichog', 'green crab', 'atlantic tomcod',
            'alewife', 'winter flounder', 'sandlance',
            'sculpin spp')

abund$species_name[abund$species_name %notin% usepec] <- 'Other'

# Find total catch per species per site per month
abund <- abund%>% 
  group_by(site_name, species_name, month) %>% 
  summarise(catch = sum(catch)) %>% 
  arrange(site_name, month, species_name) %>% as.data.frame()

abund <- unique(abund)

# Calcualte CPUE
abund <- left_join(abund, ntrips, by=c('site_name', 'month'))
abund$cpue <- abund$catch / abund$ntrips
rm(comcat, fishenc, ntrips, usepec)

# Make catch dataframe with all species
tab <- abund %>% 
  mutate(sitedate = paste0(site_name, '_', month)) %>% 
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
                    autotransform = T)

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
                             'month'))

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
rm(wss_df, n_clusters, i, wss)

# Assign k clustrers
km.out <- kmeans(data.scores[,1:2], centers = 3, nstart = 20)

# Append
data.scores$cluster <- km.out$cluster
rm(km.out)

data.scores$cluster[data.scores$cluster==3] <- 'herring-dominated'
data.scores$cluster[data.scores$cluster==2] <- 'silverside-dominated'
data.scores$cluster[data.scores$cluster==1] <- 'herring and sandlance'

data.scores$cluster <- factor(data.scores$cluster,
                              levels=c('herring-dominated',
                                       'herring and sandlance',
                                       'silverside-dominated'))

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
                 color=cluster),
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
                 color=cluster),
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
                                 site_name, month, cluster),
                   by=c('site_name'))  

sites$month[sites$month == 6] <- 'Jun'
sites$month[sites$month == 7] <- 'Jul'
sites$month[sites$month == 8] <- 'Aug'

sites$month <- factor(sites$month, 
                      levels=c('Jun', 'Jul', 'Aug'))

sites <- sites %>% filter(!is.na(cluster))

sim <- ggplot() + 
  geom_sf(data=shoreline) +
  geom_sf(data = sites, 
          aes(col = (cluster))) +
  coord_sf(xlim=c(-70.3, -70.15),
           ylim=c(43.55, 43.75)) +
  labs(col='Cluster') +
  facet_wrap(vars(month)) +
  theme(legend.position = 'right',
        axis.text.x = element_blank(),
        axis.text.y = element_blank(),
        legend.margin = margin(0,0,0,0))
sim

# Always silverside-dominated: Presumpscot Moorings
# Always herring-dominated: none (Cushing Jun-Jul)
# Always mixed: Alewife Cove, SMCC, Mack North, GDI

# ggsave(plot = sim,
#        here('Monthly_CS_Similarity.png'),
#        dpi=300, width = 9, height = 5, units='in')

data.scores$sitedate <- paste0(data.scores$site_name, '_',
                               data.scores$month)
abund$sitedate <- paste0(abund$site_name, '_', 
                         abund$month)

abund <- left_join(abund, dplyr::select(data.scores,
                                        sitedate, cluster),
                   by=c('sitedate'))

char <- abund %>% 
  dplyr::select(-sitedate) %>% 
  group_by(cluster, species_name) %>% 
  summarise(cpue = mean(cpue))

char$species_name[char$species_name == 'atlantic herring'] <- 'herring'
char$species_name[char$species_name == 'atlantic silverside'] <- 'silverside'
char$species_name[char$species_name == 'atlantic tomcod'] <- 'tomcod'
char$species_name[char$species_name == 'sculpin spp'] <- 'sculpin'
char$species_name[char$species_name == 'winter flounder'] <- 'flounder'
char$species_name[char$species_name == 'mummichog'] <- 'mummi.'

char$species_name <- factor(char$species_name,
                            levels=c('tomcod',
                                     'Other',
                                     'sculpin',
                                     'flounder',
                                     'mummi.',
                                     'green crab',
                                     'silverside',
                                     'alewife',
                                     'sandlance',
                                     'herring'))

ggplot() +
  geom_tile(data=char,
              aes(x=species_name, y=cluster, 
                  fill=log(cpue)),
            width = 0.95, height = 0.98) +
  scale_fill_viridis_c(na.value = 'transparent') +
  labs(fill = 'CPUE', x='Species', y='') +
  theme(
    legend.position = 'right')

## SIMPER
simper <- with(data.scores, simper(tab, cluster))

simper$`herring and sandlance_herring-dominated`$p[
  simper$`herring and sandlance_herring-dominated`$p <= 
    0.05] # herring, flounder, tomcod

simper$`herring and sandlance_silverside-dominated`$p[
  simper$`herring and sandlance_silverside-dominated`$p <= 
    0.05] # silverside, mummichog

simper$`herring-dominated_silverside-dominated`$p[
  simper$`herring-dominated_silverside-dominated`$p <= 
    0.05] # silverside, mummichog

# Herring, silverside, mummichog, flounder, tomcod
