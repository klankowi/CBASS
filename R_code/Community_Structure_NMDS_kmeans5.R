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

#Load abundance and trip data
abund <- read.csv(here('Clean_Data/Seine/abund_through_2024.csv'))
trips <- read.csv(here('Clean_Data/Seine/trips_through_2024.csv'))

# Clean
trips <- trips %>% 
  # Remove QBC
  filter(site_id < 20) %>% 
  # Format date, add time IDs
  mutate(date = as.Date(date, format = '%m/%d/%Y')) %>% 
  mutate(month = month(date)) %>% 
  mutate(week = isoweek(date)) %>% 
  mutate(day = day(date)) %>% 
  mutate(doy = yday(date)) %>% 
  # Remove 2019 and shoulder weeks
  filter(year != 2019) %>% 
  filter(week >=24) %>% 
  filter(week<=39)

# Remove instances where they measured by "scoops"
#trips <- trips[trips$loc_id != '2016_036_04',]
#trips <- trips[trips$loc_id != '2014_005_09',]

# Get rid of "bad" seines tho-- noted bad sets
trips <- trips %>% 
  filter(notes %notin% c(
    'very low tide, seine taken in about 1 ft of water',
    'site assumed, not indicated on sheet',
    'bad set',
    'tide moving too fast, seine set was flipped',
    'net snagged, probably released most of catch',
    'tide moving too fast, no fish, had to walk in seine net',
    'bad set, net did not get a chance to open up, mummichog outside of net',
    'no fish, bag tangled in low water'
  ))

# Split into approx monthly increments
trips$datper <- NA
trips$datper[trips$week >=24 & trips$week <=28] <- 'EarlySummer'
trips$datper[trips$week >=29 & trips$week <=33] <- 'MidSummer'
trips$datper[trips$week >=34 & trips$week <=39] <- 'LateSummer'

trips$datper <- factor(trips$datper,
                       levels = c('EarlySummer',
                                  'MidSummer', 'LateSummer'))

# Check site coverage in each month
sitecov <- trips %>% 
  group_by(datper, site_name, year) %>% 
  summarise(ntrips = n())
ggplot(data=sitecov) +
  geom_tile(aes(x=datper, y=site_name, fill=ntrips),
            width = 0.9) +
  binned_scale(aesthetics = "fill",
               scale_name = "stepsn", 
               palette = function(x) c(viridis::viridis(n=7)),
               breaks = c(1, 2, 3, 4),
               limits = c(0, 5),
               guide = "colorsteps"
  ) +
  facet_wrap(vars(year)) +
  theme(legend.key.width = unit(0.5, 'in')) +
  ggtitle('Assessment of sampling effort')
rm(sitecov)

trips <- droplevels(trips)

# Save outcome-- number trips conducted at each site in each month
ntrips <- trips %>% 
  group_by(site_name, datper) %>% 
  summarise(ntrips = n()) %>% 
  as.data.frame()
summary(ntrips)
# 10 to 25 samples to characterize seasonal spatiotemp chars.

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

# Get rid of other grouped (unidentified) species
abund <- abund %>% 
  filter(species_name %notin% c('hake spp', 
                                'killifish spp',
                                'river herring', 
                                'shiner spp', 
                                'stickleback spp',
                                'sturgeon spp', 
                                'sculpin spp',
                                'slimy sculpin'
                                ))

# Add year back
abund <- abund %>% 
  mutate(year = year(date))

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

# Find fish encountered every year
fishyr <- abund %>% 
  dplyr::select(species_name, year) %>% 
  unique() %>% 
  group_by(species_name) %>% 
  summarise(nyear = n()) %>% 
  arrange(nyear) %>% as.data.frame()
tail(fishyr, 15)

# 10 species make up >99% of catch
# And are each caught in more than 1% of all encounters (7 ore more hauls)
# And are seen in 8/10 sampled years
usepec <- unique(
  abund$species_name[#abund$species_name %in% 
                     #   comcat$species_name[comcat$propcatch >= 0.05] &
                     abund$species_name %in% 
                       fishenc$species_name[fishenc$prop_enc >= 1] &
                     abund$species_name %in% 
                        fishyr$species_name[fishyr$nyear >=8]
                     ])
sum(comcat$propcatch[comcat$species_name %in% usepec])
usepec

# Others are excluded
abund <- abund[abund$species_name %in%
                 usepec,]

# Find total catch per species per site per period
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
                    autotransform = F,
                    try = 999, trymax = 999)

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
factoextra::fviz_nbclust(tab, kmeans, 'silhouette',
                         k.max = 20)
factoextra::fviz_nbclust(tab, kmeans, 'wss',
                         k.max = 20)
rm(wss_df, n_clusters, i, wss)

# Assign k clustrers
km.out <- kmeans(data.scores[,1:2], centers = 3, nstart = 20)

# Append
data.scores$cluster <- km.out$cluster
rm(km.out)

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

data.scores$cluster[data.scores$cluster==1] <- 'silverside'
data.scores$cluster[data.scores$cluster==2] <- 'herring'
data.scores$cluster[data.scores$cluster==3] <- 'sandlance'

data.scores$cluster <- factor(data.scores$cluster,
                              levels=c('herring',
                                       'sandlance',
                                       'silverside'))

sites <- sites %>%
  dplyr::select(-bay_location,
                -site_number)

sites <- left_join(sites, 
                   dplyr::select(data.scores,
                                 site_name, datper, cluster),
                   by=c('site_name'))  

sites$datper <- factor(sites$datper, 
                       levels=c('EarlySummer',
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

# Always silverside: Presumpscot, Skitterygusset
# Always herring: Cushing Island
# Always sandlance: Alewife Cove



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

test <- tab %>% 
  as.data.frame()

test$sitedate <- rownames(test)
rownames(test) <- NULL

test <- test %>% 
  pivot_longer(cols=1:10, names_to='species_name',
               values_to = 'cpue')

test <- left_join(test, dplyr::select(data.scores, sitedate, cluster))

char <- test %>% 
  dplyr::select(-sitedate) %>% 
  group_by(cluster, species_name) %>% 
  summarise(cpue = mean(cpue)) %>% 
  as.data.frame()

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
char$species_name[char$species_name == 'green crab'] <- 
  'g.crab'

char$species_name <- factor(char$species_name,
                            levels=c('bluefish','pipefish',
                                     'tomcod', 'flounder',
                                     'g.crab', 'alewife',
                                     'herring', 'silverside',
                                     'mummi.', 'sandlance'))

char$cluster <- factor(char$cluster,
                       levels = c('herring', 
                                  'sandlance',
                                  'silverside'))

ggplot() +
  geom_tile(data=char,
            aes(x=species_name, y=cluster, 
                fill=log(cpue+1)),
            width = 0.95, height = 0.98) +
  scale_fill_viridis_c(na.value = 'transparent') +
  labs(fill = 'log(CPUE+1)', x='Species', y='') +
  theme(axis.text.x = element_text(angle = 90),
        legend.position = 'right')

## ANOSIM
ano = anosim(tab, data.scores$cluster, 
             distance = "bray", permutations = 9999)
ano
summary(ano)

## SIMPER
simper <- with(data.scores, simper(tab, cluster))

comparisons <- names(simper)

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
table(simper.results$Species[simper.results$p < 0.05])
# Herring, silverside, mummichog, tomcod, bluefish, sandlance

# Most differences: herring-silverside (silver, herring, mumm, bluefish)
# Least diff: sandlance-silverside (silver, mummi)
# Mid diff: sandlance-herring (herring, sandlance, tomcod)
