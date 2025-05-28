rm(list=ls())

set.seed(123)

library(here)
library(tidyverse)
library(vegan)

trips <- read.csv(here('Clean_Data/Seine/trips_through_2024.csv'))

# Fix substrate
trips$substrate[trips$substrate %in% c('gravel/shell')] <- 'gravel'
trips$substrate[trips$substrate %in% c('mud/shell')] <- 'mud'
trips$substrate[trips$substrate %in% c('sand/shell')] <- 'sand'

# Order by grain size: (big to small)
  # Rock, rock/gravel, gravel, sand/gravel, sand/mud/gravel,
  # sand, mud/sand, mud/gravel, mud
trips$substrate <- factor(trips$substrate,
                          levels=c('rock', 'rock/gravel', 'gravel',
                                   'sand/gravel', 'sand/mud/gravel',
                                   'sand', 'mud/sand', 'mud/gravel',
                                   'mud'))
# Characerize by site
sitesub <- data.frame(
  site_name = unique(trips$site_name),
  substrate = NA
)
sitesub <- sitesub[with(sitesub, order(site_name)),]
sitesub$substrate <- c('sand/gravel', 'sand', 'mud',
                       'sand', 'sand', 'sand/gravel', #Garrison
                       'sand/gravel', 'sand/gravel', # Long Point
                       'sand', 'mud/sand', 'mud', #Lowell
                       'sand', 'mud', 'mud', 'mud', 
                       'sand/gravel', 'sand/mud/gravel',
                       'rock/gravel', 'gravel')
sitesub$substrate <- factor(sitesub$substrate,
                            levels=c('rock/gravel', 'gravel', 
                                     'sand/gravel', 'sand/mud/gravel',
                                     'sand', 'mud/sand', 'mud'))
sitesub$substrate <- as.numeric(sitesub$substrate)

trips <- trips %>% 
  dplyr::select(-substrate)

trips <- left_join(trips, sitesub, by=c('site_name'))

chars <- read.csv(here('Clean_Data/Seine/site_characteristics.csv'))

trips <- left_join(trips, chars, by=c('site_name'))

locs <- read.csv(here('Clean_Data/Seine/sites_cleaned.csv'))
locs <- dplyr::select(locs, site_name, latitude)

trips <- left_join(trips, locs, by=c('site_name'))

trips <- trips %>% 
  filter(site_id < 20) %>% 
  mutate(date = as.Date(date, format='%m/%d/%Y')) %>% 
  dplyr::select(site_name, 
                temp_degc, #do_mg.l,
                salinity_ppt, 
                date, substrate,
                fetch, aspect, latitude) %>% 
  mutate(month = month(date),
         year = year(date)) %>% 
  filter(month %in% seq(6, 9, 1)) %>% 
  group_by(site_name, month) %>% 
  summarise(temp_degc = mean(temp_degc, na.rm=T),
            #do_mg.l = mean(do_mg.l, na.rm=T),
            sal_ppt = mean(salinity_ppt, na.rm=T),
            substrate = mean(substrate),
            fetch = mean(fetch),
            #aspect = mean(aspect),
            latitude = mean(latitude)) %>% 
  as.data.frame() %>% unique()

trips$sitedate = paste0(trips$site_name, '_', trips$month)#, '_', 
                        #trips$year)

trips$site_name <- NULL; trips$month <- NULL; trips$year <- NULL

trips <- trips[complete.cases(trips),]

tab <- data.frame(
  matrix(nrow = ncol(trips)-1, ncol = nrow(trips))
)

colnames(tab) <- trips$sitedate
rownames(tab) <- colnames(trips)[1:(ncol(trips)-1)]

for(i in 1:nrow(tab)){
  tab[i,] <- trips[,i]
}

vare.mds <- metaMDS(tab)  #using all the defaults

data.scores <- as.data.frame(scores(vare.mds)[[2]])  #Using the scores function from vegan to extract the site scores and convert to a data.frame
data.scores$site <- rownames(data.scores)  # create a column of site names, from the rownames of data.scores
rownames(data.scores) <- NULL

data.scores <- data.scores %>%
  separate(site, "_", into=c('site_name', 'month'))

head(data.scores)  #look at the data

# Decide how many clusters to look at
n_clusters <- 10

# Initialize total within sum of squares error: wss
wss <- numeric(n_clusters)

set.seed(123)

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

# k = 5
km.out <- kmeans(data.scores[,1:2], centers = 5, nstart = 20)

data.scores$cluster <- km.out$cluster

ggplot() + 
  geom_point(data=data.scores,
             aes(x=NMDS1,y=NMDS2,
                 colour=factor(cluster)),
             alpha=0.8) +
  coord_equal()

sites <- read.csv(here('Clean_Data/Seine/sites_cleaned.csv'))
sites <- st_as_sf(sites, coords = c('longitude', 'latitude'),
                  crs= "EPSG:4326")
#sites$site <- sites$site_name
sites <- left_join(sites, data.scores, by =c('site_name'))

shoreline <- st_read(here('GIS/us_medium_shoreline_poly.shp'),
                     quiet=T)
shoreline <- st_transform(shoreline, st_crs(sites))

ggplot() + 
  geom_sf(data=shoreline) +
  geom_sf(data = sites[sites$site_number < 20 & 
                         sites$site_name != 'The Brothers - South',], 
          aes(col = as.factor(cluster))) +
  coord_sf(xlim=c(-70.3, -70.15),
           ylim=c(43.55, 43.75)) +
  labs(col='Cluster') +
  facet_wrap(vars(month)) +
  theme(legend.position = 'right',
        axis.text.x = element_blank(),
        axis.text.y = element_blank())

rm(list=setdiff(ls(), c("sites", "shoreline")))

# Cluster by catch
abund <- read.csv(here('Clean_Data/Seine/abund_through_2024.csv'))
trips <- read.csv(here('Clean_Data/Seine/trips_through_2024.csv'))

abund <- abund %>% 
  filter(site_id < 20) %>% 
  mutate(date = as.Date(date, format='%m/%d/%Y')) %>% 
  mutate(month = month(date)) %>% 
  filter(month %in% seq(6, 9, 1)) %>% 
  filter(species_name %in% c('atlantic silverside', 
                             'atlantic herring', 
                             'mummichog', 'alewife',
                             'green crab', 'sandlance', 
                             'winter flounder')) %>% 
  group_by(site_name, species_name, month) %>% 
  summarise(catch = sum(catch)) %>% 
  arrange(site_name, month, species_name) %>% as.data.frame()

catchdf <- data.frame(
  site_name = rep(c('Alewife Cove', 'Audubon',
                    'Back Cove', 'Cushing Island',
                    'Great Diamond Island', 
                    'Mackworth Island - Beach',
                    'Mackworth Island - North',
                    'Mussel Cove', 'Presumpscot Moorings',
                    'Skitterygusset', 'SMCC', 
                    'The Brothers - North'), 
                  7),
  species_name = c(rep('alewife', 12),
                   rep('atlantic herring', 12),
                   rep('atlantic silverside', 12),
                   rep('green crab', 12),
                   rep('mummichog', 12),
                   rep('sandlance', 12),
                   rep('winter flounder', 12)
)
)

catchdf <- rbind(catchdf %>% mutate(month = 6),
                 catchdf %>% mutate(month = 7),
                 catchdf %>% mutate(month = 8),
                 catchdf %>% mutate(month = 9))

abund <- merge(catchdf, abund, by=c('site_name',
                                        'species_name',
                                    'month'),
                   all=T)
abund$catch[is.na(abund$catch)] <- 0

trips <- trips %>% 
  mutate(date = as.Date(date, format = '%m/%d/%Y')) %>% 
  mutate(month = month(date)) %>% 
  filter(month %in% seq(6, 9, 1)) %>% 
  filter(site_id < 20) %>% 
  group_by(site_name, month) %>% 
  summarise(n = n())

abund <- merge(abund, trips, by=c('site_name', 'month'))
abund$n[is.na(abund$n)] <- 0

abund$cpue <- abund$catch / abund$n
abund <- unique(abund)

abund$sitedate <- paste0(abund$site_name, '_', 
                         abund$month)

abund <- abund %>% dplyr::select(-site_name, -month, 
                                 -catch, -n)

####
tab <- data.frame(
  matrix(nrow = 7, ncol = 48)
)

colnames(tab) <- unique(abund$sitedate)
rownames(tab) <- unique(abund$species_name)

for(i in 1:nrow(tab)){
  fish <- rownames(tab)[i]
  
  tab[i,] <- abund$cpue[abund$species_name == fish]
}

vare.mds <- metaMDS(tab)  #using all the defaults

data.scores <- as.data.frame(scores(vare.mds)[[2]])  #Using the scores function from vegan to extract the site scores and convert to a data.frame
data.scores$site <- rownames(data.scores)  # create a column of site names, from the rownames of data.scores
rownames(data.scores) <- NULL

data.scores <- data.scores %>%
  separate(site, "_", into=c('site_name', #'month',
                             'month'))


head(data.scores)  #look at the data

# Decide how many clusters to look at
n_clusters <- 10

# Initialize total within sum of squares error: wss
wss <- numeric(n_clusters)

set.seed(123)

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
    linetype = 'dashed', 
    #col = c(rep('#000000',4),'#FF0000', rep('#000000', 5))
  )

# k = 5
km.out <- kmeans(data.scores[,1:2], centers = 5, nstart = 20)

data.scores$cluster <- km.out$cluster

data.scores$cluster <- letters[data.scores$cluster]


ggplot() + 
  geom_point(data=data.scores,
             aes(x=NMDS1,y=NMDS2,
                 colour=factor(cluster)),
             alpha=0.8) +
  coord_equal()

sites <- sites %>%
  dplyr::select(-NMDS1, -NMDS2, -bay_location,
                -site_number) %>%
  rename(env.cluster = cluster)

sites <- left_join(sites, 
                   dplyr::select(data.scores,
                                 site_name, month, cluster),
                   by=c('site_name', 'month'))  

sites <- sites[!is.na(sites$env.cluster),]

sites$month[sites$month == 6] <- 'Jun'
sites$month[sites$month == 7] <- 'Jul'
sites$month[sites$month == 8] <- 'Aug'
sites$month[sites$month == 9] <- 'Sep'

sites$month <- factor(sites$month, 
                      levels=c('Jun', 'Jul', 'Aug', 'Sep'))

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

ggsave(plot = sim,
       here('Monthly_CS_Similarity.png'),
       dpi=300, width = 5, height = 9, units='in')

data.scores$sitedate <- paste0(data.scores$site_name, '_',
                               data.scores$month)

abund <- left_join(abund, dplyr::select(data.scores,
                                        sitedate, cluster),
                   by=c('sitedate'))

char <- abund %>% 
  dplyr::select(-sitedate) %>% 
  group_by(cluster, species_name) %>% 
  summarise(cpue = mean(cpue))

char$species_name[char$species_name == 'atlantic herring'] <- 'herring'
char$species_name[char$species_name == 'atlantic silverside'] <- 'silverside'

ggplot() +
  geom_raster(data=char,
              aes(x=species_name, y=cluster, fill=log(cpue))) +
  scale_fill_viridis_c(na.value = 'transparent') +
  labs(fill = 'CPUE', x='Species', y='') +
  theme(
        legend.position = 'right')

a <- ggplot() +
    geom_raster(data=char[char$cluster == 'a',],
                aes(x=species_name, y=cluster, fill=cpue)) +
    scale_fill_viridis_c(na.value = 'transparent') +
      labs(fill = 'CPUE', x='Species', y='') +
    theme(axis.text.y = element_blank(),
          legend.position = 'right') +
    ggtitle(paste0('Cluster a'))

b <- ggplot() +
  geom_raster(data=char[char$cluster == 'b',],
              aes(x=species_name, y=cluster, fill=cpue)) +
  scale_fill_viridis_c(na.value = 'transparent') +
  labs(fill = 'CPUE', x='Species', y='') +
  theme(axis.text.y = element_blank(),
        legend.position = 'right') +
  ggtitle(paste0('Cluster b'))

c <- ggplot() +
  geom_raster(data=char[char$cluster == 'c',],
              aes(x=species_name, y=cluster, fill=cpue)) +
  scale_fill_viridis_c(na.value = 'transparent') +
  labs(fill = 'CPUE', x='Species', y='') +
  theme(axis.text.y = element_blank(),
        legend.position = 'right') +
  ggtitle(paste0('Cluster c'))

d <- ggplot() +
  geom_raster(data=char[char$cluster == 'd',],
              aes(x=species_name, y=cluster, fill=cpue)) +
  scale_fill_viridis_c(na.value = 'transparent') +
  labs(fill = 'CPUE', x='Species', y='') +
  theme(axis.text.y = element_blank(),
        legend.position = 'right') +
  ggtitle(paste0('Cluster d'))

e <- ggplot() +
  geom_raster(data=char[char$cluster == 'e',],
              aes(x=species_name, y=cluster, fill=cpue)) +
  scale_fill_viridis_c(na.value = 'transparent') +
  labs(fill = 'CPUE', x='Species', y='') +
  theme(axis.text.y = element_blank(),
        legend.position = 'right') +
  ggtitle(paste0('Cluster e'))

wholp <- ggpubr::ggarrange(a, b, c, d, e, nrow=5)
ggsave(plot=wholp,
       here('Community_Structure_Key.png'), dpi = 300,
       width = 8, height = 8, units ='in')
