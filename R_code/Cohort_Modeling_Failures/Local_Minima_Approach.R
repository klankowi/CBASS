# Identify cohorts by length
rm(list=ls())

# Load packages
library(data.table)
library(qgam)
library(mgcViz)
library(tidyverse)
library(readxl)
library(here)

# Negate function
'%notin%' <- function(x,y)!('%in%'(x,y))

# Find local maxima
localMaxima <- function(x) {
  # Use -Inf instead if x is numeric (non-integer)
  y <- diff(c(-.Machine$integer.max, x)) > 0L
  rle(y)$lengths
  y <- cumsum(rle(y)$lengths)
  y <- y[seq.int(1L, length(y), 2L)]
  if (x[[1]] == x[[2]]) {
    y <- y[-1]
  }
  y
}

# Find local minima
localMinima <- function(x) {
  # Use -Inf instead if x is numeric (non-integer)
  y <- diff(c(.Machine$integer.max, x)) < 0L
  rle(y)$lengths
  y <- cumsum(rle(y)$lengths)
  y <- y[seq.int(1L, length(y), 2L)]
  if (x[[1]] == x[[2]]) {
    y <- y[-1]
  }
  y
}

# Set GGplot auto theme
theme_set(theme(panel.grid.major = element_line(color='lightgray'),
                panel.grid.minor = element_blank(),
                panel.background = element_blank(),
                panel.border = element_rect(color='black', linewidth=1, fill=NA),
                legend.position = "bottom",
                axis.text.x=element_text(size=12),
                axis.text.y=element_text(size=12),
                axis.title.x=element_text(size=14),
                axis.title.y=element_text(size=14, angle=90, vjust=2),
                plot.title=element_text(size=14, hjust = 0, vjust = 1.2),
                plot.caption=element_text(hjust=0, face='italic', size=12),
                strip.text.x=element_text(size=12)))

# Load data
# Set the name of the workbook
fname <- paste0(here('Clean_Data/seine_compiled_clean.xlsx'))
# Get info about all excel sheet names in workbook
sheets <- readxl::excel_sheets(fname)
# Read in as list item, each item is a sheet
data.all <- invisible(lapply(sheets, 
                             function(x) readxl::read_excel(fname, sheet = x)))
names(data.all) <- sheets
# Coerce list items to dataframes
data.all <- lapply(data.all, as.data.frame)

rm(fname, sheets)

#### Join similar species, lengths ####
data.all$bio <- data.all$bio[
  data.all$bio$species_name %notin% c('periwinkle',
                                      'horseshoe crab'),]

data.all$bio$species_name[
  data.all$bio$species_name %in% c('american eel',
                                   'glass eel elver')
] <- 'american eel'

data.all$bio$species_name[
  data.all$bio$species_name %in% c('hake', 
                                   'red hake',
                                   'spotted hake',
                                   'white hake')
] <- 'hake spp'

data.all$bio$species_name[
  data.all$bio$species_name %in% c('shortnose sturgeon',
                                   'unID sturgeon')
] <- 'sturgeon spp'

data.all$bio$species_name[
  data.all$bio$species_name %in% c('grubby sculpin',
                                   'longhorn sculpin',
                                   'shorthorn sculpin',
                                   'slimy sculpin',
                                   'striped sculpin',
                                   'unID sculpin')
] <- 'sculpin spp'

data.all$bio$species_name[
  data.all$bio$species_name %in% c('atlantic herring',
                                   'herring')
] <- 'atlantic herring'

data.all$bio$species_name[
  data.all$bio$species_name %in% c('banded killifish',
                                   'killifish')
] <- 'killifish spp'

data.all$bio$species_name[
  data.all$bio$species_name %in% c('northern pipefish',
                                   'pipefish')
] <- 'northern pipefish'

data.all$bio$species_name[
  data.all$bio$species_name %in% c('white mullet',
                                   'mullet')
] <- 'white mullet'

data.all$bio$species_name[
  data.all$bio$species_name %in% c('emerald shiner',
                                   'golden shiner',
                                   'unID shiner')
] <- 'shiner spp'

data.all$bio$species_name[
  data.all$bio$species_name %in% c('rock gunnel',
                                   'unID gunnel')
] <- 'rock gunnel'

data.all$bio$species_name[
  data.all$bio$species_name %in% c('threespine stickleback',
                                   'fourspine stickleback',
                                   'ninespine stickleback',
                                   'unID stickelback')
] <- 'stickleback spp'

# Pull lengths
lengths <- dplyr::select(data.all$bio,
                         species_name, length_mm, date)

# Remove one large mummichog
lengths <- lengths[lengths$length_mm != 385,]

# Set week
lengths$wk <- lubridate::isoweek(lengths$date)

# Remove early weeks
lengths <- lengths[lengths$wk >=24,]

# Set year
lengths$year <- lubridate::year(lengths$date)

# Force to numeric
lengths$length_mm <- as.numeric(lengths$length_mm)

# Remove missing values
lengths <- lengths[!is.na(lengths$length_mm),]

# Fix data types
lengths <- lengths %>% 
  mutate_at('species_name', as.factor) %>% 
  mutate_at(c('length_mm', 'wk', 'year'), as.numeric)

# Find biological limits
biolims <- lengths %>% 
  group_by(species_name) %>% 
  summarise(min.length = min(length_mm),
            max.length = max(length_mm))

#### Focus on top 9 ####
lengths <- lengths[lengths$species_name %in% 
                     c('alewife', 'atlantic silverside',
                       'atlantic herring', 'green crab', 
                       'winter flounder', 'mummichog', 
                       'tomcod', 'sandlance',
                       'sculpin spp'),]
biolims <- biolims[biolims$species_name %in% lengths$species_name,]

lengths <- lengths[with(lengths, order(species_name, wk, length_mm)),]

#### Split by period ####
lengths$period[lengths$year %in% c(2014, 2015, 2017, 2018, 2019)] <- 'cold'
lengths$period[lengths$year %in% c(2016, 2020, 2021, 2022, 2023)] <- 'hot'

lengths.1 <- lengths[lengths$period == 'cold',]
lengths.2 <- lengths[lengths$period == 'hot',]


#### Test on silversides ####
temp <- lengths[lengths$species_name == "atlantic silverside",]

# Plot raw
nrecords <- as.data.table(temp)[, .N, by = 'wk'][order(wk)]
times <- NULL

for(j in 1:nrow(nrecords)){
  if(nrecords[j]$N == 1){
    times <- c(times, nrecords[j]$wk)
  }else{
    times <- c(times, rep(nrecords[j]$wk, 2 ^ 8))
  }
}

ggplot() +
  geom_jitter(data = temp, aes(x = wk, y = length_mm, col=year),
              alpha=0.35, width=0.2, stroke=NA, cex=2) +
  scale_color_viridis_c(option = 'viridis', name='Year') +
  theme(legend.text = element_text(angle = 45, vjust = 0.4)) +
  geom_density(data = temp, 
               aes(x = after_stat(scaled) * 0.9,
                   y = length_mm, group = wk),
               n = 2 ^ 8,
               position = position_nudge(x = times),
               trim = T, adjust = 1.5) +
  xlab('Week of year') + ylab('Length (mm)') +
  ggtitle(str_to_title('atlantic silverside'),
                       ' raw length frequency')


#### Loop by week ####
# Filter
temp.list <- split(temp, f=temp$wk)

for(i in 1:length(temp.list)){
  tempwk <- temp.list[[i]]
  
  ## K means clustering to determine if the dist is bimodal
  # Decide how many clusters to look at
  n_clusters <- 5
  
  # Initialize total within sum of squares error: wss
  wss <- numeric(n_clusters)
  
  set.seed(123)
  
  # Look over 1 to n possible clusters
  for (k in 1:n_clusters) {
    # Fit the model: km.out
    km.out <- kmeans(tempwk$length_mm, centers = k, nstart = 20)
    # Save the within cluster sum of squares
    wss[k] <- km.out$tot.withinss
  }
  
  # Produce a scree plot
  wss_df <- tibble(clusters = 1:n_clusters, wss = wss)
  
  tempwk$cluster_id <- factor(km.out$cluster)
  
  
  # Pull density
  tempdens <- density(tempwk$length_mm)
  tempdens <- as.data.frame(cbind(tempdens$x, tempdens$y))
  colnames(tempdens) <- c('length_mm', 'density')
  
  # Remove biologically unlikely values
  tempdens <- tempdens[tempdens$length_mm > 0 &
                         tempdens$length_mm <= biolims$max.length[
                           biolims$species_name == tempwk$species_name[1]
                         ],]
  
  # Find local minima
  localmins  <- tempdens[localMinima(tempdens$density),]$length_mm
  
  locals <- data.frame(
    length_mm = round(localmins),
    cohort=seq(1:length(localmins))
  )
  
  locals <- rbind(data.frame(length_mm=0, cohort=0), locals)
  locals$cohort[locals$length_mm == 0 ] <- 0
  locals <- unique(locals)
  
  locals$cohort <- as.factor(as.numeric(as.factor(locals$cohort)) - 1)
  
  message(temp.list[[i]]$wk[1])
  print(locals)
}


locals <- locals[with(locals, order(length_mm)),]
locals$cohort[locals$localpt == 'Max'] <- 
  seq(1:nrow(locals[locals$localpt == 'Max',]))
for(i in 3:nrow(locals)){
  locals$trail[i] <- locals$cohort[(i-1)]
}

tempwk$cohort <- NA

locals$length_mm <- round(locals$length_mm)
locals <- locals %>% 
  filter(!is.na(trail)) %>% 
  select(-localpt, -cohort) %>% 
  rename(cohort=trail)

locals <- rbind(data.frame(length_mm=0, cohort=0), locals)

for(i in 1:nrow(locals)){
  tempwk$cohort[tempwk$length_mm <= locals$length_mm[(i+1)] &
                tempwk$length_mm > locals$length_mm[i]] <- i
}



## Plot
ggplot() +
  geom_jitter(data = tempwk, aes(x = wk, y = length_mm, col=year),
              alpha=0.35, width=0.2, stroke=NA, cex=2) +
  scale_color_viridis_c(option = 'viridis', name='Year') +
  theme(legend.text = element_text(angle = 45, vjust = 0.4)) +
  new_scale_color() +
  geom_hline(data=locals,
             aes(col=localpt, yintercept=length_mm)) +
  xlab('Week of year') + ylab('Length (mm)') +
  ggtitle(str_to_title(paste0('atlantic silverside'),
                       ' raw length frequency'))
