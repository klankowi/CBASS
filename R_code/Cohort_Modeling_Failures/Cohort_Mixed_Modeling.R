# Identify cohorts by length
rm(list=ls())

# Load packages
library(data.table)
library(tidyverse)
library(readxl)
library(here)
library(mixtools)
library(diptest)

# Duplicatability 
set.seed(109861235)

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
                axis.title.x=element_text(size=14),
                axis.title.y=element_text(size=14, angle=90, vjust=2),
                plot.title=element_text(size=14, hjust = 0, vjust = 1.2),
                plot.caption=element_text(hjust=0, face='italic', size=12),
                strip.text.x=element_text(size=12)))

#### Load data ####
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
                         species_name, length_mm, date,
                         site_name)

# Remove one large mummichog
lengths <- lengths[lengths$length_mm != 385 & 
                     lengths$site_name != 'mummichog',]

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
  mutate_at(c('species_name', 'site_name'), as.factor) %>% 
  mutate_at(c('length_mm', 'wk', 'year'), as.numeric)

# # Remove QBC
# lengths <- lengths[lengths$site_name %notin% c(
#   'Cedar Beach', 'Garrison Cove', 'Long Point Cove',
#   'Lowell Cove', 'Orrs Cove', 'Snow Island'
# ),]

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

# Test on 2022
temp <- temp[temp$year == 2020,]
#temp <- temp[temp$period== 'hot',]

# Remove weeks with fewer than 5 fish caught
temp.wk <- split(temp, f=temp$wk)
temp.wk <- temp.wk[sapply(temp.wk, nrow) > 5]
temp <- do.call(rbind, temp.wk)

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
  geom_jitter(data = temp,
              aes(x = wk, y = length_mm),
              alpha=0.35, width=0.2, stroke=NA, cex=2) +
  theme(legend.text = element_text(angle = 45, vjust = 0.4)) +
  geom_density(data = temp,
               aes(x = after_stat(scaled) * 0.9,
                   y = length_mm, group = wk),
               n = 2 ^ 8,
               position = position_nudge(x = times),
               trim = T, adjust = 1.5) +
  geom_smooth(data=temp,
              aes(x=wk, y=length_mm)) +
  xlab('Week of year') + ylab('Length (mm)') +
  ggtitle(paste0(str_to_title('atlantic silverside'),
                 ' raw length frequency ',
                 paste0(temp$year[1]))) +
  ylim(c(5, 215)) +
  xlim(c(23, 40))

## Cluster fish that are similar in length every week of every year
library(LaplacesDemon)
library(cutoff)
library(mixtools)

Cohort.Tracking <- data.frame(
  Group=NA,
  lambda=NA,
  wk=NA,
  mu=NA,
  ll=NA,
  ul=NA
)

for(i in 1:length(unique(temp$wk))){
  tempwk <- temp[temp$wk == paste0(unique(temp$wk)[i]),]
  
  nmodes <- length(Modes(tempwk$length_mm)$modes)
  
  if(nmodes > 1){
    out <- normalmixEM(tempwk$length_mm, k=nmodes)
    
    out <- cbind(out$mu, out$sigma, out$lambda)
    out <- as.data.frame(out)
    colnames(out) <- c('mu', 'sigma', 'lambda')
    out$Group <- as.factor(as.numeric(rownames(out)))
    
    out$ul <- out$mu + (2 * out$sigma)
    out$ll <- out$mu - (2 * out$sigma)
    out$wk <- tempwk$wk[1]
    
    # Plot
    print(
      ggplot() +
        geom_density(data=tempwk, aes(x=length_mm, group=as.factor(wk)))+
        geom_vline(data=out,
                   aes(xintercept=mu, col=Group), lwd=1) +
        geom_vline(data=out,
                   aes(xintercept=ll, col=Group), lty=2) +
        geom_vline(data=out,
                   aes(xintercept=ul, col=Group), lty=2) +
        xlab('Length (mm)') + ylab('Density') +
        ggtitle(paste0('Week ', tempwk$wk[i]))+
        xlim(c(5, 215))
    )
    
    Cohort.Tracking <- rbind(Cohort.Tracking,
                             dplyr::select(out, -sigma))
    
  }
  
  if(nmodes == 1){
    # Find modes assuming normal distribution of lengths around means
    # Regroup data
    out <- data.frame(
      Group = 1,
      lambda = 1,
      mu = Modes(tempwk$length_mm)$modes,
      sigma = sd(tempwk$length_mm)
    )
    
    # Calculate upper and lower limits
    out$ul <- out$mu + (2 * out$sigma)
    out$ll <- out$mu - (2 * out$sigma)
    out$Group <- as.factor(out$Group)
    out$wk <- tempwk$wk[1]
    
    # Plot
    print(
      ggplot() +
        geom_density(data=tempwk, aes(x=length_mm, group=as.factor(wk)))+
        geom_vline(data=out,
                   aes(xintercept=mu, col=Group), lwd=1) +
        geom_vline(data=out,
                   aes(xintercept=ll, col=Group), lty=2) +
        geom_vline(data=out,
                   aes(xintercept=ul, col=Group), lty=2) +
        xlab('Length (mm)') + ylab('Density') +
        ggtitle(paste0('Week ', tempwk$wk[i]))+
        xlim(c(5, 215))
    )
    
    Cohort.Tracking <- rbind(Cohort.Tracking,
                             dplyr::select(out, -sigma))
    
  }
}

Cohort.Tracking <- Cohort.Tracking[!is.na(Cohort.Tracking$Group),]

ggplot(data=Cohort.Tracking[Cohort.Tracking$lambda >= 0.05,]) +
  geom_point(aes(x=wk, y=mu, col=Group)) +
  geom_errorbar(aes(x=wk, ymin=ll, ymax=ul, col=Group))

#### This is as far as I got ####

Cohort.Week <- split(Cohort.Tracking, f=Cohort.Tracking$wk)

for(i in 1:length(Cohort.Week)){
  if(nrow(Cohort.Week[[i]]) > 1){
    Cohort.Week[[i]]$Co.Dist <- 
      max(Cohort.Week[[i]]$mu) - 
      min(Cohort.Week[[i]]$mu)
  }
  if(nrow(Cohort.Week[[i]]) == 1){
    Cohort.Week[[i]]$Co.Dist <- 0
  }
}
Cohort.Tracking <- do.call(rbind, Cohort.Week)
Cohort.Tracking$Recruitment[
  Cohort.Tracking$Co.Dist == max(Cohort.Tracking$Co.Dist[Cohort.Tracking$wk > 27])
] <- 'Recruited'
week.rec <- Cohort.Tracking$wk[!is.na(Cohort.Tracking$Recruitment) &
                                 Cohort.Tracking$Recruitment == 'Recruited'][1]
Cohort.Tracking$Recruitment[Cohort.Tracking$wk >= week.rec] <- 'Recruited'
Cohort.Tracking <- Cohort.Tracking[!is.na(Cohort.Tracking$Recruitment),]
Cohort.Tracking$Recruitment <- NULL
Cohort.Tracking$Co.Dist <- NULL

Cohort.Week <- split(Cohort.Tracking, f=Cohort.Tracking$wk)

Cohort.Week[[1]] <- Cohort.Week[[1]][
  Cohort.Week[[1]]$mu == min(Cohort.Week[[1]]$mu),
]

for(i in 2:length(Cohort.Week)){
  thisweek <- Cohort.Week[[i]]
  lastweek <- Cohort.Week[[(i-1)]]
  
  if(!is.null(nrow(lastweek))){
    thisweek <- thisweek[thisweek$mu >
                           lastweek$mu,]
    
    if(nrow(thisweek) == 1){
      thisweek$Group <- 1
      Cohort.Week[[i]] <- thisweek
    }
    
    if(nrow(thisweek) > 1){
      thisweek$dif <- thisweek$mu - lastweek$mu
      thisweek <- thisweek[thisweek$dif == min(thisweek$dif),]
      thisweek$dif <- NULL
      Cohort.Week[[i]] <- thisweek
    }
    
    if(nrow(thisweek) == 0){
      Cohort.Week[[i]] <- 'No Growth'
    }
    
  }
  
  if(is.null(nrow(lastweek))){
    Cohort.Week[[i]] <- 'No Growth'
  }
  
}

Cohort.Int <- do.call(rbind, Cohort.Week)
Cohort.Int <- Cohort.Int[!is.na(Cohort.Int$wk) &
                           Cohort.Int$wk != "No Growth",]
Cohort.Int <- Cohort.Int %>% 
  mutate_at(c('wk', 'mu', 'ul', 'll'), as.numeric) %>% 
  mutate_at(c('Group'), as.factor)

