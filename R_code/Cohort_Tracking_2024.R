# Identify cohorts by length
rm(list=ls())

# Load packages
library(data.table)
library(tidyverse)
#library(readxl)
library(here)
library(mixtools)
#library(diptest)
#library(mgcv)
#library(TropFishR)
library(LaplacesDemon)

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
bio <- read.csv(here('Clean_Data/Seine/lengths_through_2024.csv'))

#### Clean ####
lengths <- bio %>% 
  mutate(date = as.Date(date, format = '%m/%d/%Y')) %>% 
  mutate(month = month(date),
         year = year(date),
         week = isoweek(date)) %>% 
  mutate(length_mm = as.numeric(length_mm)) %>% 
  filter(!is.na(length_mm) & site_id < 20 & year != 2019 & 
           week>=24 & week<=39) %>% 
  dplyr::select(species_name, length_mm, date, week, year)

# Remove fish so tiny that we deffo can't measure them accurately
lengths <- lengths[lengths$length_mm > 5,]

lengths$species_name <- as.factor(lengths$species_name)

biolims <- lengths %>% 
  group_by(species_name) %>% 
  summarise(min.length = min(length_mm),
            max.length = max(length_mm))

#### Focus on top 8 ####
lengths <- lengths[lengths$species_name %in% 
                     c('alewife', 'atlantic silverside',
                       'atlantic herring', 
                       'winter flounder', 'mummichog', 
                       'sandlance'),]
biolims <- biolims[biolims$species_name %in% lengths$species_name,]
biolims <- biolims[with(biolims, order(species_name)),]
rownames(biolims) <- NULL

Linf.all <- data.frame(
  species_name = biolims$species_name,
  L.inf.cm = c(40, 45, 22, 
               15, 23.5, 64.0),
  agemax = c(9, 25, 2,
             4, 12, 14)
)
Linf.all$Linf <- Linf.all$L.inf.cm * 10
Linf.all$L.inf.cm <- NULL

biolims <- merge(biolims, Linf.all, by=c('species_name'))

lengths <- lengths[with(lengths, order(species_name, week, length_mm)),]

### herring and silverside only
lengths <- lengths[lengths$species_name %in% c('atlantic silverside',
                                               'atlantic herring'),]
biolims <- biolims[biolims$species_name %in% c('atlantic silverside',
                                               'atlantic herring'),]
biolims$species_name <- as.character(biolims$species_name)
rownames(biolims) <- NULL

#### Split by period ####
lengths$period[lengths$year %in% c(2014, 2015, 2017, 2018, 2019)] <- 'cold'
lengths$period[lengths$year %in% c(2016, 2020, 2021, 2022, 2023, 2024)] <- 'hot'
lengths$species_name <- droplevels(lengths$species_name)

# #### Model weekly length distribution ####
# # Empty outer dataframe (holds results for both species)
# Out.Cohort <-     
#   data.frame(
#     species_name = NA,
#     year= NA,
#     Group=NA,
#     lambda=NA,
#     week=NA,
#     mu=NA,
#     ll=NA,
#     ul=NA
#   )
# 
# # Loop through species
# for(i in 1:length(biolims$species_name)){
#   # Set species of interest
#   spec.use <- as.character(biolims$species_name[i])
#   # Call their length information
#   len.use <- lengths[lengths$species_name == paste0(spec.use),]
#   
#   # Split length information by year
#   yearlist <- split(len.use, f=len.use$year)
#   
#   # Blank dataframe to hold information for each year
#   Big.Cohort <- 
#     data.frame(
#       species_name = NA,
#       year= NA,
#       Group=NA,
#       lambda=NA,
#       week=NA,
#       mu=NA,
#       ll=NA,
#       ul=NA
#     )
#   
#   # Loop through years
#   for(j in 1:length(yearlist)){
#     # Extract year j in list
#     temp.yr <- yearlist[[j]]
#     
#     # Skip 2023 for herring (only caught in one week)
#     if(spec.use == 'atlantic herring' & j==9){
#       next()
#     }
#     
#     # Split into weeks
#     temp.yr <- split(yearlist[[j]], f=yearlist[[j]]$week)
#     
#     # Remove weeks with fewer than 7 fish caught
#     temp.yr <- temp.yr[sapply(temp.yr, nrow) > 7]
#     
#     # Blank dataframe to hold weekly data for this year
#     Cohort.Tracking <- data.frame(
#       species_name=NA,
#       year=NA,
#       Group=NA,
#       lambda=NA,
#       week=NA,
#       mu=NA,
#       ll=NA,
#       ul=NA
#     )
#     
#     # Loop through weeks
#     for(k in 1:length(temp.yr)){
#       # Extract week k
#       temp.wk <- temp.yr[[k]]
#       
#       # Identify outliers
#       df_outliers <- temp.wk %>% 
#         group_by(week) %>% 
#         rstatix::identify_outliers("length_mm") 
#       
#       # Remove extreme outliers (will fuck dist modeling)
#       temp.wk <- temp.wk[temp.wk$length_mm %notin%
#                      df_outliers$length_mm[df_outliers$is.extreme == TRUE],]
#       
#       # Don't proceed if fewer than 7 fish
#       if(nrow(temp.wk) < 7){
#         next()
#       }
#       
#       # Identify number of modes (centers of cohort length dist)
#       nmodes <- length(Modes(temp.wk$length_mm)$modes)
#       
#       if(nmodes >= 2){
#         out <- normalmixEM(temp.wk$length_mm,
#                            k=nmodes,
#                            arbmean = TRUE,
#                            maxit = 10000000000,
#                            maxrestarts = 10000000)
#         
#         out <- cbind(out$mu, out$sigma, out$lambda)
#         out <- as.data.frame(out)
#         colnames(out) <- c('mu', 'sigma', 'lambda')
#         out$Group <- as.factor(as.numeric(rownames(out)))
#         
#         out$ul <- out$mu + (2 * out$sigma)
#         out$ll <- out$mu - (2 * out$sigma)
#         out$week <- temp.wk$week[1]
#         
#         out$species_name = temp.wk$species_name[1]
#         out$year = temp.wk$year[1]
#       }
#       
#       if(nmodes == 1){
#         # Find modes assuming normal distribution of lengths around means
#         # Regroup data
#         out <- data.frame(
#           Group = 1,
#           lambda = 1,
#           mu = Modes(temp.wk$length_mm)$modes[which(
#             Modes(temp.wk$length_mm)$size == max(Modes(temp.wk$length_mm)$size))],
#           sigma = sd(temp.wk$length_mm)
#         )
#         
#         # Calculate upper and lower limits
#         out$ul <- out$mu + (2 * out$sigma)
#         out$ll <- out$mu - (2 * out$sigma)
#         out$Group <- as.factor(out$Group)
#         out$week <- temp.wk$week[1]
#         
#         out$species_name = temp.wk$species_name[1]
#         out$year = temp.wk$year[1]
#       }
#       
#       out <- out[with(out, order(ul)),]
#       out$Group <- seq(1, nrow(out), by=1)
#       
#       Cohort.Tracking <- rbind(Cohort.Tracking,
#                                dplyr::select(out, -sigma))
#       
#       Cohort.Tracking <- Cohort.Tracking %>% 
#         filter(!is.na(Group))
#       
#       rm(out)
#       
#     }
#     
#     Big.Cohort <- rbind(Big.Cohort, Cohort.Tracking)
#     
#     nrecords <- as.data.table(yearlist[[j]])[, .N, by = 'week'][order(week)]
#     times <- NULL
#     
#     for(q in 1:nrow(nrecords)){
#       if(nrecords[q]$N == 1){
#         times <- c(times, nrecords[q]$week)
#       }else{
#         times <- c(times, rep(nrecords[q]$week, 2 ^ 8))
#       }
#     }
#     
#     print(
#       ggplot() +
#         geom_jitter(data = yearlist[[j]],
#                     aes(x = week, y = length_mm),
#                     alpha=0.35, width=0.2, stroke=NA, cex=2) +
#         scale_color_viridis_d(option = "viridis") +
#         geom_density(data = yearlist[[j]], aes(x = after_stat(scaled) * 0.9, 
#                                                y = length_mm, group = week),
#                      n = 2 ^ 8,
#                      position = position_nudge(x = times),
#                      trim = T, adjust = 1.5) +
#         ggnewscale::new_scale_color() +
#         xlab('Week of year') + ylab('Length (mm)') +
#         ggtitle(paste0(str_to_title(yearlist[[j]]$species_name[1]),
#                        ' raw length frequency ',
#                        paste0(temp.wk$year[1]))) +
#         geom_point(data=Cohort.Tracking,
#                    aes(x=week, y=mu, col=as.factor(Group))) +
#         geom_errorbar(data=Cohort.Tracking,
#                       aes(x=week, ymin=ll, ymax=ul, col=as.factor(Group)),
#                       lwd=1, width=0.2) +
#         labs(color='Group')
#     )
#     
#     rm(Cohort.Tracking, nrecords, times)
#     
#   }
#   
#   Out.Cohort <- rbind(Out.Cohort, Big.Cohort)
#   rm(Big.Cohort)
#   
# }
# # Clean final dataset
# Out.Cohort <- Out.Cohort %>% 
#   filter(!is.na(week)) %>% 
#   mutate_at(c('species_name', 'Group'), as.factor)
# 
# summary(Out.Cohort)
# 
# #### Clean weekly cohort estimates ####
# # Split into weekly species cohorts
# weekly.spec <- split(Out.Cohort, f=paste0(Out.Cohort$species_name, "_",
#                                           Out.Cohort$year, "_",
#                                           Out.Cohort$week))
# 
# # Check for consistency in cohort measures
# for(i in 1:length(weekly.spec)){
#   # Skip if there's only one cohort this week
#   if(nrow(weekly.spec[[i]]) ==1){
#     next()
#   }
#   
#   # Test if the entirety of one cohort's LD is contained in the other's
#   # If they are, join them.
#   
#   # Set up blank vector
#   contained <- NA
#   
#   # If there are two cohorts, see if their limits envelop each other
#   if(nrow(weekly.spec[[i]]) > 1){
#     for(q in 1:nrow(weekly.spec[[i]])){
#       for(z in 1:nrow(weekly.spec[[i]])){
#         if(q == z){
#           next()
#         }
#         if(q != z){
#           if(weekly.spec[[i]]$ll[q] >= weekly.spec[[i]]$ll[z] &
#              weekly.spec[[i]]$ul[q] <= weekly.spec[[i]]$ul[z]){
#             
#             contained <- c(contained, q)
#           }
#         }
#       }
#     }
#     
#     # Which length dist is contained within the other
#     contained <- contained[!is.na(contained)]
#     
#     # If this envelopment happens, remove that "cohort"
#     if(length(contained) != 0){
#       weekly.spec[[i]] <- weekly.spec[[i]][-contained,]
#       weekly.spec[[i]]$Group <- seq(1, nrow(weekly.spec[[i]]), 1)
#     }
#     
#     contained <- NA
#   }
# }
# 
# # Rebind
# test <- do.call(rbind, weekly.spec)
# test <- test[with(test, order(species_name, year, week)),]
# rownames(test) <- NULL
# 
# # Split again into weeks
# test.split <- split(test, f=paste0(test$species_name, '_',
#                                    test$year, "_",
#                                    test$week))
# 
# # Calculate range overlap between cohorts in the same week
# for(i in 1:length(test.split)){
#   if(nrow(test.split[[i]]) ==1){
#     test.split[[i]]$overlap <- 0
#   }
#   
#   if(nrow(test.split[[i]]) > 1){
#     for(q in 1:nrow(test.split[[i]])){
#       for(z in 1:nrow(test.split[[i]])){
#         if(q == z){
#           next()
#         }
#         if(q != z){
#           range <- test.split[[i]]$ul[q] - test.split[[i]]$ll[q]
#           overlap <- test.split[[i]]$ul[q] - test.split[[i]]$ll[z]
#           rangecover <- 100 * (overlap / range)
#           if(rangecover > 100){
#             rangecover <- 0
#           }
#           if(rangecover < 0){
#             rangecover <- 0
#           }
#           test.split[[i]]$overlap[q] <- rangecover
#           
#         }
#       }
#     }
#   }
# }
# # Rebind
# test <- do.call(rbind, test.split)
# test <- test[with(test, order(species_name, year, week)),]
# rownames(test) <- NULL
# 
# # Bind to biolims
# test <- left_join(test, biolims, by=c('species_name'))
# 
# # Find range of weekly length limits
# test$range <- test$ul - test$ll
# 
# # How large is this range compared to the estimated max size of the fish
# test$coverage <- test$range / test$Linf
# 
# # Find outliers
# test2 <- test %>% 
#   group_by(species_name) %>% 
#   rstatix::identify_outliers('range') 
# 
# # If the range of a cohort is more than 40% of maximum length, remove
# test3 <- anti_join(test, test2, by=c(colnames(test)))
# test3 <- test3[test3$coverage <= 0.40 &
#                  test3$coverage >=0.01,]
# 
# # If a lower limit goes below 0, remove
# test3 <- test3[test3$ll >= 0,]
# 
# ggplot() +
#   geom_boxplot(data=test3,
#                aes(x=species_name, y=range)) +
#   geom_jitter(data=test3,
#               aes(x=species_name, y=range),
#               alpha=0.3, stroke=NA) +
#   labs(x='Species', y='Range')
# 
# # If ranges overlap by >50%, merge
# # Create yearly species list
# test3.list <- split(test3, f=paste0(test3$species_name, '_',
#                                     test3$year))
# test4.list <- test3.list
# 
# # Loop through species-years
# for(i in 1:length(test3.list)){
#   # Weekly list
#   week.list <- split(test3.list[[i]], f=test3.list[[i]]$week)
#   
#   # Loop through weeks
#   for(j in 1:length(week.list)){
#     if(nrow(week.list[[j]]) ==1){
#       next()
#     }
#     if(nrow(week.list[[j]]) > 1){
#       if(any(week.list[[j]]$overlap > 50)==TRUE){
#         keep <- week.list[[j]]
#         keep <- keep[1,]
#         keep$ll <- min(week.list[[j]]$ll)
#         keep$ul <- max(week.list[[j]]$ul)
#         keep$range <- keep$ul - keep$ll
#         keep$overlap <- 0
#         
#         week.list[[j]] <- keep
#       }
#     }
#   }
#   
#   test4.list[[i]] <- do.call(rbind, week.list)
#   
#   print(
#     ggplot(data=test4.list[[i]]) +
#       geom_jitter(data=lengths[lengths$species_name == 
#                                  paste0(test4.list[[i]]$species_name[1]) &
#                                  lengths$year == 
#                                  paste0(test4.list[[i]]$year[1]),],
#                   aes(x=week, y=length_mm),
#                   width=0.2, alpha=0.3, stroke=NA) +
#       geom_point(aes(x=week, y=mu, col=Group)) +
#       geom_errorbar(aes(x=week, ymin=ll, ymax=ul, col=Group),
#                     width=0.2, lwd=1) +
#       labs(x='Week', y='Length (mm)') +
#       ggtitle(paste0(str_to_sentence(test4.list[[i]]$species_name[1]), ' ',
#                      test4.list[[i]]$year))
#   )
# }
# # Rebind
# test4 <- do.call(rbind, test4.list)
# test4 <- test4[with(test4, order(species_name,
#                                  year,
#                                  week)),]
# rownames(test4) <- NULL
# 
# #### Save output ####
# # write.csv(test4,
# #           here('Clean_Data/Cohort_Tracking_MTI_2024.csv'),
# #           row.names = F)

#### model limits of cohort size ranges ####
# Clear out shit
rm(list=setdiff(ls(), c('lengths', 'biolims')))

# Load expert-modified data
cbs <- read.csv(here("Clean_Data/Cohort_Tracking_MTI_2024.csv"))

# Outer blank dataframe
out.cohort <- data.frame(
  side=NA,
  Intercepts = NA,
  Coefficients = NA,
  species_name=NA,
  year=NA,
  Group=NA
)

for(i in 1:nrow(biolims)){
  # Call useful data
  spec.use <- as.character(biolims$species_name[i])
  len.use <- lengths[lengths$species_name == paste0(spec.use),]
  cbs.use <- cbs[cbs$species_name == paste0(spec.use),]
  
  # Split by year
  yearlist <- split(len.use, f=len.use$year)
  
  # Blank dataframe per year
  keep.cohort <- data.frame(
    side=NA,
    Intercepts = NA,
    Coefficients = NA,
    species_name=NA,
    year=NA,
    Group=NA
  )
  
  # Loop through years
  for(j in 1:length(yearlist)){
    # Call useful data
    cbs.inner <- cbs.use[cbs.use$year == paste0(yearlist[[j]]$year[1]),]
    
    # Blank datarame for week
    mostinside.cohort <- data.frame(
      side=NA,
      Intercepts = NA,
      Coefficients = NA,
      species_name=NA,
      year=NA,
      Group=NA
    )
    
    # Skip if there are no identified cohorts that year
    if(nrow(cbs.inner) == 0){
      next()
    }
    
    # Loop through identified groups (just one for now, YOY)
    for(b in 1:length(unique(cbs.inner$Group))){
      cbs.final <- cbs.inner[cbs.inner$Group == paste0(
        unique(cbs.inner$Group)[b]),]
      
      # Model lower and upper length limits by week
      lowlm <- lm(ll ~ week, data=cbs.final)
      upplm <- lm(ul ~ week, data=cbs.final)
      
      # Add cohort limit data to dataframe
      cohort.growth <- as.data.frame(
        rbind(as.vector(lowlm$coefficients),
              as.vector(upplm$coefficients))
      )
      colnames(cohort.growth) <- c('Intercepts', 'Coefficients')
      
      cohort.growth$species_name = yearlist[[j]]$species_name[1]
      cohort.growth$year = yearlist[[j]]$year[1]
      cohort.growth$side <- c('lower', 'upper')
      cohort.growth$Group <- paste0(cbs.final$Group[1])
      
      mostinside.cohort <- rbind(mostinside.cohort, cohort.growth)
      
    }
    # Bind
    keep.cohort <- rbind(keep.cohort, mostinside.cohort)
    
  }
  # Bind
  out.cohort <- rbind(out.cohort, keep.cohort)
  # Remove initialization blanks
  out.cohort <- out.cohort %>% 
    filter(!is.na(Intercepts))
  
}

# Blank dataframe for weekly growth estimates
growth.est <- data.frame(
  species_name=NA,
  year=NA,
  growth=NA,
  Group=NA
)

# Blank dataframe to hold YOY fish identification info
cohort.1 <- data.frame(
  species_name =NA,
  length_mm=NA,
  date=NA,
  week=NA,
  year=NA,
  period=NA,
  Group=NA
)

# Blank dataframe to hold YOY growth model info
model.1 <- data.frame(
  species_name=NA,
  year=NA,
  week=NA,
  ll=NA,
  ul=NA,
  Group=NA
)

# Loop through species
for(i in 1:nrow(biolims)){
  # Call useful data
  spec.use <- as.character(biolims$species_name[i])
  len.use <- lengths[lengths$species_name == paste0(spec.use),]
  mod.use <- out.cohort[out.cohort$species_name == paste0(spec.use),]
  
  # Split by year
  yearlist <- split(len.use, f=len.use$year)
  
  # Blank dataframe by year
  in.cohort <- data.frame(
    species_name=NA,
    year=NA,
    growth=NA,
    Group=NA
  )
  # Blank cohort datframe
  cohort.2 <- data.frame(
    species_name =NA,
    length_mm=NA,
    date=NA,
    week=NA,
    year=NA,
    period=NA,
    Group=NA
  )
  
  # Loop through years
  for(j in 1:length(yearlist)){
    # Call useful information
    year.use <- yearlist[[j]]$year[1]
    mod.year <- mod.use[mod.use$year == paste0(year.use),]
    
    # Blank dataframe for cohort
    cohort.3 <- data.frame(
      species_name =NA,
      length_mm=NA,
      date=NA,
      week=NA,
      year=NA,
      period=NA,
      Group=NA
    )
    
    in.group <- data.frame(
      species_name=NA,
      year=NA,
      growth=NA,
      Group=NA
    )
    
    if(nrow(mod.year) == 0){
      next()
    }
    
    # Loop through groups (just one, YOY, for now)
    for(b in 1:length(unique(mod.year$Group))){
      # Call useful information
      mod.final <- mod.year[mod.year$Group == 
                              paste0(unique(mod.year$Group)[b]),]
      
      len.year <- len.use[len.use$year == paste0(year.use),]
      
      # Blank dataframe for modeled weekly size range
      mod.len <- data.frame(
        week = seq(20, 40, 1),
        ll = rep(NA, 21),
        ul = rep(NA, 21)
      )
      
      # Calcualte uper and lower limits
      mod.len$ul <- mod.final$Intercepts[mod.final$side == 'upper'] +
        mod.final$Coefficients[mod.final == 'upper'] * mod.len$week
      
      mod.len$ll <- mod.final$Intercepts[mod.final$side == 'lower'] +
        mod.final$Coefficients[mod.final == 'lower'] * mod.len$week
      
      # UL has to be more than LL, LL has to be more than 0
      mod.len <- mod.len[mod.len$ul > mod.len$ll,]
      mod.len <- mod.len[mod.len$ll > 0,]
      
      # Split by week
      wk.list <- split(len.year, f=len.year$week)
      for(k in 1:length(wk.list)){
        # in each week, identify fishes of our YOY cohort according to size limits
        wk.list[[k]] <- wk.list[[k]][wk.list[[k]]$length_mm >=
                                       mod.len$ll[mod.len$week == paste0(wk.list[[k]]$week[1])] &
                                       wk.list[[k]]$length_mm <=
                                       mod.len$ul[mod.len$week == paste0(wk.list[[k]]$week[1])],]
      }
      # Bind
      len.year <- do.call(rbind, wk.list)
      len.year$Group <- mod.final$Group[1]
      rownames(len.year) <- NULL
      
      # Call lengths
      oldlen <- lengths[lengths$species_name == 
                               paste0(len.year$species_name[1]) &
                               year(lengths$date) == 
                               paste0(len.year$year[1]),]
      oldlen$week <- isoweek(oldlen$date)
      oldlen$length_mm <- as.numeric(oldlen$length_mm)
      oldlen <- oldlen[paste0(oldlen$date, '-', oldlen$length_mm) %notin%
                         paste0(len.year$date, '-', len.year$length_mm),]
      
      # Plot
      print(
        ggplot() +
          geom_ribbon(data=mod.len,
                      aes(x=week, ymin=ll, ymax=ul),
                      fill='blue', col=NA, alpha=0.2)+
          geom_jitter(data = oldlen,
                      aes(x = week, y = length_mm),
                      alpha=0.55, width=0.2, stroke=NA, cex=2,
                      col='gray') +
          geom_jitter(data = len.year,
                      aes(x = week, y = length_mm),
                      alpha=0.55, width=0.2, stroke=NA, cex=2,
                      col='black') +
          xlab('Week of year') + ylab('Length (mm)') +
          ggtitle(paste0(str_to_title(yearlist[[j]]$species_name[1]),
                         ' raw length frequency ',
                         paste0(yearlist[[j]]$year[1]),
                         ", Group ",
                         paste0(mod.final$Group[1]))) +
          theme(legend.position = "N")
      )
      
      # Use Weighted Least Squares Regression to model growth
      mod <- glm(length_mm ~ week,
                 data=len.year,
                 family='gaussian')
      
      if(lmtest::bptest(mod)$p.value < 0.05){
        print(paste0(len.year$species_name[1], ' in ',
                     len.year$year[1], ' has failed BP test, using WLS'))
        wt <- 1 / lm(abs(mod$residuals) ~ mod$fitted.values)$fitted.values^2
        
        mod <- lm(length_mm ~ week,
                  data = len.year,
                  weights = wt)
        
        rm(wt)
      }
      

      
      # New dataframe to save growth info
      keep.cohort <- data.frame(
        species_name=NA,
        year=NA,
        growth=NA,
        Group=NA
      )
      
      keep.cohort$species_name <- yearlist[[j]]$species_name[1]
      keep.cohort$year <- yearlist[[j]]$year[1]
      keep.cohort$growth <- coef(mod)[2]
      keep.cohort$Group <- mod.final$Group[1]
      
      # Bind
      in.group <- rbind(in.group, keep.cohort)
      cohort.3 <- rbind(cohort.3, len.year)
      
      # Add identifying info
      mod.len$species_name <- keep.cohort$species_name[1]
      mod.len$year <- keep.cohort$year[1]
      mod.len$Group <- keep.cohort$Group[1]
      
      # Bind
      model.1 <- rbind(model.1, mod.len)
      
    }
    # Bind
    in.cohort <- rbind(in.cohort, in.group) 
    cohort.2 <- rbind(cohort.2, cohort.3)
  }
  # Bind
  growth.est <- rbind(growth.est, in.cohort)
  cohort.1 <- rbind(cohort.1, cohort.2)
}

# Remove blank initializations
growth.est <- growth.est %>% 
  filter(!is.na(species_name))

cohort.1 <- cohort.1 %>% 
  filter(!is.na(species_name))

model.1 <- model.1 %>% 
  filter(!is.na(species_name))

# Assign heat relative to CRP
growth.est$period[growth.est$year %in% c(2014, 2015, 2017, 2018, 2019)] <- 'cold'
growth.est$period[growth.est$year %in% c(2016, 2020, 2021, 2022, 2023, 2024)] <- 'hot'


# Can't have negative growth
growth.bad <- growth.est[growth.est$growth <=0,]

growth.est <- growth.est[growth.est$growth > 0,]

# Remove negative growth
cohort.1 <- cohort.1[paste0(cohort.1$species_name, cohort.1$year,
                            cohort.1$Group) %notin%
                       paste0(growth.bad$species_name, growth.bad$year,
                              growth.bad$Group),]

# Loop through species, plot yearly cohort ID
for(i in 1:nrow(biolims)){
  # Extract lengths for this species within the limits
  lengths.in <- lengths[lengths$species_name == 
                               paste0(biolims$species_name[i]),]
  lengths.in$week <- isoweek(lengths.in$date)
  lengths.in$year <- year(lengths.in$date)
  lengths.in$length_mm <- as.numeric(lengths.in$length_mm)
  lengths.in <- lengths.in[!is.na(lengths.in$length_mm),]
  
  # Plot
  print(
    ggplot() +
      
      geom_jitter(data=lengths.in,
                  aes(x = week, y = length_mm),
                  alpha=0.25, width=0.2, stroke=NA, cex=2) +
      
      geom_ribbon(data=model.1[
        model.1$species_name == paste0(biolims$species_name[i]),],
        aes(x=week, ymin=ll,
            ymax=ul, fill=as.factor(Group)),
        alpha=0.3) +
      
      labs(color='Cohort', x='Week of year', y='Length (mm)') +
      
      ggtitle(paste0(str_to_title(paste0(str_to_sentence(biolims$species_name[i]))),
                     ' modeled growth')) +
      
      facet_wrap(vars(year))
  )
  
}

# Plot estimated growth rates, compare hot/cold
growth.est$period[growth.est$period == 'hot'] <- 'Warmer than CRP'
growth.est$period[growth.est$period == 'cold'] <- 'Cooler than CRP'

growth.est$species_name[growth.est$species_name == 'atlantic herring'] <- 'Herring'
growth.est$species_name[growth.est$species_name == 'atlantic silverside'] <- 'Silverside'

growth.est$period <- factor(growth.est$period,
                            levels = c('Warmer than CRP',
                                       'Cooler than CRP'))

growth.est$xpos <- c(2.25,
                     2.25,
                     0.75,
                     1.75,
                     1.75,
                     0.75,
                     1.25,
                     0.75,
                     0.75,
                     
                     2.25, #2014
                     2.25,
                     1.25,
                     2.25,
                     1.75, #2018
                     0.75, #2020
                     1.25,
                     1.25, #2022
                     0.75,
                     0.75) #2024

# Remove herring 2024
growth.old <- growth.est
growth.est <- growth.est[growth.est$year != 2024,]

growthcomp <- ggplot(data=growth.est) +
  geom_boxplot(aes(x=period, y=growth,
                   fill=period),
               alpha=0.8,
               width=0.2,
               lwd=0.75) +
  geom_point(
    aes(x=period, y=growth),
    cex=0.7,
    col='gray30'
  ) +
  geom_text(
    aes(x=xpos,
        y=growth, 
        label=year,
        col=period)
  ) +
  facet_wrap(vars(species_name),
             scales = "free_x") +
  labs(y='Growth rate (mm/week)', 
       x='Cohort',
       fill=' ',
       col=' ') +
  theme(axis.text.x = element_blank(),
        axis.ticks.x = element_blank())
growthcomp

ggsave(plot=growthcomp,
       here('Documentation/MEPS/Figures/Growth Comparison.png'),
       width = 169, height = 100, units='mm')

cohort.1$period[cohort.1$period == 'cold'] <- 'Cooler than CRP'
cohort.1$period[cohort.1$period == 'hot'] <- 'Warmer than CRP'

# Remove 2024
cohort.1 <- cohort.1[cohort.1$year != 2024,]

# Plot length distribution and modeled growth
for(i in 1:nrow(biolims)){
  print(
    ggplot(data=cohort.1[cohort.1$species_name == 
                           biolims$species_name[i],]) +
      geom_jitter(aes(x = week, y = length_mm, col=as.factor(year)),
                  alpha=0.25, width=0.2, stroke=NA, cex=2) +
      geom_smooth(
        aes(x=week, y=length_mm, col=as.factor(year)),
        se=F, method='glm', fullrange=T) +
      scale_color_viridis_d(option='viridis') +
      labs(color='Year', x='Week of year', y='Length (mm)', lty='Heat') +
      ggtitle(paste0(str_to_title(paste0(str_to_sentence(biolims$species_name[i]))),
                     ' modeled growth')) +
      facet_wrap(vars(period))
  )
}

cohort.1$species_name[cohort.1$species_name == 'atlantic herring'] <- 'Herring'
cohort.1$species_name[cohort.1$species_name == 'atlantic silverside'] <- 'Silverside'

growthplot <- ggplot(data=cohort.1) +
    geom_jitter(aes(x = week, y = length_mm, col=as.factor(year)),
                alpha=0.25, width=0.2, stroke=NA, cex=2) +
    geom_smooth(
      aes(x=week, y=length_mm, col=as.factor(year)),
      se=F, method='glm', fullrange=T, lwd=0.5) +
    scale_color_viridis_d(option='viridis') +
    labs(color='Year', x='Week of year', y='Length (mm)', lty='Heat') +
    ggh4x::facet_grid2(species_name~period) +
  theme(legend.margin = margin(0,0,0,0),
        legend.text = element_text(size=8),
        strip.text = element_text(size=12),
        legend.spacing.y = unit(0, 'mm'),
        legend.spacing.x = unit(0, 'mm'),
        legend.key.size = unit(4, 'mm')) +
  guides(color=guide_legend(byrow=TRUE, nrow=1))

ggsave(plot=growthplot,
       here('Documentation/MEPS/Figures/Modeled Growth.png'),
       width = 169, height = 100, units='mm')

# Compare herring growths
t.test(growth.est$growth[growth.est$species_name == 'Herring' &
                    growth.est$period == 'Cooler than CRP'],
       growth.est$growth[growth.est$species_name == 'Herring' &
                    growth.est$period == 'Warmer than CRP'])

# Compare silver growths
t.test(growth.est$growth[growth.est$species_name == 'Silverside' &
                           growth.est$period == 'Cooler than CRP'],
       growth.est$growth[growth.est$species_name == 'Silverside' &
                           growth.est$period == 'Warmer than CRP'])

rm(cbs, cbs.final, cbs.inner, cbs.use, cohort.11, cohort.12,
   cohort.2, cohort.3, cohort.growth, data.all, growth.bad,
   growth.est.1, growth.est.2, in.cohort, in.group, in.cohort,
   len.use, len.year, lengths, Linf.all, lowlm, mod, mod.final,
   mod.len, mod.use, mod.year, model.11, model.12, mostinside.cohort,
   oldlen, out.cohort, upplm, wk.list, yearlist, b, i, j, spec.use,
   year.use, lengths.in, keep.cohort, k, biolims, 
   "%notin%")

save.image(file=here('Clean_Data/Cohort_Data_2024.RData'))
