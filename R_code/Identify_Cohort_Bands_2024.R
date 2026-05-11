# Identify cohorts by length
rm(list=ls())

# Load packages
library(data.table)
library(tidyverse)
library(readxl)
library(here)
library(mixtools)
library(diptest)
library(mgcv)
library(TropFishR)
library(LaplacesDemon)

# Duplicatability 
set.seed(109861235)

# Negate function
'%notin%' <- function(x,y)!('%in%'(x,y))

# Set GGplot auto theme
theme_set(theme(panel.grid.major = element_line(color='lightgray'),
                panel.grid.minor = element_blank(),
                panel.background = element_blank(),
                panel.border = element_rect(color='black', linewidth=1, 
                                            fill=NA),
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
#abund <- read.csv(here('Data/Clean_Data/Seine/abund_through_2025.csv'))
#trips <- read.csv(here('Data/Clean_Data/Seine/trips_through_2025.csv'))
bio <- read.csv(here('Data/Clean_Data/Seine/lengths_through_2025.csv'))

# # Clean
# abund <- abund %>% 
#   mutate(date = as.Date(date, format="%m/%d/%Y")) %>% 
#   mutate(month = month(date),
#          year = year(date),
#          week = isoweek(date))
# abund$collector[abund$site_id >20] <- 'QBC'
# abund$collector[abund$site_id <20] <- 'GMRI'
# 
# trips <- trips %>% 
#   mutate(date = as.Date(date, format="%m/%d/%Y")) %>% 
#   mutate(month = month(date),
#          year = year(date),
#          week = isoweek(date)) %>% 
#   mutate(set_time = as.POSIXct(paste0(date, ' ', set_time_local), format='%Y-%m-%d %H:%M'))
# 
# trips$collector <- NA
# trips$collector[trips$site_id >20] <- 'QBC'
# trips$collector[trips$site_id <20] <- 'GMRI'
# trips$set_time = trips$set_time - lubridate::hours(4)

bio <- bio %>% 
  mutate(date = as.Date(date, format = '%m/%d/%Y')) %>% 
  mutate(month = month(date),
         year = year(date),
         week = isoweek(date)) %>% 
  mutate(length_mm = as.numeric(length_mm)) %>% 
  filter(year %notin% c(2019, 2024, 2025)) %>% 
  filter(species_name %in% c('atlantic silverside', 'atlantic herring')) %>% 
  mutate(site_name = trimws(site_name)) %>% 
  filter(!is.na(site_name) & site_name %notin% c('Cedar Beach', 'Garrison Cove',
                                                 'Long Point Cove', 'Lowell Cove',
                                                 'Orrs Cove', 'Snow Island', 
                                                 'Stovers Point')) %>% 
  filter(!is.na(length_mm) & length_mm >5) %>% 
  dplyr::select(-sex, -notes, -bay_location, -site_id,
                -loc_id)

lengths <- dplyr::select(bio,
                         species_name, length_mm, date)

lengths$wk <- lubridate::isoweek(lengths$date)
lengths$year <- year(lengths$date)

# Remove early weeks
lengths <- lengths[lengths$wk %in% seq(24, 39, 1),]

lengths$species_name <- as.factor(lengths$species_name)
lengths$wk <- as.numeric(lengths$wk)
lengths$year <- as.numeric(lengths$year)

biolims <- lengths %>% 
  group_by(species_name) %>% 
  summarise(min.length = min(length_mm),
            max.length = max(length_mm))

#### Focus on top 2 ####

Linf.all <- data.frame(
  species_name = c('atlantic herring', 'atlantic silverside'),
  L.inf.cm = c(35, 14),
  agemax = c(15, 2)
)
Linf.all$Linf <- Linf.all$L.inf.cm * 10
Linf.all$L.inf.cm <- NULL

biolims <- merge(biolims, Linf.all, by=c('species_name'))

lengths <- lengths[with(lengths, order(species_name, wk, length_mm)),]

#### Test on silversides ####
Out.Cohort <-     
  data.frame(
  species_name = NA,
  year= NA,
  Group=NA,
  lambda=NA,
  wk=NA,
  mu=NA,
  ll=NA,
  ul=NA
)
biolims$species_name <- as.character(biolims$species_name)
rownames(biolims) <- NULL

sort.group <- data.frame(species_name = NA, length_mm = NA,
                         date = NA, wk = NA, year = NA, Group = NA)

for(i in 1:length(biolims$species_name)){
  spec.use <- as.character(biolims$species_name[i])
  len.use <- lengths[lengths$species_name == paste0(spec.use),]
  
  yearlist <- split(len.use, f=len.use$year)
  
  if(spec.use == 'atlantic herring'){
    yearlist <- yearlist[-9]
  }
  
  Big.Cohort <- 
    data.frame(
      species_name = NA,
      year= NA,
      Group=NA,
      lambda=NA,
      wk=NA,
      mu=NA,
      ll=NA,
      ul=NA
    )
  
  for(j in 1:length(yearlist)){
    temp.wk <- yearlist[[j]]
    
    # Remove weeks with fewer than 5 fish caught
    temp.wk <- split(yearlist[[j]], f=yearlist[[j]]$wk)
    temp.wk <- temp.wk[sapply(temp.wk, nrow) > 5]
    
    Cohort.Tracking <- data.frame(
      species_name=NA,
      year=NA,
      Group=NA,
      lambda=NA,
      wk=NA,
      mu=NA,
      ll=NA,
      ul=NA
    )
    
    for(k in 1:length(temp.wk)){
      
      temp <- temp.wk[[k]]
      
      df_outliers <- temp %>% 
        group_by(wk) %>% 
        rstatix::identify_outliers("length_mm") 
      
      #temp <- temp[temp$length_mm %notin%
      #               df_outliers$length_mm[df_outliers$is.outlier == TRUE],]
      
      if(nrow(temp) < 5){
        next()
      }
      
      nmodes <- length(Modes(temp$length_mm)$modes)
      
      if(nmodes >= 2){
        out <- normalmixEM(temp$length_mm,
                                k=nmodes,
                                arbmean = TRUE,
                           maxit = 10000000000,
                           maxrestarts = 10000000)
        
        posterio <- as.data.frame(out$posterior)
        Group <- data.frame(Group = rep(NA, nrow(posterio)))
        for(z in 1:nrow(posterio)){
          Group$Group[z] <- max.col(posterio[z,])
        }
        
        temp <- cbind(temp, Group)
        
        out <- cbind(out$mu, out$sigma, out$lambda)
        out <- as.data.frame(out)
        colnames(out) <- c('mu', 'sigma', 'lambda')
        out$Group <- as.factor(as.numeric(rownames(out)))
        
        out$ul <- out$mu + (2 * out$sigma)
        out$ll <- out$mu - (2 * out$sigma)
        out$wk <- temp$wk[1]
        
        out$species_name = temp$species_name[1]
        out$year = temp$year[1]
        temp.wk[[k]] <- temp
      }
      
      if(nmodes == 1){ 
        # Find modes assuming normal distribution of lengths around means
        # Regroup data
        out <- data.frame(
          Group = 1,
          lambda = 1,
          mu = Modes(temp$length_mm)$modes[which(
            Modes(temp$length_mm)$size == max(Modes(temp$length_mm)$size))],
          sigma = sd(temp$length_mm)
        )
        
        # Calculate upper and lower limits
        out$ul <- out$mu + (2 * out$sigma)
        out$ll <- out$mu - (2 * out$sigma)
        out$Group <- as.factor(out$Group)
        out$wk <- temp$wk[1]
        
        out$species_name = temp$species_name[1]
        out$year = temp$year[1]
        
        temp$Group[temp$length_mm >=out$ll & temp$length_mm<= out$ul] <- 1
        temp.wk[[k]] <- temp
      }
      
      out <- out[with(out, order(ul)),]
      out$Group <- seq(1, nrow(out), by=1)
      
      Cohort.Tracking <- rbind(Cohort.Tracking,
                               dplyr::select(out, -sigma))
      
      Cohort.Tracking <- Cohort.Tracking %>% 
        filter(!is.na(Group))
      
      rm(out)
      
    }
    
    Big.Cohort <- rbind(Big.Cohort, Cohort.Tracking)
    
    nrecords <- as.data.table(yearlist[[j]])[, .N, by = 'wk'][order(wk)]
    times <- NULL
    
    for(q in 1:nrow(nrecords)){
      if(nrecords[q]$N == 1){
        times <- c(times, nrecords[q]$wk)
      }else{
        times <- c(times, rep(nrecords[q]$wk, 2 ^ 8))
      }
    }
    
    print(
      ggplot() +
        geom_jitter(data = yearlist[[j]],
                    aes(x = wk, y = length_mm),
                    alpha=0.35, width=0.2, stroke=NA, cex=2) +
        scale_color_viridis_d(option = "viridis") +
        geom_density(data = yearlist[[j]], aes(x = after_stat(scaled) * 0.9, 
                                      y = length_mm, group = wk),
                     n = 2 ^ 8,
                     position = position_nudge(x = times),
                     trim = T, adjust = 1.5) +
        ggnewscale::new_scale_color() +
        xlab('Week of year') + ylab('Length (mm)') +
        ggtitle(paste0(str_to_title(yearlist[[j]]$species_name[1]),
                       ' raw length frequency ',
                       paste0(temp$year[1]))) +
        geom_point(data=Cohort.Tracking,
                  aes(x=wk, y=mu, col=as.factor(Group))) +
        geom_errorbar(data=Cohort.Tracking,
                     aes(x=wk, ymin=ll, ymax=ul, col=as.factor(Group)),
                     lwd=1, width=0.2) +
        labs(color='Group')
    )
    
    plotuse <- do.call(rbind, temp.wk)
    
    print(
      ggplot(data = plotuse) +
        geom_jitter(aes(x=wk, y=length_mm, col=as.factor(Group)),
                    alpha = 0.5, width = 0.2, height = 0 , stroke = NA, cex=2) +
        scale_x_continuous(breaks = seq(min(plotuse$wk), max(plotuse$wk), 1)) +
        ggtitle(paste0(plotuse$species_name[1], ' ', plotuse$year[1]))
    )
    
    rm(Cohort.Tracking, nrecords, times)
    
    yearlist[[j]] <- plotuse

  }
  
  len.use <- do.call(rbind, yearlist)
  sort.group <- rbind(sort.group, len.use)
  
  Out.Cohort <- rbind(Out.Cohort, Big.Cohort)
  rm(Big.Cohort)
  
}

summary(Out.Cohort)

Out.Cohort <- Out.Cohort %>% 
  filter(!is.na(wk)) %>% 
  mutate_at(c('species_name', 'Group'), as.factor)

summary(Out.Cohort)

weekly.spec <- split(Out.Cohort, f=paste0(Out.Cohort$species_name, "_",
                                          Out.Cohort$year, "_",
                                          Out.Cohort$wk))

for(i in 1:length(weekly.spec)){
  if(nrow(weekly.spec[[i]]) ==1){
    next()
  }

  contained <- NA

  if(nrow(weekly.spec[[i]]) > 1){
    for(q in 1:nrow(weekly.spec[[i]])){
      for(z in 1:nrow(weekly.spec[[i]])){
        if(q == z){
          next()
        }
        if(q != z){
          if(weekly.spec[[i]]$ll[q] >= weekly.spec[[i]]$ll[z] &
             weekly.spec[[i]]$ul[q] <= weekly.spec[[i]]$ul[z]){

            contained <- c(contained, q)
          }
        }
      }
    }

    contained <- contained[!is.na(contained)]

    if(length(contained) != 0){
      weekly.spec[[i]] <- weekly.spec[[i]][-contained,]
      weekly.spec[[i]]$Group <- seq(1, nrow(weekly.spec[[i]]), 1)
    }

    contained <- NA
  }
}

test <- do.call(rbind, weekly.spec)
test <- test[with(test, order(species_name, year, wk)),]
rownames(test) <- NULL

test.split <- split(test, f=paste0(test$species_name, '-', test$year))

# for(i in 1:length(test.split)){
#   len.use <- lengths[lengths$species_name == paste0(
#                           test.split[[i]]$species_name[1]) &
#                      lengths$year == paste0(test.split[[i]]$year[1]),]
#   print(
#   ggplot() +
#     geom_jitter(data=len.use,
#                 aes(x=wk, y=length_mm),
#                 alpha=0.3, stroke=NA, width=0.2) +
#     geom_errorbar(data=test.split[[i]],
#                   aes(x=wk, ymin=ll, ymax=ul, col=Group),
#                   width=0.2, lwd=1) +
#     ggtitle(paste0(str_to_sentence(test.split[[i]]$species_name[1]), ' ',
#                    test.split[[i]]$year[1])) +
#     coord_cartesian(xlim=c(22, 39),
#                     ylim=c(biolims$min.length[biolims$species_name ==
#                             paste0(test.split[[i]]$species_name[1])],
#                            biolims$max.length[biolims$species_name ==
#                             paste0(test.split[[i]]$species_name[1])])
#                     )
#   )
# 
# }

test.split <- split(test, f=paste0(test$species_name, '_',
                                   test$year, "_",
                                   test$wk))

for(i in 1:length(test.split)){
  if(nrow(test.split[[i]]) ==1){
    test.split[[i]]$overlap <- 0
  }
  
  if(nrow(test.split[[i]]) > 1){
    for(q in 1:nrow(test.split[[i]])){
      for(z in 1:nrow(test.split[[i]])){
        if(q == z){
          next()
        }
        if(q != z){
          range <- test.split[[i]]$ul[q] - test.split[[i]]$ll[q]
          overlap <- test.split[[i]]$ul[q] - test.split[[i]]$ll[z]
          rangecover <- 100 * (overlap / range)
          if(rangecover > 100){
            rangecover <- 0
          }
          if(rangecover < 0){
            rangecover <- 0
          }
          test.split[[i]]$overlap[q] <- rangecover

          }
        }
      }
    }
  }
test <- do.call(rbind, test.split)
test <- test[with(test, order(species_name, year, wk)),]
rownames(test) <- NULL

test <- left_join(test, biolims, by=c('species_name'))

test$range <- test$ul - test$ll

test$coverage <- test$range / test$Linf

test2 <- test %>% 
  group_by(species_name) %>% 
  rstatix::identify_outliers('range') 

test3 <- anti_join(test, test2, by=c(colnames(test)))
test3 <- test3[test3$coverage <= 0.40 &
               test3$coverage >=0.01,]

test3 <- test3[test3$ll >= 0,]

ggplot() +
  geom_boxplot(data=test3,
               aes(x=species_name, y=range)) +
  geom_jitter(data=test3,
              aes(x=species_name, y=range),
              alpha=0.3, stroke=NA) +
  labs(x='Species', y='Range')

test3.list <- split(test3, f=paste0(test3$species_name, '_',
                                    test3$year))
test4.list <- test3.list

for(i in 1:length(test3.list)
    #18
    ){
  # print(
  #   ggplot(data=test3.list[[i]]) +
  #     geom_jitter(data=lengths[lengths$species_name == 
  #                               paste0(test3.list[[i]]$species_name[1]) &
  #                              lengths$year == paste0(
  #                                 test3.list[[i]]$year[1]),],
  #                 aes(x=wk, y=length_mm),
  #                 width=0.2, alpha=0.3, stroke=NA) +
  #     geom_point(aes(x=wk, y=mu, col=Group)) +
  #     geom_errorbar(aes(x=wk, ymin=ll, ymax=ul, col=Group),
  #                   width=0.2, lwd=1) +
  #     labs(x='Week', y='Length (mm)') +
  #     ggtitle(paste0(str_to_sentence(test3.list[[i]]$species_name[1]), ' ',
  #                    test3.list[[i]]$year))
  # )
  
  wk.list <- split(test3.list[[i]], f=test3.list[[i]]$wk)
  for(j in 1:length(wk.list)){
    if(nrow(wk.list[[j]]) ==1){
      next()
    }
    if(nrow(wk.list[[j]]) > 1){
      if(any(wk.list[[j]]$overlap > 50)==TRUE){
        keep <- wk.list[[j]]
        keep <- keep[1,]
        keep$ll <- min(wk.list[[j]]$ll)
        keep$ul <- max(wk.list[[j]]$ul)
        keep$range <- keep$ul - keep$ll
        keep$overlap <- 0
        
        wk.list[[j]] <- keep
      }
    }
  }
  
  test4.list[[i]] <- do.call(rbind, wk.list)
  
  print(
    ggplot(data=test4.list[[i]]) +
      geom_jitter(data=lengths[lengths$species_name == 
                                 paste0(test4.list[[i]]$species_name[1]) &
                                 lengths$year == 
                                 paste0(test4.list[[i]]$year[1]),],
                  aes(x=wk, y=length_mm),
                  width=0.2, alpha=0.3, stroke=NA) +
      geom_point(aes(x=wk, y=mu, col=Group)) +
      geom_errorbar(aes(x=wk, ymin=ll, ymax=ul, col=Group),
                    width=0.2, lwd=1) +
      labs(x='Week', y='Length (mm)') +
      ggtitle(paste0(str_to_sentence(test4.list[[i]]$species_name[1]), ' ',
                     test4.list[[i]]$year))
  )
}

test4 <- do.call(rbind, test4.list)
test4 <- test4[with(test4, order(species_name,
                                 year,
                                 wk)),]
rownames(test4) <- NULL

write.csv(test4,
          here('Data/Clean_Data/Cohort/Cohort_Tracking_MTI_2024_march26.csv'),
          row.names = F)
