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
                         species_name, length_mm, date)

# Remove one large mummichog
lengths <- lengths[lengths$length_mm != 385,]

# Set week
lengths$wk <- lubridate::isoweek(lengths$date)

# Remove early weeks
#lengths <- lengths[lengths$wk >=24,]

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

#### Focus on top 8 ####
lengths <- lengths[lengths$species_name %in% 
                     c('alewife', 'atlantic silverside',
                       'atlantic herring', 
                       'winter flounder', 'mummichog', 
                       'tomcod', 'sandlance'),]
biolims <- biolims[biolims$species_name %in% lengths$species_name,]
biolims <- biolims[with(biolims, order(species_name)),]
rownames(biolims) <- NULL

Linf.all <- data.frame(
  species_name = biolims$species_name,
  L.inf.cm = c(40, 45, 18, 
               15, 23.5, 38.1, 64.0),
  agemax = c(9, 25, 2,
             4, 12, 4, 14)
)
Linf.all$Linf <- Linf.all$L.inf.cm * 10
Linf.all$L.inf.cm <- NULL

biolims <- merge(biolims, Linf.all, by=c('species_name'))

lengths <- lengths[with(lengths, order(species_name, wk, length_mm)),]

#### Split by period ####
lengths$period[lengths$year %in% c(2014, 2015, 2017, 2018, 2019)] <- 'cold'
lengths$period[lengths$year %in% c(2016, 2020, 2021, 2022, 2023)] <- 'hot'
lengths$species_name <- droplevels(lengths$species_name)

biolims <- biolims[biolims$species_name %notin% 
                     c('tomcod'),]

lengths <- lengths[lengths$species_name %in% biolims$species_name,]

cbs <- read.csv(here('Clean_Data/Cohort_Tracking_MTI.csv'))

out.cohort <- data.frame(
  side=NA,
  Intercepts = NA,
  Coefficients = NA,
  species_name=NA,
  year=NA,
  Group=NA
)

for(i in 1:nrow(biolims)){
  
  spec.use <- as.character(biolims$species_name[i])
  len.use <- lengths[lengths$species_name == paste0(spec.use),]
  cbs.use <- cbs[cbs$species_name == paste0(spec.use),]
  
  yearlist <- split(len.use, f=len.use$year)
  
  keep.cohort <- data.frame(
    side=NA,
    Intercepts = NA,
    Coefficients = NA,
    species_name=NA,
    year=NA,
    Group=NA
  )
  
  for(j in 1:length(yearlist)){
    
    cbs.inner <- cbs.use[cbs.use$year == paste0(yearlist[[j]]$year[1]),]
    
    mostinside.cohort <- data.frame(
      side=NA,
      Intercepts = NA,
      Coefficients = NA,
      species_name=NA,
      year=NA,
      Group=NA
    )
    
    if(nrow(cbs.inner) == 0){
      next()
    }
    
    for(b in 1:length(unique(cbs.inner$Group))){
      cbs.final <- cbs.inner[cbs.inner$Group == paste0(
        unique(cbs.inner$Group)[b]),]
      
      # print(
      #   ggplot() +
      #     geom_jitter(data = yearlist[[j]],
      #                 aes(x = wk, y = length_mm),
      #                 alpha=0.35, width=0.2, stroke=NA, cex=2) +
      #     scale_color_viridis_d(option = "viridis") +
      #     ggnewscale::new_scale_color() +
      #     xlab('Week of year') + ylab('Length (mm)') +
      #     ggtitle(paste0(str_to_title(yearlist[[j]]$species_name[1]),
      #                    ' raw length frequency ',
      #                    paste0(yearlist[[j]]$year[1])))
      # )
      
      lowlm <- lm((ll) ~ wk, data=cbs.final)
      upplm <- lm(ul ~ wk, data=cbs.final)
      
      
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
    
    keep.cohort <- rbind(keep.cohort, mostinside.cohort)
    
  }
  
  out.cohort <- rbind(out.cohort, keep.cohort)
  
  out.cohort <- out.cohort %>% 
    filter(!is.na(Intercepts))
  
}

growth.est <- data.frame(
  species_name=NA,
  year=NA,
  growth=NA,
  Group=NA
)

cohort.1 <- data.frame(
  species_name =NA,
  length_mm=NA,
  date=NA,
  wk=NA,
  year=NA,
  period=NA,
  Group=NA
)

model.1 <- data.frame(
  species_name=NA,
  year=NA,
  wk=NA,
  ll=NA,
  ul=NA,
  Group=NA
)

for(i in 1:nrow(biolims)){
  
  spec.use <- as.character(biolims$species_name[i])
  len.use <- lengths[lengths$species_name == paste0(spec.use),]
  mod.use <- out.cohort[out.cohort$species_name == paste0(spec.use),]
  
  yearlist <- split(len.use, f=len.use$year)
  
  in.cohort <- data.frame(
    species_name=NA,
    year=NA,
    growth=NA,
    Group=NA
  )
  
  cohort.2 <- data.frame(
    species_name =NA,
    length_mm=NA,
    date=NA,
    wk=NA,
    year=NA,
    period=NA,
    Group=NA
  )
  
  for(j in 1:length(yearlist)){
    
    year.use <- yearlist[[j]]$year[1]
    mod.year <- mod.use[mod.use$year == paste0(year.use),]
    
    cohort.3 <- data.frame(
      species_name =NA,
      length_mm=NA,
      date=NA,
      wk=NA,
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
    
    for(b in 1:length(unique(mod.year$Group))){
      mod.final <- mod.year[mod.year$Group == 
                              paste0(unique(mod.year$Group)[b]),]
      
      len.year <- len.use[len.use$year == paste0(year.use),]
      
      mod.len <- data.frame(
        wk = seq(20, 40, 1),
        ll = rep(NA, 21),
        ul = rep(NA, 21)
      )
      
      mod.len$ul <- mod.final$Intercepts[mod.final$side == 'upper'] +
        mod.final$Coefficients[mod.final == 'upper'] * mod.len$wk
      
      mod.len$ll <- mod.final$Intercepts[mod.final$side == 'lower'] +
        mod.final$Coefficients[mod.final == 'lower'] * mod.len$wk
      
      mod.len <- mod.len[mod.len$ul > mod.len$ll,]
      mod.len <- mod.len[mod.len$ll > 0,]
    
      wk.list <- split(len.year, f=len.year$wk)
      for(k in 1:length(wk.list)){
        wk.list[[k]] <- wk.list[[k]][wk.list[[k]]$length_mm >=
                                       mod.len$ll[mod.len$wk == paste0(wk.list[[k]]$wk[1])] &
                                       wk.list[[k]]$length_mm <=
                                       mod.len$ul[mod.len$wk == paste0(wk.list[[k]]$wk[1])],]
      }
      len.year <- do.call(rbind, wk.list)
      len.year$Group <- mod.final$Group[1]
      rownames(len.year) <- NULL
      
      oldlen <- data.all$bio[data.all$bio$species_name == 
                               paste0(len.year$species_name[1]) &
                               year(data.all$bio$date) == 
                               paste0(len.year$year[1]),]
      oldlen$wk <- isoweek(oldlen$date)
      oldlen$length_mm <- as.numeric(oldlen$length_mm)
      oldlen <- oldlen[paste0(oldlen$date, '-', oldlen$length_mm) %notin%
                         paste0(len.year$date, '-', len.year$length_mm),]
      
      print(
        ggplot() +
          geom_ribbon(data=mod.len,
                      aes(x=wk, ymin=ll, ymax=ul),
                      fill='blue', col=NA, alpha=0.2)+
          geom_jitter(data = oldlen,
                      aes(x = wk, y = length_mm),
                      alpha=0.55, width=0.2, stroke=NA, cex=2,
                      col='gray') +
          geom_jitter(data = len.year,
                      aes(x = wk, y = length_mm),
                      alpha=0.55, width=0.2, stroke=NA, cex=2,
                      col='black') +
          #geom_smooth(data=len.year,
          #           aes(x=wk, y=length_mm),
          #           se=F, method='glm', fullrange=F) +
          xlab('Week of year') + ylab('Length (mm)') +
          ggtitle(paste0(str_to_title(yearlist[[j]]$species_name[1]),
                         ' raw length frequency ',
                         paste0(yearlist[[j]]$year[1]),
                         ", Group ",
                         paste0(mod.final$Group[1]))) +
          # geom_point(data=Cohort.Tracking,
          #            aes(x=wk, y=mu, col=as.factor(Group))) +
          # geom_errorbar(data=Cohort.Tracking,
          #               aes(x=wk, ymin=ll, ymax=ul, col=Group),
          #               lwd=1, width=0.2) +
          # labs(color='Group') +
          theme(legend.position = "N")
      )
      
      mod <- glm(length_mm ~ wk,
                 data=len.year,
                 family='gaussian')
      
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
      
      in.group <- rbind(in.group, keep.cohort)
      cohort.3 <- rbind(cohort.3, len.year)
      
      mod.len$species_name <- keep.cohort$species_name[1]
      mod.len$year <- keep.cohort$year[1]
      mod.len$Group <- keep.cohort$Group[1]
      
      model.1 <- rbind(model.1, mod.len)
      
      }
    
   in.cohort <- rbind(in.cohort, in.group) 
   cohort.2 <- rbind(cohort.2, cohort.3)
  }
  
  growth.est <- rbind(growth.est, in.cohort)
  cohort.1 <- rbind(cohort.1, cohort.2)
}

growth.est <- growth.est %>% 
  filter(!is.na(species_name))

cohort.1 <- cohort.1 %>% 
  filter(!is.na(species_name))

model.1 <- model.1 %>% 
  filter(!is.na(species_name))

growth.est$period[growth.est$year %in% c(2014, 2015, 2017, 2018, 2019)] <- 'cold'
growth.est$period[growth.est$year %in% c(2016, 2020, 2021, 2022, 2023)] <- 'hot'

growth.est$period <- factor(growth.est$period,
                            levels=c('hot', 'cold'))

growth.bad <- growth.est[growth.est$growth <=0,]

growth.est <- growth.est[growth.est$growth > 0,]

cohort.1 <- cohort.1[paste0(cohort.1$species_name, cohort.1$year,
                            cohort.1$Group) %notin%
                       paste0(growth.bad$species_name, growth.bad$year,
                              growth.bad$Group),]

for(i in 1:nrow(biolims)){
  lengths.in <- data.all$bio[data.all$bio$species_name == 
                               paste0(biolims$species_name[i]),]
  lengths.in$wk <- isoweek(lengths.in$date)
  lengths.in$year <- year(lengths.in$date)
  lengths.in$length_mm <- as.numeric(lengths.in$length_mm)
  lengths.in <- lengths.in[!is.na(lengths.in$length_mm),]
  lengths.in <- lengths.in[lengths.in$length_mm != 385,]
  
  print(
    ggplot() +
      
      geom_jitter(data=lengths.in,
                  aes(x = wk, y = length_mm),
                  alpha=0.25, width=0.2, stroke=NA, cex=2) +
      
      geom_ribbon(data=model.1[
        model.1$species_name == paste0(biolims$species_name[i]),],
        aes(x=wk, ymin=ll,
            ymax=ul, fill=as.factor(Group)),
        alpha=0.3) +
      
      labs(color='Cohort', x='Week of year', y='Length (mm)') +
      
      ggtitle(paste0(str_to_title(paste0(str_to_sentence(biolims$species_name[i]))),
                     ' modeled growth')) +
      
      facet_wrap(vars(year))
  )
  
}

# Mummichogs and silversides only can have 2 groups
growth.est.1 <- growth.est[growth.est$Group == 1,]
growth.est.2 <- growth.est[growth.est$Group == 2,]
growth.est.2 <- growth.est.2[growth.est.2$species_name %in% 
                               c('atlantic silverside',
                                 'mummichog'),]
growth.est <- rbind(growth.est.1, growth.est.2)
growth.est <- growth.est[with(growth.est, order(
  species_name, year, Group
)),]
rownames(growth.est) <- NULL

model.11 <- model.1[model.1$Group == 1,]
model.12 <- model.1[model.1$Group == 2,]
model.12 <- model.12[model.12$species_name %in% 
                       c('atlantic silverside',
                         'mummichog'),]
model.1 <- rbind(model.11, model.12)
model.1 <- model.1[with(model.1, order(
  species_name, year, wk, Group
)),]
rownames(model.1) <- NULL

cohort.11 <- cohort.1[cohort.1$Group == 1,]
cohort.12 <- cohort.1[cohort.1$Group == 2,]
cohort.12 <- cohort.12[cohort.12$species_name %in% 
                       c('atlantic silverside',
                         'mummichog'),]
cohort.1 <- rbind(cohort.11, cohort.12)
cohort.1 <- cohort.1[with(cohort.1, order(
  species_name, year, wk, Group
)),]
rownames(cohort.1) <- NULL

for(i in 1:nrow(biolims)){
  lengths.in <- data.all$bio[data.all$bio$species_name == 
                               paste0(biolims$species_name[i]),]
  lengths.in$wk <- isoweek(lengths.in$date)
  lengths.in$year <- year(lengths.in$date)
  lengths.in$length_mm <- as.numeric(lengths.in$length_mm)
  lengths.in <- lengths.in[!is.na(lengths.in$length_mm),]
  lengths.in <- lengths.in[lengths.in$length_mm != 385,]
  
  print(
    ggplot() +
      
      geom_jitter(data=lengths.in,
                  aes(x = wk, y = length_mm),
                  alpha=0.25, width=0.2, stroke=NA, cex=2) +
      
      geom_ribbon(data=model.1[
        model.1$species_name == paste0(biolims$species_name[i]),],
        aes(x=wk, ymin=ll,
            ymax=ul, fill=as.factor(Group)),
        alpha=0.3) +
      
      labs(color='Cohort', x='Week of year', y='Length (mm)') +
      
      ggtitle(paste0(str_to_title(paste0(str_to_sentence(biolims$species_name[i]))),
                     ' modeled growth')) +
      
      facet_wrap(vars(year))
  )
  
}


ggplot(data=growth.est) +
  geom_boxplot(aes(x=Group, y=growth/7,
                   fill=period)) +
  facet_wrap(vars(species_name),
             scales = "free_x") +
  labs(y='Growth rate (mm/day)', 
       x='Cohort',
       fill='Heat period')



for(i in 1:nrow(biolims)){
  print(
    ggplot(data=cohort.1[cohort.1$species_name == 
                           biolims$species_name[i],]) +
      geom_jitter(aes(x = wk, y = length_mm, col=as.factor(year)),
                  alpha=0.25, width=0.2, stroke=NA, cex=2) +
      geom_smooth(
                  aes(x=wk, y=length_mm, col=as.factor(year), lty=period),
                  se=F, method='glm', fullrange=T) +
      scale_color_viridis_d(option='viridis') +
      labs(color='Year', x='Week of year', y='Length (mm)', lty='Heat') +
      ggtitle(paste0(str_to_title(paste0(str_to_sentence(biolims$species_name[i]))),
                     ' modeled growth')) +
      facet_wrap(vars(Group))
  )
}

rm(cbs, cbs.final, cbs.inner, cbs.use, cohort.11, cohort.12,
   cohort.2, cohort.3, cohort.growth, data.all, growth.bad,
   growth.est.1, growth.est.2, in.cohort, in.group, in.cohort,
   len.use, len.year, lengths, Linf.all, lowlm, mod, mod.final,
   mod.len, mod.use, mod.year, model.11, model.12, mostinside.cohort,
   oldlen, out.cohort, upplm, wk.list, yearlist, b, i, j, spec.use,
   year.use, lengths.in, keep.cohort, k, biolims, 
   "%notin%")

save.image(file=here('Clean_Data/Cohort_Data.RData'))
