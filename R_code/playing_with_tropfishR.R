rm(list=ls())

# Packages
library(TropFishR)
library(here)
library(tidyverse)

# Duplicatability 
set.seed(1)

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
load(here('Data/Clean_Data/Cohort/Cohort_Data_2024.RData'))
rm(abund, bio, growth.est, model.1, trips)

tarfish <- cohort.1[cohort.1$species_name == 'atlantic silverside',]
tarfish$date <- as.Date(tarfish$date)
tarfish$length_cm <- tarfish$length_mm / 10
yearuse <- unique(year(tarfish$date))
yearuse <- yearuse[yearuse != 2024]

tot.list <- vector(mode = 'list', length = length(yearuse))

for(i in 1:length(yearuse)){
  silver <- tarfish[tarfish$species_name == 'atlantic silverside' & 
                      year(tarfish$date) == yearuse[i],]
  
  slf <- vector(mode = 'list', length = 4)
  names(slf) <- c('sample.no', 'midLengths', 'dates', 'catch')
  
  slf$dates <- unique(silver$date)
  slf$midLengths <- seq(min(silver$length_cm), max(silver$length_cm), 0.1)
  rawl <- data.frame(
    length_cm = seq(min(silver$length_cm), max(silver$length_cm), 0.1),
    date = slf$dates[[1]])
  
  for(i in 2:length(slf$dates)){
    rawl <- rbind(rawl,
                  data.frame(length_cm = seq(min(silver$length_cm), max(silver$length_cm), 0.1),
                             date = slf$dates[[i]]))
  }
  
  usel <- silver %>% 
    group_by(date, length_cm) %>% 
    summarise(n = n()) %>% 
    mutate(length_cm = as.numeric(length_cm)) %>% 
    as.data.frame()
  
  rawl <- left_join(rawl, usel, by=c('date', 'length_cm'))
  rawl[is.na(rawl)] <- 0
  rawl$n <- as.integer(rawl$n)
  
  rawl <- rawl %>% 
    pivot_wider(names_from = 'date', values_from = 'n')
  
  rawl <- dplyr::select(rawl, -length_cm)
  rawl <- as.matrix(rawl)
  colnames(rawl) <- NULL
  
  slf$catch <- rawl
  
  slf$sample.no <- as.integer(seq(1, length(slf$midLengths), 1))
  
  class(slf) <- 'lfq'
  
  # TropFishR
  lfq <- slf
  
  ## set seed value for reproducible results
  set.seed(1)
  
  ## adjust bin size
  lfq_bin2 <- lfqModify(lfq, bin_size = 0.5)
  
  ## plot raw and restructured LFQ data
  ma <- 5
  lfq_bin2_res <- lfqRestructure(lfq_bin2, MA = ma, addl.sqrt = FALSE)
  
  # opar <- par(mfrow = c(2,1), mar = c(2,5,2,3), oma = c(2,0,0,0))
  # plot(lfq_bin2_res, Fname = "catch", date.axis = "modern")
  # plot(lfq_bin2_res, Fname = "rcounts", date.axis = "modern")
  # par(opar)
  
  ## coarse estimate of Linf
  linf_guess <- 150
  
  ## lower search space bounds
  low_par <- list(Linf = 0.8 * linf_guess,
                  K = 0.01,
                  t_anchor = 0,
                  C = 0,
                  ts = 0)
  
  ## upper search space bounds
  up_par <- list(Linf = 2 * linf_guess,
                 K = 4,
                 t_anchor = 1,
                 C = 1,
                 ts = 1)
  
  ## run ELEFAN with simulated annealing
  res_SA <- ELEFAN_SA(lfq_bin2, 
                      #SA_time = 60*0.5, 
                      #SA_temp = 6e5,
                      MA = ma, 
                      seasonalised = FALSE, 
                      addl.sqrt = FALSE,
                      init_par = list(Linf = linf_guess,
                                      K = 0.5,
                                      t_anchor = 0.5,
                                      C=0.5,
                                      ts = 0.5),
                      low_par = low_par,
                      up_par = up_par)
  
  ## show results
  unlist(res_SA$par)
  print(plot(res_SA))
  
  tot.list[i] <- unlist(res_SA$par)
  
  rm(res_SA, up_par, low_par, silver, dtab, slf, rawl, usel, lfq, lfq_bin2, 
     ma, lfq_bin2_res, opar, linf_guess)
}



res_SA$par
res_SA$Rn_max
