rm(list=ls())

# Libraries
library(tidyverse)
library(ks)

# Set GGplot auto theme
theme_set(theme(panel.grid.major = element_line(color='lightgray'),
                panel.grid.minor = element_blank(),
                panel.background = element_blank(),
                panel.border = element_rect(color='black', size=1, fill=NA),
                legend.position = "n",
                axis.text.x=element_text(size=12),
                axis.text.y=element_text(size=12),
                axis.title.x=element_text(size=14),
                axis.title.y=element_text(size=14, angle=90, vjust=2),
                plot.title=element_text(size=14, hjust = 0, vjust = 1.2),
                plot.caption=element_text(hjust=0, face='italic', size=12)))

# Negate function
'%notin%' <- function(x,y)!('%in%'(x,y))

#### Goals ####
# Overall: convert total wet weight to abundance
# Intermediate: Sex frequency
# Intermediate: Sex-specific relationships of weight frequency
# Secondary: Sex-specific relationships of lenght-weight
# Secondary: sex-specific relationship of length frequency

#### Load data ####
crabs <- read.csv("C:/Users/klankowicz/Downloads/2023 Green Crab Survey - Data_Raw.csv")

# View
head(crabs)

# Scrape
crabs <- dplyr::select(crabs, Sex, Carapace_Width_mm, Weight_g,
                       Claws_Missing, Legs_Missing, notes)

# Structure
str(crabs)
crabs$Sex <- factor(crabs$Sex,
                    levels=c('m', 'f'))
colnames(crabs) <- tolower(colnames(crabs))

#### Sex-specific weight frequency ####
# Subset
mal <- subset(crabs, crabs$sex == 'm')
fem <- subset(crabs, crabs$sex == 'f')

# Initialize table
freqtab <- data.frame(
  sex=c(rep('m', 4),
        rep('f', 4)),
  legs_mising=c('no', 'no', 'yes', 'yes',
                'no', 'no', 'yes', 'yes'),
  claw_missing=c('no', 'yes', 'no', 'yes',
                 'no', 'yes', 'no', 'yes'),
  freq.sex=rep(NA, 8),
  freq.missing = rep(NA, 8)
)

# Frequency of sex
freqtab$freq.sex[freqtab$sex == 'm'] <- 
  (nrow(crabs[crabs$sex == 'm',]) / nrow(crabs) * 100)
freqtab$freq.sex[freqtab$sex == 'f'] <- 
  (nrow(crabs[crabs$sex == 'f',]) / nrow(crabs) * 100)

# Frequency of males missing limbs
freqtab$freq.missing[1] <- (nrow(mal[mal$claws_missing == 0 & mal$legs_missing == 0,]) /
                              nrow(mal) * 100)
freqtab$freq.missing[2] <- (nrow(mal[mal$claws_missing != 0 & mal$legs_missing == 0,]) /
                              nrow(mal) * 100)
freqtab$freq.missing[3] <- (nrow(mal[mal$claws_missing == 0 & mal$legs_missing != 0,]) /
                              nrow(mal) * 100)
freqtab$freq.missing[4] <- (nrow(mal[mal$claws_missing != 0 & mal$legs_missing != 0,]) /
                              nrow(mal) * 100)

# Frequency of females missing limbs
freqtab$freq.missing[5] <- (nrow(fem[fem$claws_missing == 0 & fem$legs_missing == 0,]) /
                              nrow(fem) * 100)
freqtab$freq.missing[6] <- (nrow(fem[fem$claws_missing != 0 & fem$legs_missing == 0,]) /
                              nrow(fem) * 100)
freqtab$freq.missing[7] <- (nrow(fem[fem$claws_missing == 0 & fem$legs_missing != 0,]) /
                              nrow(fem) * 100)
freqtab$freq.missing[8] <- (nrow(fem[fem$claws_missing != 0 & fem$legs_missing != 0,]) /
                              nrow(fem) * 100)

# Overall frequency
freqtab$freq.over <- freqtab$freq.sex * (freqtab$freq.missing / 100)
# Should equal 100%
sum(freqtab$freq.over)

#### Kernel density estimate of weight-based frequency, males ####
min.male <- 15
max.male <- 160
# Kernel density estimation of weight distribution, males
kd.m0 <- kde.boundary(mal[mal$claws_missing == 0 & mal$legs_missing == 0,]$weight_g,
                      xmin=min.male, 
                      xmax=max.male,
                      #eval.points=seq(from=15,to=160,by=0.5),
                      boundary.kernel="beta")

ggplot() +
  geom_line(data=as.data.frame(cbind(kd.m0$eval.points, kd.m0$estimate)),
            aes(x=V1, y=V2)) +
  xlim(0,160) +
  xlab('Weight (g)') +
  ylab('Frequency') +
  annotate("text", x = 0, y = 0, 
           label = paste0('n = ',
                          nrow(mal[mal$claws_missing == 0 & mal$legs_missing == 0,]))) +
  ggtitle('Intact males')

kd.mc <- kde.boundary(mal[mal$claws_missing != 0 & mal$legs_missing == 0,]$weight_g,
                      xmin=min.male, 
                      xmax=max.male,
                      #eval.points=seq(from=15,to=160,by=0.5),
                      boundary.kernel="beta")
ggplot() +
  geom_line(data=as.data.frame(cbind(kd.mc$eval.points, kd.mc$estimate)),
            aes(x=V1, y=V2)) +
  xlim(0,160) +
  xlab('Weight (g)') +
  ylab('Frequency') +
  annotate("text", x = 0, y = 0, 
           label = paste0('n = ',
                          nrow(mal[mal$claws_missing != 0 & mal$legs_missing == 0,]))) +
  ggtitle('Missing claw(s) males')

kd.ml <- kde.boundary(mal[mal$claws_missing == 0 & mal$legs_missing != 0,]$weight_g,
                      xmin=min.male, 
                      xmax=max.male,
                      #eval.points=seq(from=15,to=160,by=0.5),
                      boundary.kernel="beta")
ggplot() +
  geom_line(data=as.data.frame(cbind(kd.ml$eval.points, kd.ml$estimate)),
            aes(x=V1, y=V2)) +
  xlim(0,160) +
  xlab('Weight (g)') +
  ylab('Frequency') +
  annotate("text", x = 0, y = 0, 
           label = paste0('n = ',
                          nrow(mal[mal$claws_missing == 0 & mal$legs_missing != 0,]))) +
  ggtitle('Missing leg(s) males')

kd.mcl <- kde.boundary(mal[mal$claws_missing != 0 & mal$legs_missing != 0,]$weight_g,
                       xmin=min.male, 
                       xmax=max.male,
                       #eval.points=seq(from=15,to=160,by=0.5),
                       boundary.kernel="beta")
ggplot() +
  geom_line(data=as.data.frame(cbind(kd.mcl$eval.points, kd.mcl$estimate)),
            aes(x=V1, y=V2)) +
  xlim(0,160) +
  xlab('Weight (g)') +
  ylab('Frequency') +
  annotate("text", x = 0, y = 0, 
           label = paste0('n = ',
                          nrow(mal[mal$claws_missing != 0 & mal$legs_missing != 0,]))) +
  ggtitle('Missing claw(s) and leg(s) males')

#### Kernel density estimate of weight-based frequency, females ####
min.female <- 10
max.female <- 80
# Kernel density estimation of weight distribution, males
kd.f0 <- kde.boundary(fem[fem$claws_missing == 0 & fem$legs_missing == 0,]$weight_g,
                      xmin=min.female, 
                      xmax=max.female,
                      #eval.points=seq(from=15,to=160,by=0.5),
                      boundary.kernel="beta")

ggplot() +
  geom_line(data=as.data.frame(cbind(kd.f0$eval.points, kd.f0$estimate)),
            aes(x=V1, y=V2)) +
  xlim(0,160) +
  xlab('Weight (g)') +
  ylab('Frequency') +
  annotate("text", x = 0, y = 0, 
           label = paste0('n = ',
                          nrow(fem[fem$claws_missing == 0 & fem$legs_missing == 0,]))) +
  ggtitle('Intact females')

#### Integrate area under curve for bins of 1g, males ####
# Initialize
m0.df <- data.frame(
  min.weight=seq(15, 160,1),
  freq=rep(NA, length(seq(15, 160,1)))
)
allm.df <- rbind(m0.df, m0.df, m0.df, m0.df)
allm.df$status <- c(rep('intact', 146),
                    rep('miss.claw', 146),
                    rep('miss.leg', 146),
                    rep('miss.both', 146))

m.mods <- list(kd.m0, kd.mc, kd.ml, kd.mcl)

for(i in 1:length(m.mods)){
  m.xx <- m.mods[[i]]$eval.points
  m.dx <- m.xx[2L] - m.xx[1L]
  m.yy <- m.mods[[i]]$estimate
  m.f <- approxfun(m.xx, m.yy)
  for(j in seq(1, 146)*i){
    allm.df$freq[j] <- integrate(m.f,
                                 allm.df$min.weight[j*i],
                                 allm.df$min.weight[(j+1)*i])$value
    if(j %in% c(146, 292, 438, 584)){
      allm.df$freq[j] <- NA
    }
  }
}


m0.xx <- kd.m0$eval.points
m0.dx <- m0.xx[2L] - m0.xx[1L]
m0.yy <- kd.m0$estimate
m0.f <- approxfun(m0.xx, m0.yy)
for(i in 1:(nrow(m0.df)-1)){
  m0.df$freq[i] <- integrate(m0.f, 
                             m0.df$min.weight[i], m0.df$min.weight[(i+1)])$value
}

# Add weight share
m0.df$wt.share <- ((freqtab$freq.over[1]/100) * 643.194) * (m0.df$freq)
# Add abundance at weight
m0.df$abund <- m0.df$wt.share / (m0.df$min.weight/1000)
# Round to integer
m0.df$abund <- round(m0.df$abund)
# Remove NA
m0.df <- m0.df[complete.cases(m0.df),]
# Total
sum(m0.df$abund)
