rm(list=ls())

# Libraries
library(tidyverse)

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

#### Exploration ####
summary(crabs)
# Sex-based
table(crabs$sex)
table(crabs$sex, crabs$legs_missing)
table(crabs$sex, crabs$claws_missing)
# Leg-based
table(crabs$legs_missing)
table(crabs$legs_missing, crabs$claws_missing)

#### Executive decisions ####
# Cannot proceed with female-specific modeling, too few.


#### Sex frequency ####
round(nrow(crabs[crabs$sex == 'm',]) / nrow(crabs) * 100, 2)
round(nrow(crabs[crabs$sex == 'f',]) / nrow(crabs) * 100, 2)

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

# Kernel density estimation of weight distribution, males
kd.m0 <- bde(mal[mal$claws_missing == 0 & mal$legs_missing == 0,]$weight_g,
             estimator="boundarykernel", lower.limit=15, upper.limit = 160,
             dataPointsCache = seq(15,160))

plot(kd.m0)
kd.mc <- bde(mal[mal$claws_missing != 0 & mal$legs_missing == 0,]$weight_g,
             estimator="boundarykernel", lower.limit=0, upper.limit = 160)
plot(kd.mc)
kd.ml <- bde(mal[mal$claws_missing == 0 & mal$legs_missing != 0,]$weight_g,
             estimator="boundarykernel", lower.limit=0, upper.limit = 160)
plot(kd.ml)
kd.mcl <- bde(mal[mal$claws_missing != 0 & mal$legs_missing != 0,]$weight_g,
              estimator="boundarykernel", lower.limit=0, upper.limit = 160)
plot(kd.mcl)

# Kernel density estimation of weight distribution, females
kd.f0 <- bde(fem[fem$claws_missing == 0 & fem$legs_missing == 0,]$weight_g,
             estimator="boundarykernel", lower.limit=0, upper.limit = 160)
plot(kd.f0)
kd.fc <- bde(fem[fem$claws_missing != 0 & fem$legs_missing == 0,]$weight_g,
             estimator="boundarykernel", lower.limit=0, upper.limit = 160)
# Cannot estimate weight dist for females with mising claws, too few
kd.fl <- bde(fem[fem$claws_missing == 0 & fem$legs_missing != 0,]$weight_g,
             estimator="boundarykernel", lower.limit=0, upper.limit = 160)
# Cannot estimate weight dist for females with missing legs, too few
kd.fcl <- bde(fem[fem$claws_missing != 0 & fem$legs_missing != 0,]$weight_g,
              estimator="boundarykernel", lower.limit=0, upper.limit = 160)
# Cannot estimate weight dist for females with missing claws and legs, too few

# Traps subsampled generated 1418 lbs, 643.194 kg
# So, 643.194 * freqtab$freq.over[freq$sex=='m' & freq$legs_missing == 'no' & 
# freq$claws_missing == 'no',] == 1060.306 lbs, 480.9469 kg from intact males.
# Estimate frequency at each 1g bin
# Will be area under curve for each histogram-style bin at intervals of 1 unit






xx <- kd.m0$eval.points  
dx <- xx[2L] - xx[1L]  
yy <- kd.m0$estimate
f <- approxfun(xx, yy)
C <- integrate(f, 15, 160)$value

# Initialize
m0.df <- data.frame(
  min.weight=seq(15, 160,1),
  freq=rep(NA, length(seq(15, 160,1)))
)
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


#### Plots ####
# Weight, full crabs, sexed
ggplot(data=crabs[crabs$legs_missing == 0 &
                    crabs$claws_missing == 0,],
       aes(x=sex, y=weight_g)) +
  geom_boxplot() +
  xlab("Sex") +
  ylab('Weight (g)')

# Carapace width
ggplot(data=crabs[crabs$legs_missing == 0 &
                    crabs$claws_missing == 0,],
       aes(x=sex, y=carapace_width_mm)) +
  geom_boxplot() +
  xlab("Sex") +
  ylab('Carapace width (mm)')










# Length to weight, all crabs
ggplot(data=crabs, 
       aes(x=carapace_width_mm, y=weight_g)) +
  geom_point(aes(col=sex)) +
  geom_smooth() +
  ylim(20, 160) +
  xlim(40, 90) +
  xlab('Carapace width (mm)') + ylab('Weight (g)')

# Length to weight, only full crabs
ggplot(data=crabs[crabs$claws_missing == 0 & crabs$legs_missing == 0,],
       aes(x=carapace_width_mm, y=weight_g)) +
  geom_point(aes(col=sex)) +
  geom_smooth() +
  ylim(20, 160) +
  xlim(40, 90) +
  xlab('Carapace width (mm)') + ylab('Weight (g)')

# Length to weight, only full crabs, sexed
ggplot(data=crabs[crabs$claws_missing == 0 & crabs$legs_missing == 0,],
       aes(x=carapace_width_mm, y=weight_g)) +
  geom_point(aes(col=sex)) +
  facet_wrap(vars(sex)) +
  geom_smooth() +
  ylim(20, 160) +
  xlim(40, 90) +
  xlab('Carapace width (mm)') + ylab('Weight (g)')
# Not enough females to create sex-specific length-wt relationship

# Continue with only males
ggplot(data=crabs[crabs$claws_missing == 0 & crabs$legs_missing == 0 &
                    crabs$sex == 'm',],
       aes(x=carapace_width_mm, y=weight_g)) +
  geom_point(aes(col=sex)) +
  facet_wrap(vars(sex)) +
  geom_smooth() +
  ylim(20, 160) +
  xlim(40, 90) +
  xlab('Carapace width (mm)') + ylab('Weight (g)')
nrow(crabs[crabs$claws_missing == 0 & crabs$legs_missing == 0 &
             crabs$sex == 'm',])
