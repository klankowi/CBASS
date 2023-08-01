# Integrate tides to CBASS data

rm(list=ls())

# Load trip info
trips <- read.csv(here('Clean_Data/trips_cleaned.csv'))
head(trips)

# Load tides info
tides <- read.csv(here('Raw_Data/Portland_tides_byminute.csv'))
tides <- dplyr::select(tides, TimeLocal, TideHT.m, tide.num2)
colnames(tides)[1] <- 'set_time'
colnames(tides)[3] <- 'numeric.tide'

# Merge
str(trips)
trips$set_time <- as.POSIXct(trips$set_time,
                             format="%Y-%m-%d %H:%M:%S")
str(tides)
tides$set_time <- as.POSIXct(tides$set_time,
                             format="%Y-%m-%d %H:%M:%S")
test <- left_join(trips, tides, by=c())

head(test)

# Save
write.csv(test,
          here('Clean_Data/trips_cleaned_tides.csv'),
          row.names = F)
