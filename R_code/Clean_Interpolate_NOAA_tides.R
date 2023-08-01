# Combine monthly 6min-interval tidal data to one dataframe
# Find position along ebb/flow by water level
# Where 0 = low tide and 1 = high tide

rm(list=ls())

# Load libraries
library(here)
library(tidyverse)
library(VulnToolkit)

# Read files
file_list <- list.files(here('Raw_Data/portland_tides'))

# Set first file name==1
file=1

# Set up temporary index of all files in working directory, read first file
temp <- read.csv(here(paste0('Raw_Data/portland_tides/', file_list[[file]])))

# For loop: reads in all data in files seen in working directory
for (file in 2:length(file_list)){
  # Read next file's information
  dataset <- read.csv(here(paste0('Raw_Data/portland_tides/', file_list[[file]])))
  # Append this information to dataframe with info from previous files
  temp <- rbind(temp, dataset)
  # End loop
}

# Remove intermediate
rm(dataset)

# Convert time to posixct
temp$TimeUTC <- as.POSIXct(temp$TimeUTC, 
                           format="%Y-%m-%d %H:%M:%S")

# Keep only time and tide height
temp <- dplyr::select(temp, TimeUTC, TideHT.m)

# Re-format time to local time (EST/EDT depending on date)
# Function courtesy Josh Pachner
mtn_ts = function(utcTime){
  library(lubridate)
  toTz = "America/New_York"
  utcTime = force_tz(utcTime,tzone= 'UTC')
  dt = as.POSIXct(format(utcTime,tz = toTz,origin ='UTC', usetz=TRUE))
  dt = force_tz(dt,tzone= toTz)
  return(dt)
}
# Force to Eastern Time
temp$TimeLocal <- mtn_ts(temp$TimeUTC)

# Add month, year, day, hour columns
temp$year <- year(temp$TimeLocal)
temp$month <- month(temp$TimeLocal)
temp$month <- paste0('0', temp$month)
temp$day <- day(temp$TimeLocal)
temp$day[temp$day < 10] <- str_pad(temp$day[temp$day<10],
                                   2, 'left', '0')
temp$hour <- hour(temp$TimeLocal)
temp$hour[temp$hour < 10] <- str_pad(temp$hour[temp$hour<10],
                                   2, 'left', '0')

# Find deltas
temp$delta <- NA
for(i in 2:nrow(temp)){
  temp$delta[i] <- temp$TideHT.m[i] - temp$TideHT.m[(i-1)]
}

# Find instances of high and low tide
# Load function (courtesy Troy Hill)
source(here('R_code/utilities/extract_tides.R'))
# Extract tides
tides <-  HL(level = temp$TideHT.m, 
             time = temp$TimeLocal)

# Merge tides to dataset
colnames(tides) <- c('TideHT.m', 'TimeLocal', 'tide.stage')
test <- left_join(temp, tides, by=c('TimeLocal', 'TideHT.m'))

# Find first instance of tidal extreme (high or low)
row_index <- which(test$tide.stage %in% c('H', 'L'), arr.ind = TRUE)[1]

# Remove rows prior to first tidal extreme (high or low)
test <- test[row_index:nrow(test),]
rownames(test) <- NULL

# Find last instance of tidal extreme (high or low)
row_index <- which(test$tide.stage %in% c('H', 'L'), arr.ind = TRUE)
row_index <- row_index[length(row_index)]

# Remove rows after last tidal extreme (high or low)
test <- test[1:row_index,]
rownames(test) <- NULL

# Assign names to each ebb/flow tide
test$tidename <- NA
for(i in 1:nrow(test)){
  if(test$tide.stage[i] %in% c('H', 'L') &
     !is.na(test$tide.stage[i])){
    test$tidename[i] <- paste0(test$year[i], "_",
                                test$month[i], "_",
                                test$day[i], "_",
                                test$hour[i], "_",
                                test$tide.stage[i])
  }
}

# Copy down through ebb/flow
test$tidename <- zoo::na.locf(test$tidename)

# Clean (remove instances where tide delta does not match ebb/flood stage)
test.list <- split(test, f=test$tidename)
for(i in 1:(length(test.list)-1)){
  temp <- test.list[[i]]
  
  for(j in 2:nrow(temp)){
    # Remove instance where water level increases during ebb tide
    if(temp$tide.stage[1] == 'H' & temp$delta[j] > 0){
      temp$delta[j] <- NA
    }
    # Remove instances where water level decreases during flow tide
    if(temp$tide.stage[1]== 'L' & temp$delta[j] < 0){
      temp$delta[j] <- NA
    }
  }
  
  test.list[[i]] <- temp
}

# Re-bind to dataframe
test <- do.call(rbind, test.list)

# Remove rows with missing deltas
test <- test[!is.na(test$delta),]
rownames(test) <- NULL

# Add numeric tide stage
test$tide.num <- NA
test$tide.num[test$tide.stage == "H"] <- 1
test$tide.num[test$tide.stage == "L"] <- 0

# Split by ebb/flow
tide.list <- split(test, f=test$tidename)

# Interpolate numeric advancement through tide for each stage by water height
for(i in 1:(length(tide.list)-1)){
  # Determine tidal period
  temp$period <- difftime(temp$TimeLocal[nrow(temp)],
                          temp$TimeLocal[1],
                          units='secs')
  temp$period <- as.numeric(temp$period)
  temp$period <- temp$period / 60 / 60
  
  # Pull out tidal stage
  temp <- tide.list[[i]]

  # Find tidal amplitude
  amp <- abs(temp$TideHT.m[1] - temp$TideHT.m[nrow(temp)])
  
  # Find delta
  temp$delta <- NA
  temp$delta[1] <- 0
  for(j in 2:nrow(temp)){
    temp$delta[j] <- abs(temp$TideHT.m[j-1] - temp$TideHT.m[j])
  }
  
  temp$delta2 <- temp$delta / amp
  
  # Convert to numeric tide shift
  temp$tide.num2 <- NA
  if(temp$tide.num[1]==1){
    temp$tide.num2[1] <- 1
    for(j in 2:nrow(temp)){
      temp$tide.num2[j] <- temp$tide.num2[(j-1)] - temp$delta2[j]
    }
  }
  
  if(temp$tide.num[1]==0){
    temp$tide.num2[1] <- 0
    for(j in 2:nrow(temp)){
      temp$tide.num2[j] <- temp$tide.num2[(j-1)] + temp$delta2[j]
    }
  }
  
  # Save
  tide.list[[i]] <- temp
}

# Remove last tide
tide.list <- tide.list[-length(tide.list)]

# Rebind
tides <- do.call(rbind, tide.list)
rownames(tides) <- NULL
tides <- unique(tides)
negs <- tides[tides$tide.num2 < 0,]
pos <- tides[tides$tide.num2 > 1,]
tides <- tides[tides$tide.num2 < 1 & tides$tide.num2 > 0,]

# Remove intermediates
rm(negs, pos, temp, test.list, tide.list, test, amp, file, file_list,
   i, j, row_index)

# Split into list by year
year.list <- split(tides, f=tides$year)

# Within each year, interpolate by minute (we skip non-summer months)
for(i in 1:length(year.list)){
  temp <- year.list[[i]]
  temp$TimeUTC <- NULL
  temp <- temp %>% 
    pad(interval='min') %>% 
    mutate(tide.num2 = na.approx(tide.num2),
           TideHT.m = na.approx(TideHT.m))
  temp <- dplyr::select(temp, TimeLocal, TideHT.m, tidename, tide.num2)
  temp$tidename <- na.locf(temp$tidename)
  year.list[[i]] <- temp
}

# Rebind to dataframe
tides <- do.call(rbind, year.list)
rownames(tides) <- NULL

# Re-add time stuff
tides$month <- month(tides$TimeLocal)
tides$day <- day(tides$TimeLocal)
tides$year <- year(tides$TimeLocal)

# Check with plot
plot(tides$tide.num2[tides$year==2014 & tides$month==5 & tides$day%in%c(1,2,3)])

# Save
write.csv(tides,
          here('Raw_Data/Portland_tides_byminute.csv'),
          row.names = F)
