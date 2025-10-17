rm(list=ls())

# Load packages
library(here)
library(tidyverse)
library(noaaoceans)
library(suntools)
#library(tomorrowior)
library(zoo)
#library(kableExtra)
#library(gridExtra)
library(readxl)

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
                plot.caption=element_text(hjust=0, face='italic', size=12)))

#### Load logger data ####
# Load from Downloads folder
foul <- read_excel('C:/Users/Katie/Downloads/6 buofouled 2025-06-25 09_10_09 EDT (Data EDT).xlsx')
clean <- read_excel('C:/Users/Katie/Downloads/6 buofouled 2025-06-25 09_10_09 EDT (Data EDT).xlsx')

# Clean Biofoul bag data
foul <- foul %>% 
  # Rename date, temperature, and light columns to not contain spaces
  rename(Date = `Date-Time (EDT)`,
         Temperature = `Temperature , °C`,
         Light = `Light , lux`) %>% 
  # Remove instances where data were not recorded
  filter(!is.na(Temperature) &
           !is.na(Light)) %>% 
  # Format date variable so R understands it as a date
  mutate(Date = as.POSIXct(Date,
                           format='%Y-%m-%d %H:%M:%S',
                           tz='America/New_York')) %>%
  # It keeps defaulting to UTC, so add 4 hours to get Eastern Time
  mutate(Date = Date + hours(4)) %>% 
  # Add name of treatment
  mutate(Treatment = 'foul') %>% 
  # Select only the columns that are useful to us
  dplyr::select(Treatment, Date, Temperature, Light) %>% 
  # Format as data frame
  as.data.frame()

# Clean up names of columns and stuff in Clean bag
clean <- clean %>% 
  # Rename date, temperature, and light columns to not contain spaces
  rename(Date = `Date-Time (EDT)`,
         Temperature = `Temperature , °C`,
         Light = `Light , lux`) %>% 
  # Remove instances where data were not recorded
  filter(!is.na(Temperature) &
           !is.na(Light)) %>% 
  # Format date variable so R understands it as a date
  mutate(Date = as.POSIXct(Date,
                           format='%Y-%m-%d %H:%M:%S',
                           tz='America/New_York')) %>%
  # It keeps defaulting to UTC, so add 4 hours to get Eastern Time
  mutate(Date = Date + hours(4)) %>% 
  # Add name of treatment
  mutate(Treatment = 'clean') %>% 
  # Select only the columns that are useful to us
  dplyr::select(Treatment, Date, Temperature, Light) %>% 
  # Format as data frame
  as.data.frame()

# Bind together
dat <- rbind(foul, clean)

# Arrange by timestamp and treatment
dat <- dat %>% 
  arrange(Date, Treatment)

#### Functions ####
#### Sunlight ####
get.suncondition.data <- function(x){
  
  ## Create vector of dates that covers the logger period
  # Starting date
  startdate <- as.Date(x$Date[1])
  # Ending date
  enddate <- as.Date(x$Date[nrow(x)])
  # Vector of all dates in between
  datevec <- seq(startdate, enddate, 1)
  
  # Sunrise time
  sunrises <- round_date(
    sunriset(
      matrix(c(-69.917007, 43.758762), nrow = 1),
      as.POSIXct(datevec+1, tz = "America/New_York"),
      direction = "sunrise",
      POSIXct.out = TRUE)$time,
    unit='1 minute'
  )
  
  # Sunset time
  sunsets <- round_date(
    sunriset(
      matrix(c(-69.917007, 43.758762), nrow = 1),
      as.POSIXct(datevec+1, tz = "America/New_York"),
      direction = "sunset",
      POSIXct.out = TRUE)$time,
    unit='1 minute'
  )
  
  # Merge into single dataset
  suncondition <-  c(sunrises, sunsets)
  
  # Order by time
  suncondition <- suncondition[order(suncondition)]
  
  # Convert to dataframe
  suncondition <- data.frame(
    Date = suncondition
  )
  
  # Assign crepuscular period
  suncondition$condition <- rep(c('sunrise', 'sunset'),
                                nrow(suncondition)/2)
  
  # Return to user on completion
  return(suncondition)
}

# Run the function to get data
sunlight <- get.suncondition.data(dat)

# Make sunlight data minute-by-minute
suncon <- seq.POSIXt(sunlight$Date[1], 
                     sunlight$Date[nrow(sunlight)],
                     by='min')

suncon <- data.frame(
  Date = suncon
)

# Merge the data and the minute dataframe
suncon <- left_join(suncon, sunlight, by=c("Date"))

# For every minute, carry down the previous observation of light conditions
suncon$condition <- zoo::na.locf(suncon$condition)

# Assign color for light/ dark conditions
suncon$shade <- NA
suncon$shade[suncon$condition == 'sunrise'] <- NA
suncon$shade[suncon$condition == 'sunset'] <- 'gray60'

#### Tides ####
get.tidal.data <- function(x){
  
  ## Create vector of dates centered on logger deployment
  # Start date
  startdate <- as.Date(x$Date[1])
  # End date
  enddate <- as.Date(x$Date[nrow(x)])
  # Sequence of all dates between
  datevec <- seq(startdate, enddate, 1)
  
  # Query and pull predicted tides at Portland Harbor gauge
  preds <- query_coops_data(
    station_id = 8418150,
    start_date=startdate,
    end_date = enddate,
    data_product = "water_level",
    units='english',
    time_zone = 'gmt',
    datum = 'MLLW'
  )
  
  # Convert data types to date and numeric
  preds$t <- as.POSIXct(preds$t,
                        format="%Y-%m-%d %H:%M",
                        tz='GMT')
  preds$t <- with_tz(preds$t, "America/New_York")
  preds$v <- as.numeric(preds$v)
  
  # Approximate data to fit Cundy's Harbor
  # NOAA Parameters:
  # HeightOffsetLow: * 0.98
  # HeightOffsetHigh: * 0.98
  # TimeOffsetLow: -2
  # TimeOffsetHigh: -1
  preds$t.cundy <- preds$t - 60
  preds$v.cundy <- preds$v * 0.98
  
  # Save Cundy's Harbor data alterations
  preds <- preds %>%
    dplyr::select(-t, -v, -station) %>%
    rename(time = t.cundy,
           tide.predicted = v.cundy)
  
  # Query and pull predicted High and Low times at Portland
  hilo <- query_coops_data(
    station_id = 8418150,
    start_date=startdate,
    end_date = enddate,
    data_product = "predictions",
    interval='hilo',
    units='english',
    time_zone = 'gmt',
    datum = 'MLLW'
  )
  
  # Convert datetime to posixCT
  hilo$t <- as.POSIXct(hilo$t,
                       format="%Y-%m-%d %H:%M",
                       tz='GMT')
  hilo$t <- with_tz(hilo$t, "America/New_York")
  
  # Approximate Cundy's Harbor hi-lo times
  hilo$t.cundy <- hilo$t - 60
  hilo$v.cundy <- as.numeric(hilo$v) * 0.98
  
  # Save Cundy's Harbor altered data
  hilo <- hilo %>%
    dplyr::select(-t, -v, -station) %>%
    rename(time = t.cundy,
           tide.predicted=v.cundy)
  
  # Merge
  preds <- merge(preds, hilo, all=T)
  
  # Find and fill missing tidal stage values
  preds <- preds %>%
    mutate(prev_val = (type),
           next_val = (type)) %>%
    fill(prev_val, .direction = "down") %>%
    fill(next_val, .direction = "up") %>%
    mutate(stage = ifelse(!is.na(prev_val) & !is.na(next_val) &
                            prev_val == next_val, type, NA)) %>%
    mutate(stage = ifelse(!is.na(prev_val) & !is.na(next_val) &
                            prev_val == 'H' &
                            next_val == 'L', 'falling', stage)) %>%
    mutate(stage = ifelse(!is.na(prev_val) & !is.na(next_val) &
                            prev_val == 'L' &
                            next_val == 'H', 'rising', stage)) %>%
    mutate(stage = ifelse(is.na(prev_val) & !is.na(next_val) &
                            next_val == 'H', 'rising', stage)) %>%
    mutate(stage = ifelse(is.na(prev_val) & !is.na(next_val) &
                            next_val == 'L', 'falling', stage)) %>%
    mutate(stage = ifelse(!is.na(prev_val) & is.na(next_val) &
                            prev_val == 'H', 'falling', stage)) %>%
    mutate(stage = ifelse(!is.na(prev_val) & is.na(next_val) &
                            prev_val == 'L', 'falling', stage)) %>%
    select(-prev_val, -next_val, -type)
  
  # Remove unnecessary columns
  preds <- dplyr::select(preds, -s, -f, -q)
  
  # Return to user on completion
  return(preds)
}

tides <- get.tidal.data(dat)

# Cut to times we have HOBO data for
tides <- tides %>% 
  filter(time >= as.POSIXct('2025-06-19 07:20:00') &
           time <= as.POSIXct('2025-06-25 09:10:00')) %>% 
  rename(Date = time)

#### Air temp and wind ####
# Load weather station data (pulled from our ambientweather account)
temp <- read.csv(here("AmbientWeather_StationData/Bethel_June.csv"))

# Clean data
temp <- temp %>%
  # Format date to so R understands it as a date
  mutate(Date = as.POSIXct(Simple.Date,
                           format='%Y-%m-%d %H:%M:%S')) %>% 
  # Rename column names to not have spaces
  rename(Temp = Outdoor.Temperature...F.,
         Wind = Wind.Speed..mph.,
         WindDir = Wind.Direction....,
         Rain = Rain.Rate..in.hr.) %>% 
  # Keep only pertinent information
  dplyr::select(Date, Temp, Wind, WindDir, Rain) %>% 
  # COnvert fahrenheit to celsius (may have to install weathermetrics pkg)
  mutate(Temp = weathermetrics::fahrenheit.to.celsius(Temp)) %>% 
  # Sort by date
  arrange(Date) %>%
  # Format as data frame
  as.data.frame()

# Cut to times we have HOBO data for
temp <- temp %>% 
  filter(Date >= as.POSIXct('2025-06-19 07:20:00') &
           Date <= as.POSIXct('2025-06-25 09:10:00'))

#### Combine ####
# Round logger data to minute
dat$Date <- round_date(dat$Date, unit='minute')

# Create minute vector of period of interest
dateseq <- seq.POSIXt(from = as.POSIXct('2025-06-19 07:23:00'),
                      to = as.POSIXct('2025-06-25 09:05:00'),
                      by='min')

# Combine available environmental data
dat.list <- data.frame(
  Date = dateseq)

# Merge logger data and tides
dat.list <- left_join(dat.list, tides, by=c('Date'))

# Merge logger data and weather station data
dat.list <- left_join(dat.list, temp, by=c('Date'))

# Rename columns so difference between air and water temp is clear
dat.list <- dat.list %>% rename(AirTemp = Temp)

# Estimate water levels per minute (interpolating from 6-minute NOAA data)
dat.list <- dat.list%>% 
  mutate(tide.interp = zoo::na.approx(tide.predicted),
         stage.interp = zoo::na.locf(stage))

# Cut to data we have weather station data for
dat.list <- dat.list %>% 
  filter(Date >= as.POSIXct('2025-06-19 07:23:00') &
           Date <= as.POSIXct('2025-06-25 08:53:00'))

# Estimate meterological data per minute (interpolating from 10-min data)
dat.list <- dat.list %>% 
  mutate(AirTemp.interp = zoo::na.approx(AirTemp),
         Wind.interp = zoo::na.approx(Wind),
         WindDir.interp = zoo::na.approx(WindDir),
         Rain.interp = zoo::na.approx(Rain)
  )

# Save only pertinent variables
dat.list <- dat.list %>% 
  dplyr::select(Date, stage.interp, tide.interp, AirTemp.interp,
                Wind.interp, WindDir.interp, Rain.interp)

# Round timestamps for logger data to minute
foul$Date <- round_date(foul$Date, unit='minute')

# Join foul HOBO data to dataframe
dat.foul <- left_join(dat.list, foul, by=c('Date'))

# Cut to times we have HOBO data for
dat.foul <- dat.foul[dat.foul$Date >= as.POSIXct('2025-06-19 07:25:00') &
                       dat.foul$Date <= as.POSIXct('2025-06-25 08:50:00'),]

# Interpolate to minute
dat.foul <- dat.foul %>% 
  mutate(Temp.interp = zoo::na.approx(Temperature),
         Light.interp = zoo::na.approx(Light),
         Treatment = zoo::na.locf(Treatment)) %>% 
  dplyr::select(-Temperature, -Light)

# Round clean HOBO logger data to minute
clean$Date <- round_date(clean$Date, unit='minute')

# Join clean HOBO data to dataframe
dat.clean <- left_join(dat.list, clean, by=c('Date'))

# Cut to times we have HOBO data for
dat.clean <- dat.clean[dat.clean$Date >= as.POSIXct('2025-06-19 07:25:00') &
                         dat.clean$Date <= as.POSIXct('2025-06-25 08:50:00'),]

# Interpolate to minute
dat.clean <- dat.clean %>% 
  mutate(Temp.interp = zoo::na.approx(Temperature),
         Light.interp = zoo::na.approx(Light),
         Treatment = zoo::na.locf(Treatment)) %>% 
  dplyr::select(-Temperature, -Light)

# Combine all data
dat.all <- rbind(dat.foul, dat.clean)

# Remove intermediates
rm(clean, dat, dat.clean, dat.foul,
   dat.list, foul, sunlight, tides, temp,
   dateseq)

#### Plot ####
# Pivot for easier plotting
dat.lf <- dat.all %>% 
  group_by(Treatment) %>% 
  pivot_longer(cols=c('tide.interp', 'AirTemp.interp', 'Wind.interp', 
                      'WindDir.interp', 'Rain.interp',
                      'Temp.interp', 'Light.interp'),
               names_to = 'Variable') %>% 
  as.data.frame()

# Cut to period we have HOBO data for
suncon <- suncon[suncon$Date >= as.Date('2025-06-19'),]

# Join sunlight conditions to dataframe
dat.lf <- left_join(dat.lf, suncon, by=c("Date"))
dat.lf$condition[dat.lf$condition == 'sunrise'] <- NA

# Remove data from before we deployed HOBO
dat.lf <- dat.lf[dat.lf$Date >=
                   as.POSIXct('2025-06-19 15:30:00',
                              tz='America/New_York'),]

Airtemp <- ggplot(data=dat.lf[ dat.lf$Date >= as.Date('2025-06-19') &
                                 
                                 dat.lf$Variable == 'AirTemp.interp' &
                                 dat.lf$Treatment== 'foul',]) +
  geom_rect(aes(xmin = Date, 
                xmax = lead(Date), 
                ymin = -Inf, ymax = Inf,
                fill = condition),
            alpha=0.2) +
  scale_fill_manual(values = c("gray60", "#FFFFFF00"),
                    na.value = 'transparent') +
  guides(fill=FALSE) +
  geom_line(aes(x=Date, 
                y=value)) +
  facet_wrap(~Variable, scales='free_y') +
  theme(axis.text.x = element_blank(),
        axis.title.x = element_blank(),
        axis.ticks.x = element_blank())+
  scale_x_datetime(breaks='2 days')

Lux <- ggplot(data=dat.lf[dat.lf$Variable == 'Light.interp' &
                            dat.lf$Treatment == 'foul',]) +
  geom_rect(aes(xmin = Date, 
                xmax = lead(Date), 
                ymin = -Inf, ymax = Inf,
                fill = condition),
            alpha=0.2) +
  scale_fill_manual(values = c("gray60", "#FFFFFF00"),
                    na.value = 'transparent') +
  guides(fill=FALSE) +
  geom_line(aes(x=Date, 
                y=value)) +
  facet_wrap(~Variable, scales='free_y') +
  theme(axis.text.x = element_blank(),
        axis.title.x = element_blank(),
        axis.ticks.x = element_blank())+
  scale_x_datetime(breaks='2 days')

Rain <- ggplot(data=dat.lf[ 
  
  dat.lf$Variable == 'Rain.interp' &
    dat.lf$Treatment== 'foul',]) +
  geom_rect(aes(xmin = Date, 
                xmax = lead(Date), 
                ymin = -Inf, ymax = Inf,
                fill = condition),
            alpha=0.2) +
  scale_fill_manual(values = c("gray60", "#FFFFFF00"),
                    na.value = 'transparent') +
  guides(fill=FALSE) +
  geom_line(aes(x=Date, 
                y=value)) +
  facet_wrap(~Variable, scales='free_y') +
  theme(axis.text.x = element_blank(),
        axis.title.x = element_blank(),
        axis.ticks.x = element_blank())+
  scale_x_datetime(breaks='2 days')

Temp <- ggplot(data=dat.lf[ dat.lf$Date >= as.Date('2025-06-19') &
                              
                              dat.lf$Variable == 'Temp.interp' #&
                            #dat.lf$Site == 'Garrison'
                            ,]) +
  geom_rect(data=dat.lf[ dat.lf$Date >= as.Date('2025-06-19') &
                           
                           dat.lf$Variable == 'Temp.interp' &
                           dat.lf$Treatment == 'foul'
                         ,],
            aes(xmin = Date, 
                xmax = lead(Date), 
                ymin = -Inf, ymax = Inf,
                fill = condition),
            alpha=0.2) +
  scale_fill_manual(values = c("gray60", "#FFFFFF00"),
                    na.value = 'transparent') +
  guides(fill=FALSE) +
  geom_line(aes(x=Date, 
                y=value,
                col=Treatment)) +
  facet_wrap(~Variable, scales='free_y')+
  scale_x_datetime(breaks='2 days')

Tide <- ggplot(data=dat.lf[ dat.lf$Date >= as.Date('2025-06-19') &
                              
                              dat.lf$Variable == 'tide.interp' &
                              dat.lf$Treatment== 'foul',]) +
  geom_rect(aes(xmin = Date, 
                xmax = lead(Date), 
                ymin = -Inf, ymax = Inf,
                fill = condition),
            alpha=0.2) +
  scale_fill_manual(values = c("gray60", "#FFFFFF00"),
                    na.value = 'transparent') +
  guides(fill=FALSE) +
  geom_line(aes(x=Date, 
                y=value)) +
  facet_wrap(~Variable, scales='free_y') +
  theme(axis.text.x = element_blank(),
        axis.title.x = element_blank(),
        axis.ticks.x = element_blank())+
  scale_x_datetime(breaks='2 days')

Wind <- ggplot(data=dat.lf[ dat.lf$Date >= as.Date('2025-06-19') &
                              
                              dat.lf$Variable == 'Wind.interp' &
                              dat.lf$Treatment== 'foul',]) +
  geom_rect(aes(xmin = Date, 
                xmax = lead(Date), 
                ymin = -Inf, ymax = Inf,
                fill = condition),
            alpha=0.2) +
  scale_fill_manual(values = c("gray60", "#FFFFFF00"),
                    na.value = 'transparent') +
  guides(fill=FALSE) +
  geom_line(aes(x=Date, 
                y=value)) +
  facet_wrap(~Variable, scales='free_y') +
  theme(axis.text.x = element_blank(),
        axis.title.x = element_blank(),
        axis.ticks.x = element_blank())+
  scale_x_datetime(breaks='2 days')

WindDir <- ggplot(data=dat.lf[ dat.lf$Date >= as.Date('2025-06-19') &
                                 
                                 dat.lf$Variable == 'WindDir.interp' &
                                 dat.lf$Treatment== 'foul',]) +
  geom_rect(aes(xmin = Date, 
                xmax = lead(Date), 
                ymin = -Inf, ymax = Inf,
                fill = condition),
            alpha=0.2) +
  scale_fill_manual(values = c("gray60", "#FFFFFF00"),
                    na.value = 'transparent') +
  scale_y_continuous(breaks=c(0, 90, 180, 270, 360),
                     labels =c('N', 'E', 'S', 'W', 'N')) +
  guides(fill=FALSE) +
  geom_line(aes(x=Date, 
                y=value)) +
  facet_wrap(~Variable, scales='free_y') +
  theme(axis.text.x = element_blank(),
        axis.title.x = element_blank(),
        axis.ticks.x = element_blank()) +
  scale_x_datetime(breaks='2 days')

ggsave(plot=
         egg::ggarrange(Airtemp, Tide, 
                        Wind, WindDir, 
                        Rain, Lux, 
                        Temp,
                        ncol=1
         ),
       here('Megan_SensorPlot.png'),
       height=8.5, width = 11, units = 'in')
