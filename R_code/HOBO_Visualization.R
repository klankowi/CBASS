rm(list=ls())

library(here)
library(tidyverse)
library(noaaoceans)
library(suntools)
library(tomorrowior)
library(here)
library(zoo)
library(kableExtra)
library(gridExtra)

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
long <- read.csv('AmbientWeather_StationData/Long_Logger2.csv')
garrison <- read.csv('AmbientWeather_StationData/Garrison_Logger2.csv')
orrs <- read.csv('AmbientWeather_StationData/Orr_Logger2.csv')
snow <- read.csv('AmbientWeather_StationData/Snow_Logger2.csv')
stovers <- read.csv('AmbientWeather_StationData/Stovers_Logger.csv')
lowell <- read.csv('AmbientWeather_StationData/Lowell_Logger2.csv')

long$Site <- 'Long'
snow$Site <- 'Snow'
orrs$Site <- 'Orrs'
garrison$Site <- 'Garrison'
stovers$Site <- 'Stovers'
lowell$Site <- 'Lowell'

long <- long %>% 
  filter(!is.na(Temperature.....C.)) %>% 
  dplyr::select(-X., -Started, -Host.Connected, -Button.Down, -Button.Up,
                -End.of.File)
colnames(long) <- c('Date', 'Temp', 'Site')
long <- long %>% 
  mutate(Date = as.POSIXct(Date, format='%m/%d/%Y %H:%M:%S')) %>% 
  filter(Date >= as.POSIXct('2025-06-06 10:00:00'))

garrison <- garrison %>% 
  filter(!is.na(Temperature.....C.)) %>% 
  dplyr::select(-X., -Started, -Host.Connected, #-Button.Down, -Button.Up,
                -End.of.File)
colnames(garrison) <- c('Date', 'Temp', 'Site')
garrison <- garrison %>% 
  mutate(Date = as.POSIXct(Date, format='%m/%d/%Y %H:%M:%S')) %>% 
  filter(Date >= as.POSIXct('2025-06-12 14:00:00'))

lowell <- lowell %>% 
  filter(!is.na(Temperature.....C.)) %>% 
  dplyr::select(-X., -Started, -Host.Connected, #-Button.Down, -Button.Up,
                -End.of.File)
colnames(lowell) <- c('Date', 'Temp', 'Site')
lowell <- lowell %>% 
  mutate(Date = as.POSIXct(Date, format='%m/%d/%Y %H:%M:%S')) %>% 
  filter(Date >= as.POSIXct('2025-06-12 14:00:00'))

stovers <- stovers %>% 
  filter(!is.na(Temperature.....C.)) %>% 
  dplyr::select(-X., -Started, #-Host.Connected, 
                -Button.Down, -Button.Up,
                -End.of.File)
colnames(stovers) <- c('Date', 'Temp', 'Site')
stovers <- stovers %>% 
  mutate(Date = as.POSIXct(Date, format='%m/%d/%Y %H:%M:%S')) %>% 
  filter(Date >= as.POSIXct('2025-06-12 14:00:00'))

snow <- snow %>% 
  filter(!is.na(Temperature.....C.)) %>% 
  dplyr::select(-X., -Started, -Host.Connected, 
                #-Button.Down, -Button.Up,
                -End.of.File)
colnames(snow) <- c('Date', 'Temp', 'Site')
snow <- snow %>% 
  mutate(Date = as.POSIXct(Date, format='%m/%d/%Y %H:%M:%S')) %>% 
  filter(Date >= as.POSIXct('2025-06-06 10:00:00'))

orrs <- orrs %>% 
  filter(!is.na(Temperature.....C.)) %>% 
  dplyr::select(-X., -Started, -Host.Connected, -Button.Down, -Button.Up,
                -End.of.File)
colnames(orrs) <- c('Date', 'Temp', 'Lux', 'Site')
orrs <- orrs %>% 
  mutate(Date = as.POSIXct(Date, format='%m/%d/%Y %H:%M:%S')) %>% 
  filter(Date >= as.POSIXct('2025-06-06 10:00:00'))

dat <- rbind(long, snow, garrison, lowell, stovers)
dat$Lux <- NA
dat <- rbind(dat, orrs)

#dat$Date <- as.POSIXct(dat$Date,
#                       format='%m/%d/%Y %H:%M:%S')

# Arrange by timestamp
dat <- dat %>% 
  arrange(Date, Site)

# Remove latest data
dat <- dat %>% 
  filter(Date <= as.POSIXct('2025-06-25 15:00:00'))

#### Functions ####
#### Sunlight ####
get.suncondition.data <- function(x){
  
  # Create vector of dates that covers the logger period
  startdate <- as.Date(x$Date[1])
  enddate <- as.Date(x$Date[nrow(x)])
  datevec <- seq(startdate, enddate, 1)
  
  # Dawn: start of "civil twilight" before sunrise (sun 6deg below horizon)
  dawns <- round_date(
    crepuscule(
      matrix(c(-69.917007, 43.758762), nrow = 1),
      as.POSIXct(datevec+1, tz = "America/New_York"),
      solarDep = 6,
      direction = "dawn",
      POSIXct.out = TRUE)$time,
    unit='1 minute'
  )
  
  # Sunrise: self explanatory
  sunrises <- round_date(
    sunriset(
      matrix(c(-69.917007, 43.758762), nrow = 1),
      as.POSIXct(datevec+1, tz = "America/New_York"),
      direction = "sunrise",
      POSIXct.out = TRUE)$time,
    unit='1 minute'
  )
  
  # Sunset: self explanatory
  sunsets <- round_date(
    sunriset(
      matrix(c(-69.917007, 43.758762), nrow = 1),
      as.POSIXct(datevec+1, tz = "America/New_York"),
      direction = "sunset",
      POSIXct.out = TRUE)$time,
    unit='1 minute'
  )
  
  # Dusk: start of "civil twilight" after sunset. Sun 6deg below horizon.
  dusks <- round_date(
    crepuscule(
      matrix(c(-69.917007, 43.758762), nrow = 1),
      as.POSIXct(datevec+1, tz = "America/New_York"),
      solarDep = 6,
      direction = "dusk",
      POSIXct.out = TRUE)$time,
    unit='1 minute'
  )
  
  # Merge into single dataset
  suncondition <-  c(dawns, sunrises, sunsets, dusks)
  
  # Order by time
  suncondition <- suncondition[order(suncondition)]
  
  # Convert to dataframe
  suncondition <- data.frame(
    time = suncondition
  )
  
  # Assign crepuscular period
  suncondition$condition <- rep(c('dawn', 'sunrise', 'sunset', 'dusk'),
                                nrow(suncondition)/4)
  
  # Return to user on completion
  return(suncondition)
}

# Get data
sunlight <- get.suncondition.data(dat)
sunlight <- sunlight[sunlight$condition %in% c('sunrise', 'sunset'),]

suncon <- seq.POSIXt(sunlight$time[1], sunlight$time[nrow(sunlight)],
                     by='min')

suncon <- data.frame(
  time = suncon,
  condition = NA
)

suncon$condition[suncon$time %in%
                   sunlight$time[sunlight$condition == 'sunrise']] <- 'light'
suncon$condition[suncon$time %in%
                   sunlight$time[sunlight$condition == 'sunset']] <- 'dark'

suncon$condition <- zoo::na.locf(suncon$condition)
suncon$shade <- NA
suncon$shade[suncon$condition == 'light'] <- NA
suncon$shade[suncon$condition == 'dark'] <- 'gray60'

#### Tides ####
get.tidal.data <- function(x){
  
  # Create vector of dates centered on logger deployment
  # You can make this date vector as long as you want
  startdate <- as.Date(x$Date[1])
  enddate <- as.Date(x$Date[nrow(x)])
  datevec <- seq(startdate, enddate, 1)
  #startdate <- format(startdate,
  #                    "%m/%d/%Y")
  #enddate <- format(enddate,
  #                  "%m/%d/%Y")
  
  # Query and pull predicted tides at Portland Harbor gauge
  preds <- query_coops_data(
    station_id = 8418150,
    start_date=startdate,
    end_date = enddate,
    data_product = "predictions",
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
  
  # Return to user on completion
  return(preds)
}

tides <- get.tidal.data(dat)

tides <- tides %>% 
  filter(time >= as.POSIXct('2025-06-06 00:00:00') &
         time <= as.POSIXct('2025-06-25 15:00:00'))

#### Air temp and wind ####
temp <- read.csv(here("AmbientWeather_StationData/Bethel_June.csv"))

temp <- temp %>% 
  mutate(Date = as.POSIXct(Simple.Date,
                           format='%Y-%m-%d %H:%M:%S')) %>% 
  rename(Temp = Outdoor.Temperature...F.,
         Wind = Wind.Speed..mph.,
         WindDir = Wind.Direction....,
         Rain = Rain.Rate..in.hr.) %>% 
  dplyr::select(Date, Temp, Wind, WindDir, Rain) %>% 
  mutate(Temp = weathermetrics::fahrenheit.to.celsius(Temp)) %>% 
  arrange(Date) %>% 
  as.data.frame()

temp <- temp %>% 
  filter(Date >= as.POSIXct('2025-06-06 00:00:00') &
         Date <= as.POSIXct('2025-06-25 13:38:00'))

#### Combine ####
# Round logger data to flat minute
dat$Date <- round_date(dat$Date, unit='minute')

# Create minute vector of period of interest
dateseq <- seq.POSIXt(from = as.POSIXct('2025-06-06 00:05:00'),
                      to = as.POSIXct('2025-06-25 13:35:00'),
                      by='min')

tides$Date <- tides$time
# Combine available environmental data
dat.list <- data.frame(
  Date = dateseq)

dat.list <- left_join(dat.list, tides, by=c('Date'))
dat.list <- left_join(dat.list, temp, by=c('Date'))
dat.list <- dat.list %>% rename(AirTemp = Temp)
dat.list$time <- NULL

dat.list <- dat.list%>% 
  mutate(tide.interp = zoo::na.approx(tide.predicted),
         stage.interp = zoo::na.locf(stage))

dat.list <- dat.list %>% 
  filter(Date >= as.POSIXct('2025-06-06 00:08:00') &
         Date <= as.POSIXct('2025-06-25 13:23:00'))

dat.list <- dat.list %>% 
  mutate(AirTemp.interp = zoo::na.approx(AirTemp),
         Wind.interp = zoo::na.approx(Wind),
         WindDir.interp = zoo::na.approx(WindDir),
         Rain.interp = zoo::na.approx(Rain)
  )

dat.list <- dat.list %>% 
  dplyr::select(Date, stage.interp, tide.interp, AirTemp.interp,
                Wind.interp, WindDir.interp, Rain.interp)

snow$Date <- round_date(snow$Date, unit='minute')

dat.snow <- left_join(dat.list, snow, by=c('Date'))
dat.snow$Site <- 'Snow'
dat.snow <- dat.snow[dat.snow$Date >= as.POSIXct('2025-06-06 10:01:00') &
                     dat.snow$Date <= as.POSIXct('2025-06-25 13:06:00'),]

dat.snow <- dat.snow %>% 
  mutate(Temp.interp = zoo::na.approx(Temp)) %>% 
  dplyr::select(-Temp)

long$Date <- round_date(long$Date, unit='minute')
dat.long <- left_join(dat.list, long, by=c('Date'))
dat.long$Site <- 'Long'
dat.long <- dat.long[dat.long$Date >= as.POSIXct('2025-06-06 10:04:00') &
                       dat.long$Date <= as.POSIXct('2025-06-25 12:24:00'),]

dat.long <- dat.long %>% 
  mutate(Temp.interp = zoo::na.approx(Temp)) %>% 
  dplyr::select(-Temp)

lowell$Date <- round_date(lowell$Date, unit='minute')
dat.lowell <- left_join(dat.list, lowell, by=c('Date'))
dat.lowell$Site <- 'Lowell'
dat.lowell <- dat.lowell[dat.lowell$Date >= as.POSIXct('2025-06-12 14:01:00') &
                           dat.lowell$Date <= as.POSIXct('2025-06-25 11:41:00'),]

dat.lowell <- dat.lowell %>% 
  mutate(Temp.interp = zoo::na.approx(Temp)) %>% 
  dplyr::select(-Temp)

garrison$Date <- round_date(garrison$Date, unit='minute')
dat.garrison <- left_join(dat.list, garrison, by=c('Date'))
dat.garrison$Site <- 'Garrison'
dat.garrison <- dat.garrison[dat.garrison$Date >= as.POSIXct('2025-06-12 14:04:00') &
                               dat.garrison$Date <= as.POSIXct('2025-06-25 11:04:00'),]

dat.garrison <- dat.garrison %>% 
  mutate(Temp.interp = zoo::na.approx(Temp)) %>% 
  dplyr::select(-Temp)

orrs$Date <- round_date(orrs$Date, unit='minute')
orrs$Lux[orrs$Lux > 30000] <- NA
dat.orrs <- left_join(dat.list, orrs, by=c('Date'))
dat.orrs$Site <- 'Orrs'
dat.orrs <- dat.orrs[dat.orrs$Date >= as.POSIXct('2025-06-06 10:03:00') &
                       dat.orrs$Date <= as.POSIXct('2025-06-25 13:33:00'),]

dat.orrs <- dat.orrs %>% 
  mutate(Temp.interp = zoo::na.approx(Temp),
         Lux.interp = zoo::na.approx(Lux)) %>% 
  dplyr::select(-Temp, -Lux)

dat.snow$Lux.interp <- NA
dat.long$Lux.interp <- NA
dat.lowell$Lux.interp <- NA
dat.garrison$Lux.interp <- NA
dat.all <- rbind(dat.orrs, dat.long, dat.snow, dat.lowell, dat.garrison)

#### Plot ####
dat.lf <- dat.all %>% 
  group_by(Site) %>% 
  pivot_longer(cols=c('tide.interp', 'AirTemp.interp', 'Wind.interp', 
                      'WindDir.interp', 'Rain.interp',
                      'Temp.interp', 'Lux.interp'),
               names_to = 'Variable') %>% 
  as.data.frame()

suncon$Date <- suncon$time

suncon <- suncon[suncon$Date >= as.Date('2025-06-19'),]

dat.lf <- left_join(dat.lf, suncon, by=c("Date"))
dat.lf$condition[dat.lf$condition == 'light'] <- NA

Airtemp <- ggplot(data=dat.lf[ dat.lf$Date >= as.Date('2025-06-19') &
                    
                   dat.lf$Variable == 'AirTemp.interp' &
                   dat.lf$Site == 'Snow',]) +
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

Lux <- ggplot(data=dat.lf[ 
                                 
                                dat.lf$Variable == 'Lux.interp' &
                                dat.lf$Site == 'Orrs',]) +
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
                                dat.lf$Site == 'Snow',]) +
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
                        dat.lf$Site == 'Snow'
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
                col=Site)) +
  facet_wrap(~Variable, scales='free_y')+
   scale_x_datetime(breaks='2 days')

Tide <- ggplot(data=dat.lf[ dat.lf$Date >= as.Date('2025-06-19') &
                                 
                                dat.lf$Variable == 'tide.interp' &
                                dat.lf$Site == 'Snow',]) +
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
                                dat.lf$Site == 'Snow',]) +
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
                                dat.lf$Site == 'Snow',]) +
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

#ggsave(plot=
         egg::ggarrange(Airtemp, #Tide, 
                        Wind, WindDir, 
                        #Rain, Lux, 
                        #Temp,
               ncol=1
               )#,
       #here('SensorPlot.png'),
       #height=8.5, width = 11, units = 'in')
