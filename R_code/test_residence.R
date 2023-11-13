## Data exploration, CBASS
rm(list=ls())

library(tidyverse)
library(readxl)
library(here)
library(sf)

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

#### Join similar species ####
data.all$abund$species_name[
  data.all$abund$species_name %in% c('grubby sculpin',
                                     'longhorn sculpin',
                                     'shorthorn sculpin',
                                     'slimy sculpin',
                                     'striped sculpin',
                                     'unID sculpin')
] <- 'sculpin spp'

data.all$abund$species_name[
  data.all$abund$species_name %in% c('atlantic herring',
                                     'blueback herring',
                                     'herring',
                                     'river herring')
] <- 'herring spp'

data.all$abund$species_name[
  data.all$abund$species_name %in% c('banded killifish',
                                     'killifish')
] <- 'killifish spp'

data.all$abund$species_name[
  data.all$abund$species_name %in% c('northern pipefish',
                                     'pipefish')
] <- 'pipefish spp'

data.all$abund$species_name[
  data.all$abund$species_name %in% c('white mullet',
                                     'mullet')
] <- 'mullet spp'

data.all$abund$species_name[
  data.all$abund$species_name %in% c('emerald shiner',
                                     'golden shiner',
                                     'unID shiner')
] <- 'shiner spp'

data.all$abund$species_name[
  data.all$abund$species_name %in% c('rock gunnel',
                                     'unID gunnel')
] <- 'gunnel spp'

data.all$abund$species_name[
  data.all$abund$species_name %in% c('threespine stickleback',
                                     'ninespine stickleback',
                                     'unID stickelback')
] <- 'stickleback spp'

data.all$abund$species_name[
  data.all$abund$species_name %in% c('hake',
                                     'red hake',
                                     'white hake',
                                     'spotted hake')
] <- 'hake spp'

data.all$abund$species_name[
  data.all$abund$species_name %in% c('white sucker',
                                     'common sucker')
] <- 'sucker spp'

data.all$abund$id <- 
  paste0(data.all$abund$loc_id, 'qrst',
         data.all$abund$species_name)

pullinfo <- data.all$abund

data.all$abund <- data.all$abund %>% 
  group_by(id) %>% 
  summarise(catch = sum(catch))

data.all$abund <- data.all$abund %>% 
  separate(id, into=c('loc_id', 'species_name'),
           sep = "qrst")

pullinfo <- dplyr::select(pullinfo, -catch, -id)
pullinfo <- unique(pullinfo)

data.all$abund <- left_join(data.all$abund, pullinfo, 
                            by=c('loc_id', 'species_name'))
data.all$abund <- as.data.frame(data.all$abund)

#### Residence time and phenology ####
tot.abund <- data.all$abund 
tot.abund$year <- lubridate::year(tot.abund$date)
tot.abund$period[tot.abund$year %in% c(2014:2018)] <- 1
tot.abund$period[tot.abund$year %in% c(2019:2023)] <- 2
tot.abund$id <- paste0(tot.abund$species_name, "_", tot.abund$period)

tot.abund <- tot.abund %>% 
  group_by(id) %>% 
  summarise(total.catch = sum(catch))

tot.abund <- tot.abund %>% 
  separate(id, c('species_name', 'period'), "_")

tot.abund <- tot.abund[with(tot.abund, order(species_name)),]

poss <- tot.abund[tot.abund$total.catch >= 10,]
spc <- as.data.frame(table(poss$species_name))
spc <- spc[spc$Freq == 2,]

top8 <- data.all$abund[data.all$abund$species_name %in% spc$Var1,]
#[data.all$abund$species_name %in%
                 #        c('green crab', 'winter flounder',
                #           'alewife', 'atlantic herring',
                 #          'atlantic silverside', 'tomcod',
                  #         'mummichog', 'sandlance'),]

head(top8)
top8$doy <- lubridate::yday(top8$date)
top8$year <- lubridate::year(top8$date)
top8$period[top8$year <=2018] <- 1
top8$period[top8$year >=2019] <- 2

top8$id <- paste0(top8$species_name, "_", top8$period, "_", 
                  top8$doy)

top8.c <- top8 %>% 
  group_by(id) %>% 
  summarise(catch = sum(catch))

top8.s <- top8.c %>% 
  separate(id, 
           sep="_",
           c('species_name', 'period', 'doy'))

top8.l <- data.frame(
  species_name = rep(unique(top8.s$species_name), 2),
  period = c(rep(1, length(unique(top8.s$species_name))),
             rep(2, length(unique(top8.s$species_name))))
)

for(i in 1:nrow(top8.l)){
  top8.l$first.in[i] <- 
    min(top8.s$doy[top8.s$species_name ==
                     paste0(top8.l$species_name[i]) &
                     top8.s$period == 
                     paste0(top8.l$period[i])])
  top8.l$last.out[i] <- 
    max(top8.s$doy[top8.s$species_name ==
                     paste0(top8.l$species_name[i]) &
                     top8.s$period ==
                     paste0(top8.l$period[i])])
}

top8.l <- top8.l[complete.cases(top8.l),]
spc <- as.data.frame(table(top8.l$species_name))
spc <- spc[spc$Freq == 2,]
top8.l <- top8.l[top8.l$species_name %in% spc$Var1,]

top8.l <- top8.l[with(top8.l, order(species_name, period)),]

cruises <- top8.l[1:2,]
cruises$species_name <- c('aa.period1', 'aa.period2') 
cruises$period <- c(1,2)
cruises$first.in <- c(163, 151)
cruises$last.out <- c(267, 273)

top8.l <- rbind(top8.l, cruises)

ggplot() +
  geom_point(data=top8.l,
            aes(x=first.in, y=species_name, col=period)) +
  geom_point(data=top8.l,
             aes(x=last.out, y=species_name, col=period))

interesting <- top8.l[top8.l$first.in != c(163, 151),]
interesting <- interesting[interesting$species_name %in%
                             c('bluefish', 'pipefish spp',
                               'sandlance', 'stickleback spp',
                               'sucker spp'),]

interesting <- rbind(interesting, cruises)

ggplot() +
  geom_point(data=interesting,
             aes(x=first.in, y=species_name, col=period)) +
  geom_point(data=interesting,
             aes(x=last.out, y=species_name, col=period))
