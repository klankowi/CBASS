rm(list=ls())

# Random seed for reproducibility
set.seed(123)

library(here)
library(tidyverse)
library(aquamapsdata)
library(sf)

# Negate function
'%notin%' <- function(x,y)!('%in%'(x,y))

fishes <- read.csv(here('Clean_Data/seine_encountered_species.csv'))

# Clean
fishes <- fishes %>% 
  dplyr::select(Common, Genus, species) %>% 
  filter(Common %notin% c('green crab', 'shortfin squid'))

distlist <- vector(mode = 'list', length = nrow(fishes))

# downloads about 2 GB of data, approx 10 GB when unpacked
download_db()
default_db("sqlite")

for(i in 1:nrow(fishes)){
  fu <- fishes$Common[i]
  key <- am_search_fuzzy(fu)$key
}
