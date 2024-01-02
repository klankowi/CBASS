## Pull ranges of all fish species caught in CBASS seines
rm(list=ls())

# Load packages
library(tidyverse)
library(here)
library(rfishbase)
library(sf)
library(ncdf4)
library(ncdf4.helpers)
library(RCurl)
library(stars)

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

#### Pull species ####
species <- (data.all$abund$species_name)
species <- unique(species)

# Remove non-fish
species <- species[species %notin% c('horseshoe crab',
                                     'periwinkle',
                                     'green crab',
                                     'shortfin squid')]

# Remove fish only identified to genus level
species <- species[-grep("unID", species)]
species <- species[species %notin% c('hake',
                                     'mullet',
                                     'herring',
                                     'killifish',
                                     'smelt')]

# Fix errors of specific fish
# These are either not real fish or duplicated
species <- species[species %notin% c('pipefish',
                                     'glass eel elver',
                                     'striped sculpin')]
# The name is wrong on these two
species[species=='eastern silver minnow'] <- 
  'eastern silvery minnow'
species[species=='grubby sculpin'] <- 'grubby'
# This is the same name for white sucker
species <- species[species != 'common sucker']

# Fix errors which require appending 'Atlantic'
species[species=='tomcod'] <- 'atlantic tomcod'
species[species=='sandlance'] <- 'american sand lance'
species[species=='butterfish'] <- 'atlantic butterfish'
species[species=='pollock'] <- 'saithe'
#species[species=='permit'] <- 'atlantic permit'

# Common names to scientific
fullspec <- common_to_sci(species)

# See if we missed any
fullspec$ComName <- tolower(fullspec$ComName)
missingspec <- species[species %notin% fullspec$ComName]
missingspec

# Remove species that we didn't ask for
fullspec <- fullspec[fullspec$ComName %in% species,]

# Remove duplicates
fullspec <- fullspec[!duplicated(fullspec),]

# Fix obvious errors
# Bluefish are Pomatomus salatrix
fullspec <- fullspec[fullspec$SpecCode %notin% 
                       c(512, 2693, 5806, 12706),]
# Lumpfish are Cyclopterus lumpus
fullspec <- fullspec[fullspec$SpecCode %notin%
                       c(46297),]
# Crevalle jack is Caranx hippos
fullspec <- fullspec[fullspec$SpecCode %notin%
                       c(1918),]
# White mullet is Mugil curema
fullspec <- fullspec[fullspec$SpecCode %notin%
                       c(80, 14701, 63),]
# Permit is Trachinotus falcatus
fullspec <- fullspec[fullspec$SpecCode %notin%
                       c(1011, 1963, 1969),]


# Check for duplicates
dupsci <- fullspec$Species[duplicated(fullspec$Species)]
dupsci <- fullspec[fullspec$Species %in% dupsci,]
dupsci <- dupsci[with(dupsci, order(Species)),]
rownames(dupsci) <- NULL
dupcom <- fullspec$Species[duplicated(fullspec$ComName)]
dupcom <- fullspec[fullspec$Species %in% dupcom,]
dupcom <- dupcom[with(dupcom, order(Species)),]
rownames(dupcom) <- NULL
dupdup <- rbind(dupsci, dupcom)
dupdup <- unique(dupdup)

dups <- fullspec[fullspec$Species %in% dupdup$Species |
                 fullspec$ComName %in% dupdup$ComName,]
dups

# Fix the alosines (very annoying)
fullspec <- fullspec[fullspec$ComName != 'river herring',]
river.herring <- dups[dups$SpecCode == 1578,]
fullspec <- rbind(fullspec, river.herring)

# Remove intermediates
rm(data.all, dupcom, dupdup, dups, dupsci, river.herring,
   missingspec, species)

#### Extract species range maps ####
fullspec$orientation <- NA
fullspec$dist.to.center <- NA

# Load coast
coast <- ecodata::coast
coast <- st_transform(coast, 'EPSG:4326')

cs <- st_read(here('GIS/Continental_Shelf_noHB.shp'))
cs <- st_transform(cs, 'EPSG:4326')

cascobay <- data.frame(
  x = -70.036985,
  y = 43.655700,
  range='Casco Bay'
)

cascobay.sf <- st_as_sf(cascobay, coords=c('x', 'y'))
st_crs(cascobay.sf) <- st_crs(cs)

urlFileExist <- function(url){
  HTTP_STATUS_OK <- 200
  hd <- httr::HEAD(url)
  status <- hd$all_headers[[1]]$status
  list(exists = status == HTTP_STATUS_OK, status = status)
}

# # Download data (do only once or when maps updated externally)
# for(i in 1:nrow(fullspec)
#     #5 #use for testing
#     ){
#   # Set species name, replace spaces with underscores
#   specuse <- fullspec$Species[i]
#   specuse <- sub(" ", "_", specuse)
#   comuse <- fullspec$ComName[i]
# 
#   # Check if map exists
#   urlch <- paste0("https://thredds.d4science.org/thredds/fileServer/public/netcdf/AquaMaps_11_2019/",
#                   specuse, ".nc")
# 
#   # If map exists
#   if(lapply(urlch, urlFileExist)[[1]]$exists=='TRUE'){
#     # Download file (Externally generated by Aquamaps 11/2019)
#     download.file(paste0("https://thredds.d4science.org/thredds/fileServer/public/netcdf/AquaMaps_11_2019/",
#                          specuse, ".nc"),
#                   here('Species_Dists',
#                        paste0(specuse,'.nc')),
#                   mode = 'wb',
#                   quiet=TRUE)
#   }
# 
#   # If map does not exist
#   if(lapply(urlch, urlFileExist)[[1]]$exists!='TRUE'){
#     print(paste0('NetCDF does not exist for ',
#                  comuse))
#     next()
#   }
# 
# }

# Remove species that do not have maps
filenames <- list.files(here('Species_Dists'))
fullspec$filename <- paste0(sub(" ", "_", fullspec$Species),
                            '.nc')
fullspec <- fullspec[fullspec$filename %in% filenames,]

fullspec <- fullspec[with(fullspec, order(ComName)),]

# Remove species whose maps do not contain useful information
fullspec <- fullspec[fullspec$Species %notin%
                       c('Limanda limanda',
                         'Alosa chrysochloris'),]

# Make list format for RShiny
fishdata <- vector(mode='list', length=nrow(fullspec))
names(fishdata) <- unique(fullspec$ComName)
for(i in 1:length(fishdata)){
  fishdata[[i]] <- vector(mode='list', length = 13)
  names(fishdata[[i]]) <- c('specuse', 'comuse', 'fish',
                            'prob', 'lon', 'lat', 'pr',
                            'pr.sf', 'pf.sf', 'pcr', 
                            'centr', 'centr.df', 'centr.sf')
  fishdata[[i]]$specuse <- 
    fullspec$Species[fullspec$ComName == names(fishdata)[i]]
  fishdata[[i]]$specuse <- sub(" ", "_", fishdata[[i]]$specuse)
  fishdata[[i]]$comuse <- 
    names(fishdata)[i]
  
  fishdata[[i]]$fish <- nc_open(here('Species_Dists', paste0(fishdata[[i]]$specuse,'.nc')))
  
  # Pull variables: lat, lon, prob. of occurrence (rasterized)
  fishdata[[i]]$prob <- ncvar_get(fishdata[[i]]$fish, "probability")
  fishdata[[i]]$lon <- ncvar_get(fishdata[[i]]$fish, "longitude")
  fishdata[[i]]$lat <- ncvar_get(fishdata[[i]]$fish, 'latitude')
  
  # Convert format for plotting
  colnames(fishdata[[i]]$prob) <- fishdata[[i]]$lat
  rownames(fishdata[[i]]$prob) <- fishdata[[i]]$lon
  fishdata[[i]]$pr <- as.data.frame(as.table(fishdata[[i]]$prob))
  fishdata[[i]]$pr <- fishdata[[i]]$pr %>% 
    mutate_at(c('Var1', 'Var2', 'Freq'), as.character) %>% 
    mutate_at(c('Var1', 'Var2', 'Freq'), as.numeric) 
  
  # Remove occurrence outside of Continental Shelf
  fishdata[[i]]$pr.sf <- st_as_sf(fishdata[[i]]$pr, coords=c('Var1', 'Var2'))
  fishdata[[i]]$pr.sf$id <- seq(1:nrow(fishdata[[i]]$pr.sf))
  st_crs(fishdata[[i]]$pr.sf) <- 'EPSG:4326'
  fishdata[[i]]$pf.sf <- st_intersection(fishdata[[i]]$pr.sf, cs)
  fishdata[[i]]$pr.sf$Freq[fishdata[[i]]$pr.sf$id %notin% fishdata[[i]]$pf.sf$id] <- NA
  fishdata[[i]]$pr <- sfheaders::sf_to_df(fishdata[[i]]$pr.sf, fill=T)
  fishdata[[i]]$pr <- fishdata[[i]]$pr %>% 
    dplyr::select(-sfg_id, -point_id,
                  -id)
  
  # Find range edges
  fishdata[[i]]$pf <- sfheaders::sf_to_df(fishdata[[i]]$pf.sf[!is.na(fishdata[[i]]$pf.sf$Freq),],
                            fill=T)
  
  # Find weighted center of polygon
  # Convert NA values to 0
  fishdata[[i]]$pcr <- fishdata[[i]]$pr
  fishdata[[i]]$pcr$Freq[is.na(fishdata[[i]]$pcr$Freq)] <- 0
  fishdata[[i]]$centr = c(weighted.mean(x=fishdata[[i]]$pcr[,2],
                                        w=fishdata[[i]]$pcr[,1], 
                                        na.rm=TRUE), 
            weighted.mean(x=fishdata[[i]]$pcr[,3],
                          w=fishdata[[i]]$pcr[,1], 
                          na.rm=TRUE))
  fishdata[[i]]$centr.df <- data.frame(
    x=c(min(fishdata[[i]]$pf$x), 
        fishdata[[i]]$centr[1], 
        max(fishdata[[i]]$pf$x)),
    y=c(min(fishdata[[i]]$pf$y), 
        fishdata[[i]]$centr[2], 
        max(fishdata[[i]]$pf$y)),
    range=c('Southern', 'Weighted Center', 'Northern')
  )
  fishdata[[i]]$centr.sf <- st_as_sf(fishdata[[i]]$centr.df, coords=c('x', 'y'))
  st_crs(fishdata[[i]]$centr.sf) <- st_crs(fishdata[[i]]$pf.sf)
  
  fishdata[[i]]$centr.df <- rbind(fishdata[[i]]$centr.df, cascobay)

  fishdata[[i]]$dist.to.center[i] <- cascobay$y - 
    fishdata[[i]]$centr.df$y[fishdata[[i]]$centr.df$range == 'Weighted Center']
  
  if(cascobay$y > fishdata[[i]]$centr.df$y[
    fishdata[[i]]$centr.df$range =='Weighted Center']){
    fishdata[[i]]$orientation <- 'North of center'
  }
  
  if(cascobay$y < fishdata[[i]]$centr.df$y[
    fishdata[[i]]$centr.df$range =='Weighted Center']){
    fishdata[[i]]$orientation <- 'South of center'
  }
}

save.image('C:/Users/klankowicz/Documents/RShiny_Holder/CBASS_Fish_Dist/ShinyData.Rdata')

# Map data
for(i in 1:nrow(fullspec)
    #5 # use for testing
    ){
  # Set species name, replace spaces with underscores
  specuse <- fullspec$Species[i]
  specuse <- sub(" ", "_", specuse)
  comuse <- fullspec$ComName[i]
  
  # Open NetCDF
  if(file.exists(here('Species_Dists', paste0(specuse, '.nc')))==FALSE){
    print(paste0('File does not exist for ', comuse))
    rm(specuse, comuse)
    next()
  }
  
  if(file.exists(here('Species_Dists', paste0(specuse, '.nc')))==TRUE){
    fish <- nc_open(here('Species_Dists', paste0(specuse,'.nc')))
  }
  
  # Pull variables: lat, lon, prob. of occurrence (rasterized)
  prob <- ncvar_get(fish, "probability")
  lon <- ncvar_get(fish, "longitude")
  lat <- ncvar_get(fish, 'latitude')
  
  # Convert format for plotting
  colnames(prob) <- lat
  rownames(prob) <- lon
  pr <- as.data.frame(as.table(prob))
  pr <- pr %>% 
    mutate_at(c('Var1', 'Var2', 'Freq'), as.character) %>% 
    mutate_at(c('Var1', 'Var2', 'Freq'), as.numeric) 
  
  # Remove occurrence outside of Continental Shelf
  pr.sf <- st_as_sf(pr, coords=c('Var1', 'Var2'))
  pr.sf$id <- seq(1:nrow(pr.sf))
  st_crs(pr.sf) <- 'EPSG:4326'
  pf.sf <- st_intersection(pr.sf, cs)
  pr.sf$Freq[pr.sf$id %notin% pf.sf$id] <- NA
  pr <- sfheaders::sf_to_df(pr.sf, fill=T)
  pr <- pr %>% 
    dplyr::select(-sfg_id, -point_id,
                  -id)
  
  # Find range edges
  pf <- sfheaders::sf_to_df(pf.sf[!is.na(pf.sf$Freq),],
                            fill=T)
  
  # Find weighted center of polygon
  # Convert NA values to 0
  pcr <- pr
  pcr$Freq[is.na(pcr$Freq)] <- 0
  centr = c(weighted.mean(x=pcr[,2],w=pcr[,1], na.rm=TRUE), 
            weighted.mean(x=pcr[,3],w=pcr[,1], na.rm=TRUE))
  centr.df <- data.frame(
    x=c(min(pf$x), centr[1], max(pf$x)),
    y=c(min(pf$y), centr[2], max(pf$y)),
    range=c('Southern', 'Weighted Center', 'Northern')
  )
  centr.sf <- st_as_sf(centr.df, coords=c('x', 'y'))
  st_crs(centr.sf) <- st_crs(pf.sf)
  
  # Plot occurrence along NWA Continental Shelf
  occur <- 
    ggplot() +
     geom_raster(data=pr,
                 aes(x=x, y=y, fill=Freq)) +
     scale_fill_viridis_c(na.value=NA) +
     geom_sf(data=coast) +
     geom_sf(data=centr.sf,
             col='red',
             cex=1, pch=19) +
     geom_sf(data=cascobay.sf,
             col='red',
             cex=5, pch='x') +
     coord_sf(xlim=c(-84.25, -42),
              ylim=c(24.5, 78.5)) +
     labs(x='Longitude',
          y='Latitude',
          fill='Rel. prob. of\noccurrence') +
     ggtitle(paste0(str_to_sentence(comuse)))
  
  # ggsave(plot=occur,
  #        filename=here('Species_Dists/Plots', paste0(comuse, '.png')),
  #        device='png',
  #        width=7.5, height=10, units='in')
  
  centr.df <- rbind(centr.df, cascobay)

  dens <- ggplot() +
    geom_density(data=pf,
                 aes(x=y)) +
    geom_vline(data=centr.df[centr.df$range %in% c('Weighted Center',
                                                   'Casco Bay'),],
               aes(xintercept = y,
                   col=range)) +
    labs(col='Northing (dec. deg.)') +
    ggtitle(paste0(str_to_sentence(comuse)))
  
  # ggsave(plot=dens,
  #        filename=here('Species_Dists/Plots', 
  #                      paste0(comuse, '_dens.png')),
  #        device='png',
  #        width=10, height=7.5, units='in')
  
  fullspec$dist.to.center[i] <- cascobay$y - 
    centr.df$y[centr.df$range == 'Weighted Center']
  
  if(cascobay$y > centr.df$y[centr.df$range =='Weighted Center']){
    fullspec$orientation[i] <- 'North of center'
  }
  
  if(cascobay$y < centr.df$y[centr.df$range =='Weighted Center']){
    fullspec$orientation[i] <- 'South of center'
  }
  
  rm(fish, pr.sf, pr, prob, comuse, lat, lon, specuse,
     centr.df, centr.sf, pcr, pf, pf.sf, centr)
}

