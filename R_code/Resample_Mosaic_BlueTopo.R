rm(list=ls())

library(terra)
library(tidyverse)

# Load URL dataframe
topofiles <- read.csv('C:/Users/Katie/Desktop/BlueTopo_NAtlantic_CellNames.csv')
topofiles <- topofiles %>% 
  filter(!is.na(GeoTIFF_Link) &
           GeoTIFF_Link != "")

# Specify the save location
tif_path <- "C:/Users/Katie/BT_Elevation/"

# List all TIFF files in the directory
list_of_tiffs <- list.files(path = tif_path, 
                            pattern = "\\.tif$", 
                            full.names = TRUE)

# Resample TIFFs (16m original resoltuion)
topofiles$Resolution[topofiles$tile == 'BF2JW2MM'] <- '4m'
resamps <- topofiles[topofiles$Resolution == '16m',]
for(i in 9:nrow(resamps)){
  print(i)
  test <- terra::rast(paste0(
    'C:/Users/Katie/BT_Elevation/', 
    resamps$tile[i], '.tif'
  ))
  test <- disagg(test, fact = 2, method="near") 
  
  terra::writeRaster(test, 
                     filename = paste0(tif_path, '/', 
                                       resamps$tile[i], '.tif'), 
                     filetype = "GTiff", 
                     overwrite = TRUE)
  rm(test)
  
  topofiles$Resolution[topofiles$tile == resamps$tile[i]] <- '4m'
}

test <- terra::rast('C:/Users/Katie/BT_Elevation/BF2JW2MM.tif')
test2 <- disagg(test, fact = 2, method="near") 

# Set template raster
t1 <- terra::rast('C:/Users/Katie/BT_Elevation/BH5225G9.tif')

# Create a SpatRasterCollection
ras_col <- sprc(list_of_tiffs)

# Mosaic the collection into a single raster
# The 'fun' argument handles overlapping areas (e.g., "mean", "first", "last")
merged_raster <- mosaic(ras_col, fun = "mean")

# Write the result to a new GeoTIFF file
writeRaster(merged_raster, filename = "merged_output.tif", filetype = "GTiff", overwrite = TRUE)
