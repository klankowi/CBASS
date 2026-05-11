rm(list=ls())

library(tidyverse)
library(here)
library(sf)

# Load URL dataframe
topofiles <- read.csv('C:/Users/Katie/Desktop/BlueTopo_NAtlantic_CellNames.csv')
topofiles <- topofiles %>% 
  filter(!is.na(GeoTIFF_Link) &
           GeoTIFF_Link != "") %>% 
  filter(tile %in%
           c("BF2JL2MJ", "BH52C5H5", "BH52D5H5",
             "BH5295GX", "BH5285GZ", "BH5295GZ",
             "BH52B5GZ", "BH52C5GZ", "BH5285H2",
             "BH5295H2", "BH52B5H2", "BH52D5H2",
             "BH5295H4", "BH52B5H4", "BH52C5H4",
             "BH52D5H4", "BH52F5H7", "BH52B5H5",
             "BH5275H2", "BH52C5H2", "BH52F5H2",
             "BH5285H4", "BH52D5H6", "BH52F5H6",
             "BH52F5H4", "BH5285GX", "BH5295H5",
             "BH52F5H5", "BH5275GZ"
             ))

# Specify the download location
file_path <- "C:/Users/Katie/BlueTopo/"
tif_path <- "C:/Users/Katie/BT_Elevation/"

# Loop through tiles
for(i in 1:nrow(topofiles)){
  message(topofiles$tile[i])
  # Specify downloaded filename
  file_name <- paste0(topofiles$tile[i], '.tif')
  
  # Download tif
  download.file(url=topofiles$GeoTIFF_Link[i],
                paste0(file_path, file_name),
                mode='wb')
  
  # Save only Elevation layer
  test <- terra::rast(paste0("C:/Users/Katie/Bluetopo/", file_name),
                      lyrs=1)
  
  print(paste0('Resolution == ', res(test)[1]))
  
  if(res(test)[1]== 4){
    terra::writeRaster(test, 
                       filename = paste0(tif_path, '/', file_name), 
                       filetype = "GTiff", 
                       overwrite = TRUE)
    rm(test)
    
    file.remove(paste0(file_path, file_name))
    
    next()
  }
  
  if(res(test)[1] == 8){
    print('Resampling')
    test <- disagg(test, fact = 2, method="near")
    
    terra::writeRaster(test,
                       filename = paste0(tif_path, '/',
                                         topofiles$tile[i], '.tif'),
                       filetype = "GTiff",
                       overwrite = TRUE)
    rm(test)
    
    file.remove(paste0(file_path, file_name))
    
    next()
  }
  
  if(res(test)[1] == 16){
    print('Uh oh, resolution too low')
    break()
  }
}

# List all TIFF files in the directory
list_of_tiffs <- list.files(path = tif_path, 
                            pattern = "\\.tif$", 
                            full.names = TRUE)

# Create a SpatRasterCollection
ras_col <- sprc(list_of_tiffs)

# Mosaic the collection into a single raster
# The 'fun' argument handles overlapping areas (e.g., "mean", "first", "last")
merged_raster <- mosaic(ras_col, fun = "mean")

# Load Casco Bay shapefile
cb <- st_read(here('GIS/CascoBay_Polygon.shp'), quiet=T)
terra::crs(cb) == terra::crs(merged_raster)
cb <- st_transform(cb, st_crs(merged_raster))

# Crop to CB BBOX
cropped_raster <- crop(merged_raster, cb)

# Mask to CB
masked_raster <- mask(cropped_raster, cb)

# Remove land
masked_raster[masked_raster > 0] <- NA

# Write the result to a new GeoTIFF file
writeRaster(masked_raster, 
            filename = here("Data/Clean_Data/Bathy/BlueTopo_CascoBay_bathy.tif"), 
            filetype = "GTiff", 
            overwrite = TRUE)
