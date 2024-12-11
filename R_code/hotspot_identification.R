# Spatiotemporal hotspot identification using Getis-Ord Gi* 
# Small cod

rm(list=ls())

# Load libraries
library(VAST)
library(tidyverse)
library(here)
library(sf)
library(sfdep)
library(spdep)

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

# Load data
load(here("VAST_runs/small/Overall_BC/ALL/Overall_BC_smallcod_allstrat_natsplin_fsON_ALL.Rdata"))

# CLean workspace
rm(list=setdiff(ls(), c('fit')))


# Extract Data
Y_gt = fit$Report$D_gct[,1,]
map_list = make_map_info(Region = fit$extrapolation_list$Area_km2_x,
                         spatial_list = fit$spatial_list,
                         Extrapolation_List = fit$extrapolation_list)
panel_labels = fit$year_labels
file_name = "hotspot"
working_dir = here('VAST_runs/small/Overall_BC/ALL')
setwd(working_dir)
fun = mean

# Call data
zlim = range(Y_gt, na.rm = TRUE)
MapSizeRatio = map_list$MapSizeRatio
Y_gt = Y_gt[map_list$PlotDF[which(map_list$PlotDF[, "Include"] > 
                                    0), "x2i"], , drop = FALSE]
n_cells = nrow(Y_gt)

# Call location list
loc_g = map_list$PlotDF[which(map_list$PlotDF[, "Include"] > 
                                0), c("Lon", "Lat")]
# Call projections
CRS_orig = sp::CRS("+proj=longlat")
CRS_proj = sp::CRS("EPSG:26919")

# Make list to append data
Big_Data <- vector("list", length=length(panel_labels))

# Call other spatial data
coast <- st_transform(ecodata::coast, CRS_proj)
regions <- st_read(here("Data/GIS/codstox.shp"), quiet=T)
regions <- st_transform(regions, CRS_proj)
regions <- st_make_valid(regions)

for (tI in 1: ncol(Y_gt)) {
  Points_orig = sp::SpatialPointsDataFrame(coords = loc_g, 
                                           data = data.frame(y = Y_gt[, tI]), 
                                           proj4string = CRS_orig)
  Points_LongLat = sp::spTransform(Points_orig, sp::CRS("+proj=longlat"))
  Points_proj = sp::spTransform(Points_orig, CRS_proj)
  Zlim = zlim
  xlim = Points_proj@bbox[1, ]
  ylim = Points_proj@bbox[2, ]
  
  cell.size = mean(diff(Points_proj@bbox[1, ]), diff(Points_proj@bbox[2, 
  ]))/floor(sqrt(n_cells))
  Points_sf = sf::st_as_sf(Points_proj)
  grid = sf::st_make_grid(Points_sf, cellsize = cell.size)
  grid_i = sf::st_intersects(Points_sf, grid)
  grid = sf::st_sf(grid, y = tapply(Points_sf$y, INDEX = factor(as.numeric(grid_i), 
                                                                levels = 1:length(grid)), FUN = mean, na.rm = TRUE))
  grid$TS <- panel_labels[tI]
  grid$ID <- seq(1, nrow(grid), 1)
  
  Big_Data[[tI]] <- grid
  
}

allData <- do.call("rbind", Big_Data)
allData <- allData %>% 
  separate(TS, into=c('Year', 'Season')) %>% 
  mutate(Year = as.numeric(Year)) %>% 
  mutate(Season = factor(Season, levels=c('Spring', 'Fall')))

allData$logy <- log(allData$y)

# Clear workspace
rm(list=setdiff(ls(), c('allData', 'coast', 'regions')))

# Split
allData.list <- split(allData, f=allData$Season)
for(i in 1:length(allData.list)){
  allData.list[[i]] <- split(allData.list[[i]], f=allData.list[[i]]$Year)
}

#### Getis-Ord Gi* ####
for(i in 1:length(allData.list)){
  for(j in 1:length(allData.list[[i]])){
    print(allData.list[[i]][[j]]$Year[1])
    
    # Pull subset
    tes_subset <- allData.list[[i]][[j]]
    tes_subset <- tes_subset[!is.na(tes_subset$y),]
    
    # Identify neighbors with queen contiguity (edge/vertex touching)
    tes_nb <- poly2nb(tes_subset, queen = TRUE)
    
    # Binary weighting assigns a weight of 1 to all neighboring features 
    # and a weight of 0 to all other features
    tes_w_binary <- nb2listw(tes_nb, style="B")
    
    # Calculate spatial lag of y
    tes_lag <- lag.listw(tes_w_binary, tes_subset$y)
    
    # Identify neighbors, create weights, calculate spatial lag
    tes_nbs <- tes_subset |> 
      mutate(
        nb = st_contiguity(grid),        # neighbors share border/vertex
        wt = st_weights(nb),                 # row-standardized weights
        tes_lag = st_lag(y, nb, wt)          # calculate spatial lag of y
      ) 
    
    # Calculate the Gi using local_g_perm
    tes_hot_spots <- tes_nbs |> 
      mutate(
        Gi = local_g_perm(y, nb, wt, nsim = 999)
        # nsim = number of Monte Carlo simulations (999 is default)
      ) |> 
      # The new 'Gi' column itself contains a dataframe 
      # We can't work with that, so we need to 'unnest' it
      unnest(Gi) 
    
    # Create a new data frame called 'tes_hot_spots"
    tes_hot_spots <- tes_hot_spots |> 
      # with the columns 'gi' and 'p_folded_sim"
      # 'p_folded_sim' is the p-value of a folded permutation test
      select(gi, p_folded_sim, ID) |> 
      mutate(
        # Add a new column called "classification"
        classification = case_when(
          # Classify based on the following criteria:
          gi > 0 & p_folded_sim <= 0.01 ~ "Very hot",
          gi > 0 & p_folded_sim <= 0.05 ~ "Hot",
          gi > 0 & p_folded_sim <= 0.1 ~ "Somewhat hot",
          gi < 0 & p_folded_sim <= 0.01 ~ "Very cold",
          gi < 0 & p_folded_sim <= 0.05 ~ "Cold",
          gi < 0 & p_folded_sim <= 0.1 ~ "Somewhat cold",
          TRUE ~ "Insignificant"
        ),
        # Convert 'classification' into a factor for easier plotting
        classification = factor(
          classification,
          levels = c("Very hot", "Hot", "Somewhat hot",
                     "Insignificant",
                     "Somewhat cold", "Cold", "Very cold")
        )
      )
    
    tes_hot_spots <- sfheaders::sf_to_df(tes_hot_spots, fill=T)
    tes_hot_spots <- dplyr::select(tes_hot_spots,ID, gi, p_folded_sim, classification)
    tes_hot_spots <- unique(tes_hot_spots)                               
    
    allData.list[[i]][[j]] <- 
      merge(allData.list[[i]][[j]], tes_hot_spots, by=c('ID'))
    
    rm(tes_subset, tes_hot_spots, tes_nbs, tes_nb, tes_w_binary, tes_lag)
    
  }
}

# Rebind
allData2 <- allData.list

for(i in 1:length(allData2)){
  allData2[[i]] <- do.call(rbind, allData2[[i]])
}
allData2 <- do.call(rbind, allData2)
rownames(allData2) <- NULL

allData2$classification <- factor(allData2$classification,
                                  levels = c("Very hot", "Hot", "Somewhat hot",
                                             "Insignificant",
                                             "Somewhat cold", "Cold", "Very cold"))

# Plot
spring <- ggplot(data=allData2[allData2$Season == 'Spring',]) +
  geom_sf(aes(fill=classification), col=NA) +
  scale_fill_brewer(type = "div", palette = 5) +
  labs(
    fill = "Hot Spot\nClassification"
  ) +
  
  geom_sf(data=coast, fill='gray', col='gray', alpha=0.7) +
  
  coord_sf(xlim=c(-115489.4, 770308.2),
           ylim=c(4060391.8, 4966321.1)) +
  
  facet_wrap(vars(Year)) +
  theme(legend.position = 'bottom',
        axis.text.x = element_text(size=8, angle=35),
        axis.text.y = element_text(size=8))

fall <- ggplot(data=allData2[allData2$Season == 'Fall',]) +
  geom_sf(aes(fill=classification), col=NA) +
  scale_fill_brewer(type = "div", palette = 5) +
  labs(
    fill = "Hot Spot\nClassification"
  ) +
  
  geom_sf(data=coast, fill='gray', col='gray', alpha=0.7) +
  
  coord_sf(xlim=c(-115489.4, 770308.2),
           ylim=c(4060391.8, 4966321.1)) +
  
  facet_wrap(vars(Year)) +
  theme(legend.position = 'bottom',
        axis.text.x = element_text(size=8, angle=35),
        axis.text.y = element_text(size=8))

ggsave(spring, 
       filename=here('VAST_runs/small/Overall_BC/spring_hotspots.pdf'),
       width = 8.5, height = 11, dpi=300)

ggsave(fall, 
       filename=here('VAST_runs/small/Overall_BC/fall_hotspots.pdf'),
       width = 8.5, height = 11, dpi=300)

# Check temporal 
# Larger class groupings
allData2$Class[allData2$classification %in% c('Very cold', 'Cold', 'Somewhat cold')] <- 
  'Cold'
allData2$Class[allData2$classification %in% c('Very hot', 'Hot', 'Somewhat hot')] <- 
  'Hot'
allData2$Class[allData2$classification == 'Insignificant'] <- 'Insignificant'

allData2$Class <- factor(allData2$Class, levels=c('Hot', 'Insignificant', 'Cold'))

# Merge with regions
allData3 <- st_intersection(allData2, dplyr::select(regions, -OBJECTID, -Shape_Leng, -Shape_Area))

# Plot regional cell-time raster
ggplot(data=allData3[allData3$Season == 'Fall' & 
                     !is.na(allData3$y),]) +
  geom_tile(aes(x=Year, y=(as.factor(ID)), fill=Class)) +
  scale_fill_brewer(type='div', palette = 5) +
  facet_wrap(vars(STOCK), scales='free') +
  labs(y='Cell', fill='Spot\nClass') +
  theme(axis.text.y = element_blank(),
        panel.grid.major = element_blank())

# Make table of class grouping in spring
springtab <- as.data.frame(table(allData2$ID[allData2$Season == 'Spring'],
                                    allData2$Class[allData2$Season == 'Spring']))
springtab <- springtab %>% 
  rename(ID = Var1,
         Class = Var2) %>% 
  filter(Freq != 0)

springtab <- springtab[with(springtab, order(ID, Class)),]
rownames(springtab) <- NULL

# Find persistence (90% of time series)
persistentcold <- springtab[springtab$Freq >=36 &
                              springtab$Class == 'Cold',]

persistenthot <- springtab[springtab$Freq >=36 &
                              springtab$Class == 'Hot',]

springp <- ggplot() +
  geom_sf(data=allData2[allData2$Year == 1990 & allData2$Season == 'Spring' &
                          allData2$ID %in% persistentcold$ID,],
          fill='blue', col=NA, alpha=0.6) +
  geom_sf(data=allData2[allData2$Year == 1990 & allData2$Season == 'Spring' &
                          allData2$ID %in% persistenthot$ID,],
          fill='red', col=NA, alpha=0.6) +
  geom_sf(data=regions, lwd=0.15, fill=NA, col='gray20') +
  ggtitle('Spring 90% persistence')

falltab <- as.data.frame(table(allData2$ID[allData2$Season == 'Fall'],
                                 allData2$Class[allData2$Season == 'Fall']))
falltab <- falltab %>% 
  rename(ID = Var1,
         Class = Var2) %>% 
  filter(Freq != 0)

falltab <- falltab[with(falltab, order(ID, Class)),]
rownames(falltab) <- NULL

persistentcold <- falltab[falltab$Freq >=36 &
                              falltab$Class == 'Cold',]

persistenthot <- falltab[falltab$Freq >=36 &
                             falltab$Class == 'Hot',]

fallp <- ggplot() +
  geom_sf(data=allData2[allData2$Year == 1990 & allData2$Season == 'Fall' &
                          allData2$ID %in% persistentcold$ID,],
          fill='blue', col=NA, alpha=0.6) +
  geom_sf(data=allData2[allData2$Year == 1990 & allData2$Season == 'Fall' &
                          allData2$ID %in% persistenthot$ID,],
          fill='red', col=NA, alpha=0.6) +
  geom_sf(data=regions, lwd=0.15, fill=NA, col='gray20') +
  ggtitle('Fall 90% persistence')

ggsave(springp,
       filename = here('VAST_runs/small/Overall_BC/spring_hotspot_persistence.pdf'),
       width = 8.5, height = 11, dpi=300)

ggsave(fallp,
       filename = here('VAST_runs/small/Overall_BC/fall_hotspot_persistence.pdf'),
       width = 8.5, height = 11, dpi=300)
