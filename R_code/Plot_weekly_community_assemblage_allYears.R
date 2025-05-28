rm(list=ls())

########## WHAT YEAR DO WE WANT TO MAKE THIS PLOT FOR #############
# yearuse <- 2014
######### CHANGE HERE, WILL CARRY THROUGH BELOW ###########

# Packages
library(tidyverse)

# Homemade negate function
'%notin%' <- function(x,y)!('%in%'(x,y))

# Set GGplot auto theme
theme_set(theme(panel.grid.major = element_line(color='lightgray'),
                panel.grid.minor = element_blank(),
                panel.background = element_blank(),
                panel.border = element_rect(color='black', size=1, fill=NA),
                legend.position = "bottom",
                axis.text.x=element_text(size=11),
                axis.text.y=element_text(size=11),
                axis.title.x=element_text(size=12),
                axis.title.y=element_text(size=12, angle=90, vjust=2),
                plot.title=element_text(size=14, hjust = 0, vjust = 1.2),
                plot.caption=element_text(hjust=0, face='italic', size=12)))

# Dataframe of spec abbrev
# If we catch new fish, they will need to be included here
specab <- data.frame(
  name = c('Black sea bass', 'Cod', 'Cunner', 'Cusk', 'Haddock',
           'Hake', 'Longhorn sculpin', 'Mackerel', 'Pollock',
           'Red Hake', 'Redfish', 'Sculpin', 'Sea Raven', 
           'Shorthorn sculpin', 'White Hake', 'Whiting'),
  label = c('Bk.SB', 'Cod', 'Cunner', 'Cusk', 'Hddk',
            'Hake', 'L.Sculp', 'Mkrl', 'Pllk', 
            'R.Hake', 'R.Fish', 'Un.Sculp', 'S.Raven',
            'S.Sculp', 'W.Hake', 'Whtg')
)

# Load data
abund <- read.csv('C:/Users/klankowicz/Documents/GitHub/CBASS/Clean_Data/Jigging/jigging_through_2024.csv')

# Dataframe of date starting each week in summers
# Want to make it look like, for example "Aug 1"
# This is wildly clunky but it works
weeks <- rep(seq(min(abund$Week.Num), max(abund$Week.Num, 1)), 
             length(unique(abund$Year)))
weeks <- weeks[order(weeks)]
years <- unique(abund$Year)
years <- years[order(years)]
years <- rep(years, length(unique(weeks)))

weekdf <- data.frame(
  Week.Num = weeks,
  Year = years
)

weekdf <- weekdf %>% 
  mutate(Week.of = as.Date(paste(Year, Week.Num, 1, sep = "-"),
                           format = "%Y-%U-%u")) %>% 
  mutate(month = month(Week.of),
         day = day(Week.of)) %>% 
  mutate(month = month.abb[month]) %>% 
  mutate(Week.of = paste0(month, ' ', day)) %>% 
  dplyr::select(-month, -day)


# Append week starting date to abundance dataframe
# Important to use all=T to make sure end result plots show all
# possible sampling weeks each year (based on minimum and maximum
# week number across all sampling years)
abund <- merge(abund, weekdf, by=c('Year', 'Week.Num'),
               all=T)

# Listify
abund <- split(abund, f=abund$Year)

# Trim to year of interest (set above) for this viz
#abund <- abund[abund$Year == yearuse,]

for(i in 1:length(abund)){
  # Pivot so there's a row for each species in each week
  abund[[i]] <- abund[[i]] %>% 
    # In the future, might have to change column numbers. Just
    # avoid accidentally including the columns for year and week of, 
    # week number.
    pivot_longer(cols = 3:18)
  
  abund[[i]]$name <- gsub(".", " ", abund[[i]]$name, fixed=TRUE)
  
  # Convert decimal probability to percent
  abund[[i]]$value <- abund[[i]]$value * 100
  
  # Append spec abb.
  abund[[i]] <- merge(abund[[i]], specab, by="name")
  
  # Set color order via species name (personal preference)
  # If we ever catch new fish, they will have to be included here
  abund[[i]]$name <- factor(abund[[i]]$name,
                                levels=c('Cod', 
                                         'Cusk', 
                                         'Hake',
                                         'Longhorn sculpin',
                                         'Black sea bass', 
                                         'Mackerel', 
                                         'Shorthorn sculpin',
                                         'Cunner', 
                                         'Whiting',
                                         'White Hake',
                                         'Redfish',
                                         'Haddock',
                                         'Sculpin',
                                         'Sea Raven',
                                         'Red Hake',
                                         'Pollock'
                                         ))
  
  abund[[i]] <- abund[[i]][!is.na(abund[[i]]$name),]
  
  # Order
  abund[[i]] <- abund[[i]][with(abund[[i]], order(Year, Week.Num, name)),]
  head(abund[[i]], 10)
  
  # This is just to make sure the positions of the labels show up right
  # Split each year into its component weeks
  psum <- split(abund[[i]], f=abund[[i]]$Week.Num)
  for(j in 1:length(psum)){
    # Initialize columns for position values
    psum[[j]]$psum <- NA
    psum[[j]]$ypos <- NA
    
    # Change NA catch to 0
    psum[[j]]$value[is.na(psum[[j]]$value)] <- 0
    
    # Order by species name (we set this order above)
    psum[[j]] <- psum[[j]][with(psum[[j]],
                                order(name, decreasing = T)),]
    
    # Find first species caught (according to the order we set above)
    fishuse <- psum[[j]]$name[!is.na(psum[[j]]$value)][1]
    # Find row number of our dataframe where this happens
    numuse <- which(psum[[j]]$name == fishuse)
    
    # Initialize with the catch for the first species caught
    psum[[j]]$psum[numuse] <- psum[[j]]$value[numuse]
    
    # For all following rows, find the % pf catch summed for all previous species
    for(k in (numuse + 1):nrow(psum[[j]])){
      psum[[j]]$psum[k] <- psum[[j]]$psum[(k-1)] + psum[[j]]$value[k]
    }
    
    # For the first row, calculate the y-position that would be the "middle" of the species bar
    psum[[j]]$ypos[numuse] <- psum[[j]]$psum[numuse] / 2
    
    # Do the same for all following rows
    for(k in (numuse+1):nrow(psum[[j]])){
      if(!is.na(psum[[j]]$label[k])){
        psum[[j]]$ypos[k] <- psum[[j]]$psum[(k-1)] + (psum[[j]]$value[k]/2)
      }
    }
  }
  
  # Rebind all list items to one dataframe
  psum <- do.call(rbind, psum)
  
  # Put back into order
  psum <- psum[with(psum, order(Week.Num, name)),]
  
  # Remove row names (they're annoying)
  rownames(psum) <- NULL
  
  # Trim label so it only appears for species with more than 10% of catch
  psum$ypos[psum$value < 15] <- NA
  
  # Rename
  abund[[i]] <- psum
  
}
abund <- do.call(rbind, abund)

abund$value[abund$value == 0] <- NA

abund$Week.of <- as.Date(paste0(abund$Week.of, ' ', abund$Year),
                         format = '%b %d %Y')

# Plot
# Name plot
commass <- 
  # Set up aesthetics
  ggplot(data=abund[abund$Year %in% c(2014, 2015, 2023, 2024),],
         aes(x=Week.of, y=value, col=name, fill=name, 
             label = label)) +
  # Add bars
  geom_bar(stat='identity', position = 'stack', width=6) +
  # Add labels for bars
  geom_label(aes(x=Week.of, y=ypos),
             size = 3,
             col='black',
             fill='white') +
  # Use Viridis color scale for fill and outline color of bars
  scale_fill_viridis_d() +
  scale_color_viridis_d() +
  # Change axis labels
  labs(x='Week', y='% of Weekly catch', col='Species', fill='Species') +
  # Specify the limits of x axis, set labels of x axis
  # We do this so it's consistent across years
  # scale_x_continuous(breaks=weekdf$Week.Num,
  #                    labels=weekdf$Week.of) +
  
  facet_wrap(vars(Year), scales='free_x') +
  # Add title
  ggtitle('Jigging community assemblage') +
  # Move legend to right and change angle of x axis tick labels
  theme(legend.position = 'right',
        axis.text.x = element_text(angle = 35)) +
  # Make sure the legend has only one column
  guides(fill=guide_legend(ncol=1),
         color=guide_legend(ncol=1))

# View
commass

# Where am I saving to
getwd()

# Save
ggsave(plot=commass,
       'total_jig_communityassemblage.png',
       width = 12, height = 8.5, units='in')
