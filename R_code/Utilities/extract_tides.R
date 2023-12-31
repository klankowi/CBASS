### Important note: I'm no longer maintaining this gist, since this code is incorporated into the
### VulnToolkit package.
### Because it's not being maintained, the code below might not work. The latest, updated version
### is available at https://github.com/troyhill/VulnToolkit/blob/master/R/HL.r



### Function to pick out high and low tides from a dataset of tidal water levels
###
### Arguments: 
### - "level" is a numeric vector of water levels.
### - "time" is a vector (numeric or POSIX*) indicating the time of water level measurements. Units must be minutes.
### - "period" is a single numeric or integer estimate of tidal period (full tidal cycle). Units must be hours.
### - "phantom" (TRUE by default) is a protective measure taken to prevent the inclusion of an artificial
###     high or low tide at the end of the dataset. If the water level measurements 
###     end precisely at a low or high tide, this can be changed to FALSE.
### - "tides" is used to optionally subset the output to include only high or low tides.
###     This argument can be "all" (default), "H", or "L"
###
### Function returns a 3-column dataframe with date/time, water level, and a H/L indicator.
###
### Have a problem? Find a bug? Email Hill.Troy@gmail.com


HL <- function(level, time, period = 13, phantom = TRUE, tides = "all") {
  
  # Check arguments
  if(is.numeric(level) == FALSE) 
    stop("invalid entry: 'level' must be numeric")
  
  if(is.numeric(time) == FALSE & class(time)[1] != "POSIXlt" & class(time)[1] != "POSIXct" )
    stop("invalid entry: 'time' must be numeric or POSIX*")
  
  if(is.numeric(period) == FALSE & class(period) != "integer") 
    stop("invalid entry: 'period' must be numeric")
  
  if(tides != "all" & tides != "L" & tides != "H") 
    stop("invalid entry: 'tides' must be 'all', 'H', or 'L'")
  
  
  # Set locals
  partial.tide <- period * 60  * 60          # seconds
  t.int <- as.numeric( time[2] ) - as.numeric( time[1] )      # seconds
  wll.2 <- data.frame(1:length(level), level, time)
  width <- partial.tide / t.int
  
  # Find high tides
  t.x <- which.max(level[1:(1 + width)])
  
  for(i in 1:length(level)) {
    if(!is.na(t.x[i] + 0.5 * width)) {
      temp <- which.max(level[(t.x[i] + 0.5 * width) : (t.x[i] + 1.25 * width)])
      val <- temp - 1 + (t.x[i] + 0.5 * width)
      t.x <- rbind(t.x, val)
    } else t.x <- t.x 
  }
  
  ht <- wll.2[t.x[,1], ]          # filters high tides from initial dataset, including dates/times
  ht$tide <- rep("H", nrow(ht))
  
  
  # Repeat process for low tides
  t.y <- which.min(level[1:(1 + width)])
  
  for(i in 1:length(level)) {
    if(!is.na(t.y[i] + 0.5 * width)) {
      temp <- which.min(level[(t.y[i] + 0.5 * width) : (t.y[i] + 1.25 * width)])
      val <- temp - 1 + (t.y[i] + 0.5 * width)
      t.y <- rbind(t.y, val)
    } else t.y <- t.y 
  }
  
  lt <- wll.2[t.y[,1], ]          
  lt$tide <- rep("L", nrow(lt))
  
  
  
  if(tides == "H") {
    hl <- ht[,-1]               # removes index variable
    rownames(hl) <- seq(length=nrow(hl))  # re-numbers rows, mostly for aesthetics
    
    if(phantom == TRUE) {
      hl <- hl[-nrow(hl), ]
    } else if (phantom == FALSE) {
      hl <- hl
    } else stop("invalid entry: 'phantom' must be 'TRUE' or 'FALSE' ")
    invisible(hl)
    
  } else if(tides == "L") {
    hl <- lt[,-1]               
    rownames(hl) <- seq(length=nrow(hl)) 
    
    if(phantom == TRUE) {
      hl <- hl[-nrow(hl), ]
    } else if (phantom == FALSE) {
      hl <- hl
    } else stop("invalid entry: 'phantom' must be 'TRUE' or 'FALSE' ")
    invisible(hl)
    
  } else if(tides == "all") {
    hl <- rbind(ht, lt)
    hl <- hl[order(hl[,1]),]    
    hl <- hl[,-1]               
    rownames(hl) <- seq(length=nrow(hl))  
    
    if(phantom == TRUE) {
      hl <- hl[-nrow(hl), ]
    } else if (phantom == FALSE) {
      hl <- hl
    } else stop("invalid entry: 'phantom' must be 'TRUE' or 'FALSE' ")
    
    invisible(hl)
    
  }
}