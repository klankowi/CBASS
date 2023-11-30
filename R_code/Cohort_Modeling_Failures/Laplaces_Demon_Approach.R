# Identify cohorts by length
rm(list=ls())

# Load packages
library(data.table)
library(tidyverse)
library(readxl)
library(here)
library(mixtools)
library(diptest)

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

#### Join similar species, lengths ####
data.all$bio <- data.all$bio[
  data.all$bio$species_name %notin% c('periwinkle',
                                      'horseshoe crab'),]

data.all$bio$species_name[
  data.all$bio$species_name %in% c('american eel',
                                   'glass eel elver')
] <- 'american eel'

data.all$bio$species_name[
  data.all$bio$species_name %in% c('hake', 
                                   'red hake',
                                   'spotted hake',
                                   'white hake')
] <- 'hake spp'

data.all$bio$species_name[
  data.all$bio$species_name %in% c('shortnose sturgeon',
                                   'unID sturgeon')
] <- 'sturgeon spp'

data.all$bio$species_name[
  data.all$bio$species_name %in% c('grubby sculpin',
                                   'longhorn sculpin',
                                   'shorthorn sculpin',
                                   'slimy sculpin',
                                   'striped sculpin',
                                   'unID sculpin')
] <- 'sculpin spp'

data.all$bio$species_name[
  data.all$bio$species_name %in% c('atlantic herring',
                                   'herring')
] <- 'atlantic herring'

data.all$bio$species_name[
  data.all$bio$species_name %in% c('banded killifish',
                                   'killifish')
] <- 'killifish spp'

data.all$bio$species_name[
  data.all$bio$species_name %in% c('northern pipefish',
                                   'pipefish')
] <- 'northern pipefish'

data.all$bio$species_name[
  data.all$bio$species_name %in% c('white mullet',
                                   'mullet')
] <- 'white mullet'

data.all$bio$species_name[
  data.all$bio$species_name %in% c('emerald shiner',
                                   'golden shiner',
                                   'unID shiner')
] <- 'shiner spp'

data.all$bio$species_name[
  data.all$bio$species_name %in% c('rock gunnel',
                                   'unID gunnel')
] <- 'rock gunnel'

data.all$bio$species_name[
  data.all$bio$species_name %in% c('threespine stickleback',
                                   'fourspine stickleback',
                                   'ninespine stickleback',
                                   'unID stickelback')
] <- 'stickleback spp'

# Pull lengths
lengths <- dplyr::select(data.all$bio,
                         species_name, length_mm, date)

# Remove one large mummichog
lengths <- lengths[lengths$length_mm != 385,]

# Set week
lengths$wk <- lubridate::isoweek(lengths$date)

# Remove early weeks
lengths <- lengths[lengths$wk >=24,]

# Set year
lengths$year <- lubridate::year(lengths$date)

# Force to numeric
lengths$length_mm <- as.numeric(lengths$length_mm)

# Remove missing values
lengths <- lengths[!is.na(lengths$length_mm),]

# Fix data types
lengths <- lengths %>% 
  mutate_at('species_name', as.factor) %>% 
  mutate_at(c('length_mm', 'wk', 'year'), as.numeric)

# Find biological limits
biolims <- lengths %>% 
  group_by(species_name) %>% 
  summarise(min.length = min(length_mm),
            max.length = max(length_mm))

#### Focus on top 9 ####
lengths <- lengths[lengths$species_name %in% 
                     c('alewife', 'atlantic silverside',
                       'atlantic herring', 'green crab', 
                       'winter flounder', 'mummichog', 
                       'tomcod', 'sandlance',
                       'sculpin spp'),]
biolims <- biolims[biolims$species_name %in% lengths$species_name,]

lengths <- lengths[with(lengths, order(species_name, wk, length_mm)),]

#### Split by period ####
lengths$period[lengths$year %in% c(2014, 2015, 2017, 2018, 2019)] <- 'cold'
lengths$period[lengths$year %in% c(2016, 2020, 2021, 2022, 2023)] <- 'hot'

lengths.1 <- lengths[lengths$period == 'cold',]
lengths.2 <- lengths[lengths$period == 'hot',]


#### Test on silversides ####
temp <- lengths[lengths$species_name == "atlantic silverside",]

# Test on 2022
temp <- temp[temp$year == 2022,]
  
  # Plot raw
  nrecords <- as.data.table(temp)[, .N, by = 'wk'][order(wk)]
  times <- NULL
  
  for(j in 1:nrow(nrecords)){
    if(nrecords[j]$N == 1){
      times <- c(times, nrecords[j]$wk)
    }else{
      times <- c(times, rep(nrecords[j]$wk, 2 ^ 8))
    }
  }
  
  ggplot() +
    geom_jitter(data = temp, 
                aes(x = wk, y = length_mm),
                alpha=0.35, width=0.2, stroke=NA, cex=2) +
    theme(legend.text = element_text(angle = 45, vjust = 0.4)) +
    geom_density(data = temp, 
                 aes(x = after_stat(scaled) * 0.9,
                     y = length_mm, group = wk),
                 n = 2 ^ 8,
                 position = position_nudge(x = times),
                 trim = T, adjust = 1.5) +
    geom_smooth(data=temp,
                aes(x=wk, y=length_mm)) +
    xlab('Week of year') + ylab('Length (mm)') +
    ggtitle(paste0(str_to_title('atlantic silverside'),
            ' raw length frequency ',
            paste0(temp$year[1]))) +
    ylim(c(5, 215)) +
    xlim(c(23, 40))

## Assume ages
d <- gam(length_mm ~ s(wk, bs='cs', k=15), 
         data=temp, method='REML')
plotData <- list()
trace(mgcv:::plot.gam, at = list(c(27, 1)), 
      ## tested for mgcv_1.8-4. other versions may need different at-argument.
      quote({
        message("ooh, so dirty -- assigning into globalenv()'s plotData...")
        plotData <<- pd
      }))
mgcv::plot.gam(d, seWithMean = TRUE, pages = 1)
plotData <- as.data.frame(cbind(plotData[[1]]$x, plotData[[1]]$fit))
colnames(plotData) <- c('wk', 'gamfit')

localmins  <- plotData[plotData$gamfit == min(plotData$gamfit),]
recruitweek <- floor(localmins$wk)
  
tempwk <- split(temp, f=temp$wk)

for(i in 1:length(tempwk)){
  dat <- tempwk[[i]]$length_mm
  
  y <- log(dat)
  X <- cbind(1, as.matrix(log(demonsnacks[,10]+1)))
  J <- ncol(X)
  for (j in 2:J) X[,j] <- CenterScale(X[,j])
  
  #########################  Data List Preparation  #########################
  mon.names <- "mu[1]"
  parm.names <- as.parm.names(list(beta=rep(0,J), sigma=0))
  pos.beta <- grep("beta", parm.names)
  pos.sigma <- grep("sigma", parm.names)
  PGF <- function(Data) {
    beta <- rnorm(Data$J)
    sigma <- runif(1)
    return(c(beta, sigma))
  }
  MyData <- list(J=J, PGF=PGF, X=X, mon.names=mon.names,
                 parm.names=parm.names, pos.beta=pos.beta, 
                 pos.sigma=pos.sigma, y=y)
  
  ##########################  Model Specification  ##########################
  Model <- function(parm, Data)
  {
    ### Parameters
    beta <- parm[Data$pos.beta]
    sigma <- interval(parm[Data$pos.sigma], 1e-100, Inf)
    parm[Data$pos.sigma] <- sigma
    ### Log-Prior
    beta.prior <- sum(dnormv(beta, 0, 1000, log=TRUE))
    sigma.prior <- dhalfcauchy(sigma, 25, log=TRUE)
    ### Log-Likelihood
    mu <- tcrossprod(Data$X, t(beta))
    LL <- sum(dnorm(Data$y, mu, sigma, log=TRUE))
    ### Log-Posterior
    LP <- LL + beta.prior + sigma.prior
    Modelout <- list(LP=LP, Dev=-2*LL, Monitor=mu[1],
                     yhat=rnorm(length(mu), mu, sigma), parm=parm)
    return(Modelout)
  }
  
  ############################  Initial Values  #############################
  #Initial.Values <- GIV(Model, MyData, PGF=TRUE)
  Initial.Values <- rep(0,J+1)
  
  Fit <- LaplacesDemon(Model, Data=MyData, Initial.Values,
                       Covar=NULL, Iterations=1000, Status=100, Thinning=1,
                       Algorithm="AFSS", 
                       Specs=list(A=500, B=NULL, m=100, n=0, w=1))
  
  Fit
  print(Fit)
  #PosteriorChecks(Fit)
  #caterpillar.plot(Fit, Parms="beta")
  #plot(Fit, MyData, PDF=FALSE)
  #Pred <- predict(Fit, Model, MyData, CPUs=1)
  #summary(Pred, Discrep="Chi-Square")
  #plot(Pred, Style="Covariates", Data=MyData)
  #plot(Pred, Style="Density", Rows=1:9)
  #plot(Pred, Style="Fitted")
  #plot(Pred, Style="Jarque-Bera")
  #plot(Pred, Style="Predictive Quantiles")
  #plot(Pred, Style="Residual Density")
  #plot(Pred, Style="Residuals")
  #Levene.Test(Pred)
  #Importance(Fit, Model, MyData, Discrep="Chi-Square")
  
  #Fit$Covar is scaled (2.38^2/d) and submitted to LaplacesDemon as Covar.
  #Fit$Summary[,1] is submitted to LaplacesDemon as Initial.Values.
  #End
  
  
}
