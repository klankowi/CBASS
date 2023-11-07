# Intro to R lab 1 answers

# Load file
setwd("C:/Users/klankowicz/Documents/GitHub/CBASS/Learning_R")
baptisms2 <- read.csv('us_baptisms.csv')

# Overview of data
summary(baptisms2)
dim(baptisms2)
colnames(baptisms2)
# Years: 1940-2002. Dimensions: 63 rows, 3 columns. Names: years, boys, girls

# Line plot
plot(baptisms2$year, baptisms2$boys / (baptisms2$girls + baptisms2$boys), type='l')
# OR, you could do
baptisms2$boy_proportion <- baptisms2$boys / (baptisms2$girls + baptisms2$boys)
plot(baptisms2$year, baptisms2$boy_proportion, type='l')
# Either way, higher proportion of boys to girls with slight decreasing trend. 
# Massive dip in proportion near end of dataset. Most years have more boys.

# Total number
sum(baptisms2$boys + baptisms2$girls)

# Years with more girls
baptisms2$year[baptisms2$girls > baptisms2$boys]
