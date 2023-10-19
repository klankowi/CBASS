# Welcome to R! 

# This file, called a script, will serve as your basic introduction to working
# in R.

# These lines of text are grayed out. This is because I have designated them as
# lines to NOT be run in the software. If you ever need to write a quick note
# to yourself but DON'T want that line to be run, use a # symbol at the beginning
# of the line. Watch what happens if we forget that symbol.

This line is not grayed out and R will try to process it. Not good.
  
# Later, we will learn to use RMarkdown documents, which are much better for 
# documenting working processes and creating publishable reports. For now, we 
# will use this shortcut method of writing to ourselves.
  
# R is a great workhorse, but it needs constant direction to understand what 
# you want it to do. We will start by learning how to set a "working directory"
# in which R will search for inputs and save any potential outputs.

# Press CTRL+Enter/ CMD+Enter while your cursor is in a line to run it.
  
setwd("C:/Users/klankowicz/Documents/GitHub/CBASS/Learning_R")

# Your exact file path will depend on where you saved the 'baptisms.csv' file.

# You can use the following line to check your working directory at any point.
getwd()

# setwd() and getwd() are functions, which are a collection of related commands
# run the same way every time to achieve a goal. Functions can be built-in, like
# these, or user-defined.

# Now let's load in our data.
baptisms <- read.csv("baptisms.csv")

# This is a record of baptisms in London from the 1600s to 1700s. It will be our
# data for this learning experience. We have assigned the "baptisms" object to 
# be the contents of this comma-separated values file (.csv).

# We can check the contents of this object by using the following
head(baptisms, 10)
colnames(baptisms)
summary(baptisms)
dim(baptisms)

# What are the column names?

# What is the range of years?

# How many rows of data are there?

# Let's dig into the data. We can isolate one column of data like so:
baptisms$boys
# This shows us the number of boys baptized in each year of the dataset.

# How would we do this to pull the data for baptisms of girls?

# Let's make a scatterplot the number of boy baptisms per year.
plot(x=baptisms$year, y=baptisms$boys)

# If we want this to be a lineplot with connected dots, we use this command:
plot(x=baptisms$year, y=baptisms$boys, type='l')

# For the plot funciton, and any other R function, you can learn more about
# possible inputs and options by using a question mark
?plot

# R can be used as a calculator for everything from basic arithmetic to advanced
# calculus. Let's find the total number of baptisms in every year. 
# We technically could do this the long way, like adding for every year
head(baptisms)
5218 + 4693

# But there is a much faster way. R can handle adding across two vectors, like so:
baptisms$boys + baptisms$girls

# R can also add these vectors together.
sum(baptisms$boys + baptisms$girls)

# Let's plot the sum of boy and girl baptisms for every year in the dataset.
plot(baptisms$year, baptisms$boys + baptisms$girls, type='l')

# Now let's find the proportion of baptisms that are for girls in each year.
# Similar to before, we could do this the long way.
head(baptisms)
4683 / (5218 + 4683)

# But R can do this quicker, too.
baptisms$girls / (baptisms$boys + baptisms$girls)

# Remember to use proper order of operations. R does not have a brain like you do,
# so it will do whatever operation you tell it to without knowing if it is what 
# you intended or not.

# Let's save this proportion as a new column in our data.
baptisms$girl_proportion <- baptisms$girls / (baptisms$boys + baptisms$girls)

# We have now assigned a new column using the arrow. The shortcut for the arrow
# is ALT + - / Option + -

# You can find all keyboard shortcuts at
# https://support.posit.co/hc/en-us/articles/200711853-Keyboard-Shortcuts-in-the-RStudio-IDE

# How would we plot this new girl proportion column per year?

# Now, let's find out which years had more girl baptisms than boys
baptisms$girls > baptisms$boys

# This is a vector of logical data, where R returns whether the expression is 
# TRUE or FALSE for each row. Let's save this as a column, too.

baptisms$girl_more <- baptisms$girls > baptisms$boys

# We can check how many years have more girl baptisms by the following function
table(baptisms$girl_more)

# Two years had more girl baptisms than boys.

# Here is how we subset the data to find out which years these were.
subset(baptisms, girl_more == 'TRUE')
new <- subset(baptisms, girl_more == 'TRUE')

# Or we could do this
baptisms$year[baptisms$girl_more == 'TRUE']
baptisms[baptisms$girl_more == "TRUE",]

# For any R-related question, Googling will yield pretty good information.
# I like to use stackoverflow.com or stackexchange.com.

rm(new)
rm(list=ls())