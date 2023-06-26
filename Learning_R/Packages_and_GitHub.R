# Welcome back!

# In this script, we will learn how to load packages, the function of the "here"
# package, and how to use GitHub.

# We'll start with packages

# Let's completely wipe the slate clean from our last session.
rm(list=ls())

# Then go to Session in the top bar, and hit Restart R. This will unload any
# old package libraries you have loaded and wipe the old working directory.

#### Packages ####
# R is a highly-flexible open-source statistical software. People have developed
# thousands of functions to do a million different things, as we have discussed.

# If you develop enough functions that are related to a single topic, you can 
# put them all together and publish them as a package. This makes it much easier
# for you and everyone else to keep all your related work together.

# Packages need to be installed to your version of R. THIS ONLY NEEDS TO HAPPEN
# ONCE. Do not attempt to install packages multiple times. It isn't necessary.

# Let's install a very commonly-used package, tidyverse.

install.packages('tidyverse')

# Once you run this line of code, your computer should be able to automatically
# connect to the repository of packages hosted by R on the Comprehensive R
# Archive Network (CRAN).

# Every time you want to use a function from this package, you need to load the
# package into your current R session. This needs to happen once every time you
# open R. 

# This is like looking up a formula in a math book. If you don't have it memorized,
# you'll need to open the book with the formula in it to the correct page every 
# time you want to use the formula.

# R does not "remember" the functions within these packages from session to session.
# So it's like you need to open the book to the formula page for it every time.

library(tidyverse)

# Now you can use the various functions within this package.

#### GitHub ####

# The field of natural sciences is highly collaborative. Frequently, you'll be
# working with people on a research paper. You'll also frequently be updating 
# your data and analyses. It can be really challenging to maintain data integrity.

# This is where version control comes in. If you have a dataset OR a script with
# code in it, you'll want to know WHO changes WHAT and WHEN.

# GitHub is a commonly used cloud- and platform-based software for version control.

# Please go to GitHub.com and make an account, if you do not already have one.

# Then download GitHub desktop (https://desktop.github.com/)

# Let's make sure you all have a copy of our CBASS data. I (klankowi) am the host
# for this repository. Please login to GitHub and go to 
# https://github.com/klankowi/CBASS

# Once on the page, click 'Fork.' This is the language used when you want to 
# copy someone's repository. For now, select the option that you want to copy my
# files for your own use, not to upload to the master version. This way, you can
# play with the data present in the repository without any risk of affecting the
# master data.

# Once you've forked the repository, go to GitHub desktop. Click File, then
# Clone Repository. This will copy all my files to your computer.

# In the top right of RStudio, you'll see a button for "projects." Click on it.

# Click New Project, then Existing Directory, then navigate to the folder on your
# machine that has all the files from the CBASS GitHub repository you just cloned.

# Once you have this project set up, you have super-easy access to everything in 
# our GitHub repository.

#### here package ####
# I use this package every day to make data loading easy. Let's install it.
install.packages("here")

# Make sure that you have the CBASS project open in RStudio. Then, load the package.
library(here)

# This automatically sets your working directory to our copied GitHub folder.

# Now you can set your working directory super easily using this syntax
setwd(here())

# Notice that you don't need that long string of "C:/Username/blahblah" that 
# you needed to provide before.

# You can also navigate through subfolders. I host our "Learning R" data in a 
# subfolder called "Learning_R". You can get there like this:
setwd(here("Learning_R"))

# That's the end of this particular session. Please switch over to 
# "Intro_to_RMarkdown.RMD" to finish out what we are doing.


