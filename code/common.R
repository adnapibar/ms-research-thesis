# Initise some common stuff
# Reqiuired libraries
library(xts)
library(lubridate)
library(forecast)
library(caret)
library(ggplot2)

# Figures directory
plots.dir <- "plots/"

# The VicRoads volume data is a time series data that is 
# collected using road traffic sensors at a 15 minutes interval. 
# The data we have is for 1084 site locations over the period 
# starting from 2008-01-01 to 2013-07-26, a total of 195168 observations.

# Read the volume data from the csv into a dataframe, where
# rows as observations and columns as locations.
if(!exists('volume.data')){
  volume.data <- read.csv('data/volume_data.csv', header = FALSE)
}

# Handle Missing Data - one way of doing this is to fill the missing
# data with adjacent values. Or we can replace the missing values with
# the average data
handleMissingData <- function (site.data){
  avg <- mean(site.data) 
  site.data[site.data==0] <- avg
  site.data
}