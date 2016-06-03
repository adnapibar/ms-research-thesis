# Initise some common stuff
# Reqiuired libraries
library(xts)
library(lubridate)
library(forecast)
library(caret)
library(ggplot2)
library(plotly)
# Figures directory
plots.dir <- "latex-thesis/Plots/"

# The VicRoads volume data is a time series data that is 
# collected using road traffic sensors at a 15 minutes interval. 
# The data we have is for 1084 site locations over the period 
# starting from 2008-01-01 to 2013-07-26, a total of 195168 observations.

# Read the volume data from the csv into a dataframe, where
# rows as observations and columns as locations.
if(!exists('volume.data')){
  # volume.data <- read.csv('data/volume_data.csv', header = FALSE)
  # hf.list <- read.csv('data/hf_list.csv')
  # hf.ref <- read.csv('data/hf_ref.csv')
  load("saved_data.RData")
}

# Handle Missing Data - one way of doing this is to fill the missing
# data with adjacent values. Or we can replace the missing values with
# the average data
handleMissingData <- function (site.data){
  avg <- mean(site.data) 
  site.data[site.data==0] <- NA
  # use the forecast package's na.interp
  return(na.interp(site.data))
}

getIndexByHF<- function(hfNo){
  match(hfNo, hf.list[,1])
}

getRoadDetails<- function(hf.no){
  road <- hf.ref[hf.ref$HF==hf.no,]
}

plot.xts2 <- function (x, y = NULL, type = "l", auto.grid = TRUE, major.ticks = "auto", 
                       minor.ticks = TRUE, major.format = TRUE, bar.col = "grey", 
                       candle.col = "white", ann = TRUE, axes = TRUE, col = "black", ...) 
{
  series.title <- deparse(substitute(x))
  ep <- axTicksByTime(x, major.ticks, format = major.format)
  otype <- type
  if (xts:::is.OHLC(x) && type %in% c("candles", "bars")) {
    x <- x[, xts:::has.OHLC(x, TRUE)]
    xycoords <- list(x = .index(x), y = seq(min(x), max(x), 
                                            length.out = NROW(x)))
    type <- "n"
  }
  else {
    if (NCOL(x) > 1) 
      warning("only the univariate series will be plotted")
    if (is.null(y)) 
      xycoords <- xy.coords(.index(x), x[, 1])
  }
  plot(xycoords$x, xycoords$y, type = type, axes = FALSE, ann = FALSE, 
       col = col, ...)
  if (auto.grid) {
    abline(v = xycoords$x[ep], col = "grey", lty = 4)
    grid(NA, NULL)
  }
  if (xts:::is.OHLC(x) && otype == "candles") 
    plot.ohlc.candles(x, bar.col = bar.col, candle.col = candle.col, 
                      ...)
  dots <- list(...)
  if (axes) {
    if (minor.ticks) 
      axis(1, at = xycoords$x, labels = FALSE, col = "#BBBBBB", 
           ...)
    axis(1, at = xycoords$x[ep], labels = names(ep), las = 1, 
         lwd = 1, mgp = c(3, 2, 0), ...)
    axis(2, ...)
  }
  box()
  if (!"main" %in% names(dots)) 
    title(main = series.title)
  do.call("title", list(...))
  assign(".plot.xts", recordPlot(), .GlobalEnv)
}

# Plot predictions for the motnh of June 2013 = 96 * 30 observations
plot.predictions <- function(actual, predicted, step){
  by <- 15 * step
  by <- paste(by,'min')
  print(by)
  t <- seq(ymd_hms("2013-06-01 00:00:00"), by = by, length.out=length(actual))
  print(paste(step, '=', by, '-',length(actual), '=', length(t)))
  p <- plot_ly(x = t, y = actual, name = "Actual") %>%
    layout(xaxis = list(title = "Time"), yaxis = list(title = "Volume"), 
           width = 600, height = 350) %>% 
    add_trace(x = t, y = predicted, name = "Predicted")
  return(p)
}

source("code/naive.R")
source("code/meanf.R")
source("code/linear_regression.R")
source("code/arima.R")
source("code/nnetar.R")
source("code/exponential_smoothing.R")

#VICTORIA STREET 12612     W  BD 16913
index <- getIndexByHF(16913)
site.data <- volume.data[,index]
# For time series methods use a small subset of this data
# Jan - May 2013 for modelling and predict for June (181 days)
# end = 195168 - 26 * 96 = 192672
# start = 192672 - 181 * 96 + 1 = 175297
site.data <- site.data[175297:192672]
site.data <- handleMissingData(site.data)
lambda <- BoxCox.lambda(site.data)
road <- getRoadDetails(16913)