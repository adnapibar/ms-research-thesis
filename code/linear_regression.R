####################################################################################################
# Fit a linear model with time series components
# tslm is largely a wrapper for lm() except that it allows variables "trend" and "season" which are 
# created on the fly from the time series characteristics of the data. The variable "trend" is a 
# simple time trend and "season" is a factor indicating the season (e.g., the month or the quarter 
# depending on the frequency of the data).
####################################################################################################
performLinearRegression <- function(){
  
  
  forecasted15 <- c()
  forecasted30 <- c()
  forecasted45 <- c()
  
  train.winsize = 96*151    # size of the training window from 01-01-2013 to 31-05-2013 
  freq <- 96
  time.slices = createTimeSlices(1:length(site.data), train.winsize, 1)
  train.slices = time.slices[[1]]
  
  # test 
  n <- length(train.slices)
  for(i in 1:n){
    train.data.ts <- ts(site.data[train.slices[[i]]], start = c(1,1), frequency = freq)
    lmfit <- tslm(train.data.ts~season, lambda = lambda)
    fc <- forecast(lmfit, h = 1)
    forecasted15[i] <- sum(fc$mean)
    
    if(i%%2 != 0){
      fc <- forecast(lmfit, h = 2)
      forecasted30[i] <- sum(fc$mean)
    }
    if((i-1)%%3 == 0){
      fc <- forecast(lmfit, h = 3)
      forecasted45[i] <- sum(fc$mean)
    }
  }
  
  return(list(forecasted15, forecasted30, forecasted45))

}