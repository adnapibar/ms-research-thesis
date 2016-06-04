####################################################################################################
# naive() returns forecasts and prediction intervals for an ARIMA(0,1,0) 
# random walk model applied to x
####################################################################################################
performNaiveForecast <- function(){
  
  forecasted15 <- c()
  forecasted30 <- c()
  forecasted45 <- c()
  
  train.winsize = 96*151    # size of the training window from 01-01-2012 to 31-05-2012 
  freq <- 96
  time.slices = createTimeSlices(1:length(site.data), train.winsize, 1)
  train.slices = time.slices[[1]]
  
  # test 
  n <- length(train.slices)
  for(i in 1:n){
    train.data.ts <- ts(site.data[train.slices[[i]]], start = c(1,1), frequency = freq)
    fc <- naive(train.data.ts,h = 1, lambda = lambda)
    forecasted15[i] <- sum(fc$mean)
    
    if(i%%2 != 0){
      fc <- naive(train.data.ts,h = 2, lambda = lambda)
      forecasted30[i] <- sum(fc$mean)
    }
    if((i-1)%%3 == 0){
      fc <- naive(train.data.ts,h = 3, lambda = lambda)
      forecasted45[i] <- sum(fc$mean)
    }
  }
  
  return(list(forecasted15, forecasted30, forecasted45))
}