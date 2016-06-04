####################################################################################################
# Perform Exponential smoothing state space model

# The methodology is fully automatic. The only required argument for ets is the time series. 
# The model is chosen automatically if not specified. This methodology performed extremely well on 
# the M3-competition data
####################################################################################################
performExpSmoothing <- function(){
  
  # split the site data into train and test where training window from 01-01-2012 to 31-05-2012
  # and test data is a sliding window for month of June
  # list to contain the forecasted values
  forecasted15 <- c()
  forecasted30 <- c()
  forecasted45 <- c()
  
  # Fit a model, frequency is set to 24 as the ets model is unable to handle high frequency
  train.data <- site.data[1:(96*151)]
  train.data.ts <- ts(train.data, start = c(1,1), frequency = 24)
  mdl <- ets(train.data.ts, lambda = lambda)
  fc <- forecast(mdl, h = 1)
  forecasted15[1] <- sum(fc$mean)
  fc <- forecast(mdl, h = 2)
  forecasted30[1] <- sum(fc$mean)
  fc <- forecast(mdl, h = 3)
  forecasted45[1] <- sum(fc$mean)
  # test data
  test.data <- site.data[(96*151+1):length(site.data)]
  
  
  # test 
  n <- length(test.data)
  for(i in 2:n){
    train.data[length(train.data) + 1] <- test.data[i-1]
    train.data.ts <- ts(train.data, start = c(1,1), frequency = 24)
    refit <- ets(train.data.ts, model=mdl, lambda = lambda)
    
    fc <- forecast(refit, h = 1)
    forecasted15[i] <- sum(fc$mean)
    
    if(i%%2 != 0){
      fc <- forecast(refit, h = 2)
      forecasted30[i] <- sum(fc$mean)
    }
    if((i-1)%%3 == 0){
      fc <- forecast(refit, h = 3)
      forecasted45[i] <- sum(fc$mean)
    }
  }
  
  return(list(forecasted15, forecasted30, forecasted45, mdl))
  
}