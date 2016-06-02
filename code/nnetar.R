####################################################################################################
# Perform NNETAR modelling
# A feed-forward neural network is fitted with lagged values of x as inputs and a single hidden 
# layer with size nodes. The inputs are for lags 1 to p, and lags m to mP where m=frequency(x). 
# If there are missing values in x or xreg), the corresponding rows (and any others which depend on 
# them as lags) are omitted from the fit. A total of repeats networks are fitted, each with random 
# starting weights. These are then averaged when computing forecasts. The network is trained for 
# one-step forecasting. Multi-step forecasts are computed recursively.
#
# For non-seasonal data, the fitted model is denoted as an NNAR(p,k) model, where k is the number 
# of hidden nodes. This is analogous to an AR(p) model but with nonlinear functions. For seasonal 
# data, the fitted model is called an NNAR(p,P,k)[m] model, which is analogous to an 
# ARIMA(p,0,0)(P,0,0)[m] model but with nonlinear functions.
####################################################################################################
performNNETAR <- function(){
  
  # split the site data into train and test where training window from 01-01-2013 to 31-05-2013
  # and test data is a sliding window for month of June
  # list to contain the forecasted values
  forecasted15 <- c()
  forecasted30 <- c()
  forecasted45 <- c()
  
  # Fit a model
  train.data <- site.data[1:(96*151)]
  train.data.ts <- ts(train.data, start = c(1,1), frequency = 96)
  mdl <- nnetar(train.data.ts, lambda = lambda)
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
    train.data.ts <- ts(train.data, start = c(1,1), frequency = 96)
    refit <- nnetar(train.data.ts, model=mdl, lambda = lambda)
    
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
