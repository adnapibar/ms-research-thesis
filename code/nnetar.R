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
performNNETAR <- function(horizon){
  train.winsize = 96*28    # size of the training window, 96 observations per day for 7 days
  slide.by = 96*5         # slide the training window by 5 days
  freq <- 96
  
  # 1 Step ahead forecast
  test.winsize = horizon     # forecast window
  
  time.slices = createTimeSlices(1:length(site.data), train.winsize, test.winsize, skip = slide.by)
  train.slices = time.slices[[1]]
  test.slices = time.slices[[2]]
  
  nn.forecast <- c()
  n <- length(train.slices)
 
  mdl <- NULL
  mdl.data <- NULL
  for(i in 1:n){
    train.site.data <- ts(site.data[train.slices[[i]]], start = c(i,1), frequency = freq)
    
    if(is.null(mdl)){
      mdl <- nnetar(train.site.data, lambda = lambda)
      fc <- forecast(mdl, h = test.winsize)
      mdl.data <- train.site.data
    }else{
      nn.model <- nnetar(c(mdl.data,train.site.data), model=mdl, lambda = lambda)
      fc <- forecast(nn.model, h = test.winsize)
    }
    nn.forecast[i] <- sum(fc$mean[1:test.winsize])
  }
  
  # Actual observations
  actual <- c()
  for(i in 1:n){
    actual[i] <- sum(site.data[test.slices[[i]]])
  }
  plot.predictions(actual,nn.forecast, "NNAR", paste("nnetar",horizon,sep=''))
  
  print(accuracy(actual,nn.forecast))
  
}
