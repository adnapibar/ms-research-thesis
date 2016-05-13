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
performNNETAR <- function(freq){
  train.start.idx <- 1
  nn.forecast <- list()
  n <- length(train.slices)
  rmse <- matrix(NA, n)   # Record the error for each iteration
  # Actual observations
  actual <- list()
  for(i in 1:n){
    actual[i] <- site.data[test.slices[[i]]]
  }
  
  for(i in 1:n){
    train.site.data <- ts(site.data[train.slices[[i]]], start = c(train.start.idx,1), frequency = freq)
    
    test.start.idx <- end(train.site.data)[1]+1
    test.site.data <- ts(site.data[test.slices[[i]]], start = c(test.start.idx,1), frequency = freq)
    
    # Feed-forward neural networks with a single hidden layer and lagged inputs for forecasting univariate time series.
    nnfit <- nnetar(train.site.data)
    fc <- forecast(nnfit, h = 1)
    
    # Find out how to collect the point forcasts 
    nn.forecast[i] <- fc$mean[1]
    
    acc <- data.frame(accuracy(fc, test.site.data))
    rmse[i] <- acc$RMSE[2]
    
    # Slide the training window
    train.start.idx <- train.start.idx + 3
  }
  
  # Plot the actual vs forecast values
  pdf(paste(plots.dir,'nnetar.pdf', sep = ''))
  plot(1:n,actual, type='l', col=2, xlab='Iteration', ylab='Traffic Volume (15 min)')
  lines(1:n, nn.forecast, type='l',col=3)
  legend("topleft",legend=c("Actual","Neural Network Time Series"),col=2:3,lty=1)
  dev.off()
  
  return(rmse)
}
