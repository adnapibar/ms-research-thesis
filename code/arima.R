####################################################################################################
# Fit an ARIMA model to a univariate time series.
# Different definitions of ARMA models have different signs for the AR and/or MA coefficients. 
# The definition used here has

# X[t] = a[1]X[t-1] + ... + a[p]X[t-p] + e[t] + b[1]e[t-1] + ... + b[q]e[t-q]

# and so the MA coefficients differ in sign from those of S-PLUS. Further, if include.mean is true 
# (the default for an ARMA model), this formula applies to X - m rather than X. For ARIMA models 
# with differencing, the differenced series follows a zero-mean ARMA model. If am xreg term is 
# included, a linear regression (with a constant term if include.mean is true and there is no 
# differencing) is fitted with an ARMA model for the error term.

# The variance matrix of the estimates is found from the Hessian of the log-likelihood, and so may 
# only be a rough guide.

# Optimization is done by optim. It will work best if the columns in xreg are roughly scaled to zero 
# mean and unit variance, but does attempt to estimate suitable scalings.
####################################################################################################
performArima <- function(freq){
  train.start.idx <- 1
  arima.forecast <- list()
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
    
    # ARIMA
    arima.model <- Arima(train.site.data, order=c(5,0,3))
    fc <- forecast(arima.model, h = 1)
    
    # Find out how to collect the point forcasts 
    arima.forecast[i] <- fc$mean[1]
    
    acc <- data.frame(accuracy(fc, test.site.data))
    rmse[i] <- acc$RMSE[2]
    
    # Slide the training window
    train.start.idx <- train.start.idx + 3
  }
  
  # Plot the actual vs forecast values
  pdf(paste(plots.dir,'arima.pdf', sep = ''))
  plot(1:n,actual, type='l', col=2, xlab='Iteration', ylab='Traffic Volume (15 min)')
  lines(1:n, arima.forecast, type='l',col=3)
  legend("topleft",legend=c("Actual","ARIMA"),col=2:3,lty=1)
  dev.off()
  
  return(rmse)
}
