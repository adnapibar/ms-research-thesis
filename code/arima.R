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

  arima.forecast <- c()
  n <- length(train.slices)
  me <- c()
  rmse <- c()
  mae <- c()
  mpe <- c()
  mape <- c()
  mase <- c()
  # Actual observations
  actual <- c()
  for(i in 1:n){
    actual[i] <- site.data[test.slices[[i]]]
  }
  
  mdl <- NULL
  mdl.data <- NULL
  
  for(i in 1:n){
    train.site.data <- ts(site.data[train.slices[[i]]], start = c(i,1), frequency = freq)
    
    if(is.null(mdl)){
      mdl <- auto.arima(train.site.data, approximation=FALSE,trace=FALSE, lambda = lambda)
      fc <- forecast(mdl, h = 1)
      mdl.data <- train.site.data
    }else{
      arima.model <- Arima(c(mdl.data,train.site.data), model=mdl,  lambda = lambda)
      fc <- forecast(arima.model, h = 1)
    }
    arima.forecast[i] <- fc$mean[1]
    
    acc <- data.frame(accuracy(fc, site.data[test.slices[[i]]]))
    me[i] <- acc$ME[2]
    rmse[i] <- acc$RMSE[2]
    mae[i] <- acc$MAE[2]
    mpe[i] <- acc$MPE[2]
    mape[i] <- acc$MAPE[2]
    mase[i] <- acc$MASE[2]
  }
  
  # Plot the actual vs forecast values
  pdf(paste(plots.dir,'arima.pdf', sep = ''))
  plot(1:n, actual, type='l', col='blue', xlab='Iteration', ylab='Traffic Volume (15 min)')
  lines(1:n, arima.forecast, type='l', col='red')
  legend("topleft",legend=c("Actual","ARIMA"), col=c('blue','red'),lty=1)
  dev.off()
  
  print("ARIMA.....")
  print(paste("ME = ", mean(me)))
  print(paste("RMSE = ", mean(rmse)))
  print(paste("MAE = ", mean(mae)))
  print(paste("MPE = ", mean(mpe)))
  print(paste("MAPE = ", mean(mape)))
  print(paste("MASE = ", mean(mase)))
}
