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
performArima <- function(horizon){
  train.winsize = 96*28    # size of the training window, 96 observations per day for 7 days
  slide.by = 96*5         # slide the training window by 5 days
  freq <- 96
  
  # 1 Step ahead forecast
  test.winsize = horizon     # forecast window
  
  time.slices = createTimeSlices(1:length(site.data), train.winsize, test.winsize, skip = slide.by)
  train.slices = time.slices[[1]]
  test.slices = time.slices[[2]]
  arima.forecast <- c()
  n <- length(train.slices)
  
  mdl <- NULL
  mdl.data <- NULL
  
  for(i in 1:n){
    train.site.data <- ts(site.data[train.slices[[i]]], start = c(i,1), frequency = freq)
    
    if(is.null(mdl)){
      mdl <- auto.arima(train.site.data, approximation=FALSE,trace=FALSE, lambda = lambda)
      fc <- forecast(mdl, h = test.winsize)
      mdl.data <- train.site.data
    }else{
      arima.model <- Arima(c(mdl.data,train.site.data), model=mdl,  lambda = lambda)
      fc <- forecast(arima.model, h = test.winsize)
    }
    arima.forecast[i] <- sum(fc$mean[1:test.winsize])
  }
  # Actual observations
  actual <- c()
  for(i in 1:n){
    actual[i] <- sum(site.data[test.slices[[i]]])
  }
  plot.predictions(actual,arima.forecast, "ARIMA", paste("arima",horizon,sep=''))
  
  print(accuracy(actual,arima.forecast))
 
}
