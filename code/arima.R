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
performArima <- function(){
  # split the site data into train and test where training window from 01-01-2013 to 31-05-2013
  # and test data is a sliding window for month of June
  # list to contain the forecasted values
  forecasted15 <- c()
  forecasted30 <- c()
  forecasted45 <- c()
  
  # Fit a model
  train.data <- site.data[1:(96*151)]
  train.data.ts <- ts(train.data, start = c(1,1), frequency = 96)
  mdl <- auto.arima(train.data.ts, lambda = lambda)
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
    refit <- Arima(train.data.ts, model=mdl, lambda = lambda)
    
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
  
  return(list(forecasted15, forecasted30, forecasted45,mdl))
}
