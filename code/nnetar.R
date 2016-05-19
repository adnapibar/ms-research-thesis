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
performNNETAR <- function(freq, test.winsize){
  train.start.idx <- 1
  nn.forecast <- c()
  n <- length(train.slices)
  me <- c()
  rmse <- c()
  mae <- c()
  mpe <- c()
  mape <- c()
  mase <- c()
 
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
    nn.forecast[i] <- fc$mean[1]
    
    acc <- data.frame(accuracy(fc, site.data[test.slices[[i]]]))
    me[i] <- acc$ME[2]
    rmse[i] <- acc$RMSE[2]
    mae[i] <- acc$MAE[2]
    mpe[i] <- acc$MPE[2]
    mape[i] <- acc$MAPE[2]
    mase[i] <- acc$MASE[2]
  }
  
  if(test.winsize == 1){
    # Actual observations
    actual <- c()
    for(i in 1:n){
      actual[i] <- site.data[test.slices[[i]]]
    }
    
    # Plot the actual vs forecast values
    pdf(paste(plots.dir,'nnetar.pdf', sep = ''))
    plot(1:n,actual, type='l', col='red', xlab='Test Number', ylab='Traffic Volume (15 min)')
    lines(1:n, nn.forecast, type='l',col='blue')
    legend("topleft",legend=c("Actual","Feedforward Neural Network"),col=c('red','blue'),lty=1)
    dev.off()
  }
  
  print("Feedforward Neural Network.....")
  print(paste("ME = ", mean(me)))
  print(paste("MAE = ", mean(mae)))
  print(paste("RMSE = ", mean(rmse)))
  print(paste("MPE = ", mean(mpe)))
  print(paste("MAPE = ", mean(mape)))
  print(paste("MASE = ", mean(mase[!is.infinite(mase)])))
  
  return(list(me,mae,rmse,mpe,mape,mase))
}
