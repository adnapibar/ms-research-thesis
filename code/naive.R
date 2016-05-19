####################################################################################################
# naive() returns forecasts and prediction intervals for an ARIMA(0,1,0) 
# random walk model applied to x
####################################################################################################
performNaiveForecast <- function(freq){
  train.start.idx <- 1
  naive.forecast <- c()
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
  
  for(i in 1:n){
    train.site.data <- ts(site.data[train.slices[[i]]], start = c(train.start.idx,1), frequency = freq)
    test.start.idx <- end(train.site.data)[1]+1
    test.site.data <- ts(site.data[test.slices[[i]]], start = c(test.start.idx,1), frequency = freq)

    fc <- naive(train.site.data, h=1, lambda = lambda)
    
    naive.forecast[i] <- fc$mean[1]
    
    acc <- data.frame(accuracy(fc, test.site.data))
    me[i] <- acc$ME[2]
    rmse[i] <- acc$RMSE[2]
    mae[i] <- acc$MAE[2]
    mpe[i] <- acc$MPE[2]
    mape[i] <- acc$MAPE[2]
    mase[i] <- acc$MASE[2]
    
    # Slide the training window
    train.start.idx <- train.start.idx + 3
  }
  
  # Plot the actual vs forecast values
  pdf(paste(plots.dir,'naive.pdf', sep = ''))
  plot(1:n, actual, type='l', col='red', xlab='Iteration', ylab='Traffic Volume (15 min)')
  lines(1:n, naive.forecast, type='l',col='blue')
  legend("topleft",legend=c("Actual","Naive"),col=c('red','blue'),lty=1)
  dev.off()
  
  print("Naive...")
  print(paste("ME = ", mean(me)))
  print(paste("RMSE = ", mean(rmse)))
  print(paste("MAE = ", mean(mae)))
  print(paste("MPE = ", mean(mpe)))
  print(paste("MAPE = ", mean(mape)))
  print(paste("MASE = ", mean(mase)))
}