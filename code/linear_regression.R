####################################################################################################
# Fit a linear model with time series components
# tslm is largely a wrapper for lm() except that it allows variables "trend" and "season" which are 
# created on the fly from the time series characteristics of the data. The variable "trend" is a 
# simple time trend and "season" is a factor indicating the season (e.g., the month or the quarter 
# depending on the frequency of the data).
####################################################################################################
performLinearRegression <- function(freq, test.winsize){
  train.start.idx <- 1
  lm.forecast <- c()
  n <- length(train.slices)
  me <- c()
  rmse <- c()
  mae <- c()
  mpe <- c()
  mape <- c()
  mase <- c()
  
  for(i in 1:n){
    train.site.data <- ts(site.data[train.slices[[i]]], start = c(train.start.idx,1), frequency = freq)
    test.start.idx <- end(train.site.data)[1]+1
    test.site.data <- ts(site.data[test.slices[[i]]], start = c(test.start.idx,1), frequency = freq)
    
    # Linear Regression
    lmfit <- tslm(train.site.data~trend, lambda = lambda)
    fc <- forecast(lmfit, h = test.winsize)
    
    # Find out how to collect the point forcasts 
    lm.forecast[i] <- fc$mean[1]
    
    acc <- data.frame(accuracy(fc, test.site.data))
    me[i] <- acc$ME[2]
    rmse[i] <- acc$RMSE[2]
    mae[i] <- acc$MAE[2]
    mpe[i] <- acc$MPE[2]
    mape[i] <- acc$MAPE[2]
    mase[i] <- acc$MASE[2]
    
    # Slide the training window
    train.start.idx <- train.start.idx + 1
  }
  
  if(test.winsize == 1){
    # Actual observations
    actual <- c()
    for(i in 1:n){
      actual[i] <- site.data[test.slices[[i]]]
    }
    # Plot the actual vs forecast values
    pdf(paste(plots.dir,'linear-regression.pdf', sep = ''))
    plot(1:n, actual, type='l', col='blue', xlab='Test Number', ylab='Traffic Volume (15 min)')
    lines(1:n, lm.forecast, type='l',col='red')
    legend("topleft",legend=c("Actual","Linear Regression"),col=c('blue','red'),lty=1)
    dev.off()
  }
  
  print("Linear Regression...")
  print(paste("ME = ", mean(me)))
  print(paste("MAE = ", mean(mae)))
  print(paste("RMSE = ", mean(rmse)))
  print(paste("MPE = ", mean(mpe)))
  print(paste("MAPE = ", mean(mape)))
  print(paste("MASE = ", mean(mase[!is.infinite(mase)])))
  
  return(list(me,mae,rmse,mpe,mape,mase))
}