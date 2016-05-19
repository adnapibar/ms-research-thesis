####################################################################################################
# Perform Exponential smoothing state space model

# The methodology is fully automatic. The only required argument for ets is the time series. 
# The model is chosen automatically if not specified. This methodology performed extremely well on 
# the M3-competition data
####################################################################################################
performExpSmoothing <- function(freq){
  train.start.idx <- 1
  ets.forecast <- c()
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
      mdl <- ets(train.site.data, lambda = lambda)
      fc <- forecast(mdl, h = 1)
      mdl.data <- train.site.data
    }else{
      ets.model <- ets(c(mdl.data,train.site.data), model=mdl, lambda = lambda)
      fc <- forecast(ets.model, h = 1)
    }
    ets.forecast[i] <- fc$mean[1]
    
    acc <- data.frame(accuracy(fc, site.data[test.slices[[i]]]))
    me[i] <- acc$ME[2]
    rmse[i] <- acc$RMSE[2]
    mae[i] <- acc$MAE[2]
    mpe[i] <- acc$MPE[2]
    mape[i] <- acc$MAPE[2]
    mase[i] <- acc$MASE[2]
  }
  
  # Plot the actual vs forecast values
  pdf(paste(plots.dir,'exp-smoothing.pdf', sep = ''))
  plot(1:n,actual, type='l', col='red', xlab='Iteration', ylab='Traffic Volume (15 min)')
  lines(1:n, ets.forecast, type='l',col='blue')
  legend("topleft",legend=c("Actual","Exponential smoothing"),col=c('red','blue'),lty=1)
  dev.off()
  
  print("Exponential Smoothing.....")
  print(paste("ME = ", mean(me)))
  print(paste("RMSE = ", mean(rmse)))
  print(paste("MAE = ", mean(mae)))
  print(paste("MPE = ", mean(mpe)))
  print(paste("MAPE = ", mean(mape)))
  print(paste("MASE = ", mean(mase)))
  
}