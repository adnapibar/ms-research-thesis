####################################################################################################
# Perform Exponential smoothing state space model

# The methodology is fully automatic. The only required argument for ets is the time series. 
# The model is chosen automatically if not specified. This methodology performed extremely well on 
# the M3-competition data
####################################################################################################
performExpSmoothing <- function(freq){
  train.start.idx <- 1
  ets.forecast <- list()
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
    
    # Exponential smoothing state space model
    ets.model <- ets(train.site.data)
    fc <- forecast(ets.model, h = 1)
    
    # Find out how to collect the point forcasts 
    ets.forecast[i] <- fc$mean[1]
    
    acc <- data.frame(accuracy(fc, test.site.data))
    rmse[i] <- acc$RMSE[2]
    
    # Slide the training window
    train.start.idx <- train.start.idx + 3
  }
  
  # Plot the actual vs forecast values
  pdf(paste(plots.dir,'exp-smoothing.pdf', sep = ''))
  plot(1:n,actual, type='l', col=2, xlab='Iteration', ylab='Traffic Volume (15 min)')
  lines(1:n, ets.forecast, type='l',col=3)
  legend("topleft",legend=c("Actual","Exponential smoothing"),col=2:3,lty=1)
  dev.off()
  
  return(rmse)
  
}