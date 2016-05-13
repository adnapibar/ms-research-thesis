#########################
# Perform NNETAR modelling
#########################
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
