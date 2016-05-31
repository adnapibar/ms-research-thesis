####################################################################################################
# The iid model is Y[t]=mu + Z[t] where Z[t] is a normal iid error. Forecasts are given by 
# Y[n+h]=mu where mu is estimated by the sample mean.
####################################################################################################
performMeanForecast <- function(horizon){
  train.winsize = 96*28    # size of the training window, 96 observations per day for 7 days
  slide.by = 96*5         # slide the training window by 5 days
  freq <- 96
  
  # 1 Step ahead forecast
  test.winsize = horizon     # forecast window
  
  time.slices = createTimeSlices(1:length(site.data), train.winsize, test.winsize, skip = slide.by)
  train.slices = time.slices[[1]]
  test.slices = time.slices[[2]]
  
  train.start.idx <- 1
  mean.forecast <- c()
  n <- length(train.slices)
 
  for(i in 1:n){
    train.site.data <- ts(site.data[train.slices[[i]]], start = c(train.start.idx,1), frequency = freq)
    test.start.idx <- end(train.site.data)[1]+1
    test.site.data <- ts(site.data[test.slices[[i]]], start = c(test.start.idx,1), frequency = freq)
    
    fc <- meanf(train.site.data, h=test.winsize, lambda = lambda)
    
    mean.forecast[i] <- sum(fc$mean[1:test.winsize])
    
    # Slide the training window
    train.start.idx <- train.start.idx + 1
  }
  
  # Actual observations
  actual <- c()
  for(i in 1:n){
    actual[i] <- sum(site.data[test.slices[[i]]])
  }
  return(list(actual, mean.forecast))
}