####################################################################################################
# Fit a linear model with time series components
# tslm is largely a wrapper for lm() except that it allows variables "trend" and "season" which are 
# created on the fly from the time series characteristics of the data. The variable "trend" is a 
# simple time trend and "season" is a factor indicating the season (e.g., the month or the quarter 
# depending on the frequency of the data).
####################################################################################################
performLinearRegression <- function(horizon){
  train.winsize = 96*28    # size of the training window, 96 observations per day for 7 days
  slide.by = 96*5         # slide the training window by 5 days
  freq <- 96
  
  # 1 Step ahead forecast
  test.winsize = horizon     # forecast window
  
  time.slices = createTimeSlices(1:length(site.data), train.winsize, test.winsize, skip = slide.by)
  train.slices = time.slices[[1]]
  test.slices = time.slices[[2]]
  train.start.idx <- 1
  lm.forecast <- c()
  n <- length(train.slices)
  
  for(i in 1:n){
    train.site.data <- ts(site.data[train.slices[[i]]], start = c(train.start.idx,1), frequency = freq)
    test.start.idx <- end(train.site.data)[1]+1
    test.site.data <- ts(site.data[test.slices[[i]]], start = c(test.start.idx,1), frequency = freq)
    
    # Linear Regression
    lmfit <- tslm(train.site.data~season, lambda = lambda)
    fc <- forecast(lmfit, h = test.winsize)
    lm.forecast[i] <- sum(fc$mean[1:test.winsize])
    # Slide the training window
    train.start.idx <- train.start.idx + 1
  }
  # Actual observations
  actual <- c()
  for(i in 1:n){
    actual[i] <- sum(site.data[test.slices[[i]]])
  }
  
  return(list(actual,lm.forecast))
}