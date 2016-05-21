####################################################################################################
# Perform Exponential smoothing state space model

# The methodology is fully automatic. The only required argument for ets is the time series. 
# The model is chosen automatically if not specified. This methodology performed extremely well on 
# the M3-competition data
####################################################################################################
performExpSmoothing <- function(horizon){
  train.winsize = 96*28    # size of the training window, 96 observations per day for 7 days
  slide.by = 96*5         # slide the training window by 5 days
  freq <- 96
  
  # 1 Step ahead forecast
  test.winsize = horizon     # forecast window
  
  time.slices = createTimeSlices(1:length(site.data), train.winsize, test.winsize, skip = slide.by)
  train.slices = time.slices[[1]]
  test.slices = time.slices[[2]]
  train.start.idx <- 1
  ets.forecast <- c()
  n <- length(train.slices)
  mdl <- NULL
  mdl.data <- NULL
  for(i in 1:n){
    train.site.data <- ts(site.data[train.slices[[i]]], start = c(i,1), frequency = freq)
    
    if(is.null(mdl)){
      mdl <- ets(train.site.data, lambda = lambda)
      fc <- forecast(mdl, h = test.winsize)
      mdl.data <- train.site.data
    }else{
      ets.model <- ets(c(mdl.data,train.site.data), model=mdl, lambda = lambda)
      fc <- forecast(ets.model, h = test.winsize)
    }
    ets.forecast[i] <- sum(fc$mean[1:test.winsize])
  }
  # Actual observations
  actual <- c()
  for(i in 1:n){
    actual[i] <- sum(site.data[test.slices[[i]]])
  }
  
  plot.predictions(actual,ets.forecast, "Exponential Smoothing", paste("ets",horizon,sep=''))
  
  print(accuracy(actual,ets.forecast))
 
  
}