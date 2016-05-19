# Load the common stuff
source("code/common.R")
source("code/naive.R")
source("code/meanf.R")
source("code/linear_regression.R")
source("code/arima.R")
source("code/nnetar.R")
source("code/exponential_smoothing.R")

train.winsize = 96*7    # size of the training window, 96 observations per day for 7 days
test.winsize = 2        # forecast next two observations
slide.by = 96*5         # slide the training window by 5 days
freq <- 96

index <- getIndexByHF(15773)
site.data <- handleMissingData(volume.data[,index])
lambda <- BoxCox.lambda(site.data)

time.slices = createTimeSlices(1:length(site.data), train.winsize, test.winsize, skip = slide.by)
train.slices = time.slices[[1]]
test.slices = time.slices[[2]]

performMeanForecast(freq, test.winsize)
performNaiveForecast(freq, test.winsize)
performLinearRegression(freq, test.winsize)
performNNETAR(freq, test.winsize)
performExpSmoothing(freq, test.winsize)
performArima(freq, test.winsize)
