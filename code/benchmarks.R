# Load the common stuff
source("code/common.R")
source("code/naive.R")
source("code/meanf.R")
source("code/linear_regression.R")
source("code/arima.R")
source("code/nnetar.R")
source("code/exponential_smoothing.R")


index <- getIndexByHF(15773)
site.data <- handleMissingData(volume.data[,index])
lambda <- BoxCox.lambda(site.data)

train.winsize = 96*7    # size of the training window, 96 observations per day for 7 days
slide.by = 96*5         # slide the training window by 5 days
freq <- 96

# 1 Step ahead forecast
test.winsize = 1      # forecast window

time.slices = createTimeSlices(1:length(site.data), train.winsize, test.winsize, skip = slide.by)
train.slices = time.slices[[1]]
test.slices = time.slices[[2]]

meanf.accuracy1 <- performMeanForecast(freq, test.winsize)
naive.accuracy1 <-performNaiveForecast(freq, test.winsize)
linear.accuracy1 <-performLinearRegression(freq, test.winsize)
nnetar.accuracy1 <-performNNETAR(freq, test.winsize)
ets.accuracy1 <-performExpSmoothing(freq, test.winsize)
arima.accuracy1 <-performArima(freq, test.winsize)

# 2 Step ahead forecast
test.winsize = 2      # forecast window

time.slices = createTimeSlices(1:length(site.data), train.winsize, test.winsize, skip = slide.by)
train.slices = time.slices[[1]]
test.slices = time.slices[[2]]

meanf.accuracy2 <- performMeanForecast(freq, test.winsize)
naive.accuracy2 <-performNaiveForecast(freq, test.winsize)
linear.accuracy2 <-performLinearRegression(freq, test.winsize)
nnetar.accuracy2 <-performNNETAR(freq, test.winsize)
ets.accuracy2 <-performExpSmoothing(freq, test.winsize)
arima.accuracy2 <-performArima(freq, test.winsize)


# Plot accuracies
# MAE
# Plot the actual vs forecast values
n <- 405
pdf(paste(plots.dir,'mae-nnetar.pdf', sep = ''))
plot(1:n, nnetar.accuracy[[2]], type='l', xlab='Test Number', ylab='MAE', col=2)
#lines(1:n, naive.accuracy[2], type='l')
#legend("topleft",legend=c("MeanF","Naive","Linear Regression", "NN", "ETS"), lty=1, col=c(1:5))
dev.off()