# Load the common stuff
source("code/common.R")
source("code/arima.R")
source("code/linear_regression.R")
source("code/nnetar.R")
source("code/exponential_smoothing.R")

train.winsize = 96*28  # size of the training window, 96 observations per day for 28 days
test.winsize = 1         # forecast next one observation
slide.by = 96*21       # slide the training window by 21 days
freq <- 7*96

site.data <- handleMissingData(volume.data[,1])

time.slices = createTimeSlices(1:length(site.data), train.winsize, test.winsize, skip = slide.by)
train.slices = time.slices[[1]]
test.slices = time.slices[[2]]
rm(train.winsize, test.winsize, time.slices, slide.by)

rmse.lm <- performLinearRegression(freq)
rmse.arima <- performArima(freq)
rmse.nn <- performNNETAR(freq)
rmse.ets <- performExpSmoothing(freq)


# Plot the RMSE for each of the models
n <- length(train.slices)
pdf(paste(plots.dir,'rmse-benchmarks.pdf', sep = ''))
plot(1:n, rmse.lm, type="l", col=2, xlab="Iteration", ylab="RMSE")
lines(1:n, rmse.arima, type="l", col=3)
lines(1:n, rmse.nn, type="l", col=4)
lines(1:n, rmse.ets, type="l", col=5)
legend("topleft",legend=c("Linear Regression","ARIMA", "Neural Network", "Exponential Smoothing"),
       col=2:4,lty=1)
dev.off()

# Mean RMSE 
mean(rmse.lm)  # Linear Regression
mean(rmse.arima)  # ARIMA
mean(rmse.nn)  # Feed-Forward Neural Network with one hidden layer
mean(rmse.ets) # Exponential smoothing state space model
