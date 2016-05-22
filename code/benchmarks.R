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

performNaiveForecast(1)
performNaiveForecast(2)

performMeanForecast(1)
performMeanForecast(2)

performLinearRegression(1)
performLinearRegression(2)

performNNETAR(1)
performNNETAR(2)

performExpSmoothing(1)
performExpSmoothing(2)

performArima(1)
performArima(2)

