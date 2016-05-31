# Load the common stuff
print("Load libraries, source codes and data...")
system.time(source("code/common.R"))
print("Done!")

print("Naive...")
# 1 step ahead
naive15 <- performNaiveForecast(1)
p <- plot.predictions(naive15[[1]],naive15[[2]])
p
print(accuracy(naive15[[1]],naive15[[2]]))

# 2 step ahead
naive30 <- performNaiveForecast(2)
p <- plot.predictions(naive30[[1]],naive30[[2]])
p
print(accuracy(naive30[[1]],naive30[[2]]))

print("Done!")

print("MEANF...")
meanf15 <- performMeanForecast(1)
p <- plot.predictions(meanf15[[1]],meanf15[[2]])
p
print(accuracy(meanf15[[1]],meanf15[[2]]))

# 2 step ahead
meanf30 <- performMeanForecast(2)
p <- plot.predictions(meanf30[[1]],meanf30[[2]])
p
print(accuracy(meanf30[[1]],meanf30[[2]]))
print("Done!")

print("Linear Regression...")
lr15 <- performLinearRegression(1)
p <- plot.predictions(lr15[[1]],lr15[[2]])
p
print(accuracy(lr15[[1]],lr15[[2]]))

# 2 step ahead
lr30 <- performLinearRegression(2)
p <- plot.predictions(lr30[[1]],lr30[[2]])
p
print(accuracy(lr30[[1]],lr30[[2]]))
print("Done!")

print("NNAR...")
nn15 <- performNNETAR(1)
p <- plot.predictions(nn15[[1]],nn15[[2]])
p
print(accuracy(nn15[[1]],nn15[[2]]))

# 2 step ahead
nn30 <- performNNETAR(2)
p <- plot.predictions(nn30[[1]],nn30[[2]])
p
print(accuracy(nn30[[1]],nn30[[2]]))
print("Done!")

print("ETS...")
ets15 <- performExpSmoothing(1)
p <- plot.predictions(ets15[[1]],ets15[[2]])
p
print(accuracy(ets15[[1]],ets15[[2]]))

# 2 step ahead
ets30 <- performExpSmoothing(2)
p <- plot.predictions(ets30[[1]],ets30[[2]])
p
print(accuracy(ets30[[1]],ets30[[2]]))
print("Done!")

print("ARIMA...")
arima15 <- performArima(1)
p <- plot.predictions(arima15[[1]],arima15[[2]])
p
print(accuracy(arima15[[1]],arima15[[2]]))

# 2 step ahead
arima30 <- performArima(2)
p <- plot.predictions(arima30[[1]],arima30[[2]])
p
print(accuracy(arima30[[1]],arima30[[2]]))
print("Done!")