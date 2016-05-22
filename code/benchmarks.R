# Load the common stuff
print("Load libraries, source codes and data...")
system.time(source("code/common.R"))
print("Done!")

print("Naive...")
system.time(for(i in 1:2){performNaiveForecast(i)})
print("Done!")

print("MEANF...")
system.time(for(i in 1:2){performMeanForecast(i)})
print("Done!")

print("Linear Regression...")
system.time(for(i in 1:2){performLinearRegression(i)})
print("Done!")

print("NNAR...")
system.time(for(i in 1:2){performNNETAR(i)})
print("Done!")

print("ETS...")
system.time(for(i in 1:2){performExpSmoothing(i)})
print("Done!")

print("ARIMA...")
system.time(for(i in 1:2){performArima(i)})
print("Done!")
