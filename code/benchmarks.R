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


# Plot errors
errors.15mins <- subset(metrics.error, Window=='15-mins', select = c(Method, MAE, RMSE, MAPE))
errors.30mins <- subset(metrics.error, Window=='30-mins', select = c(Method, MAE, RMSE, MAPE))
barplot(table(errors.15mins$MAE,errors.30mins$MAE), main='MAE',col='blue',
        legend = errors.15mins$Method, beside=TRUE)

ggplot(errors.15mins, aes(x = Method, fill = Method, y = MAE)) +
  geom_dotplot(binaxis = "y", stackdir = "center", position = "dodge",binwidth = 1.5)

