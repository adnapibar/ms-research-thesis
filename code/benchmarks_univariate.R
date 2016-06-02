# Load the common stuff
print("Load libraries, source codes and data...")
system.time(source("code/common.R"))
print("Done!")

#################################################################################
# Keep the actual values for reuse
actual15 <- site.data[(96*151+1):length(site.data)]
actual30 <- actual15[seq(1,length(actual15),2)] + actual15[seq(2,length(actual15),2)]
actual45 <- actual15[seq(1,length(actual15),3)] + actual15[seq(2,length(actual15),3)] 
+ actual15[seq(3,length(actual15),3)] 

################################################################################
print("Naive...")
naiveforecasts <- performNaiveForecast()
f15 <- naiveforecasts[[1]]
f30 <- naiveforecasts[[2]]
f30 <- f30[!is.na(f30)]
f45 <- naiveforecasts[[3]]
f45 <- f45[!is.na(f45)]
p <- plot.predictions(actual15,f15,1)
p
print(accuracy(actual15,f15))

p <- plot.predictions(actual30,f30,2)
p
print(accuracy(actual30,f30))

p <- plot.predictions(actual45,f45,3)
p
print(accuracy(actual45,f45))

print("Done!")
################################################################################
print("Linear Regression...")
lmforecast <- performLinearRegression()
f15 <- lmforecast[[1]]
f30 <- lmforecast[[2]]
f30 <- f30[!is.na(f30)]
f45 <- lmforecast[[3]]
f45 <- f45[!is.na(f45)]

p <- plot.predictions(actual15,f15,1)
p
print(accuracy(actual15,f15))

# 2 steps ahead
p <- plot.predictions(actual30,f30,2)
p
print(accuracy(actual30,f30))

# 3 steps ahead
p <- plot.predictions(actual45,f45,3)
p
print(accuracy(actual45,f45))

print("Done!")
################################################################################
print("ETS...")
etsforecasts <- performExpSmoothing()
f15 <- etsforecasts[[1]]
f30 <- etsforecasts[[2]]
f30 <- f30[!is.na(f30)]
f45 <- etsforecasts[[3]]
f45 <- f45[!is.na(f45)]

etsmodel <- etsforecasts[[4]]

p <- plot.predictions(actual15,f15,1)
p
print(accuracy(actual15,f15))

# 2 steps ahead
p <- plot.predictions(actual30,f30,2)
p
print(accuracy(actual30,f30))

# 3 steps ahead
p <- plot.predictions(actual45,f45,3)
p
print(accuracy(actual45,f45))


print("Done!")
################################################################################
print("NNAR...")
nnforecasts <- performNNETAR()
f15 <- nnforecasts[[1]]
f30 <- nnforecasts[[2]]
f30 <- f30[!is.na(f30)]
f45 <- nnforecasts[[3]]
f45 <- f45[!is.na(f45)]

nnmodel <- nnforecasts[[4]]

p <- plot.predictions(actual15,f15,1)
p
print(accuracy(actual15,f15))

# 2 steps ahead
p <- plot.predictions(actual30,f30,2)
p
print(accuracy(actual30,f30))

# 3 steps ahead
p <- plot.predictions(actual45,f45,3)
p
print(accuracy(actual45,f45))


print("Done!")

################################################################################
print("ARIMA...")
arimaforecasts <- performArima()
f15 <- arimaforecasts[[1]]
f30 <- arimaforecasts[[2]]
f30 <- f30[!is.na(f30)]
f45 <- arimaforecasts[[3]]
f45 <- f45[!is.na(f45)]

arimamodel <- arimaforecasts[[4]]

p <- plot.predictions(actual15,f15,1)
p
print(accuracy(actual15,f15))

# 2 steps ahead
p <- plot.predictions(actual30,f30,2)
p
print(accuracy(actual30,f30))

# 3 steps ahead
p <- plot.predictions(actual45,f45,3)
p
print(accuracy(actual45,f45))

print("Done!")
################################################################################