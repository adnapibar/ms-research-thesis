errors.15mins <- subset(errors, Window=='15-mins', select = c(Method, MAE, RMSE, MAPE))
errors.30mins <- subset(errors, Window=='30-mins', select = c(Method, MAE, RMSE, MAPE))

Method <- errors.30mins$Method
Error <- errors.30mins$MAE
p <- plot_ly(
  x = Method,
  y = Error,
  name = "MAE",
  type = "bar")
p

p2 <- add_trace(
  p,
  x = Method,
  y = errors.30mins$RMSE,
  name = "RMSE",
  type = "bar")
p2

p3 <- add_trace(
  p2,
  x = Method,
  y = errors.30mins$MAPE,
  name = "MAPE",
  type = "bar")
p3

# Plot computatiion time 
time <- c(0.173,1060.10,10.02,238.94,15.24,17808.57,304.84,1153.33,1952.54,825.69,2895.62,3465.49)
methods <- c("LR", "ARIMA", "ES",
             "NNAR", "KNN", "SVR","RNN", "GRU",
             "LSTM", "RNN(Multi)","GRU(Multi)",
             "LSTM(Multi)")

p <- plot_ly(
  x = methods,
  y = time,
  name = "computation time",
  type = "bar")%>%
  layout(xaxis = list(title = "", tickfont = list(size=10)), 
         yaxis = list(title = "Time (Seconds)"),width = 800, height = 450)
p
