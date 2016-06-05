methods <- c("Naive","LR", "ARIMA", "ES","NNAR", "KNN", "SVR","RNN", "GRU",
             "LSTM", "RNN(Multi)","GRU(Multi)","LSTM(Multi)")

# MAPE error measures
errors.15mins <- c()
errors.30mins <- c()
errors.45mins <- c()



p <- plot_ly(x = errors.15mins, y = methods, name = "15 mins",
             mode = "markers", 
             marker = list(color = "#FF6600")) %>%
  add_trace(x = errors.30mins, name = "30 mins", y = methods, 
            marker = list(color = "#663399"),
            mode = "markers") %>%
  add_trace(x = errors.45mins, name = "45 mins", y = methods, 
            marker = list(color = "#66CC00"),
            mode = "markers") %>%
  layout(
    xaxis = list(title = ""),
    yaxis = list(title = ""),
    margin = list(l = 80),
    width = 700, height = 450
  )
p


# Plot computatiion time 
time <- c(0.173,1060.10,10.02,238.94,15.24,15553.93,304.84,1153.33,1952.54,825.69,2895.62,3465.49)

p <- plot_ly(
  x = methods,
  y = time,
  name = "computation time",
  type = "bar") %>%
  layout(xaxis = list(title = "", tickfont = list(size=10)), 
         yaxis = list(title = "Time (Seconds)"),
         width = 800, height = 450)
p
