methods <- c("LR", "ARIMA", "ES","NNAR", "KNN", "SVR","RNN", "GRU","LSTM", "RNN(Multi)","GRU(Multi)","LSTM(Multi)")

# MAPE error measures
errors.15mins <- c(13.62,29.00,12.33,12.74,11.42,13.37,11.51,11.84,11.25,11.57,11.71,10.68,11.14)
errors.30mins <- c(13.83,27.91,11.75,12.74,10.09,12.36,9.00,9.72,10.14,9.39,9.93,8.51,8.38)
errors.45mins <- c(15.24,27.50,12.30,13.78,9.95,9.72,8.45,8.90,8.83,8.36,10.17,7.59,7.30)



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
