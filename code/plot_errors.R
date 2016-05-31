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