missingData <- function(){
  missing.days.count <- c()
  for (i in 1:1084){
    sd <- volume.data[,i]
    site.df <- data.frame(interval = seq(ymd_hms('2008-01-01 00:00:00'), 
                                         by = '15 min', length.out=195168), data = sd)
    # Create xts object
    site.xts <- xts(site.df$data, order.by=site.df$interval)
    
    site.xts.daily <- apply.daily(site.xts, sum)
    missing.days <- as.data.frame(site.xts.daily)
    missing.days.count[i] <- length(missing.days[missing.days$V1 == 0,])
  }
  return(missing.days.count)
}
missing.days.count <- missingData()
location <- c(1:1084)
p <- plot_ly(
  x = location ,
  y = missing.days.count,
  name = "Number of missing days",
  type = "bar")
p

