# Load the common stuff
source("code/common.R")

library(dplyr)
library(scales)

########################################
# Exploratory analysis at a single point
########################################
analyseSite <- function(){
  index <- getIndexByHF(hf.no)
  
  site.data <- volume.data[,index]
  
  site.df <- data.frame(interval = seq(ymd_hms('2008-01-01 00:00:00'), 
                                      by = '15 min', length.out=195168), data = site.data)
  
  # Create xts object
  site.xts <- xts(site.df$data, order.by=site.df$interval)
  
  # Plot a typical day
  typd <- index(site.xts)[1:96]
  typd <- site.xts[typd]
  typd <- as.data.frame(typd)
  
  td <- plot_ly(
    x = rownames(typd),
    y = typd$V1,
    name = "Typical day") %>%
    layout(xaxis = list(title = "Time"), yaxis = list(title = "Volume"))
  td
  
  # Plot a typical week (last week of the data 15/07/2013 - 21/07/2013)
  typw <- index(site.xts)[194113:194784]
  typw <- site.xts[typw]
  typw <- as.data.frame(typw)
  tw <- plot_ly(
    x = rownames(typw),
    y = typw$V1,
    name = "Typical week") %>%
    layout(xaxis = list(title = "Date"), yaxis = list(title = "Volume"))
  tw
  
  
  # Plot daily, weekly, monthly, yearly avarages
  daily.avg <- apply.daily(site.xts, colMeans)
  daily.avg <- as.data.frame(daily.avg)
  ad <- plot_ly(
    x = rownames(daily.avg),
    y = daily.avg$V1,
    name = "Daily average") %>%
    layout(xaxis = list(title = "Time"), yaxis = list(title = "Average 15 minute volume"))
  ad
  
  weekly.avg <- apply.weekly(site.xts, colMeans)
  weekly.avg <- as.data.frame(weekly.avg)
  aw <- plot_ly(
    x = rownames(weekly.avg),
    y = weekly.avg$V1,
    name = "Weekly average") %>%
    layout(xaxis = list(title = "Time"), yaxis = list(title = "Average 15 minute volume"))
  aw
  
  monthly.avg <- apply.monthly(site.xts, colMeans)
  monthly.avg <- as.data.frame(monthly.avg)
  am <- plot_ly(
    x = rownames(monthly.avg),
    y = monthly.avg$V1,
    name = "Monthly average") %>%
    layout(xaxis = list(title = "Time"), yaxis = list(title = "Average 15 minute volume"))
  am
  
  yearly.avg <- apply.yearly(site.xts,colMeans)
  yearly.avg <- as.data.frame(yearly.avg)
  ay <- plot_ly(
    x = rownames(yearly.avg),
    y = yearly.avg$V1,
    name = "Yearly average") %>%
    layout(xaxis = list(title = "Time"), yaxis = list(title = "Average 15 minute volume"))
  ay
  

  # Plot volume for each day of the week
  site.df <- site.df %>% mutate(DayofWeek = wday(interval,label=T), 
                                cTime = as.POSIXct(strftime(interval,format="%H:%M:%S"),
                                                  format="%H:%M:%S"))
  alltsdata <- site.df %>% group_by(DayofWeek, cTime) %>% 
    summarise(meanVol=mean(data)) %>% as.data.frame()
  
  days.name <- c("Sunday", "Monday","Tuesday", "Wednesday", "Thursday", "Friday", "Saturday")
  
  createPlot <- function(i){
    st <- 1 + 96*(i-1)
    end <- st + 95
    t <- alltsdata[st:end,"cTime"]
    v <- alltsdata[st:end,"meanVol"]
    daily <- xts(v, order.by=t)
    daily <- as.data.frame(daily)
    t <- seq(ymd_hms("2016-06-01 00:00:00"), by = '15 min', length.out=96)
    ed <- plot_ly(
      x = format(t, "%H:%M"),
      y = daily$V1,
      name = days.name[i]) %>%
      layout(xaxis = list(title = "", zeroline = TRUE), yaxis = list(title = "Volume"))
    ed
  }
  createPlot(1)
  createPlot(2)
  createPlot(3)
  createPlot(4)
  createPlot(5)
  createPlot(6)
  createPlot(7)
  
  #Decompose time series
  site.ts <- ts(site.data, start = c(2008,1), frequency = 96*365)
  site.ts.components <- decompose(site.ts)
  pdf(paste(plots.dir,'decomposition.pdf', sep = ''))
  plot(site.ts.components, col='blue')
  dev.off()
  
  #plot seasonally adjusted, i.e remove seasonaility just trend and an irregular component remains
  site.ts.season.adj <- site.ts - site.ts.components$seasonal
  pdf(paste(plots.dir,'season-adjusted.pdf', sep = ''))
  plot(site.ts.season.adj, col='blue', main='Seasonally adjusted')
  dev.off()
  
  pdf(paste(plots.dir,'acf-pacf.pdf', sep = ''))
  par(mfrow=c(2,1))
  site.data.boxcox <- BoxCox(site.data, lambda)
  Acf(site.data.boxcox, main="ACF")
  Pacf(site.data.boxcox, main="PACF")
  dev.off()
  return(road)
}

analyseSite(15773)
