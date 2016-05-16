# Load the common stuff
source("code/common.R")

library(dplyr)
library(scales)

########################################
# Exploratory analysis at a single point
########################################
analyseSite <- function(siteNum){
  site.data <- handleMissingData(volume.data[,siteNum])
    
  site.df <- data.frame(interval = seq(ymd_hms('2008-01-01 00:00:00'), 
                                      by = '15 min', length.out=195168), data = site.data)

  hf.no <- hf.list[siteNum,]
  road <- hf.ref[hf.ref$HF==hf.no,]
  
  # Create xts object
  site.xts <- xts(site.df$data, order.by=site.df$interval)
  
  # Plot daily, weekly, monthly, yearly avarages
  daily.avg <- apply.daily(site.xts, colMeans)
  weekly.avg <- apply.weekly(site.xts, colMeans)
  monthly.avg <- apply.monthly(site.xts, colMeans)
  yearly.avg <- apply.yearly(site.xts,colMeans)
  pdf(paste(plots.dir,'averages.pdf', sep = ''))
  par(mfrow=c(2,2), oma=c(0,0,2,0))
  plot(daily.avg, main = "Daily", xlab='(a)', ylab='volume')
  plot(weekly.avg, main = "Weekly", xlab = '(b)', ylab='volume')
  plot(monthly.avg, main = "Monthly", xlab = '(c)', ylab='volume')
  plot(yearly.avg, main = "Yearly", xlab = '(d)', ylab='volume')
  mtext(paste(road$NAME, "- AVERAGE FLOW"), outer = TRUE, cex = 1.5)
  dev.off()
  
  # Plot typical dayily volume
  site.df <- site.df %>% mutate(DayofWeek = wday(interval,label=T), 
                                cTime = as.POSIXct(strftime(interval,format="%H:%M:%S"),
                                                  format="%H:%M:%S"))
  alltsdata <- site.df %>% group_by(DayofWeek, cTime) %>% 
    summarise(meanVol=mean(data)) %>% as.data.frame()
  
  days.name <- c("Sunday", "Monday","Tuesday", "Wednesday", "Thursday", "Friday", "Saturday")
  
  createPlot <- function(from, to){
    for(i in from:to){
      st <- 1 + 96*(i-1)
      end <- st + 95
      t <- alltsdata[st:end,"cTime"]
      v <- alltsdata[st:end,"meanVol"]
      day.xts <- xts(v, order.by=t)
      plot(day.xts, xlab="time", ylab="volume", main=days.name[i], major.format="%H:%M")
    }
  }
  pdf(paste(plots.dir,'typical-day.pdf', sep = ''))
  par(mfrow=c(4,2), oma=c(0,0,2,0))
  createPlot(1,7)
  mtext(paste(road$NAME, "- AVERAGE FLOW"), outer = TRUE, cex = 1.5)
  dev.off()
  
  pdf(paste(plots.dir,'acf-pacf.pdf', sep = ''))
  par(mfrow=c(2,1))
  Acf(site.data, main="ACF")
  Pacf(site.data, main="PACF")
  dev.off()
  return(road)
}

# index 6 to 24 (middle one is 15)
analyseSite(15)
