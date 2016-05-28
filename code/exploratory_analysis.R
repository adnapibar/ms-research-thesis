# Load the common stuff
source("code/common.R")

library(dplyr)
library(scales)

########################################
# Exploratory analysis at a single point
########################################
analyseSite <- function(hf.no){
  #index <- getIndexByHF(hf.no)
  
  #site.data <- handleMissingData(volume.data[,index])
  
  site.df <- data.frame(interval = seq(ymd_hms('2008-01-01 00:00:00'), 
                                      by = '15 min', length.out=195168), data = site.data)

  road <- hf.ref[hf.ref$HF==hf.no,]
  
  # Create xts object
  site.xts <- xts(site.df$data, order.by=site.df$interval)
  
  # Plot a typical day
  typd <- index(site.xts)[19201:19296]
  typd <- site.xts[typd]
  pdf(paste(plots.dir,'typical-day.pdf', sep = ''))
  plot.xts2(typd, xlab="", ylab="volume", main='Typical day(19/07/2008)', 
            major.format="%H:%M", col='blue')
  dev.off()
  # Plot a typical week (last week of the data 15/07/2013 - 21/07/2013)
  typw <- index(site.xts)[194113:194784]
  typw <- site.xts[typw]
  pdf(paste(plots.dir,'typical-week.pdf', sep = ''))
  plot.xts2(typw, xlab="", ylab="volume", main='Typical week(15/07/2013 - 21/07/2013)',
            major.format="%a", col='blue')
  dev.off()
  
  # Plot daily, weekly, monthly, yearly avarages
  daily.avg <- apply.daily(site.xts, colMeans)
  weekly.avg <- apply.weekly(site.xts, colMeans)
  monthly.avg <- apply.monthly(site.xts, colMeans)
  yearly.avg <- apply.yearly(site.xts,colMeans)
  pdf(paste(plots.dir,'averages.pdf', sep = ''))
  par(mfrow=c(2,2))
  plot.xts2(daily.avg, main = "Daily", xlab='(a)', ylab='volume', col='blue')
  plot.xts2(weekly.avg, main = "Weekly", xlab = '(b)', ylab='volume', col='blue')
  plot.xts2(monthly.avg, main = "Monthly", xlab = '(c)', ylab='volume', col='blue')
  plot.xts2(yearly.avg, main = "Yearly", xlab = '(d)', ylab='volume', col='blue')
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
      pdf(paste(plots.dir,'typical',days.name[i],'.pdf', sep = ''))
      plot.xts2(day.xts, xlab="time", ylab="volume", main=days.name[i], major.format="%H:%M", col='blue')
      dev.off()
    }
  }
  createPlot(1,7)
  
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
