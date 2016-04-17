# Load the common stuff
source("code/common.R")

#############################
# ANALYSIS AT A SINGLE SITE
#############################
analyseSite <- function(siteNum){
  site.data <- handleMissingData(volume.data[,1])
    
  site.df <- data.frame(interval = seq(ymd_hms('2008-01-01 00:00:00'), 
                                      by = '15 min', length.out=195168), data = site.data)

  # Create xts object
  site.xts <- xts(site.df$data, order.by=site.df$interval)
  
  # Plot daily, weekly, monthly, yearly avarages
  daily.avg <- apply.daily(site.xts, colMeans)
  weekly.avg <- apply.weekly(site.xts, colMeans)
  monthly.avg <- apply.monthly(site.xts, colMeans)
  yearly.avg <- apply.yearly(site.xts,colMeans)
  pdf(paste(plots.dir,'averages.pdf', sep = ''))
  par(mfrow=c(2,2))
  plot(daily.avg, main = "Daily average", xlab='(a)', ylab='traffic volume')
  plot(weekly.avg, main = "Weekly average", xlab = '(b)', ylab='traffic volume')
  plot(monthly.avg, main = "Monthly average", xlab = '(c)', ylab='traffic volume')
  plot(yearly.avg, main = "Yearly average", xlab = '(d)', ylab='traffic volume')
  dev.off()
}
analyseSite(1)
