# Load the common stuff
source("code/common.R")

##############################################
# Exploratory analysis at a single HF location
##############################################
site.data <- volume.data[,getIndexByHF(16913)]
site.data <- handleMissingData(site.data)
site.df <- data.frame(interval = seq(ymd_hms('2007-01-01 00:00:00'), 
                                     by = '15 min', length.out=195168), data = site.data)

# Create xts object
site.xts <- xts(site.df$data, order.by=site.df$interval)

############################################################################################
# Hourly Variations on weekdays and weekends 
typwnd <- as.data.frame(site.xts["2012-02-19"]) # Sunday
td <- plot_ly(
  x = rownames(typwnd),
  y = typwnd$V1,
  name = "Typical day") %>%
  layout(xaxis = list(title = "Time"), yaxis = list(title = "Volume"),
         width = 600, height = 350)
td
typwd <- as.data.frame(site.xts["2012-02-21"]) # Tuesday
td <- plot_ly(
  x = rownames(typwd),
  y = typwd$V1,
  name = "Typical day") %>%
  layout(xaxis = list(title = "Time"), yaxis = list(title = "Volume"),
         width = 600, height = 350)
td

site.xts.wdays <- site.xts[!weekdays(index(site.xts)) %in% c("Saturday", "Sunday")]
z <- lapply(site.xts.wdays,function(x) aggregate(x, format(index(x), "%H:%M"), mean)) 
site.xts.wdays <- as.data.frame(z)
site.xts.wenddays <- site.xts[weekdays(index(site.xts)) %in% c("Saturday", "Sunday")]
z <- lapply(site.xts.wenddays,function(x) aggregate(x, format(index(x), "%H:%M"), mean)) 
site.xts.wenddays <- as.data.frame(z)

td <- plot_ly(
  x = rownames(site.xts.wdays),
  y = site.xts.wdays[[1]],
  name = "Weekdays") %>%
  layout(xaxis = list(title = "",tickfont = list(size=11)), yaxis = list(title = "Volume"),
         width = 600, height = 350) %>% 
  add_trace(x = rownames(site.xts.wdays), y = site.xts.wenddays[[1]], name = "Weekends") 
td

############################################################################################
# Daily Variations - Plot each day as a group
z <- lapply(split.default(site.xts, format(index(site.xts), "%A")),         
            function(x) aggregate(x, format(index(x), "%H:%M"), mean))   
site.xts.day.avg <- as.data.frame(z)
rm(z)
days.name <- c("Monday","Tuesday","Wednesday","Thursday","Friday","Saturday","Sunday")
t <- seq(ymd_hms("2016-06-01 00:00:00"), by = '15 min', length.out=96)
ed <- plot_ly(x = format(t, "%H:%M"),
              y = site.xts.day.avg[[days.name[1]]],
              name = days.name[1]) %>%
  layout(xaxis = list(title = "", tickfont = list(size=11)), 
         yaxis = list(title = "Volume"),width = 800, height = 350) %>% 
  add_trace(x = format(t, "%H:%M"),y = site.xts.day.avg[[days.name[2]]], name = days.name[2]) %>% 
  add_trace(x = format(t, "%H:%M"),y = site.xts.day.avg[[days.name[3]]], name = days.name[3]) %>% 
  add_trace(x = format(t, "%H:%M"),y = site.xts.day.avg[[days.name[4]]], name = days.name[4]) %>% 
  add_trace(x = format(t, "%H:%M"),y = site.xts.day.avg[[days.name[5]]], name = days.name[5]) %>% 
  add_trace(x = format(t, "%H:%M"),y = site.xts.day.avg[[days.name[6]]], name = days.name[6]) %>% 
  add_trace(x = format(t, "%H:%M"),y = site.xts.day.avg[[days.name[7]]], name = days.name[7])

ed
############################################################################################
# Weekly Variations 
site.xts.weeks <- lapply(split.default(site.xts, format(index(site.xts), "%W")),         
            function(x) aggregate(x, format(index(x), "%H:%M"), mean))  
site.xts.weeks <- as.data.frame(site.xts.weeks)
# January
ed <- plot_ly(x = format(t, "%H:%M"), y = site.xts.weeks[[1]], name = "Week 1") %>%
  layout(xaxis = list(title = "", tickfont = list(size=11)), 
         yaxis = list(title = "Volume"),width = 800, height = 350) %>% 
  add_trace(x = format(t, "%H:%M"),y = site.xts.weeks[[2]], name = "Week 2") %>% 
  add_trace(x = format(t, "%H:%M"),y = site.xts.weeks[[3]], name = "Week 3") %>% 
  add_trace(x = format(t, "%H:%M"),y = site.xts.weeks[[4]], name = "Week 4")

ed
# February
ed <- plot_ly(x = format(t, "%H:%M"), y = site.xts.weeks[[6]], name = "Week 1") %>%
  layout(xaxis = list(title = "", tickfont = list(size=11)), 
         yaxis = list(title = "Volume"),width = 800, height = 350) %>% 
  add_trace(x = format(t, "%H:%M"),y = site.xts.weeks[[7]], name = "Week 2") %>% 
  add_trace(x = format(t, "%H:%M"),y = site.xts.weeks[[8]], name = "Week 3") %>% 
  add_trace(x = format(t, "%H:%M"),y = site.xts.weeks[[9]], name = "Week 4")

ed

# March
ed <- plot_ly(x = format(t, "%H:%M"), y = site.xts.weeks[[10]], name = "Week 1") %>%
  layout(xaxis = list(title = "", tickfont = list(size=11)), 
         yaxis = list(title = "Volume"),width = 800, height = 350) %>% 
  add_trace(x = format(t, "%H:%M"),y = site.xts.weeks[[11]], name = "Week 2") %>% 
  add_trace(x = format(t, "%H:%M"),y = site.xts.weeks[[12]], name = "Week 3") %>% 
  add_trace(x = format(t, "%H:%M"),y = site.xts.weeks[[13]], name = "Week 4")

ed
# April
ed <- plot_ly(x = format(t, "%H:%M"), y = site.xts.weeks[[14]], name = "Week 1") %>%
  layout(xaxis = list(title = "", tickfont = list(size=11)), 
         yaxis = list(title = "Volume"),width = 800, height = 350) %>% 
  add_trace(x = format(t, "%H:%M"),y = site.xts.weeks[[15]], name = "Week 2") %>% 
  add_trace(x = format(t, "%H:%M"),y = site.xts.weeks[[16]], name = "Week 3") %>% 
  add_trace(x = format(t, "%H:%M"),y = site.xts.weeks[[17]], name = "Week 4")

ed
# July
ed <- plot_ly(x = format(t, "%H:%M"), y = site.xts.weeks[[27]], name = "Week 1") %>%
  layout(xaxis = list(title = "", tickfont = list(size=11)), 
         yaxis = list(title = "Volume"),width = 800, height = 350) %>% 
  add_trace(x = format(t, "%H:%M"),y = site.xts.weeks[[28]], name = "Week 2") %>% 
  add_trace(x = format(t, "%H:%M"),y = site.xts.weeks[[29]], name = "Week 3") %>% 
  add_trace(x = format(t, "%H:%M"),y = site.xts.weeks[[30]], name = "Week 4")

ed
# October
ed <- plot_ly(x = format(t, "%H:%M"), y = site.xts.weeks[[40]], name = "Week 1") %>%
  layout(xaxis = list(title = "", tickfont = list(size=11)), 
         yaxis = list(title = "Volume"),width = 800, height = 350) %>% 
  add_trace(x = format(t, "%H:%M"),y = site.xts.weeks[[41]], name = "Week 2") %>% 
  add_trace(x = format(t, "%H:%M"),y = site.xts.weeks[[42]], name = "Week 3") %>% 
  add_trace(x = format(t, "%H:%M"),y = site.xts.weeks[[43]], name = "Week 4")

ed

############################################################################################
# Monthly Variations 
z <- lapply(split.default(site.xts, format(index(site.xts), "%m")),         
            function(x) aggregate(x, format(index(x), "%H:%M"), mean))  
site.xts.mnths <- as.data.frame(z)
rm(z)
ed <- plot_ly(x = format(t, "%H:%M"), y = site.xts.mnths[[1]], name = month.abb[1]) %>%
  layout(xaxis = list(title = "", tickfont = list(size=11)), 
         yaxis = list(title = "Volume"),width = 1000, height = 450) %>% 
  add_trace(x = format(t, "%H:%M"),y = site.xts.mnths[[2]], name = month.abb[2]) %>% 
  add_trace(x = format(t, "%H:%M"),y = site.xts.mnths[[3]], name = month.abb[3]) %>% 
  add_trace(x = format(t, "%H:%M"),y = site.xts.mnths[[4]], name = month.abb[4]) %>% 
  add_trace(x = format(t, "%H:%M"),y = site.xts.mnths[[5]], name = month.abb[5]) %>% 
  add_trace(x = format(t, "%H:%M"),y = site.xts.mnths[[6]], name = month.abb[6]) %>% 
  add_trace(x = format(t, "%H:%M"),y = site.xts.mnths[[7]], name = month.abb[7]) %>% 
  add_trace(x = format(t, "%H:%M"),y = site.xts.mnths[[8]], name = month.abb[8]) %>% 
  add_trace(x = format(t, "%H:%M"),y = site.xts.mnths[[9]], name = month.abb[9]) %>% 
  add_trace(x = format(t, "%H:%M"),y = site.xts.mnths[[10]], name = month.abb[10]) %>% 
  add_trace(x = format(t, "%H:%M"),y = site.xts.mnths[[11]], name = month.abb[11]) %>% 
  add_trace(x = format(t, "%H:%M"),y = site.xts.mnths[[12]], name = month.abb[12])
ed

tw <- plot_ly(
  x = month.abb,
  y = colSums(site.xts.mnths),
  name = "Average month") %>%
  layout(xaxis = list(title = "Month",tickfont = list(size=11)), yaxis = list(title = "Volume"),
         width = 600, height = 350)
tw
############################################################################################
# Variations on public evetns in 2011
pedays = c("2011-01-03","2011-01-26","2011-03-14","2011-04-22","2011-04-26",
           "2011-04-25","2011-06-13","2011-11-01","2011-12-26","2011-12-27")

newyears.day <- as.data.frame(site.xts[pedays[1]])
aus.day <- as.data.frame(site.xts[pedays[2]])
labor.day <- as.data.frame(site.xts[pedays[3]])
good.friday <- as.data.frame(site.xts[pedays[4]])
easter.mon <- as.data.frame(site.xts[pedays[5]])
anzac.day <- as.data.frame(site.xts[pedays[6]])
queens.bday <- as.data.frame(site.xts[pedays[7]])
cup.day <- as.data.frame(site.xts[pedays[8]])
xmas.day <- as.data.frame(site.xts[pedays[9]])
boxing.day <- as.data.frame(site.xts[pedays[10]])

t <- seq(ymd_hms("2016-06-01 00:00:00"), by = '15 min', length.out=96)
p <- plot_ly(x = format(t, "%H:%M"), 
             y = site.xts.day.avg[["Monday"]], 
             name = "Average Monday") %>%
  layout(xaxis = list(title = "", tickfont = list(size=11)), 
         yaxis = list(title = "Volume"), width = 800, height = 350) %>% 
  add_trace(x = format(t, "%H:%M"), y = newyears.day$V1, name = "New year's day") %>% 
  add_trace(x = format(t, "%H:%M"), y = labor.day$V1, name = "Labour day") %>% 
  add_trace(x = format(t, "%H:%M"), y = anzac.day$V1, name = "ANZAC day") %>% 
  add_trace(x = format(t, "%H:%M"), y = queens.bday$V1, name = "Queens birthday") %>% 
  add_trace(x = format(t, "%H:%M"), y = xmas.day$V1, name = "Christmas day") 
p

p <- plot_ly(x = format(t, "%H:%M"), 
             y = site.xts.day.avg[["Tuesday"]], 
             name = "Average Tuesday") %>%
  layout(xaxis = list(title = "", tickfont = list(size=11)), 
         yaxis = list(title = "Volume"), width = 800, height = 350) %>% 
  add_trace(x = format(t, "%H:%M"), y = cup.day$V1, name = "Melbourne cup") %>% 
  add_trace(x = format(t, "%H:%M"), y = easter.mon$V1, name = "Easter Monday (in lieu)") %>% 
  add_trace(x = format(t, "%H:%M"), y = boxing.day$V1, name = "Boxing day") 
  
p

p <- plot_ly(x = format(t, "%H:%M"), 
             y = site.xts.day.avg[["Wednesday"]], 
             name = "Average Wednesday") %>%
  layout(xaxis = list(title = "", tickfont = list(size=11)), 
         yaxis = list(title = "Volume"),width = 800, height = 350) %>% 
  add_trace(x = format(t, "%H:%M"), y = boxing.day$V1, name = "Australia day") 
p

p <- plot_ly(x = format(t, "%H:%M"), 
             y = site.xts.day.avg[["Friday"]], 
             name = "Average Friday") %>%
  layout(xaxis = list(title = "", tickfont = list(size=11)), 
         yaxis = list(title = "Volume"),width = 800, height = 350) %>% 
  add_trace(x = format(t, "%H:%M"), y = boxing.day$V1, name = "Good Friday") 
p
############################################################################################
# Variations due to accidents
accidents <- c("2010-08-01","2010-11-10","2010-22-12","2011-01-18","2011-03-07","2011-05-11",
               "2011-05-21","2011-09-28","2012-03-16","2012-07-24","2012-10-10","2012-11-02",
               "2012-11-17","2012-11-30")

acc.traffic <- as.data.frame(site.xts3["2012-01-12"]) 
p <- plot_ly(x = format(t, "%H:%M"), 
             y = site.xts.day.avg[["Thursday"]], 
             name = "Average")  %>%
  layout(xaxis = list(title = "", tickfont = list(size=11)), 
         yaxis = list(title = "Volume"), width = 600, height = 350) %>% 
  add_trace(x = format(t, "%H:%M"), y = acc.traffic$V1, name = "Accident")
p

############################################################################################
# Plot daily, weekly, monthly, yearly avarages
daily.avg <- apply.daily(site.xts, colMeans)
daily.avg <- as.data.frame(daily.avg)
ad <- plot_ly(
  x = rownames(daily.avg),
  y = daily.avg$V1,
  name = "Daily average") %>%
  layout(xaxis = list(title = "Time"), yaxis = list(title = "Volume"),
         width = 600, height = 350)
ad

weekly.avg <- apply.weekly(site.xts, colMeans)
weekly.avg <- as.data.frame(weekly.avg)
aw <- plot_ly(
  x = rownames(weekly.avg),
  y = weekly.avg$V1,
  name = "Weekly average") %>%
  layout(xaxis = list(title = "Time"), yaxis = list(title = "Volume"),
         width = 600, height = 350)
aw

monthly.avg <- apply.monthly(site.xts, colMeans)
monthly.avg <- as.data.frame(monthly.avg)
am <- plot_ly(
  x = rownames(monthly.avg),
  y = monthly.avg$V1,
  name = "Monthly average") %>%
  layout(xaxis = list(title = "Time"), yaxis = list(title = "Volume"),
         width = 600, height = 350)
am

yearly.avg <- apply.yearly(site.xts,colMeans)
yearly.avg <- as.data.frame(yearly.avg)
ay <- plot_ly(
  x = rownames(yearly.avg),
  y = yearly.avg[[1]],
  name = "Yearly average") %>%
  layout(xaxis = list(title = "Time"), yaxis = list(title = "Volume"),
         width = 600, height = 350)
ay
############################################################################################
#Decompose time series
site.ts <- ts(site.data, start = c(2007,1), frequency = 96*365)
site.ts.components <- decompose(site.ts)
plot(site.ts.components, col='#167AC6')


fit <- stl(site.ts, s.window="period")
plot(fit,col='#167AC6')
#plot seasonally adjusted, i.e remove seasonaility just trend and an irregular component remains
site.ts.season.adj <- site.ts - site.ts.components$seasonal
plot(site.ts.season.adj, col='#167AC6', main='Seasonally adjusted')

site.data.boxcox <- BoxCox(site.data, lambda)
site.acf <- Acf(site.data.boxcox, main="ACF")
p <- plot_ly(
  x = site.acf$lag[,,],
  y = site.acf$acf[,,],
  name = "ACF",
  type = "bar") %>%
  layout(xaxis = list(title = "Lag"), yaxis = list(title = "ACF"),
         width = 600, height = 350)
p
site.pacf <- Pacf(site.data.boxcox, main="PACF")
p <- plot_ly(
  x = site.pacf$lag[,,],
  y = site.pacf$acf[,,],
  name = "PACF",
  type = "bar") %>%
  layout(xaxis = list(title = "Lag"), yaxis = list(title = "PACF"),
         width = 600, height = 350)
p
#################################################################################################
#cross correlation

#Upstream 
site.data2 <- volume.data[,getIndexByHF(16551)]
site.data2 <- handleMissingData(site.data2)
site.df2 <- data.frame(interval = seq(ymd_hms('2007-01-01 00:00:00'), 
                                     by = '15 min', length.out=195168), data = site.data2)

site.xts2 <- xts(site.df2$data, order.by=site.df2$interval)

# Adjacent
site.data3 <- volume.data[,getIndexByHF(14479)]
site.data3 <- handleMissingData(site.data3)
site.df3 <- data.frame(interval = seq(ymd_hms('2007-01-01 00:00:00'), 
                                      by = '15 min', length.out=195168), data = site.data3)

site.xts3 <- xts(site.df3$data, order.by=site.df3$interval)

# Plot weekday and weekends
site.xts.wdays2 <- site.xts2[!weekdays(index(site.xts2)) %in% c("Saturday", "Sunday")]
z <- lapply(site.xts.wdays2,function(x) aggregate(x, format(index(x), "%H:%M"), mean)) 
site.xts.wdays2 <- as.data.frame(z)
site.xts.wenddays2 <- site.xts2[weekdays(index(site.xts2)) %in% c("Saturday", "Sunday")]
z <- lapply(site.xts.wenddays2,function(x) aggregate(x, format(index(x), "%H:%M"), mean)) 
site.xts.wenddays2 <- as.data.frame(z)

site.xts.wdays3 <- site.xts3[!weekdays(index(site.xts3)) %in% c("Saturday", "Sunday")]
z <- lapply(site.xts.wdays3,function(x) aggregate(x, format(index(x), "%H:%M"), mean)) 
site.xts.wdays3 <- as.data.frame(z)
site.xts.wenddays3 <- site.xts3[weekdays(index(site.xts3)) %in% c("Saturday", "Sunday")]
z <- lapply(site.xts.wenddays3,function(x) aggregate(x, format(index(x), "%H:%M"), mean)) 
site.xts.wenddays3 <- as.data.frame(z)

td <- plot_ly(
  x = rownames(site.xts.wdays), y = site.xts.wdays[[1]], name = "Current") %>%
  layout(xaxis = list(title = "",tickfont = list(size=11)), yaxis = list(title = "Volume"),
         width = 600, height = 350) %>% 
  add_trace(x = rownames(site.xts.wdays), y = site.xts.wdays2[[1]], name = "Upstream") %>% 
  add_trace(x = rownames(site.xts.wdays), y = site.xts.wdays3[[1]], name = "Adjacent")
td

td <- plot_ly(
  x = rownames(site.xts.wenddays), y = site.xts.wenddays[[1]], name = "Current") %>%
  layout(xaxis = list(title = "",tickfont = list(size=11)), yaxis = list(title = "Volume"),
         width = 600, height = 350) %>% 
  add_trace(x = rownames(site.xts.wenddays), y = site.xts.wenddays2[[1]], name = "Upstream") %>% 
  add_trace(x = rownames(site.xts.wenddays), y = site.xts.wenddays3[[1]], name = "Adjacent")
td

site.ccf1 <- Ccf(drop(diff(site.xts,na.pad=FALSE)),drop(diff(site.xts2,na.pad=FALSE)))
p <- plot_ly(
  x = site.ccf1$lag[,,],
  y = site.ccf1$acf[,,],
  name = "CCF",
  type = "bar") %>%
  layout(xaxis = list(title = "Lag"), yaxis = list(title = "CCF"),
         width = 600, height = 350)
p

site.ccf2 <- Ccf(drop(diff(site.xts,na.pad=FALSE)),drop(diff(site.xts3,na.pad=FALSE)))
p <- plot_ly(
  x = site.ccf2$lag[,,],
  y = site.ccf2$acf[,,],
  name = "CCF",
  type = "bar") %>%
  layout(xaxis = list(title = "Lag"), yaxis = list(title = "CCF"),
         width = 600, height = 350)
p

