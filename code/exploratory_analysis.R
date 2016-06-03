# Load the common stuff
source("code/common.R")

##############################################
# Exploratory analysis at a single HF location
##############################################
site.data <- volume.data[,getIndexByHF(16913)]

site.df <- data.frame(interval = seq(ymd_hms('2008-01-01 00:00:00'), 
                                     by = '15 min', length.out=195168), data = site.data)

# Create xts object
site.xts <- xts(site.df$data, order.by=site.df$interval)

# Plot a typical day of week
typd <- as.data.frame(site.xts["2013-01-13"])
td <- plot_ly(
  x = rownames(typd),
  y = typd$V1,
  name = "Typical day") %>%
  layout(xaxis = list(title = "Time"), yaxis = list(title = "Volume"),
         width = 600, height = 350)
td
############################################################################################
# Plot a typical week (last week of the data 15/07/2013 - 21/07/2013)
typw <- as.data.frame(site.xts["2013-07-15::2013-07-21"])
tw <- plot_ly(
  x = rownames(typw),
  y = typw$V1,
  name = "Typical week") %>%
  layout(xaxis = list(title = "Date"), yaxis = list(title = "Volume"),
         width = 600, height = 350)
tw
############################################################################################
# Plot each day as a group
z <- lapply(split.default(site.xts, format(index(site.xts), "%A")),         
            function(x) aggregate(x, format(index(x), "%H:%M"), mean))   
site.xts.day.avg <- as.data.frame(z)
rm(z)
days.name <- c("Monday","Tuesday","Wednesday","Thursday","Friday","Saturday","Sunday")
createPlot <- function(i){
  t <- seq(ymd_hms("2016-06-01 00:00:00"), by = '15 min', length.out=96)
  ed <- plot_ly(x = format(t, "%H:%M"),
                y = site.xts.day.avg[[days.name[i]]],
                name = days.name[i]) %>%
    layout(xaxis = list(title = "", tickfont = list(size=11)), 
           yaxis = list(title = "Volume"),width = 600, height = 350)
  ed
}
createPlot(1)
createPlot(2)
createPlot(3)
createPlot(4)
createPlot(5)
createPlot(6)
createPlot(7)
############################################################################################
# Variations on public evetns in 2012
pedays = c("2012-01-02","2012-01-26","2012-01-12","2012-04-06","2012-04-09",
           "2012-04-25","2012-06-11","2012-11-06","2012-12-25","2012-12-26")

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
  add_trace(x = format(t, "%H:%M"), y = easter.mon$V1, name = "Easter Monday") 
p

p <- plot_ly(x = format(t, "%H:%M"), 
             y = site.xts.day.avg[["Tuesday"]], 
             name = "Average Tuesday") %>%
  layout(xaxis = list(title = "", tickfont = list(size=11)), 
         yaxis = list(title = "Volume"), width = 800, height = 350) %>% 
  add_trace(x = format(t, "%H:%M"), y = cup.day$V1, name = "Melbourne cup") %>% 
  add_trace(x = format(t, "%H:%M"), y = xmas.day$V1, name = "Christmas") 
p

p <- plot_ly(x = format(t, "%H:%M"), 
             y = site.xts.day.avg[["Wednesday"]], 
             name = "Average Wednesday") %>%
  layout(xaxis = list(title = "", tickfont = list(size=11)), 
         yaxis = list(title = "Volume"),width = 800, height = 350) %>% 
  add_trace(x = format(t, "%H:%M"), y = anzac.day$V1, name = "ANZAC day") %>% 
  add_trace(x = format(t, "%H:%M"), y = boxing.day$V1, name = "Boxing day") 
p

p <- plot_ly(x = format(t, "%H:%M"), 
             y = site.xts.day.avg[["Thursday"]], 
             name = "Average Thursday") %>%
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
               "2012-11-17","2012-11-30","2013-03-15","2013-05-15","2013-07-03","2013-08-28")

acc.traffic <- as.data.frame(site.xts[accidents[9]]) 
p <- plot_ly(x = format(t, "%H:%M"), 
             y = acc.traffic$V1, 
             name = "Accident") %>%
  layout(xaxis = list(title = "", tickfont = list(size=11)), 
         yaxis = list(title = "Volume"), width = 600, height = 350)
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
  y = yearly.avg$V1,
  name = "Yearly average") %>%
  layout(xaxis = list(title = "Time"), yaxis = list(title = "Volume"),
         width = 600, height = 350)
ay
############################################################################################
#Decompose time series
site.ts <- ts(site.data, start = c(2008,1), frequency = 96*365)
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
site.data2 <- volume.data[,getIndexByHF(16551)]
site.df2 <- data.frame(interval = seq(ymd_hms('2008-01-01 00:00:00'), 
                                     by = '15 min', length.out=195168), data = site.data2)

# Create xts object
site.xts2 <- xts(site.df2$data, order.by=site.df2$interval)
site.ccf <- Ccf(drop(diff(site.xts,na.pad=FALSE)),drop(diff(site.xts2,na.pad=FALSE)))
p <- plot_ly(
  x = site.ccf$lag[,,],
  y = site.ccf$acf[,,],
  name = "CCF",
  type = "bar") %>%
  layout(xaxis = list(title = "Lag"), yaxis = list(title = "CCF"),
         width = 600, height = 350)
p

site.data3 <- volume.data[,getIndexByHF(14479)]
site.df3 <- data.frame(interval = seq(ymd_hms('2008-01-01 00:00:00'), 
                                      by = '15 min', length.out=195168), data = site.data3)

# Create xts object
site.xts3 <- xts(site.df3$data, order.by=site.df3$interval)
site.ccf2 <- Ccf(drop(diff(site.xts,na.pad=FALSE)),drop(diff(site.xts3,na.pad=FALSE)))
p <- plot_ly(
  x = site.ccf2$lag[,,],
  y = site.ccf2$acf[,,],
  name = "CCF",
  type = "bar") %>%
  layout(xaxis = list(title = "Lag"), yaxis = list(title = "CCF"),
         width = 600, height = 350)
p

