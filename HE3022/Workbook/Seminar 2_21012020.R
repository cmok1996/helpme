setwd("C:/Users/cmok1/Desktop/Course material/Y3S2/HE3022/Workbook")
install.packages("fpp2")
library(fpp2)
tute1 <- read.csv("C:/Users/cmok1/Desktop/Course material/Y3S2/HE3022/Workbook/tute1.csv")
head(melsyd)
autoplot(melsyd[,"Economy.Class"]) +
  ggtitle("Economy class passengers: Melbourne-Sydney") +
  xlab("Year") + ylab("Thousands") 
#larger variation in 1992 onwards; outlier at 1989-1990 as data drop to 0 - airline strike

head(a10) #monthly scripts for pharmaceutical products falling under ATC
autoplot(a10) +
  ggtitle("Antidiabetic drug sales") +
  ylab("$ million") + xlab("Year")
#some seasonality, upward trend
ggseasonplot(a10,year.labels=TRUE,year.labels.left=TRUE) +
  ylab("$ million") + ggtitle("Seasonal plot: antidiabetic drug sales") #to check for seasonality
ggseasonplot(a10, polar=TRUE) + 
  ylab("$ million") + ggtitle("Polar seasonal plot: antidiabetic drug sales") #polar seasonal plot
ggsubseriesplot(a10) + ylab("$ million") + 
  ggtitle("Seasonal subseries plot: antidiabetic drug sales") #seasonal subseries plot - Average frequency for each month

head(elecdemand)
month.breaks <- cumsum(c(0,31,28,31,30,31,30,31,31,30,31,30,31)*48) #half-hour
autoplot(elecdemand[,c(1,3)],facet=TRUE) + #include facet=TRUE to have 2 different graphs plot according to scale
  xlab("Year:2014") + ylab("") +
  ggtitle("Half-hourly electricity demand: Victoria, Australia") +
  scale_x_continuous(breaks=2014+month.breaks/max(month.breaks),
  minor_breaks=NULL,labels=c(month.abb,month.abb[1]))
qplot(Temperature,Demand,data=as.data.frame(elecdemand)) + #need assign variable as datafram to plot
  ylab("Demand (GW)") + xlab("Temperature (Celsius)")
install.packages("GGally")
#vn %>% as.data.frame() %>% GGally: :ggpairs() #pipe operator, ggpairs, vn is avaialable in fpp package
beer2 <- window(ausbeer, start=1992, end=2006) #define the time-period we want to use
lag.plot(beer2, lags=9, do.lines=FALSE) #plots time-series against its lag, yt in vertical axis & yt-n in horizontal axis
#if perfectly correlated, data points are along dashed line
#see the same correlation in lag4, lag8 - quarterly seasonality
acf(beer2) #correlelogram
#autocorrelation of odd lags is insignificant, significant autocorrelation for even lags
#positive correlation every 4 quarters, some form of seasonality
aelec <- window(elec, start=1980)
autoplot(aelec) + xlab("Year") + ylab("Gwh")
ggAcf(aelec, lag=40)
#downward trend, scallop shape reflects seasonality

#to select model, we look at residual to ensure is white noise - all patterns have been extrated
set.seed(30); x <- ts(rnorm(50)); plot(x,main="White noise") #Generate 50 random numbers from normal distribution
acf(x) #all the spikes are within the band, not significant
#blue dashed line are the 95% significance level

#Example 1
#Set training data from 1992 - 2007
beer2 <- window(ausbeer,start=1992,end=c(2007,4)) #use 2007 data onwards to test how well the model forecast the series
#plot some forecasts
autoplot(beer2) + #autolayer allows overlap
  forecast::autolayer(meanf(beer2,h=11)$mean,series="Mean")+ #mean
  forecast::autolayer(naive(beer2,h=11)$mean,series="Naive")+ #last observation
  forecast::autolayer(snaive(beer2,h=11)$mean,series="Seasonal naive")+ #last seasonal series
  ggtitle("forecast for quarterly beer production") +
  xlab("Year") + ylab("Megalitres") +
  guides(colour=guide_legend(title="Forecast"))

#Example 2
# Set training data to first 250 days 
dj2 <- window(dj,end=250) 
# Plot some forecasts 
autoplot(dj2) + 
  forecast::autolayer(meanf(dj2, h=42)$mean, series="Mean") + 
  forecast::autolayer(rwf(dj2, h=42)$mean, series="Naïve") + 
  forecast::autolayer(rwf(dj2, drift=TRUE, h=42)$mean, series="Drift") + 
  ggtitle("Dow Jones Index (daily ending 15 Jul 94)") + 
  xlab("Day") + ylab("") + 
  guides(colour=guide_legend(title="Forecast"))
