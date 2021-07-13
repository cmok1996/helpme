#Load data
tute1 <- read.csv("tute1.csv", header=TRUE)
View(tute1)

#Convert the data to time series
mytimeseries <- ts(tute1[,-1], start=1981, frequency=4)
#The [,-1] removes the first column which contains the quarters
View(mytimeseries)

#Construct time series plots of each of the three series
library(fpp2)
autoplot(mytimeseries, facets=TRUE)

#Check what happens when don't include facets=TRUE
autoplot(mytimeseries)
