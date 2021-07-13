library(fpp2)
library(tseries)
#trend and stationarity implies not stationary. Cycle is okay
#differencing, still have obvious seasonality
cbind("Sales ($million)" = a10,
"Monthly log sales" = log(a10),
"Annual change in log sales" = diff(log(a10),1)) %>%
  autoplot(facets=TRUE) +
  xlab("Year") + ylab("") +
  ggtitle("Antidiabetic drug sales")

library(gridExtra)
grid.arrange(ggAcf(dj), ggAcf(diff(dj)),ncol=2)
Box.test(diff(dj), lag=10, type="Ljung-Box")

#Random walk model have long periods of apparent trends up or down, sudden and unpredictable changes in direction
#Forecast from random walk model are equal to last observation, as future movements are unpredictable, and are equally likely up ot down
#random walk model underpins naive forecasts
#random walk with drift, c is the average of the chages between consecutive observations

#seasonal differencing is the diff between an observation and corresponding observation from previous season
#basis of seasonal naive forecasts

cbind("Sales ($million)" = a10,
      "Mothly log sales" = log(a10),
      "Annual change in log sales" = diff(log(a10),12)) %>%
  autoplot(facets=TRUE) +
  xlab("Year") + ylab("") +
  ggtitle("Antidiabetic drug sales")
#we can see that trend is removed + no obvious seasonality
#we took seasonal difference, not second order differencing

cbind("Billion kwh" = usmelec,
      "Logs" = log(usmelec),
      "Seasonality difference logs" = diff(log(usmelec),12),
      "Doubly differenced logs" = diff(diff(log(usmelec),12),1)) %>%
  autoplot(facets=TRUE) +
  xlab("Year") + ylab("") +
  ggtitle("Monthly US net electricity generation")

#When compare information criterion, we have to compare ARMA models with same integration
#better to do seasonally differencing first, then second order differencing

#For multivariate regression, we need to also make sure that the predictor is stationary, otherwise showing spurrious regression because trend component
#Popular unit root test is the ADF test, augmented because it can handle time trend, up to k lags
#for order to be stationary, phi must be less than 0. Null hypothesis is phi = 0
#if phi>0, explosive, not reverting to the mean
#adf test has low power when phi is very close to zero (eg, -0.1). Should be stationary but adf test shows non-stationary
#to check against that, we can look at kpss test which reverse the nulls

#Construct non-seasonal arima models manually
#PACF measures relationship between yt and yt-k by adding one at a time
ggtsdisplay(uschange[,"Consumption"],main="") #shows both ggacf and ggpcf plot
#we choose value of p based on pacf, value of q based on acf

fit2 <- Arima(uschange[,"Consumption"], order=c(3,0,0))
summary(fit2) #AICc = 340.67
fit21 <- Arima(uschange[,"Consumption"], order=c(2,0,2))
summary(fit21) #AICc = 342.75

fit3 <- auto.arima(uschange[,"Consumption"], seasonal=FALSE,
                   stepwise=FALSE, approximation=FALSE)
summary(fit3)
elecequip %>% stl(s.window="periodic") %>% seasadj -> eeadj
autoplot(eeadj)
#timeplot shows sudden changes, particularly big drop in 2008/2009, these changes are due to global economic environment
#no evidence of changing variance, do not need a boxcox transformation
#data is clearly non stationary, as series wanders up and down for long periods
adf.test(eeadj)
kpss.test(eeadj)
checkresiduals(diff(eeadj))
#from acf and pacf, an AR(3) model with MA(1) or ARIMA(3,1,1) seems to be a likely candidate model
fit<- Arima(eeadj, order=c(3,1,1))
summary(fit) #shows lowest AICc
fit1 <- Arima(eeadj,order=c(3,1,0))
summary(fit1)
fit2 <- Arima(eeadj, order=c(4,1,0))
summary(fit2)
checkresiduals(fit)
#shows all correlations are within threshold limits, indicating that residuals are behaving like white noise