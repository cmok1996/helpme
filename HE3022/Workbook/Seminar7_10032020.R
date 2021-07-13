library(fpp2)
library(seasonal)
oildata <- window(oil, start=1996)
#estimate parameters
fc <- ses(oildata, h=5)
#Accurary of one step ahead training errors over period 1-12
round(accuracy(fc),2)
summary(fc) (alpha = 0.8339 means higher weight is given to later observations)
autoplot(fc)+
  forecast::autolayer(fitted(fc), series="Fitted") +
  ylab("Oil (millions of tonnes)") + xlab("Year")
#point forecast is level, does not change for simplified exponential smoothing

#Holt's linear trend method - follows the same weightage but apply to trend slope
#Beta* gives the weight given to the most recent observation (rise-run), vs past trend (bt-1)
air <- window(ausair, start=1990)
autoplot(air) +
  ggtitle("Airpassengers in Australia") +
  xlab("Year") + ylab("millions of passengers")
fc <- holt(air,h=5) #fit and forecast
summary(fc) #beta is small, trend slope hardly changes because most of the weight falls on earlier trend slope
autoplot(fc) +
  forecast::autolayer(fitted(fc), series="Fitted")
#forecasts generate by Holt's linear method display a constant trend indefinitely, tend to over-forecast in longer horizon

#dampens as h increases
fc <- hold(air,h=15)
fc2 <- holt(air, damped=TRUE, phi=0.9, h=15) #model will actually choose the optimal phi, phi=0.9 just to illstrate effect of dampening
autoplot(air) +
  forecast::autolayer(fc$mean, series="Holt's method") +
  forecast::autolayer(fc2$mean, series="Damped Holt's method") +
  ggtitle("Forecasts from Holt's method") +
  xlab("Year") + ylab("Air passegers in Australia (millions)") +
  guides(colour=guide_legend(title="Forecast"))

autoplot(livestock) +
  xlab("Year") + ylab("Livestock, sheep in Asia(millions)")
e1 <- tsCV(livestock, ses, h=1)
e2 <- tsCV(livestock, holt, h=1)
e3 <- tsCV(livestock, holt, damped=TRUE, h=1)
#Compare MSE
mean(e1^2, na.rm=TRUE)
mean(e2^2, na.rm=TRUE)
mean(e3^2, na.rm=TRUE)
#Compare MAE
mean(abs(e1),na.rm=TRUE)
mean(abs(e2),na.rm=TRUE)
mean(abs(e3),na.rm=TRUE)
#Damped holt's method is best whether you compare MAE or MSE
#Proceed to using damped method

fc <- holt(livestock, damped=TRUE)
fc[["model"]]
#alpha is large, beta is small, phi is large, tells us that the trend slope is damped only slightly
autoplot(fc) +
  xlab("Year") + ylab("Livestock")

#Holt-Winter seasonal method
#includes level, trend, seasonality
#additive seasonality is used when variation is constant throughout the years, better for forecasting
#gamma is the weight for the most recent seasonality 
#seasonal component = most recent seasonal component + past seasonality
#adjustments of seasonality by additive method
#multiplicative method is useful if seasonality variability changes. Cannot have a multiplicative model in a logarithmic series
aust <- window(austourists, start=2005)
fit1 <- hw(aust,seasonal="additive")
fit2 <- hw(aust, seasonal="multiplicative")
autoplot(aust) +
  forecast::autolayer(fit1$mean, series="HW additive forecasts") +
  forecast::autolayer(fit2$mean, series="HW multiplicative forecasts") +
  xlab("Year") + ylab("International visitor night in Australia (millions)") +
  guides(colour=guide_legend(title="Forecast"))
accuracy(fit1)
accuracy(fit2)
#RMSe lower in multiplicative, multiplicative is a better fit
#Mean error tells us how much bias
#Additive model gives poorer fit bus less bias
summary(fit1) #alpha = 0.3063, more weight is given to past observation
summary(fit2)
states <- cbind(fit1$model$states[,1:3], fit2$model$states[,1:3])
colnames(states) <- c("level", "slope", "seasonal", "level", "slope", "seasonal")
plot(states,xlab="Year")

#damped HW : hw(x, damped=TRUE, seasonal = "multiplicative)
fc <- hw(subset(hyndsight, end=length(hyndsight)-35),
         damped = TRUE, seasonal = 'multiplicative', h=35)

autoplot(hyndsight) +
  forecast::autolayer(fc$mean, series="HW multi damped") +
  guides(colour=guide_legend(title="Daily forecasts"))

#total of 9x2 models for ets as we allow erros in additive and mutliplicative

#ets(y, model="ZZZ", damped=NULL, alpha=NULL, beta=NULL
#gamma=NULL, phi=NULL, biasedj=FALSE, additive.only=FALsE,
#restrict=TRUE, allow.multiplicative.trend=FALSE)

