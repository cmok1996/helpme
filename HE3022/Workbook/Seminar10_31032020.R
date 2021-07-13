library(fpp2)
library(tseries)
autoplot(euretail) + ylab("Retail Index") + xlab("Year")
#Obvious there is both seasonality and trend
euretail %>% diff(lag=4) %>% ggtsdisplay
#shows seasonally-differenced plot plot the series
#shows there is still trend in the series, as shown in ACF plot
euretail %>% diff(lag=4) %>% diff() %>% ggtsdisplay
#Significant spikes at lag1 & 4 for ACF and PACF

#If we focus on ACF, we should look at q, and indicate that Q=1 ARIMA(0,1,1)(0,1,1)
#If we focus on PACF, we should look at p=1, and indicate P=1 ARIMA(1,1,0)(1,1,0)

#Try ARIMA(0,1,1)(0,1,1)
fit <- Arima(euretail, order=c(0,1,1), seasonal=c(0,1,1))
summary(fit) #AICc = 75.72
ggtsdisplay(residuals(fit))
#Gives plot of residual, ACF & PACF of residuals. Still show significant lags at lag=2,3 for ACF & PACF, indicating that non-seasonal terms need to be included
#Can try ARIMA(0,1,2),(0,1,1)
fit3 <- Arima(euretail, order=c(0,1,2), seasonal=c(0,1,1))
summary(fit3) #AICc = 74.27, smaller, better fit than 1st model
ggtsdisplay(residuals(fit3)) #still show significant spike at lag=3, & at higher lags
#Try ARIMA(0,1,3)(0,1,1)
fit4 <- Arima(euretail, order=c(0,1,3), seasonal=c(0,1,1))
ggtsdisplay(residuals(fit4)) #no more autocorrelation
checkresiduals(fit4) #Looks like white-noise, p-value = 0.9274, cannot reject null hypothesis
summary(fit4) #AICc = 68.39, lowest AICc
#Ready for forecasting

#Using auto.arima
auto.arima(euretail, stepwise=FALSE, approximation=FALSE) #Make auto.arima work harder for us
#give us exact model
fit4 %>% forecast(h=12) %>% autoplot

lh02 <- log(h02)
cbind("H02 sales (million scripts)" = h02,
      "Log H02 sales" = lh02) %>%
  autoplot(facets=TRUE) + xlab("Year") + ylab("")
#Taking log stabilizes the variance
#Data is strongly sesaonal, and non-stationary
ggtsdisplay(diff(lh02,12), main="Seasonally differenced lh02 scripts", xlab="Year")
#ACF dampening slowly, PACF show spikes at first  lags, also show spikes at 12, 24, but not for ACF, suggest AR2 in seasonal component
#Consider ARIMA(3,0,0)(2,1,0)
fit <- Arima(h02, order=c(3,0,0), seasonal=c(2,1,0),lambda=0)
summary(fit) #AICc = 475.12
#Run auto.arima, d=0, D=1, lambda=0 to make AICc comparable

fitDauto <- auto.arima(h02, lambda=0, d=0, D=1, stepwise=FALSE, approximation=FALSE)
summary(fitDauto) #AICc = -486.94, ARIMA(3,0,0)(0,1,2) with drift
#All other models have higher AIC
ggtsdisplay(residuals(fitDauto))
#Can see that both ACF and PACF both has spikes
checkresiduals(fitDauto,lag=36)
#small p-valie, null can be rejected, fail the ljung-box text. PI may not be accurate due to correlated residuals
#Search for better models
#Suppose we just run auto.arima, only with speicification lambda = 0
fitauto <- auto.arima(h02,lambda=0, stepwise=FALSE, approximation=FALSE)
summary(fitauto) #gives us double differenced model (2,1,1)(0,1,2) without drift
#Can compare using RMSE but not AICc because this is doubly differenced

#Test set evaluation - RMSE
getrmse <- function(x,h,...) #define function
{
  train.end <- time(x)[length(x)-h]
  test.start <- time(x)[length(x)-h+1]
  train <- window(x,end=train.end)
  test <- window(x, start=test.start)
  fit <- Arima(train,...)
  fc <- forecast(fit,h=h)
  return(accuracy(fc,test)[2,"RMSE"])
}

#consider different models with drift
c(getrmse(h02,h=24,order=c(3,0,0),seasonal=c(2,1,0),lambda=0, include.drift=TRUE),
  getrmse(h02,h=24,order=c(3,0,1),seasonal=c(2,1,0),lambda=0, include.drift=TRUE),
  getrmse(h02,h=24,order=c(3,0,2),seasonal=c(2,1,0),lambda=0, include.drift=TRUE),
  getrmse(h02,h=24,order=c(3,0,1),seasonal=c(1,1,0),lambda=0, include.drift=TRUE),
  getrmse(h02,h=24,order=c(3,0,1),seasonal=c(0,1,1),lambda=0, include.drift=TRUE),
  getrmse(h02,h=24,order=c(3,0,1),seasonal=c(0,1,2),lambda=0, include.drift=TRUE),
  getrmse(h02,h=24,order=c(3,0,1),seasonal=c(1,1,1),lambda=0, include.drift=TRUE),
  getrmse(h02,h=24,order=c(4,0,3),seasonal=c(0,1,1),lambda=0, include.drift=TRUE),
  getrmse(h02,h=24,order=c(3,0,3),seasonal=c(0,1,1),lambda=0, include.drift=TRUE),
  getrmse(h02,h=24,order=c(4,0,2),seasonal=c(0,1,1),lambda=0, include.drift=TRUE),
  getrmse(h02,h=24,order=c(3,0,2),seasonal=c(0,1,1),lambda=0, include.drift=TRUE))
#third model lowest AICc

c(getrmse(h02,h=24,order=c(3,0,0),seasonal=c(2,1,0),lambda=0),
  getrmse(h02,h=24,order=c(3,0,1),seasonal=c(2,1,0),lambda=0),
  getrmse(h02,h=24,order=c(3,0,2),seasonal=c(2,1,0),lambda=0),
  getrmse(h02,h=24,order=c(3,0,1),seasonal=c(1,1,0),lambda=0),
  getrmse(h02,h=24,order=c(3,0,1),seasonal=c(0,1,1),lambda=0),
  getrmse(h02,h=24,order=c(3,0,1),seasonal=c(0,1,2),lambda=0), #RMSE = 0.0621
  getrmse(h02,h=24,order=c(3,0,1),seasonal=c(1,1,1),lambda=0),
  getrmse(h02,h=24,order=c(4,0,3),seasonal=c(0,1,1),lambda=0),
  getrmse(h02,h=24,order=c(3,0,3),seasonal=c(0,1,1),lambda=0),
  getrmse(h02,h=24,order=c(4,0,2),seasonal=c(0,1,1),lambda=0),
  getrmse(h02,h=24,order=c(3,0,2),seasonal=c(0,1,1),lambda=0),
  getrmse(h02,h=24,order=c(2,1,3),seasonal=c(0,1,1),lambda=0),
  getrmse(h02,h=24,order=c(2,1,4),seasonal=c(0,1,1),lambda=0),
  getrmse(h02,h=24,order=c(2,1,5),seasonal=c(0,1,1),lambda=0),
  getrmse(h02,h=24,order=c(2,1,1),seasonal=c(0,1,2),lambda=0)) #auto.arima RMSE = 0.0634
#we see RMSE is smallest with ARIMA(3,0,1)(0,1,12), smaller without drift

fit <- Arima(h02, order=c(3,0,1), seasonal=c(0,1,2), lambda=0)
checkresiduals(fit)
#shows that ACF still have significant spike, but Ljung Box text shows p value = 0.1664 cannot reject null of no autocorrelation at 10% level
#can use this model for our foreast
autoplot(forecast(fit), ylab="h02 sales (million scripts", xlabs="Year")
#autoarima may not give us the model with best fit, and it may be worthwhile to consider variations around the models given by autoarima to give us the best fitting model

#Comparing auto.arima() and ets() on non-seasonal data
fets <- function(x,h) {
  forecast(ets(x), h=h)
}
farima <- function(x,h) {
  forecast(auto.arima(x), h=h)
}

air <- window(ausair, start=1990)
fets <- function(air,h) {
  forecast(ets(air), h=h)
}
farima <- function(air,h) {
  forecast(auto.arima(air), h=h)
}
e1 <- tsCV(air,fets,h=1) #Compute CV errors for ETS as e1
e2 <- tsCV(air, farima, h=1) #Computer CV errors for ARIMA as e2
mean(e1^2,na.rm=TRUE) #Find MSE of each model class
mean(e2^2, na.rm=TRUE)
#In this case, ets model has lower tsCV based on MSEs
air %>% ets() %>% forecast() %>% autoplot()
#Multiplicative error, addiive trend, no seasonality

#Compare seasonal ARIMA and ETS models applied to quarterly cement production data
#Because series is long, we can afford to use training and test set only using accuracy() rather than tsCV
#We create training set from 1988 Q1 and 2007 Q4, and select an ARIMA and ETS model using auto.arima and ets functions
cement <- window(qcement, start=1988)
train <- window(cement, end=c(2007,4))
fit.arima <- auto.arima(train)
fit.arima #(1,0,1)(2,1,1)
checkresiduals(fit.arima)
#from residual plots, the residual looks like white noise, still have some significant spikes at later lags but pvalue test cannot be rejected
fit.ets <- ets(train)
checkresiduals(fit.ets)
#Find that residual plot looks very much like white noise, histogram does not look normal, p-value cannot reject null at 10% level of significance
fit.arima %>% forecast(h= 4*(2013-2007)+1) %>% accuracy(qcement)
fit.ets %>% forecast(h = 4*(2013-2007)+1) %>% accuracy(qcement)
#RMSE is lower for ets model, ets seems to be better
cement %>% ets() %>% forecast(h=12) %>% autoplot()
