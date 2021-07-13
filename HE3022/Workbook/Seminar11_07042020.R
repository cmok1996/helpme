#Forecast combination

#Consider, ETS, ARIMA, STL-ETS
train <- window(auscafe, end=c(2012,9))
h <- length(auscafe) - length(train)
ETS <- forecast(ets(train), h=h)
ARIMA <- forecast(auto.arima(train, lambda=0, biasadj=TRUE), h=h)
STL <- stlf(train, lambda=0, h=h, biasadj=TRUE) #estimate training set + forecast the training set into test set using ETS
Combination <- (ETS[["mean"]] + ARIMA[["mean"]] + STL[["mean"]])/3
autoplot(auscafe) + 
  autolayer(ETS, series="ETS", PI=FALSE) +
  autolayer(ARIMA, series="ARIMA", PI=FALSE) + 
  autolayer(STL, series="STL", PI=FALSE) + 
  autolayer(Combination, series="Combination") + 
  xlab("Year") + ylab("$ billion") +  ggtitle("Australian monthly expenditure on eating out")

c(ETS = accuracy(ETS, auscafe)["Test set","RMSE"],   
  ARIMA = accuracy(ARIMA, auscafe)["Test set","RMSE"],  
  `STL-ETS` = accuracy(STL, auscafe)["Test set","RMSE"], 
  Combination = accuracy(Combination, auscafe)["Test set","RMSE"])
#Combination has the lowest RMSE

#Dynamic Regression Models
library(fpp2)
library(series)

#In chapter 5, we look at the example of US personal consumption
autoplot(uschange[,1:2], facets=TRUE) +
  xlab("Year") + ylab("") +
  ggtitle("Quarterly changes in US consumption and personal income")
fit.ci <- tslm(Consumption ~ Income, data=uschange)
summary(fit.ci)
checkresiduals(fit.ci) #p-value is small, reject null hypothesis, significant autocorrelation

#Dynamic Regression Models - error allows autocorrelation
#All the variables in the model must first be stationary
#If any one variable is non-stationary, difference all variables to retain the form
#If a regression model has ARIMA errors, then this is equivalent to regression model in differences with ARMA errors

fit <- auto.arima(uschange[,"Consumption"], xreg=uschange[,"Income"])
summary(fit) #ARIMA(1,0,2)
#Recover estimates of both the nt and et using the residuals() function
cbind("Regression Errors" = residuals(fit, type="regression"), #nt
      "ARIMA errors" = residuals(fit, type="innovation")) %>% autoplot(facet=TRUE) #et
checkresiduals(fit) #ACF plots have little significant spikes, p-value = 0.117, cannot reject null hypothesis - an improvement. Expect predition interval to be reliable

#Forecasting once errors look like white noise
fcast <- forecast(fit, xreg=rep(mean(uschange[,2])),8) #Assume that uschange for the next 8 periods is the historical mean
autoplot(fcast) + xlab("Year") + ylab("Percentage change")
#Using chnage of income as predictor, do not take into account the uncertainty for predictor variables

#Forecasting electricity and demand for next 14 days
qplot(Temperature, Demand, data=as.data.frame(elecdaily)) +
  ylab("Elec Demand (GW)") + xlab("Max daily temperature (Celsius)") + ggtitle("Figure 9.5: Daily electricity demand versus maximum daily for the state of Victoria in Australia for 2014")
autoplot(elecdaily[,c(1,3)], facets=TRUE)
xreg <- cbind(MaxTemp = elecdaily[, "Temperature"],
              MaxTempSq = elecdaily[, "Temperature"]^2,
              Workday = elecdaily[,"WorkDay"])
fit <- auto.arima(elecdaily[,"Demand"], xreg=xreg)
checkresiduals(fit) #ARIMA(2,1,2)(2,0,0)[7] seasonality of 7 because daily data, can see significant spikes - autocorrelation in residuals
fcast <- forecast(fit, 
                  xreg = cbind(MaxTemp=rep(26,14), maxTempSq=rep(26^2,14), #assume temp is constant at 26
                               Workday = c(0,1,0,0,1,1,1,1,1,0,0,1,1,1))) #Mon-Fri is 1, Sat-Sun + Public hols = 0
autoplot(fcast) + ylab("Electricity demand (GW")
#Shows the forecast from dynamic regression model when all future temperature set to 26 degrees, and working day dummy variable set to known future values
#Daily seasonality
#Point forecast looks reasonable for the next 2 weeks

#Deterministic vs Stochastic trend
autoplot(austa) + xlab("Year") +
  ylab("millions of people") +
  ggtitle("Total annual international visitors to Australia")
trend <- seq_along(austa)
(fit1 <- auto.arima(austa, d=0, xreg=trend)) #coefficient for t = 0.17, estimated growth in visitor numbers is 0.17mil per year
(fit2 <- auto.arima(austa, d=1)) #random walk model with drift
#ARIMA(0,1,1) with drift
#Although coefficient for trend is also 0.17, prediction interval varies alot
fc1 <- forecast(fit1,
                xreg = length(austa) + 1:10) #10 period ahead forecast
fc2 <- forecast(fit2, h=10)
autoplot(austa) +
  autolayer(fc2, series="Stochastic trend") +
  autolayer(fc1, series="Deterministic trend") +
  ggtitle("Forecasts from trend models") +
  xlab("Year") + ylab("Visitors to Australia (millions)") +
  guides(colour=guide_legend(title="Forecast"))
#PI for stochastic trend is so much wider than the deterministic trend

#Slope for deterministic trend is not changing over time
#If yt is stationary, use deterministic trend. If non-stationary, we difference yt to get stochastic trend

#Lagged predictors
autoplot(insurance, facets=TRUE) +
  xlab("year") + ylab("") +
  ggtitle("Insurance advertising and quotations")
#Lagged predictors. Test 0,1,2 or 3 lags. 
Advert <- cbind(
  AdLag0 = insurance[,"TV.advert"],
  AdLag1 = stats::lag(insurance[,"TV.advert"], -1),
  AdLag2 = stats::lag(insurance[,"TV.advert"], -2),
  AdLag3 = stats::lag(insurance[,"TV.advert"], -3)) %>%
  head(NROW(insurance)) #40 observations
#When comparing models, all models must use same training set
# Restrict data so models use same fitting period
fit1 <- auto.arima(insurance[4:40,1], xreg=Advert[4:40,1],
                   stationary=TRUE)
fit2 <- auto.arima(insurance[4:40,1], xreg=Advert[4:40,1:2],
                   stationary=TRUE)
fit3 <- auto.arima(insurance[4:40,1], xreg=Advert[4:40,1:3],
                   stationary=TRUE)
fit4 <- auto.arima(insurance[4:40,1], xreg=Advert[4:40,1:4],
                   stationary=TRUE)
#Choose the optimal lag length based on AICc
c(fit1[["aicc"]], fit2[["aicc"]], fit3[["aicc"]],fit4[["aicc"]])
#Best model with smalled AICc value has two lagged predictors (current + previous)
fit <- auto.arima(insurance[,1], xreg=Advert[,1:2], stationary=TRUE) #use autoarima to model
summary(fit) #AR3 model
#forecasts can be calculated using this model if the future values for advertising values are assumed
fc8 <- forecast(fit, h=20,
                xreg=cbind(AdLag0 = rep(8,20),
                           AdLag1 = c(Advert[40,1], rep(8,19))))
autoplot(fc8) + ylab("Quotes") +
  ggtitle("Forecast quotes with future advertising set to 8")
