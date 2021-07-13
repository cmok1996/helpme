library(fpp2)
library(tseries)
#the function ets() will automatically give us the appropriate model based on lowest AIC
#estimate smoothing parameters of level, trend, seasonality, dampening and initiate states by maximizing the likelihood
#might not get same point forecast because ets and exponentiate smoothing model is used on 2 different techniques (max likelihood vs min SSR)
#lambda gives you transformation, transformation gives median forecast but use biasedj=TRUE to give mean forecast
#ets restricts models to stable models only, multiplicative trend usually gives unstable results
aust <- window(austourists, start=2005)
fit <- ets(aust)
summary(fit)
#alpha = 0.198 tells us that the error gives small changes to the level
#beta = 0.0392 tells us that the trend changes by small amount each time
#gamma = 0.00019 tells us that there is hardly any change in seasonality
autoplot(fit)
#there is a decreasing trend first, but trend increases after a point. Cannot see in naked eye
#We have multiplicatie seasonality, so the pattern does not change
cbind("Residuals" = residuals(fit),
      "Forecast errors" = residuals(fit, type='response')) %>%
autoplot(facet=TRUE) + xlab("Year") + ylab("")
fit %>% forecast(h=8) %>% 
  autoplot +
  ylab("International visitor in Australia (millions)")
#et+1 becomes 0 as there is no data available
#ETS point forecasts are equal to medians of forecast distributions. For models with only addiive components, the forecast distributions are normal, so the mean and medians are equal
#For models with multiplicative error or seasonality, point forecast will not be equal to means of forecast distribution
#Prediction interval depends on forecast variance, increases with h

#auto.arima gives you the best arima time series, but less precises so still have to go through the checks to ensure that is appropriate
#note that moving average here is different from the moving average we use to extract the trend component
#focus on non-seasonal component first
#difference between ets & arima is that arima needs to be stationary. The trend and seasonality makes the series non-stationary
#naive model - yt = yt-1 + et is non-stationary because is not mean-reverting. The disturbance on et will propogate over time
#b&g are stationary series. g pattern is not time-dependent
         
cbind("Sales ($million)" = a10,
      "Monthly log sales" = log(a10),
      "Annual change in log sales" = diff(log(a10),1)) %>%
        autoplot(facet=TRUE) +
        xlab("Year") + ylab("") +
        ggtitle("Antidiabetic drug sales")
#differencing removes the trend component