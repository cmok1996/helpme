#Elimination

#COMBINATION#
combination1 <- (fit1[["mean"]] + fc6[["mean"]])/2
combination2 <- (fit1[["mean"]] + fcast_dynamic[["mean"]])/2
combination3 <- (fc6[["mean"]] + fcast_dynamic[["mean"]])/2
combination4 <- (fc6[["mean"]] + fcast_dynamic[["mean"]] + fit1[["mean"]])/3


df_checkall <- cbind(c(Benchmark = accuracy(forecast(fit1,h=36), fsitest)[1,"RMSE"],
                       ETS=accuracy(fc6,fsitest)[1,"RMSE"],
                       Regression = accuracy(fcast_dynamic, fsitest)[1,"RMSE"],
                       Combination1 = accuracy(combination1, fsitest)[1,"RMSE"],
                       Combination2 = accuracy(combination2, fsitest)[1,"RMSE"],
                       Combination3 = accuracy(combination3, fsitest)[1,"RMSE"],
                       Combination4 = accuracy(combination4, fsitest)[1,"RMSE"]), 
                     c(Benchmark = accuracy(fit1, fsitest)[2,"RMSE"],
                       ETS=accuracy(fc6,fsitest)[2,"RMSE"],
                       Regression = accuracy(fcast_dynamic, fsitest)[2,"RMSE"],
                       Combination1 = accuracy(combination1, fsitest)[2,"RMSE"],
                       Combination2 = accuracy(combination2, fsitest)[2,"RMSE"],
                       Combination3 = accuracy(combination3, fsitest)[2,"RMSE"],
                       Combination4 = accuracy(combination4, fsitest)[2,"RMSE"]))


#Best-fit model
df_full <- cbind("fsi" = FSI[,1] %>% window(c(2005,1), c(2020,2)),
                 "rsi_chained" = rsi_chained %>% window(c(2005,1), c(2020,2)),
                 "tourism" = tourism_scale %>% window(c(2005,1), c(2020,2)),
                 "cpi" = cpi %>% window(c(2005,1), c(2020,2))) %>%
  data.frame() %>%
  ts(c(2005,1), c(2020,2), 12)

fit_full <- Arima(df_full[,1], xreg=cbind("rsi"= df_full[,2], "tourism" = df_full[,3], "cpi" = df_full[,4]), order=c(0,1,1), seasonal=c(0,1,1))
summary(fit_full)
checkresiduals(fit_full)
cbind("Actual" = df_full[,1], "Fitted" = fit_full$fitted)  %>% autoplot(facets=TRUE)


#We first need to develop a simple model to predict our predictor variables

df_past <- cbind("fsi" = FSI[,1] %>% window(c(2000,6), c(2004,12)),
                 "rsi" = rsi_chained %>% window(c(2000,6), c(2004,12)),
                 "tourism" = tourism_scale %>% window(c(2000,6), c(2004,12)),
                 "cpi" = cpi %>% window(c(2000,6), c(2004,12))) %>%
  data.frame() %>%
  ts(c(2000,6), c(2004,12), 12)
autoplot(df_past, facets=TRUE) + ylab("Parameters")

#First superspreader in China in January 2003, First case of SARS in March 2003, Singapore was removed from WHO list of affected areas in May 2003
#fsi dipped significantly in April by 20 index, so does tourism & cpi
#rsi low from feb-sep by 5 index
#tourism dipped in April by 300,000, remain low till May, then bounce back up in Jul
#cpi dipped 0.5 index in May, bounce back upwards in Jul 

#ex-ante forecast
#Consider ets, stlf, ARIMA

fcast1_rsi <- auto.arima(df_full[,"rsi_chained"]) %>% forecast(h=12)
fcast2_rsi <- stlf(df_full[,"rsi_chained"], h=12, method = "ets")
checkresiduals(fcast1_rsi)
checkresiduals(fcast2_rsi)
#Choose auto.arima because no autocorrelation at lag 12
autoplot(df_full[,"rsi_chained"], series = "Actual") +
  forecast::autolayer(fcast2_rsi$mean, series = "Forecast")
fcast_cpi <- auto.arima(df_full[,"cpi"]) %>% forecast(h=12)
fcast_tourism <- auto.arima(df_full[,"tourism"]) %>% forecast(h=12)
xreg_forecast <- cbind("rsi"= fcast1_rsi$lower[,"95%"], "tourism"= fcast_tourism$lower[,"95%"], "cpi" = fcast_cpi$lower[,"95%"])
fcast_fsi <- forecast(fit_full, xreg=xreg_forecast, h=12)
autoplot(fcast_fsi)
