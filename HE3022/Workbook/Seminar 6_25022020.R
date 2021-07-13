library(fpp2)
oildata <- window(oil, start=1996)
autoplot(oildata) +
  ylab("Oil (millions of tonnes") + xlab("Year")+
  ggtitle("Saudi Arabia oil production")
#the larger the weight, the more importance is given to the more recent observation
#alpha = 1 is simply a naive model
#alpha and lamda is decided by the user
fc <- ses(oildata, h=5) #ses gives us the simple exponential smoothing model
round(accuracy(fc),2) #get error forecast
summary(fc) #info on weight (alpha) & initial forecast (l0 = 446.85 vs 445.36 in first observation)
autoplot(fc) +  
  forecast::autolayer(fitted(fc), series="Fitted") +
  ylab("Oil (millions of tonnes)") + xlab("Year")
