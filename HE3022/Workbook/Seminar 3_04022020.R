library(fpp2)
plot(elec) #increasing variation
lambda <- BoxCox.lambda(elec) #calculate lambda = 0.27
plot(BoxCox(elec,lambda),ylab="Transformed electricity demand",
     xlab="",main="Transformed monthly electricity demand") #Constant variation
#When we back transform, we will get the median, leads to bias
fc <- rwf(eggs, drift=TRUE, lambda=0, h=50, level=80) #returns forecast ad predicton interval for random walk with drift model
fc2 <- rwf(eggs, drift=TRUE, lambda=0, h=50, level=80, biasadj=TRUE) #lambda = 0 gives log transformation, h is the period forecast ahead
#difference between fc & fc2 is the adjustment of bias. fc gives median, fc2 gives mean forecast
autoplot(eggs) + 
  forecast::autolayer(fc$mean, series="Simple back transformation") + 
  forecast::autolayer(fc2$mean, series="Bias adjusted") +
  guides(colour=guide_legend(title="Forecast")) #if we omit the $mean, it gives prediction interval
#greater the h, greater the bias. Formula is a function of h
autoplot(fc2)+
  ggtitle("Biased adjusted transformed")

#Calendar adjustments - eg, CNY, stock market zero observation. February has fewer days, thus production is less
dframe <- cbind(Monthly = milk, DailyAverage = milk/monthdays(milk)) #cbind combines 2 series output, store in dframe object. monthdays adjust different days each month
autoplot(dframe, facet=TRUE) + xlab("Years") + ylab("Pounds") +
  ggtitle("Milk production per cow") #effectively remove the variation due to different month lengths

#Population adjustments - look at real terms. Is the increase real or due to increase in population?

dj2 <- window(dj, end=250)
autoplot(dj2) + xlab("Day") + ylab("") + 
  ggtitle("Dow Jones Index (daily ending 15 Jul 94)")
res <- residuals(naive(dj2)) #et = yt-yt-1
autoplot(res) + xlab("Day") + ylab("") +
  ggtitle("Residuals from naive method") #looks random, no seasonality. residual mean close to zero
gghistogram(res) + ggtitle("Histogram of residuals") #Does not look normal, skewed to the left
ggAcf(res) + ggtitle("ACF of residuals") #dashed blue line would give 95% confidence interval. Cannot reject the null of no serial correlation

#use h=10 for non-seasonal data, h=2m for seasonal data, eg monthly seasonality h=24, h=T/5 when is >T/5
Box.test(res,lag=10,fitdf=0) #p=0.385 is high, cannot reject null hypothesis of no serial correlation
Box.test(res,lag=10,fitdf=0,type="Lj") #p=0.3507
checkresiduals(naive(dj2)) #plot residuals, ggAcf, histogram, and Box test

#underfit - when the residual still contains information that is not incorporated into the model
#overfit - misinterpret noise as information. Residual error is small but forecast error is large

window(ausbeer,start=1995) #quarterly data
subset(ausbeer, start=length(ausbeer)-4*5) #extracts last 5 years of data
subset(ausbeer,quarter=1) #extracts first quarter for all years
tail(ausbeer,4*5) #extracts last 5 years of data

#Forecast errors, training data is given by 1 to T which gives residual error, and test data is T+1 onwards which gives forecast error given information at time T
#MAE & RMSE are scale-dependant, need to be in same unit. MAE gives better median forecast while RMSE gives better mean forecast
#RMSE highlights the outliers as heavier penalty is imposed
#Scale independent accruacy measures are percentage errors. pi=100ei/yi. units drop out as divide by itself
#Scaled error is the forecast error divide by the forecast error given by naive method. If qj>1, might as well use naive method
#If is seasonal model, then we compare with seasonally naive model from last season

beer2 <- window(ausbeer,start=1992,end=c(2007,4)) #Defining training set from 1992Q1 to 2007Q4; Use 2008Q1 to 2010Q2 for test set
beerfit1 <- meanf(beer2,h=10) #fit mean model + forecast 10 periods ahead
beerfit2 <- rwf(beer2,h=10) #fit random walk model +forecast 10 periods ahead
beerfit3 <- snaive(beer2,h=10) #fit seasonally-naive + forecast 10 periods ahead
autoplot(window(ausbeer,start = 1992)) +
  forecast::autolayer(beerfit1$mean,series="Mean") +
  forecast::autolayer(beerfit2$mean,series="Naive") +
  forecast::autolayer(beerfit3$mean,series="Seasonal naive")+
  xlab("Year") + ylab("Megalitres") +
  ggtitle("Forecast for quarterly beer production") +
  guides(colour=guide_legend(title="Forecast"))
beer3 <- window(ausbeer,start=2008) #Test set
accuracy(beerfit1,beer3)
accuracy(beerfit2,beer3)
accuracy(beerfit3,beer3)
rbind(accuracy(meanf(beer2,h=10),beer3)[2,c(2,3,5,6)],
      accuracy(rwf(beer2,h=10), beer3)[2,c(2,3,5,6)], 
      accuracy(snaive(beer2,h=10), beer3)[2,c(2,3,5,6)])

#Rolling forecast because the "origin" (k+i-1) at which the forecast is based rolls forward in time
e <- tsCV(dj,rwf,drift=TRUE,h=1) #a list of 1-step ahead rolling forecast error 
sqrt(mean(e^2,na.rm=TRUE)) #omit any missing oberservations, RMSE using cross validation
sqrt(mean(residuals(rwf(dj,drift=TRUE))^2,na.rm=TRUE)) #RMSE of residuals is smaller than with CV as these are residual errors and not forecast errors

naive(dj2) #One of the special commands which fits the model and gives the forecast
autoplot(naive(dj2)) #as h increases, prediction interval increases
#Prediction interval is given the assumption that distribution is normal

#Prediction interval from bootstrapped residuals
#the forecast error for predicted value is not given by a normal distribution but from a simulation from the training data set
