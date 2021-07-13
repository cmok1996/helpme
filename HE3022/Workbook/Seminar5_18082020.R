library(fpp2)
#cross-validation is an example of ex-post forecast to measure how well the model is used for forecast
#ex-ante forecast measures how well the predictor variables are forecasted. Ex-post measures how well the model is
#Does the problem lie with the model (ex-post) or the predictor variable (ex-ante)
beer2 <- window(ausbeer, start=1992)
fit.beer <- tslm(beer2 ~ trend + season)
fcast <- forecast(fit.beer) #ex-ante forecast + ex-post forecast as there is no other predictor variable
autoplot(fcast) +
  ggtitle("Forecasts of beer production using linear regression")+ xlab("Year") + ylab("Megalitres")

#scenario-based forecasting - ex-ante forecast
fit.consBest <- tslm(Consumption ~ Income + Savings + Unemployment, data = uschange)
h <- 4
# first scenario
newdata <-   cbind(Income=c(1,1,1,1),# assigns 1% rise in income for each h-ahead q
                   Savings=c(0.5,0.5,0.5,0.5),# assigns 0.5 rise in income for each h-ahead q 
                   Unemployment=c(0,0,0,0)) %>% # unemployment is unchanged
  as.data.frame()
fcast.up <- forecast(fit.consBest, newdata=newdata)
# second scenario
newdata <-   cbind(Income=rep(-1,h),   Savings=rep(-0.5,h),   Unemployment=rep(0,h)) %>% as.data.frame() #replicate 4 times
fcast.down <- forecast(fit.consBest, newdata=newdata)
autoplot(uschange[,1]) + ylab("% change in US consumption") +
  forecast::autolayer(fcast.up, PI=TRUE, series="increase") +  
  forecast::autolayer(fcast.down, PI=TRUE, series="decrease") +
  guides(colour=guide_legend(title="Scenario"))

#prediction interval
fit.cons <- tslm(Consumption ~ Income, data=uschange)
h <- 4
fcast.up <- forecast(fit.cons,
                     newdata=data.frame(Income=rep(mean(uschange[,"Income"]), h))) 
# income rises by its past mean    
fcast.down <- forecast(fit.cons,  
                       newdata=data.frame(Income=rep(10,h))) #income rises by 10% 
autoplot(uschange[,"Consumption"]) + 
  ylab("% change in US consumption") +
  forecast::autolayer(fcast.up, PI=TRUE, series="Average increase") + #prediction interval
  forecast::autolayer(fcast.down, PI=TRUE, series="Extreme increase") +
  guides(colour=guide_legend(title="Scenario"))
#scenario 1
fcast.up #% confidence interval 
#scenario 2
fcast.down

#non-linear regression
#regression splines are good at fitting the data, but not particularly well in forecasting
#slope=B1+B2 when above c for splines. If gradient becomes flatter, B2 is negative
fit.lin <- tslm(marathon ~ trend)
autoplot(marathon) +  #obvious that trend is not linear
  forecast::autolayer(fitted(fit.lin), series = "Linear") +  
  xlab("Year") + ylab("Winning times in minutes")
res.mar <- residuals(tslm(marathon ~ trend)) #residuals are not linear
autoplot(res.mar)+  
  xlab("Year") + ylab("Residuals from a linear trend")
#choice of knot is subjective, based on subsequent linear trends identified by visual or tests to endogenously determine the breaks
h <- 10 
fit.lin <- tslm(marathon ~ trend)
fcasts.lin <- forecast(fit.lin, h=h)
fit.exp <- tslm(marathon ~ trend, lambda = 0) #boxcox with lamda=0 is just a log function
fcasts.exp <- forecast(fit.exp, h=h)
#specify the knots
t <- time(marathon)
t.break1 <- 1940
t.break2 <- 1980
t1 <- ts(pmax(0,t-t.break1), start=1897) #pmax specifies the equation in piecewise format
t2 <- ts(pmax(0,t-t.break2), start=1897)
fit.pw <- tslm(marathon ~ t + t1 + t2) #spline regression with 2 knots
t.new <- t[length(t)]+seq(h)
t1.new <- t1[length(t1)]+seq(h)
t2.new <- t2[length(t2)]+seq(h)
newdata <- cbind(t=t.new,t1=t1.new,t2=t2.new)%>%as.data.frame() 
fcasts.pw <- forecast(fit.pw,newdata = newdata)
autoplot(marathon) + 
  forecast::autolayer(fitted(fit.lin), series = "Linear") + 
  forecast::autolayer(fitted(fit.exp), series="Exponential") +  
  forecast::autolayer(fitted(fit.pw), series = "Piecewise") + 
  forecast::autolayer(fcasts.pw, series="Piecewise") +  
  forecast::autolayer(fcasts.lin$mean, series = "Linear") +
  forecast::autolayer(fcasts.exp$mean, series="Exponential") +  
  xlab("Year") + ylab("Winning times in minutes") +  
  ggtitle("Boston Marathon") + 
  guides(colour=guide_legend(title=" "))
checkresiduals(fit.pw) #presence of autocorrelation and residuals not normal

#time series decomposition
fit <- stl(elecequip, s.window="periodic") #decompose timeseries into seasonal, trend and irregular components
plot(elecequip, col="gray", main="Electical equipment manufacturing",
     ylab="New orders index", xlab="")
lines(trendcycle(fit),col="red",yab="Trend-Cycle")
plot(fit)
#seasonally adjusted data just removes the seasonality component. Macroeconomic data
plot(elecequip, col="grey",
     main="Equipment manufacturing", xlab="", ylab="New orders index") 
lines(seasadj(fit),col="red",ylab="Seasonally adjusted")
#Use moving averages to extract trend cycle. 
data.vec <- round(rnorm(10)*100) #default is round off to nearest whole number; generate 10 random numbers multiplied by 100
table.out <- rbind(data.vec, ma(data.vec, order=5))
rownames(table.out) <- c("data","5-MA")
table.out #lose first and last 2 observations
autoplot(elecsales, series="Data") + 
  forecast::autolayer(ma(elecsales,5), series="5-MA") + 
  xlab("Year") + ylab("GWh") +
  ggtitle("Annual electricity sales: South Australia") +  
  scale_colour_manual(values=c("Data"="grey50","5-MA"="red"),             
                      breaks=c("Data","5-MA"))
ma5 <- ma(elecsales, 5)
table.out <- rbind(elecsales, ma5)
rownames(table.out) <- c("sales(GWh)", "5-MA") 
table.out
#The lower the order of moving average, the rougher the trend cycle
#centered weighted even-numbered MA - middle will have weight = 1/m but first & last observation have weights = 1/2m
# a 2 by 12 MA is equivalent to a 13 MA
autoplot(elecequip, series="Data") + 
  autolayer(ma(elecequip, 12, centre = FALSE), series="12-MA") + #contaminated by the seasonality
  xlab("Year") + ylab("New orders index") + 
  ggtitle("Electrical equipment manufacturing (Euro area)") +  
  scale_colour_manual(values=c("Data"="grey","12-MA"="red"),
                      breaks=c("Data","12-MA"))

#Multiplicative Decomposition is used if variation changes as level changes - there is proportionality
elecequip %>% decompose(type="multiplicative") %>% #if type additive, the variation of seasonality will change
  autoplot() + xlab("Year") +
  ggtitle("Classical multiplicative decomposition of elec equip index")
#see excel - take centered moving average of order 12 (2x12MA)
#Calculate time trend series using moving averages
#seasonal additive should add up to 12. Rescale back to 12
#Get the remainder

#X-11 decomposition applies Spencer or Henderson to include missing data & allow seasonal component to vary slowly with time
library(fpp2)
install.packages("seasonal")
library(seasonal)
#Fluctuation is due to remainder component, hardly any variation within seasons
fit2<-seas(elecequip, x11="") 
autoplot(fit2) +  ggtitle("X11 decomposition of electrical equipment index")
autoplot(elecequip, series="Data") +
  forecast::autolayer(trendcycle(fit2), series="Trend") +
  forecast::autolayer(seasadj(fit2), series="Seasonally Adjusted") + 
  xlab("Year") + ylab("New orders index") + 
  ggtitle("Electrical equipment manufacturing (Euro area)") + 
  scale_colour_manual(values=c("gray","blue","red"),                
                      breaks=c("Data","Seasonally Adjusted","Trend"))
ggsubseriesplot(seasonal(fit2)) + ylab("Seasonal") +  
  ggtitle("Sub-series plot: Electrical equipment manufacturing (Euro area)")

elecequip %>% 
  stl(t.window=13, s.window="periodic", robust=TRUE) %>% #robust to outliers
  autoplot 

#forecasting
fit <- stl(elecequip, t.window=13, s.window="periodic", robust=TRUE) 
fit %>% seasadj() %>% naive() %>% autoplot() + ylab("New orders index") + xlab("year") +
  ggtitle("Naive forecasts of seasonally adjusted data")
fit %>% forecast(method="naive") %>% autoplot() + ylab("New orders index") #to add the seasonality back into the forecast
#shortcut is to use stlf
fcast <- stlf(elecequip, method="naive")
autoplot(fcast)
