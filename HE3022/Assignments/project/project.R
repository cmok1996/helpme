#load packages
library(fpp2)
library(tseries)
library(GGally)
library(seasonal)
library(dplyr)

#Load data - data from 1985-01 to 2020-01
fnb <- read.csv("fnb_index_chained.csv", header=TRUE)
fnb <- ts(fnb[,2:6], start=c(1985,1), frequency=12)

#fnb_chained_sa <- read.csv("fnb_chained_sa.csv", header=TRUE)
tourism <- read.csv("tourism.csv", header=TRUE)
tourism_ts <- ts(tourism[,2], start = c(1978,1), end = c(2020,2), frequency=12)
tourism_df <- cbind("Total" = tourism_ts, "log-total" = log(tourism_ts), "Growth rate" = diff(log(tourism_ts),1))
tourism_window <- window(tourism_df, c(2005,1), c(2019,12))
autoplot(tourism_window, facets=TRUE)

#take log for population
population <- read.csv("ppn.csv", header=TRUE)

#pct_change <- (population$Total.Population....Number./lag(population$Total.Population....Number.)-1)*100
population_ts <- ts(population[,2], 1950, 2019, frequency = 1)
population_ts <- window(population_ts, 2005)
monthly_total <- ts(rep(population_ts, each=12), c(2005,1), c(2019,12), 12)
monthly_logtotal <- ts(rep(log(population_ts), each = 12), c(2005,1), c(2019,12), frequency=12)
monthly_rate <- ts(rep(diff(log(population_ts),1), each= 12), c(2005,1), c(2019,12), frequency=12)
population_df <- cbind("Total" = monthly_total, "log-total" = monthly_logtotal, "Growth rate" = monthly_rate)
autoplot(population_df, facets=TRUE)

#hotels
hotels <- read.csv("hotels.csv")
hotels_ts <- ts(hotels[,2], c(2008,1), c(2020,2), 12)
autoplot(hotels)
hotels_df <- cbind("Total" = hotels_ts, "log-total" = log(hotels_ts), "Growth rate" = diff(log(hotels_ts),1))
hotels_window <- window(hotels_df, c(2005,1), c(2019,12))
autoplot(hotels_window, facets=TRUE)

#labour
labour <- read.csv("labour.csv", header=TRUE)
labour <- data.frame(labour[1:72,1:2])
labour_ts <- ts(labour[1:72,1:2], c(2002,1), c(2019,4), 4)
autoplot(labour_ts, facets=TRUE)

#rsi
rsi <- read_excel("rsi.xlsx")
rsi_ts <- ts(rsi[,2], c(1985,1), c(2020,2), 12)
rsi_window <- window(rsi_ts, c(2005,1), c(2019,12))
autoplot(rsi_window)

#employment
food <- read_excel("food_netchanges.xlsx", header=TRUE)
services <- read_excel("services_netchanges.xlsx")
food <- data.frame(rep(food[,4], each = 3))
services <- rep(services[,4], each=3)
employment <- cbind(food, services)
employment_ts <- ts(employment, c(2006,1), c(2019,12),12)
autoplot(employment_ts[,1], facets=TRUE)



monthly_ppn <- rep()
annual_ppn <- population_window[,2]
monthly_ppn <- rep(log(population_window[,1]), each=12)
monthly_rate <- rep(population["percentage change"], each=12)
ppn <- data.frame(monthly_ppn, monthly_rate)
ppn <- ts(ppn[,1], c(2005,1), c(2019,12), frequency=12)

autoplot(population_window, facets=TRUE)

fnb_train <- window(fnb, start = c(2005,1), end = c(2017,12))
tourism_train <- window(tourism_window, c(2005,1), c(2017,12))
population_train <- window(ppn[,1:2], c(2005,1), c(2017,12))
fnb_test <- window(fnb, start = c(2018,1), end = c(2019,12))
tourism_test <- window(tourism_window, c(2018,1), c(2019,12))
population_test <- window(ppn, c(2018,1), c(2019,12))


df_train <- data.frame(fnb_train[,1], tourism_train, population_train[,2])
df_test <- data.frame(fnb_test[,1], tourism_test, population_test)

fit1 <- tslm(df_train[,1] ~ df_train[,2] + df_train[,3], na.rm=TRUE)
summary(fit1)
checkresiduals(fit1)

fi2 <- auto.arima(df_train[,1],stepwise=FALSE, approximation=FALSE)
summary(fit2)
checkresiduals(fit2)

fit3 <- auto.arima(df_train[,1], xreg=cbind(df_train[,2], df_train[,3]))
summary(fit3)
checkresiduals(fit3)


df_ts <- ts(df, start = c(2005,1), end = c(2019,12), frequency=12)
autoplot(df_ts, facets=TRUE)

fit1 <- tslm(df_ts[,1] ~ trend + season + df_ts[,2])
summary(fit)
checkresiduals(fit)

fit2 <- auto.arima(df_ts[,1], df_ts[,2])
summary(fit2)

autoplot(seasadj(seas(tourism_window)))

#Convert to ts
#fnb_chained_sa_ts <- ts(fnb_chained_sa[,2:6], start=c(1985,1), frequency=12)
#fnb_chained_sa_window <- window(fnb_chained_ts[,2],start=c(2005,1),end=c(2016,12))
#autoplot(fnb_chained_sa_window)

#Timeplots
autoplot(fnb, facets=TRUE) + ggtitle("FnB Index in chained volume (Overall)") + xlab("Year") + ylab("Index")

#Window to 2005 onwards - reduce from 421 data points to 181 data points
fnb_window <- window(fnb, c(2005,1), c(2016,12)) #144 observations
autoplot(fnb_window, facets=TRUE) + ggtitle("FnB Index in chained volume (Overall) from 2005 - 2016") + xlab("Year") + ylab("Index")
#fnb_chained_sa_window <- window(fnb_chained_sa_ts, c(2005,1), c(2016,12))
autoplot(fnb[,2], facets=TRUE) + ggtitle("FnB Index in chained volume (2005 onwards)") + xlab("Year") + ylab("Index")
#autoplot(fnb_chained_sa_window[,2], facets=TRUE) + ggtitle("FnB Index in chained volume seasonally adjusted (2005 onwards)") + xlab("Year") + ylab("Index")

#Split train & test
fnb_total <- fnb[,1] #only look at total
fnb_train <- window(fnb_total,c(2005,1),c(2020,1)) #split training set - 144 obs
fnb_test <- window(fnb_total,c(2017,1)) #split test set - 37 obs


### Preliminary investigation

#Box Cox?
hist(fnb_train)
jarque.bera.test(fnb_train) #p=0.3574, cannot reject null hypothesis that it follows normal

#ggpairs
fnb_window %>% as.data.frame() %>% GGally::ggpairs() + ggtitle("Correlation Matrix for F&B index in chained volume")

#acf plots
ggAcf(fnb_window, lags=24) + ggtitle("Acf plots for F&B index in chained volume")
#seasonal plots
ggseasonplot(fnb_window[,1], year.labels=TRUE, year.labels.left=TRUE) + ylab("Index") + ggtitle("Seasonal plot: FnB Total Index in Chained volume")
#ggseasonplot(fnb_chained_window[,2], year.labels=TRUE, year.labels.left=TRUE) + ylab("Index") + ggtitle("Seasonal plot: FnB Restaurants Index in Chained volume")
#ggseasonplot(fnb_chained_window[,3], year.labels=TRUE, year.labels.left=TRUE) + ylab("Index") + ggtitle("Seasonal plot: FnB FastFood Index in Chained volume")
#ggseasonplot(fnb_chained_window[,4], year.labels=TRUE, year.labels.left=TRUE) + ylab("Index") + ggtitle("Seasonal plot: FnB FoodCatering Index in Chained volume")
#ggseasonplot(fnb_chained_window[,5], year.labels=TRUE, year.labels.left=TRUE) + ylab("Index") + ggtitle("Seasonal plot: FnB FoodCourt Index in Chained volume")

#subseries plots
ggsubseriesplot(fnb_window[,1]) + ylab("Index") + ggtitle("Subseries plot: FnB Total Index in Chained volume")
ggsubseriesplot(fnb_window[,2]) + ylab("Index") + ggtitle("Subseries plot: FnB Restaurants Index in Chained volume")
ggsubseriesplot(fnb_window[,3]) + ylab("Index") + ggtitle("Subseries plot: FnB FastFood Index in Chained volume")
ggsubseriesplot(fnb_window[,4]) + ylab("Index") + ggtitle("Subseries plot: FnB FoodCatering Index in Chained volume")
ggsubseriesplot(fnb_window[,5]) + ylab("Index") + ggtitle("Subseries plot: FnB FoodCourts Index in Chained volume")



###Decompose time-series
fnb_total <- fnb[,1] #only look at total
fnb_train <- window(fnb_total,c(2005,1),c(2017,12)) #split training set - 144 obs
fnb_test <- window(fnb_total,c(2018,1)) #split test set - 37 obs
autoplot(fnb_train)

#STL
fit_stl <- stl(fnb_train, s.window="periodic", robust=TRUE)
autoplot(fit_stl) + ggtitle("Decomposition using STL")
fit_stl_seasadj <- seasadj(fit_stl) 
autoplot(fit_stl_seasadj) + ggtitle("Seasonally adjusted data using STL") +ylab("Index")

autoplot(seasadj(stl(fnb_train, s.window="periodic")))


#X11
fit_x11 <- seas(fnb_train, x11=list())
autoplot(fit_x11)
fit_x11_seasadj <- seasadj(fit_x11)
autoplot(fit_x11_seasadj) + ggtitle("Seasonally adjusted data using X11") + ylab("Index")


#X12
fit_x12 <- seas(fnb_train)
autoplot(fit_x12)
fit_x12_seasadj <- seasadj(fit_x12)
autoplot(fit_x12_seasadj) + ggtitle("Seasonally adjusted data using X12") + ylab("Index")

#forecast using decomposition
fcast1 <- stlf(fnb_train, method = "naive", h=37)
autoplot(fnb_train) +
  forecast::autolayer(fnb_test, series = "Actual") +
  forecast::autolayer(fcast1$mean, series = "STL forecast")

fit3 <- auto.arima(fnb_train)
fcast2 <- forecast(fit3, h=5)

fit_x11 %>% seasadj() %>% naive() %>% autoplot()

autoplot(seas(fnb_train))

#ARIMA forecast
fnb_seasdiff <- diff(log(fnb_train), 12)
cbind("Fnb Index" = fnb_train, "Monthly difference in Fnb Index" = fnb_diff <- diff(fnb_train,1), "Seasonal double difference in Fnb Index" = diff(fnb_seasdiff,1)) %>% autoplot(facets=TRUE)

fnb_arima <- auto.arima(fnb_train, stepwise=FALSE, approximation=FALSE)
fcast3 <- forecast(fnb_arima, length(fnb_test))

autoplot(fnb_train) +
  forecast::autolayer(fnb_test, series = "Actual") +
  forecast::autolayer(fcast3$mean, series = "SARIMA forecast")

summary(fnb_arima)

adf.test(fnb_train, alternative = "stationary")

ggtsdisplay(diff(fnb_seasdiff,1))



seasadj(decompose(fnb_total_train, type="additive"))%>% autoplot() + xlab("Year") + ggtitle("Classical additive decomposition of F&B Total Index")
seasadj(decompose(fnb_total_train, type="multiplicative"))%>% autoplot() + xlab("Year") + ggtitle("Classical multiplicative decomposition of F&B Total Index")
test1 <- seas(x=fnb_total_train) #x11 decomposition
autoplot(test1)
autoplot(seasadj(test1)) +
  xlab("Year") + ggtitle("x11 decomposition of F&B Total Index")
summary(test1)
seasonal_fit1 <- test1 <- seas(x=fnb_total_train, x11=list(), transform.function="log") #x11 decomposition with log transformation
autoplot(seasadj(seasonal_fit1),series="Seasonally adjusted using x11") + 
  forecast::autolayer(fnb_total_train, series="Training data") +
  xlab("Year") + ggtitle("x11 decomposition of log-transformed F&B Total Index")

data(holiday)
cny.ts <- genhol(cny,start=0, end=0,center="calendar",frequency=12) #obtain calendar adjustment for cny
test2 <- seas(x=fnb_total_train, xreg = cny.ts, regression.usertype = "holiday", x11=list()) #regress on cny, very significant
autoplot(seasadj(test2)) + 
  xlab("Year") + ggtitle("x11 decomposition of F&B Total Index with cny adjustments")
seasonal_fit2 <- seas(x=fnb_total_train, xreg = cny.ts, regression.usertype = "holiday", x11=list(), transform.function="log")
autoplot(seasadj(seasonal_fit2)) + xlab("Year") + ggtitle("x11 decomposition of log-F&B Total Index with cny adjustments")
summary(seasonal_fit2)

test3 <- stl(fnb_total_train, s.window="periodic")
autoplot(test3)
autoplot(seasadj(test3))

restaurants <- window(fnb_chained_ts[,2],c(2005,1), c(2016,12))
test <- seas(x=restaurants, regression.usertype = "holiday", x11=list())
autoplot(test)
test2 <- seas(x=restaurants, xreg = cny.ts, regression.usertype = "holiday", x11=list())
autoplot(test2)



short <- window(fnb_total_train,c(2005,1),c(2009,12))
autoplot(stl(short,s.window="periodic"))
autoplot(seas(short))
abc <- seas(short,xreg=cny.ts,regression.usertype="holiday",x11=list())
autoplot(seasadj(abc))
autoplot(ets(seasadj(abc)))

#Current
#fnb_current <- read.csv("fnb_index_2020.csv", header=TRUE)
#fnb_current_ts <- ts(fnb_current[,2:6], start=c(1985,1), frequency=12)
#autoplot(fnb_current_ts, facets=TRUE) + ggtitle("FnB Index in current prices (Overall)") + xlab("Year") + ylab("Index")
#fnb_current_window <- window(fnb_current_ts, 2005)
#autoplot(fnb_current_window, facets=TRUE) + ggtitle("FnB Index in current prices (2005 onwards)") + xlab("Year") + ylab("Index")
#ggseasonplot(fnb_current_window[,1], year.labels=TRUE, year.labels.left=TRUE) + ylab("Index") + ggtitle("Seasonal plot: FnB Total Index in Current prices")
#ggseasonplot(fnb_current_window[,2], year.labels=TRUE, year.labels.left=TRUE) + ylab("Index") + ggtitle("Seasonal plot: FnB Restaurants Index in Current prices")
#ggseasonplot(fnb_current_window[,5], year.labels=TRUE, year.labels.left=TRUE) + ylab("Index") + ggtitle("Seasonal plot: FnB FastFood Index in Current prices")
#ggseasonplot(fnb_current_window[,1], polar=TRUE) + ylab("Index") + ggtitle("Seasonal plot: FnB Total Index in Current prices")
#ggseasonplot(fnb_current_window[,2], polar=TRUE) + ylab("Index") + ggtitle("Seasonal plot: FnB Restaurants Index in Current prices")
#ggseasonplot(fnb_current_window[,3], polar=TRUE) + ylab("Index") + ggtitle("Seasonal plot: FnB FastFood Index in Current prices")
#ggsubseriesplot(fnb_current_window[,1]) + ylab("Index") + ggtitle("Subseries plot: FnB Total Index in Current prices")
#ggsubseriesplot(fnb_current_window[,2]) + ylab("Index") + ggtitle("Subseries plot: FnB Restaurants Index in Current prices")
#ggsubseriesplot(fnb_current_window[,3]) + ylab("Index") + ggtitle("Subseries plot: FnB FastFood Index in Current prices")
#polar seasonal plots
#ggseasonplot(fnb_chained_window[,1], polar=TRUE) + ylab("Index") + ggtitle("Seasonal plot: FnB Total Index in Chained volume")
#ggseasonplot(fnb_chained_window[,2], polar=TRUE) + ylab("Index") + ggtitle("Seasonal plot: FnB Restaurants Index in Chained volume")
#ggseasonplot(fnb_chained_window[,3], polar=TRUE) + ylab("Index") + ggtitle("Seasonal plot: FnB FastFood Index in Chained volume")
#fnb_current_window %>% as.data.frame() %>% GGally::ggpairs() + ggtitle("Correlation Matrix for F&B index in current prices")
#ggAcf(fnb_current_window, lags=24) + ggtitle("Acf plots for F&B index in current prices")
