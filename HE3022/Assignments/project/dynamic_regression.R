### Dynamic Regression
library(fpp2)
library(dplyr)
library(tseries)
library(readxl)
library(tfplot)

## Load Data

# FnB
fnb <- read.csv("fnb_index_chained.csv", header=TRUE)
fnb_ts <- ts(fnb[,2], c(1985,1), c(2020,1), 12)
fnb_SA <- stl(fnb_ts, s.window=13) %>% seasadj() 
fnb_growth <- percentChange(fnb_ts)
fnb_growth_SA <- percentChange(fnb_ts) %>% stl(s.window=13) %>% seasadj()
fnb_df <- cbind(fnb = window(fnb_ts,c(2005,1)),
                fnb_SA = window(fnb_SA, c(2005,1)), 
                fnb_rate = window(fnb_growth, c(2005,1)),
                fnb_rate_SA = window(fnb_growth_SA, c(2005,1))) %>% 
  data.frame() %>%
  ts(c(2005,1), c(2019,12),12)
autoplot(fnb_df, facets=TRUE)

# RSI
rsi <- read.csv("rsi2.csv")
rsi <- ts(rsi[,2], c(1985,1), c(2020,2),12)
rsi_sa <- read.csv("rsi.csv") #seasadj
rsi_sa <- ts(rsi_sa[,2], c(1985,1), c(2020,2),12)
rsi_df <- cbind(rsi = window(rsi,c(2005,1)),
                rsi_SA = window(rsi_sa, c(2005,1)), 
                rsi_rate = percentChange(rsi) %>% window(c(2005,1)),
                rsi_rate_SA = percentChange(rsi_sa) %>% window(c(2005,1))) %>% 
  data.frame() %>%
  ts(c(2005,1), c(2019,12),12)
autoplot(rsi_df, facets=TRUE)

# Population
ppn <- read.csv("population.csv")
ppn <- ts(ppn[,2], 1950, 2019, 1)
ppn_monthly <- (rep(ppn, each=12) %>% ts(c(1950,1), c(2019,12), 12)) / 12
ppn_monthly_SA <- ppn_monthly
ppn_monthly_growth <- (diff(log(ppn)*100,1) %>% rep(each=12) %>% ts(c(1951,1), c(2019,12), 12)) / 12
ppn_monthly_growth_SA <- ppn_monthly_growth
ppn_df <- cbind(population = window(ppn_monthly,c(2005,1)),
                population_SA = window(ppn_monthly_SA,c(2005,1)),
                population_growth = window(ppn_monthly_growth, c(2005,1)),
                population_growth_SA = window(ppn_monthly_growth_SA, c(2005,1))) %>% 
  data.frame() %>%
  ts(c(2005,1), c(2019,12),12)

autoplot(ppn_df, facets=TRUE)

# tourism
tourism <- read.csv("tourism.csv")
tourism <- ts(tourism[,2], c(1978,1), c(2020,2), 12)
tourism_SA <- stl(tourism, s.window=13) %>% seasadj()
tourism_growth <- percentChange(tourism)
tourism_growth_SA <- stl(tourism_growth, s.window=13) %>% seasadj()
tourism_df <- cbind(tourism = window(tourism,c(2005,1)),
                tourism_SA = window(tourism_SA,c(2005,1)),
                tourism_growth = window(tourism_growth, c(2005,1)),
                tourism_growth_SA = window(tourism_growth_SA, c(2005,1))) %>% 
  data.frame() %>%
  ts(c(2005,1), c(2019,12),12)
autoplot(tourism_df, facets=TRUE)

# Employment
changes_employment <- read.csv("employment_changes.csv")
services <- ts(changes_employment[,3], c(2004,1), c(2019,4),4)
services_monthly <- (rep(services, each=3) %>% ts(c(2004,1), c(2019,12),12)) / 3
services_monthly_SA <- (stl(services, s.window=13) %>% seasadj() %>% rep(each=3) %>% ts(c(2004,1), c(2019,12), 12)) / 3
changes_employment_rate <- read.csv("employment_changes_rate.csv")
services_rate <- ((changes_employment_rate[,2] - changes_employment_rate[,4]) %>% ts(c(2006,1), c(2019,4),4)) / 3 
services_monthly_rate <- (rep(services_rate, each=3) %>% ts(c(2006,1), c(2019,12), 12)) / 3
services_monthly_rate_SA <- (stl(services_rate, s.window=13) %>% seasadj() %>% rep(each=3) %>% ts(c(2006,1), c(2019,12),12)) / 3
services_df <- cbind(services_changes = window(services_monthly,c(2006,1)),
                    services_changes_SA = window(services_monthly_SA,c(2006,1)), 
                    services_rate = window(services_monthly_rate, c(2006,1)),
                    services_rate_SA = window(services_monthly_rate_SA, c(2006,1))) %>% 
  data.frame() %>%
  ts(c(2006,1), c(2019,12),12)
autoplot(services_df, facets=TRUE)

food <- ts(changes_employment[,5], c(2004,1), c(2019,4),4)
food_monthly <- (rep(food, each=3) %>% ts(c(2004,1), c(2019,12),12)) /3 
food_monthly_SA <- (stl(food, s.window=13) %>% seasadj() %>% rep(each=3) %>% ts(c(2004,1), c(2019,12), 12)) / 3
changes_employment_rate <- read.csv("employment_changes_rate.csv")
food_rate <- ((changes_employment_rate[,3] - changes_employment_rate[,5]) %>% ts(c(2006,1), c(2019,4),4)) / 3 
food_monthly_rate <- (rep(food_rate, each=3) %>% ts(c(2006,1), c(2019,12), 12)) / 3
food_monthly_rate_SA <- (stl(food_rate, s.window=13) %>% seasadj() %>% rep(each=3) %>% ts(c(2006,1), c(2019,12),12)) / 3
food_df <- cbind(food_changes = window(food_monthly,c(2006,1)),
                     food_changes_SA = window(food_monthly_SA,c(2006,1)), 
                     food_rate = window(food_monthly_rate, c(2006,1)),
                     food_rate_SA = window(food_monthly_rate_SA, c(2006,1))) %>% 
  data.frame() %>%
  ts(c(2006,1), c(2019,12),12)
autoplot(food_df, facets=TRUE)

write.csv(fnb_df, "fnb_df.csv")
write.csv(rsi_df, "rsi_df.csv")
write.csv(ppn_df, "ppn_df.csv")
write.csv(tourism_df, "tourism_df.csv")
write.csv(food_df, "food_df.csv")
write.csv(services_df, "services_df.csv")


## Make dataframes
start <- c(2006,1)
end <- c(2019,12)

# Consider actual figures
ppn_monthly <- window(ppn, 2006) %>% rep(each=12) %>% ts(c(2006,1), c(2019,12), 12)
tourism <- ts(tourism[,2], c(1978,1), c(2020,2), 12) %>% window(c(2006,1), c(2019,12))
food <- ts(food[,4], c(2006,1), c(2019,4), 4)%>% rep(each=3) %>% ts(c(2006,1), c(2019,12),12)
rsi2 <- ts(rsi2[,2], c(1985,1), c(2020,2),12) %>% window(c(2006,1), c(2019,12))


# Consider SA - actual figures
rsi <- ts(rsi[,2], c(1985,1), c(2020,2),12) %>% window(c(2006,1), c(2019,12))


# Consider rates
ppn_growth <- diff(log(ppn)*100,1) %>% window(2006) %>% rep(each=12) %>% ts(c(2006,1), c(2019,12),12)
tourism_growth <- diff(log(tourism)*100,1)
food <- ts(food[,4], c(2006,1), c(2019,4), 4)%>% rep(each=3) %>% ts(c(2006,1), c(2019,12),12)


# Consider SA - rates


## Modelling
df <- cbind(fnb[,1], ppn_growth, tourism_growth, food, changes, rsi2, rsi) %>% data.frame()
colnames(df) <- c("fnb", "ppn_growth", "tourism_growth", "fnb_employment_rate", "fnb_employment_changes", "rsi", "rsi_sa")
df_ts <- ts(df, c(2006,1), c(2019,12), 12)
df_train <- window(df_ts, c(2006,2), c(2016,12))
df_test <- window(df_ts, c(2017,1), c(2019,12))

xreg1 = cbind(ppn_growth = df_train[,2], tourism_growth = df_train[,3], fnb_employment_rate = df_train[,4], rsi = df_train[,6])
fit1 <- auto.arima(df_train[,1], xreg = xreg1)
checkresiduals(fit1)
fcast1 <- forecast(fit1, h=length(df_test), xreg=cbind(ppn_growth = df_test[,2], tourism_growth = df_test[,3], fnb_employment_rate = df_test[,4], rsi = df_test[,6]), na.rm=TRUE)
accuracy(fcast1, df_test[,1])
autoplot(df_ts[,1]) +
  forecast::autolayer(fcast1$mean, series = "Fitted") 

xreg2 = cbind(ppn_growth = df_train[,2], tourism_growth = df_train[,3], fnb_employment_rate = df_train[,4], rsi_sa = df_train[,7])
fit2 <- auto.arima(df_train[,1], xreg=cbind(ppn_growth = df_train[,2], tourism_growth = df_train[,3], fnb_employment_rate = df_train[,4], rsi_sa = df_train[,7]))
checkresiduals(fit2)
fcast2 <- forecast(fit2, h=length(df_test), xreg=cbind(ppn_growth = df_test[,2], tourism_growth = df_test[,3], fnb_employment_rate = df_test[,4], rsi_sa = df_test[,7]), na.rm=TRUE)
accuracy(fcast2, df_test[,1])
autoplot(df_ts[,1]) +
  forecast::autolayer(fcast2$mean, series = "Fitted") 

xreg3 = cbind(ppn_growth = df_train[,2], tourism_growth = stl(df_train[,3], s.window="periodic") %>% seasadj(), fnb_employment_rate = stl(df_train[,4], s.window="periodic") %>% seasadj(), rsi = df_train[,6])
fit3 <- auto.arima(df_train[,1], xreg=xreg3, allowdrift=TRUE)
checkresiduals(fit3)              
fcast3 <- forecast(fit3, h=length(df_test), xreg=cbind(ppn_growth = df_test[,2], tourism_growth = stl(df_test[,3], s.window="periodic") %>% seasadj(), fnb_employment_rate = stl(df_test[,4],s.window="periodic") %>% seasadj(), rsi = df_test[,6]))
accuracy(fcast3, df_test[,1])
autoplot(df_ts[,1]) +
  forecast::autolayer(fcast3$mean, series = "Fitted")


