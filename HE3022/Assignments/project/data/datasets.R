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

# FnB
fnb_quarterly <- read.csv("fsi_quarterly.csv", header=TRUE)
fnb_quarterly_ts <- ts(fnb_quarterly[,2], c(1985,1), c(2019,4), 4)
fnb_quarterly_SA <- stl(fnb_quarterly_ts, s.window=13) %>% seasadj() 
fnb_quarterly_growth <- percentChange(fnb_quarterly_ts)
fnb_quarterly_growth_SA <- percentChange(fnb_quarterly_ts) %>% stl(s.window=13) %>% seasadj()
fnb_quarterly_df <- cbind(fnb_quarterly = window(fnb_quarterly_ts,c(2005,1)),
                fnb_quarterly_SA = window(fnb_quarterly_SA, c(2005,1)), 
                fnb_quarterly_rate = window(fnb_quarterly_growth, c(2005,1)),
                fnb_quarterly_rate_SA = window(fnb_quarterly_growth_SA, c(2005,1))) %>% 
  data.frame() %>%
  ts(c(2005,1), c(2019,12),4)
autoplot(fnb_quarterly_df, facets=TRUE)

# FnB
fnb_current <- read.csv("fnb_current.csv", header=TRUE)
fnb_current_ts <- ts(fnb_current[,2], c(1985,1), c(2020,1), 12)
fnb_current_SA <- stl(fnb_current_ts, s.window=13) %>% seasadj() 
fnb_current_growth <- percentChange(fnb_current_ts)
fnb_current_growth_SA <- percentChange(fnb_current_ts) %>% stl(s.window=13) %>% seasadj()
fnb_current_df <- cbind(fnb_current = window(fnb_current_ts,c(2005,1)),
                fnb_current_SA = window(fnb_current_SA, c(2005,1)), 
                fnb_current_rate = window(fnb_current_growth, c(2005,1)),
                fnb_current_rate_SA = window(fnb_current_growth_SA, c(2005,1))) %>% 
  data.frame() %>%
  ts(c(2005,1), c(2019,12),12)
autoplot(fnb_current_df, facets=TRUE)

# Restaurants
restaurants <- read.csv("fnb_index_chained.csv", header=TRUE)
restaurants_ts <- ts(fnb[,3], c(1985,1), c(2020,1), 12)
restaurants_SA <- stl(restaurants_ts, s.window=13) %>% seasadj() 
restaurants_growth <- percentChange(restaurants_ts)
restaurants_growth_SA <- percentChange(restaurants_ts) %>% stl(s.window=13) %>% seasadj()
restaurants_df <- cbind(restaurants = window(restaurants_ts,c(2005,1)),
                restaurants_SA = window(restaurants_SA, c(2005,1)), 
                restaurants_rate = window(restaurants_growth, c(2005,1)),
                restaurants_rate_SA = window(restaurants_growth_SA, c(2005,1))) %>% 
  data.frame() %>%
  ts(c(2005,1), c(2019,12),12)
autoplot(restaurants_df, facets=TRUE)

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

# clothes
clothes <- read.csv("clothes.csv")
clothes <- ts(clothes[,2], c(1985,1), c(2020,2),12)
clothes_sa <- clothes
clothes_df <- cbind(clothes = window(clothes,c(2005,1)),
                clothes_SA = window(clothes_sa, c(2005,1)), 
                clothes_rate = percentChange(clothes) %>% window(c(2005,1)),
                clothes_rate_SA = percentChange(clothes_sa) %>% window(c(2005,1))) %>% 
  data.frame() %>%
  ts(c(2005,1), c(2019,12),12)
autoplot(clothes_df, facets=TRUE)

# clothes_curent
clothes_current <- read.csv("clothes_current.csv")
clothes_current <- ts(clothes_current[,2], c(1985,1), c(2020,2),12)
clothes_current_sa <- clothes_current
clothes_current_df <- cbind(clothes_current = window(clothes_current,c(2005,1)),
                    clothes_current_SA = window(clothes_current_sa, c(2005,1)), 
                    clothes_current_rate = percentChange(clothes_current) %>% window(c(2005,1)),
                    clothes_current_rate_SA = percentChange(clothes_current_sa) %>% window(c(2005,1))) %>% 
  data.frame() %>%
  ts(c(2005,1), c(2019,12),12)
autoplot(clothes_current_df, facets=TRUE)

# RSI_current
rsi_current <- read.csv("rsi_current.csv")
rsi_current <- ts(rsi_current[,2], c(1985,1), c(2020,2),12)
rsi_current_sa <- read.csv("rsi_current_sa.csv") #seasadj
rsi_current_sa <- ts(rsi_current_sa[,2], c(1985,1), c(2020,2),12)
rsi_current_df <- cbind(rsi_current = window(rsi_current,c(2005,1)),
                rsi_current_SA = window(rsi_current_sa, c(2005,1)), 
                rsi_current_rate = percentChange(rsi_current) %>% window(c(2005,1)),
                rsi_current_rate_SA = percentChange(rsi_current_sa) %>% window(c(2005,1))) %>% 
  data.frame() %>%
  ts(c(2005,1), c(2019,12),12)
autoplot(rsi_current_df, facets=TRUE)

# RSI quarterly
rsi_quarterly <- read.csv("rsi_quarterly.csv")
rsi_quarterly <- ts(rsi_quarterly[,2], c(1985,1), c(2019,4),4)
rsi_quarterly_sa <- stl(rsi_quarterly, s.window=13) %>% seasadj()
rsi_quarterly_sa <- ts(rsi_quarterly_sa, c(1985,1), c(2019,4),4)
rsi_quarterly_df <- cbind(rsi_quarterly = window(rsi_quarterly,c(2005,1)),
                rsi_quarterly_SA = window(rsi_quarterly_sa, c(2005,1)), 
                rsi_quarterly_rate = percentChange(rsi_quarterly) %>% window(c(2005,1)),
                rsi_quarterly_rate_SA = percentChange(rsi_quarterly_sa) %>% window(c(2005,1))) %>% 
  data.frame() %>%
  ts(c(2005,1), c(2019,4),4)
autoplot(rsi_quarterly_df, facets=TRUE)

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

#Unit labour cost
ulc <- read.csv("ulc_food.csv", header=TRUE)
ulc <- ts(ulc[,2], c(1980,1), c(2019,4),4)
ulc_monthly <- rep(ulc, each=3) %>% ts(c(1980,1), c(2019,12),12) 
ulc_monthly_SA <- stl(ulc, s.window=13) %>% seasadj() %>% rep(each=3) %>% ts(c(1980,1), c(2019,12), 12)
ulc_monthly_rate <- percentChange(ulc) %>% rep(each=3) %>% ts(c(1980,1), c(2019,12),12)
ulc_monthly_rate_SA <- stl(percentChange(ulc), s.window=13) %>% seasadj() %>% rep(each=3) %>% ts(c(1980,1), c(2019,12), 12)
ulc_df <- cbind(ulc = window(ulc_monthly,c(2006,1)),
                 ulc_SA = window(ulc_monthly_SA,c(2006,1)), 
                 ulc_rate = window(ulc_monthly_rate, c(2006,1)),
                 ulc_rate_SA = window(ulc_monthly_rate_SA, c(2006,1))) %>% 
  data.frame() %>%
  ts(c(2006,1), c(2019,12),12)
autoplot(ulc_df, facets=TRUE)

# ipi
ipi <- read.csv("ipi.csv")
ipi <- ts(ipi[,2], c(1983,1), c(2020,3), 12)
ipi_SA <- stl(ipi, s.window=13) %>% seasadj()
ipi_growth <- percentChange(ipi)
ipi_growth_SA <- stl(ipi_growth, s.window=13) %>% seasadj()
ipi_df <- cbind(ipi = window(ipi,c(2005,1)),
                      ipi_SA = window(ipi_SA,c(2005,1)),
                      ipi_growth = window(ipi_growth, c(2005,1)),
                      ipi_growth_SA = window(ipi_growth_SA, c(2005,1))) %>% 
  data.frame() %>%
  ts(c(2005,1), c(2019,12),12)

# equipment
equipment <- read.csv("ipi.csv")
equipment <- ts(equipment[,3], c(1983,1), c(2020,3), 12)
equipment_SA <- stl(equipment, s.window=13) %>% seasadj()
equipment_growth <- percentChange(equipment)
equipment_growth_SA <- stl(equipment_growth, s.window=13) %>% seasadj()
equipment_df <- cbind(equipment = window(equipment,c(2005,1)),
                    equipment_SA = window(equipment_SA,c(2005,1)),
                    equipment_growth = window(equipment_growth, c(2005,1)),
                    equipment_growth_SA = window(equipment_growth_SA, c(2005,1))) %>% 
  data.frame() %>%
  ts(c(2005,1), c(2019,12),12)
autoplot(equipment_df, facets=TRUE)

# fnb_ipi
fnb_ipi <- read.csv("ipi.csv")
fnb_ipi <- ts(fnb_ipi[,2], c(1983,1), c(2020,3), 12)
fnb_ipi_SA <- stl(fnb_ipi, s.window=13) %>% seasadj()
fnb_ipi_growth <- percentChange(fnb_ipi)
fnb_ipi_growth_SA <- stl(fnb_ipi_growth, s.window=13) %>% seasadj()
fnb_ipi_df <- cbind(fnb_ipi = window(fnb_ipi,c(2005,1)),
                      fnb_ipi_SA = window(fnb_ipi_SA,c(2005,1)),
                      fnb_ipi_growth = window(fnb_ipi_growth, c(2005,1)),
                      fnb_ipi_growth_SA = window(fnb_ipi_growth_SA, c(2005,1))) %>% 
  data.frame() %>%
  ts(c(2005,1), c(2019,12),12)
autoplot(fnb_ipi_df, facets=TRUE)

write.csv(fnb_df, "fnb_df.csv")
write.csv(fnb_current_df, "fnb_current_df.csv")
write.csv(fnb_quarterly_df, "fnb_quarterly_df.csv")

write.csv(restaurants_df, "restaurants_df.csv")
write.csv(rsi_df, "rsi_df.csv")
write.csv(rsi_current_df, "rsi_current_df.csv")
write.csv(rsi_quarterly_df, "rsi_quarterly_df.csv")
write.csv(clothes_df, "clothes_df.csv")
write.csv(clothes_df, "clothes_current_df.csv")

write.csv(ppn_df, "ppn_df.csv")
write.csv(tourism_df, "tourism_df.csv")
write.csv(food_df, "food_df.csv")
write.csv(services_df, "services_df.csv")
write.csv(equipment_df, "equipment_df.csv")
write.csv(fnb_ipi_df, "fnb_ipi_df.csv")
write.csv(ipi_df, "ipi_df.csv")
write.csv(ulc_df, "ulc_df.csv")


## Make dataframes
#df1 <- cbind(fnb = fnb_df[,1], rsi = rsi_df[,1], population = ppn_df[,1], tourism = tourism_df[,1], employment = food[,1])

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


