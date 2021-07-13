#Load libraries
library(fpp2)
library(tseries)
library(dplyr)
library(GGally)
library(seasonal)
library(readxl)
library(tfplot)

### Load data

FSI_Current <- read.csv("fsi.csv", header=TRUE)
FSI<-ts(FSI_Current[,2:6],start=c(1985,1),frequency=12)
fsi<-window(FSI[,1],start=c(2005,1),end=c(2019,12))
fsitrain<-window(FSI[,1],start=c(2005,1),end=c(2016,12))
fsitest<-window(FSI[,1],start=c(2017,1),end=c(2019,12))

# RSI
rsi_current <- read.csv("rsi_current.csv", header=TRUE)
rsi_current <- ts(rsi_current[,2], c(1985,1), c(2020,2),12)
rsi_chained <- read.csv("rsi_chained.csv")
rsi_chained <- ts(rsi_chained[,2], c(1985,1), c(2020,2),12)

# tourism
tourism <- read.csv("tourism.csv", header=TRUE)
tourism <- ts(tourism[,2], c(1978,1), c(2020,2), 12)
tourism_index <- tourism / tourism[445] * 100
tourism_scale <- tourism / 10000

# Employment
employment_changes <- read.csv("employment_changes.csv", header=TRUE)
overall_employment <- ts(employment_changes[,2], c(1991,1), c(2019,4),4)
overall_employment_monthly <- (rep(overall_employment, each=3) %>% ts(c(1991,1), c(2019,12),12)) /3 
food_accom_employment <- ts(employment_changes[,3], c(1991,1), c(2019,4),4)
food_accom_employment_monthly <- (rep(food_accom_employment, each=3) %>% ts(c(1991,1), c(2019,12),12)) /3 

# CPI - excluding accomodation
cpi <- read.csv("cpi.csv", header=TRUE)
cpi <- ts(cpi[,2], c(2000,1), c(2020,3),12)

# ULC - food & accomodation services
ulc <- read.csv("ulc.csv", header=TRUE)
ulc <- ts(ulc[,2], c(1980,1), c(2019,4), 4)
ulc_monthly <- rep(ulc, each=3) %>% ts(c(1980,1), c(2019,12),12)

# IPI - overall
ipi <- read.csv("ipi.csv", header=TRUE)
ipi <- ts(ipi[,2], c(1983,1), c(2020,3),12)

### Dataframe
df <- cbind("fsi" = fsi,
            "rsi_chained" = window(rsi_chained, c(2005,1), c(2019,12)),
            "tourism" = window(tourism_scale, c(2005,1), c(2019,12)),
            "cpi" = window(cpi, c(2005,1), c(2019,12)),
            "ipi" = window(ipi, c(2005,1), c(2019,12)),
            "ulc" = window(ulc_monthly, c(2005,1), c(2019,12))) %>%
  data.frame() %>%
  ts(c(2005,1), c(2019,12), 12)

df_train <- window(df, c(2005,1), c(2016,12))
df_test <- window(df, c(2017,1))

autoplot(df_train, facets=TRUE) + ylab("Leveled series")


### Multiplie regression
df_train_diff <- diff(df_train,12) %>% diff(1) 
df_test_diff <- diff(df_test,1) %>% diff(1)
df_train %>% diff(12) %>% diff(1) %>% as.data.frame %>% ggpairs() #no issue with multicollinearity
autoplot(df_train_diff, facets=TRUE) + ylab("Differenced series")

#Form models

incl = c(0,1)
i=1

mat = array(dim=c(32,5))

for (a in incl) {
  for (b in incl) {
    for (c in incl) {
      for (d in incl) {
        for (e in incl){
          mat[i, ] = c(a,b,c,d,e)
          i = i + 1                          
        }
      }
    }
  }
}

mat

value_store <- array(dim=c(32,3)) 

#try multiple regression

for (i in 2:32) {
  x1_model_train <- df_train_diff[,2] * mat[i,1]
  x2_model_train <- df_train_diff[,3] * mat[i,2]
  x3_model_train <- df_train_diff[,4] * mat[i,3]
  x4_model_train <- df_train_diff[,5] * mat[i,4]
  x5_model_train <- df_train_diff[,6] * mat[i,5]
  #xregg_train <- cbind(x1_model_train, x2_model_train, x3_model_train, x4_model_train, x5_model_train, x6_model_train, x7_model_train, x8_model_train)
  #xregg_train <- xregg_train[,colSums(xregg_train) != 0]
  fit <- tslm(df_train_diff[,1] ~ x1_model_train + x2_model_train + x3_model_train + x4_model_train + x5_model_train)
  checkCV <- CV(fit)
  value_store[i,1] <- checkCV["AICc"]
  checkCV <- CV(fit)
  value_store[i,2] <- checkCV["CV"]
  value_store[i,3] <- checkCV["AdjR2"]
} 

df_check_multiple <- cbind(mat, value_store) %>% as.data.frame
colnames(df_check_multiple) = c("RSI Chained", "Tourism", "CPI", "IPI", "ULC", "AICc", "CV", "AdjR2")
write.csv(df_check_multiple, "df_multiple.csv")
df_check_multiple %>% arrange(AICc)  #Model with lowest AICC is model with all variables except ULC
fit12 <- tslm(fsi ~ rsi_chained + tourism + cpi + ipi, data=df_train_diff)
summary(fit12)
vif(fit12)
checkresiduals(fit12) #p-value < 0.05, spike at lag=1 and lag=12


### Dynamic regression
incl = c(0,1)
i=1

mat = array(dim=c(32,5))

for (a in incl) {
  for (b in incl) {
    for (c in incl) {
      for (d in incl) {
        for (e in incl){
            mat[i, ] = c(a,b,c,d,e)
            i = i + 1               
        }
      }
    }
  }
}

mat

value_store <- array(dim=c(32,2)) 

for (i in 2:32) {
  x1_model_train <- df_train[,2] * mat[i,1]
  x2_model_train <- df_train[,3] * mat[i,2]
  x3_model_train <- df_train[,4] * mat[i,3]
  x4_model_train <- df_train[,5] * mat[i,4]
  x5_model_train <- df_train[,6] * mat[i,5]
  xregg_train <- cbind(x1_model_train, x2_model_train, x3_model_train, x4_model_train, x5_model_train)
  xregg_train <- xregg_train[,colSums(xregg_train) != 0]
  fit <- auto.arima(df_train[,1], xreg = xregg_train, d=1, D=1)
  value_store[i,1] <- fit$aicc
  check <- summary(fit)
  value_store[i,2] <- check[,2] #Get rmse
}

df_check_dynamic <- cbind(mat, value_store) %>% as.data.frame
colnames(df_check_dynamic) = c("RSI Chained", "Tourism", "CPI", "IPI", "ULC", "AICc", "RMSE")
df_check_dynamic %>% arrange(AICc)  #Model with lowest AICC is model with all variables except ULC & IPI
fit13 <- auto.arima(df_train[,"fsi"], xreg = cbind("rsi" = df_train[,"rsi_chained"], "tourism" = df_train[,"tourism"], "cpi" = df_train[,"cpi"]), d=1, D=1)
summary(fit13)
cbind("Regression Errors" = residuals(fit13, type="regression"), 
      "ARIMA errors" = residuals(fit13, type="innovation")) %>% autoplot(facets=TRUE)
checkresiduals(fit13) #p-value = 0.02788
jarque.bera.test(residuals(fit13, type="innovation"))

# Forecast using dynamic regression
fcast_dynamic <- forecast(fit13, h=36, xreg = cbind("rsi" = df_test[,"rsi_chained"], "tourism" = df_test[,"tourism"], "cpi" = df_test[,"cpi"]), bootstrap=TRUE)
accuracy(fcast_dynamic, df_test[,"fsi"]) #rmse_forecast = 3.28
autoplot(df[,"fsi"], series="Actual") +
  forecast::autolayer(fcast_dynamic$mean, series="Fitted") +
  xlab("Year") + ylab("Index")
  
autoplot(df[,"fsi"], series="Actual") +
  forecast::autolayer(fcast_dynamic$mean, series="Fitted point forecast") +
  forecast::autolayer(fcast_dynamic, series="Fitted prediction intervals") +
xlab("Year") + ylab("Index")

df_check_dynamic
df_check_dynamic %>% arrange(AICc)

# Discussion
value_store <- array(dim=c(32,4)) 

for (i in 2:32) {
  x1_model_train <- df_train[,2] * mat[i,1]
  x2_model_train <- df_train[,3] * mat[i,2]
  x3_model_train <- df_train[,4] * mat[i,3]
  x4_model_train <- df_train[,5] * mat[i,4]
  x5_model_train <- df_train[,6] * mat[i,5]
  xregg_train <- cbind(x1_model_train, x2_model_train, x3_model_train, x4_model_train, x5_model_train)
  xregg_train <- xregg_train[,colSums(xregg_train) != 0]
  fit <- auto.arima(df_train[,1], xreg = xregg_train, d=1, D=0)
  value_store[i,1] <- fit$aicc
  check <- summary(fit)
  value_store[i,2] <- check[,2] #Get rmse
  value_store[i,3] <- checkresiduals(fit)$p.value #get p.value
  x1_model_test <- df_test[,2] * mat[i,1]
  x2_model_test <- df_test[,3] * mat[i,2]
  x3_model_test <- df_test[,4] * mat[i,3]
  x4_model_test <- df_test[,5] * mat[i,4]
  x5_model_test <- df_test[,6] * mat[i,5]
  xregg_test <- cbind(x1_model_test, x2_model_test, x3_model_test, x4_model_test, x5_model_test)
  xregg_test <- xregg_test[,colSums(xregg_test) != 0]
  fcast_test <- forecast(fit, h=36, xreg = xregg_test)
  check_test <- accuracy(fcast_test, df_test[,1])
  value_store[i,4] <- check_test[2,2] #get forecast rmse
}

df_check_full <- cbind(mat, value_store) %>% as.data.frame
colnames(df_check_full) = c("RSI Chained", "Tourism", "CPI","IPI", "ULC", "AICc", "RMSE", "pVal", "RMSE_forecast")
write.csv(df_check_full, "df_check_full.csv")
df_check_full %>% arrange(AICc) 
#Model with lowest RMSE_forecast is model with all variables except ULC

df_check_auto <- cbind(mat, value_store) %>% as.data.frame
colnames(df_check_auto) = c("RSI", "Tourism", "CPI","IPI", "ULC", "AICc", "RMSE", "pVal", "RMSE_forecast")
write.csv(df_check_auto, "df_check_auto.csv")
df_check_auto %>% arrange(AICc) 
#Model with lowest RMSE_forecast is model with all variables except ULC

fit_13 <- auto.arima(df_train[,"fsi"], xreg = cbind("rsi" = df_train[,"rsi_chained"], "tourism" = df_train[,"tourism"], "cpi" = df_train[,"cpi"]))
summary(fit_13)
checkresiduals(fit_13)
fcast_test <- forecast(fit_13, h=36, xreg = cbind("rsi" = df_test[,"rsi_chained"], "tourism" = df_test[,"tourism"], "cpi" = df_test[,"cpi"]))
accuracy(fcast_test, df_test[,"fsi"])


df_full <- cbind("fsi" = FSI %>% window(c(2005,1), c(2020,2)),
                 "rsi_chained" = rsi_chained %>% window(c(2005,1), c(2020,2)),
                 "tourism" = tourism_scale %>% window(c(2005,1), c(2020,2)),
                 "cpi" = cpi %>% window(c(2005,1), c(2020,2))) %>%
  data.frame() %>%
  ts(c(2005,1), c(2020,2), 12)

fit_full <- Arima(df_full[,1], xreg=cbind(df_full[,2], df_full[,3], df_full[,4]), order = c(0,1,1), seasonal = c(1,0,1))
summary(fit_full)
checkresiduals(fit_full)
autoplot(df_full[,1]) %>%
  forecast::autolayer(fit_full$fitted.values)
