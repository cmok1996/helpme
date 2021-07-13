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

df19 <- cbind("fnb" = window(fnb_current[,2],c(2006,1)),
              "rsi" = window(rsi_current[,2],c(2006,1)),
              "tourism" = window(tourism[,2], c(2006,1),),
              "food_changes" = diffinv(food_employment[,2],1) %>% window(c(2006,1)),
              "ipi" = window(ipi[,2],c(2006,1)),
              "ulc_rate" = window(ulc[,2], c(2006,1))) %>% 
  data.frame() %>%
  ts(c(2006,1), c(2019,12), 12)

df19_train <- window(df19, c(2006,1), c(2016,12))
df19_test <- window(df19, c(2017,1))

value_store <- array(dim=c(32,4)) 

for (i in 2:32) {
  x1_model_train <- df19_train[,2] * mat[i,1]
  x2_model_train <- df19_train[,3] * mat[i,2]
  x3_model_train <- df19_train[,4] * mat[i,3]
  x4_model_train <- df19_train[,5] * mat[i,4]
  x5_model_train <- df19_train[,6] * mat[i,5]
  xregg_train <- cbind(x1_model_train, x2_model_train, x3_model_train, x4_model_train, x5_model_train)
  xregg_train <- xregg_train[,colSums(xregg_train) != 0]
  fit <- auto.arima(df19_train[,1], xreg = xregg_train, d=1, D=1)
  value_store[i,1] <- fit$aicc
  check <- summary(fit)
  value_store[i,2] <- check[,2] #Get rmse
  value_store[i,3] <- checkresiduals(fit)$p.value #get p.value
  x1_model_test <- df19_test[,2] * mat[i,1]
  x2_model_test <- df19_test[,3] * mat[i,2]
  x3_model_test <- df19_test[,4] * mat[i,3]
  x4_model_test <- df19_test[,5] * mat[i,4]
  x5_model_test <- df19_test[,6] * mat[i,5]
  xregg_test <- cbind(x1_model_test, x2_model_test, x3_model_test, x4_model_test, x5_model_test)
  xregg_test <- xregg_test[,colSums(xregg_test) != 0]
  fcast_test <- forecast(fit, h=36, xreg = xregg_test)
  check_test <- accuracy(fcast_test, df19_test[,1])
  value_store[i,4] <- check_test[2,2] #get forecast rmse
}

df_check <- cbind(mat, value_store) %>% as.data.frame
colnames(df_check) = c("RSI", "Tourism", "Food employment changes", "IPI", "ULC", "AICc", "RMSE", "pVal", "RMSE_forecast")
df_check



