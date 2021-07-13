library(fpp2)
daily20 <- head(elecdaily,20)
#Plot the data and find the regression model with temperature as an explanatory variable
autoplot(daily20, facets=TRUE)
autoplot(daily20, facets=TRUE)
daily20 %>%
  as.data.frame() %>% 
  ggplot(aes(x=Temperature, y=Demand)) +
  geom_point() +
  geom_smooth(method="lm", se=FALSE)
#I observe that there is a positive relationship in demand for electricity and temperature. This is likely becase more
#electrical appliances such as fans & air-conditioner are heavily used during periods of hot temperature. Likewise, usage of heater
#increases during period of cold temperature.

fit <- tslm(Demand ~ Temperature, data=daily20)
summary(fit)
#Produce a residual plot, is the model adequate? Are there any outliers?
checkresiduals(fit)
df <- as.data.frame(daily20)
df[,"residuals"] <- as.numeric(residuals(fit))
ggplot(df, aes(x=Temperature, y=residuals)) + geom_point() + geom_smooth(method="lm")
ggplot(df, aes(x=Demand, y=residuals)) + geom_point() + geom_smooth(method="lm")
boxplot(daily20[,"Demand"])
boxplot(daily20[,"Temperature"])
boxplot(daily20[,"Demand"])

#I think that the model is adequate since it seems that the residuals are not correlated with each other at 5% significance level as observed in the ACF plot.  However, the residuals
#However, the residuals are clearly not normally distributed so it provides evidence that there is an outlier in the model which affects the slope of the regression line
cbind(Data=daily20[,"Demand"], Fitted=fitted(fit)) %>%
  as.data.frame() %>% 
  ggplot(aes(x=Data, y=Fitted)) +
  geom_point() +
  xlab("Data actual values") +
  ylab("Fitted predicted values") +
  ggtitle("Electricity demand") +
  geom_abline(intercept=0, slope=1)


#Scenario-based forecasting
newdata <- data.frame(Temperature = c(15,35))
predicted <- forecast(fit, newdata=newdata)
predicted

#I would believe these forecasts as the demand values predicted by the model were close to the range of actual temperature in the data.
#For example, the predicted electricity demand for temperature = 35 (275.7) is similar to the actual demand for temp = 34 (258.6).
#If temperture = 15, 80% prediction interval for my forecast is (108.7, 172.5) & 95% prediction interval is (90.2, 190.9)
#If temperture = 35, 80% prediction interval for my forecast is (245.2, 306.2) & 95% prediction interval is (227.6, 323.9)

#Repeat using all of available data in elecdaily
elecdaily %>%
  as.data.frame() %>%
  ggplot(aes(x=Temperature, y=Demand)) +
  ylab("Electricity Demand") +
  xlab("Temperature") +
  geom_point() +
  geom_smooth(method="lm", se=FALSE)
#The result plot shows that the model as described above was based on a small sample size as compared to the total data. 
#Hence, the model could have explained the first 20 days well but the model was not right for the total data
#A non linear trend can be observed by studying the scatterplot of temperature and electricity demand with electricity demand decrasing at low temperatures and start to increase again after about 22 degrees.