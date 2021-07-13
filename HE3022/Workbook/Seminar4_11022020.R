library(fpp2)
uschange #quarterly data of US macroeconomic data
autoplot(uschange[,c("Consumption","Income")]) +
  ylab("% change") + xlab("Year")
#consumption as y variable, income as x variable
tslm(Consumption ~ Income, data=uschange) #timeseries linear model
uschange%>%
  as.data.frame() %>%
  ggplot(aes(x=Income, y=Consumption)) +
  ggtitle("Plot of consumption vs Income and the fitted regression line") +
  ylab("Consumption (quarterly % change)") +
  xlab("Income (quarterly % change)") +
  geom_point() + #give us the dots
  geom_smooth(method="lm", se=FALSE) #give us the linear regression line

autoplot(uschange[,c(2,3,4,5)], facets = TRUE) + #can type column number or column name
  ylab("% chage") + xlab("Year") #facets = true give us corresponding scale of axis
uschange %>%
  as.data.frame() %>%
  GGally::ggpairs() #can see that some of the predictor variables are highly correlated. eg, savings and income
#multicollinearity is when we have correlation among the predictor variables - may cause high variance in estimators
#in forecasting, it is less of an issue as we want to see how the predictor variable can forecast the dependent variable, only severe in making inferences

fit.consMR <- tslm(Consumption ~ Income + Production +
                     Unemployment + Savings, data=uschange) #fit an object after running a regression
summary(fit.consMR) #if p value is small (less than significance value), the predictor is statistifically significant
#adjust R2 for the number of parameters. If you keep adding predictor variables, R2 will keep increasing

#Use training set to get the model, then use the regression model for the test set
autoplot(uschange[,'Consumption'], series="Data") + #series provides legend
  forecast::autolayer(fitted(fit.consMR), series="Fitted") + #fitted extracts fitted values from objects returned by modelling functions
  xlab("Year") + ylab("") +
  ggtitle("Percentage change in US consumption expenditure") +
  guides(colour=guide_legend(title=" "))

cbind(Data=uschange[,"Consumption"], Fitted=fitted(fit.consMR)) %>%
  as.data.frame() %>% #store table of data as object
  ggplot(aes(x=Data, y=Fitted)) +
  geom_point() + #actual data points
  xlab("Fitted (predicted values)") +
  ylab("Data (actual values") +
  ggtitle("Percentage change in US Consumption expenditure") +
  geom_abline(intercept=0, slope=1) #45deg line, if good model then the points should be in the line

beer2 <- window(ausbeer, start=1992)
autoplot(beer2) + xlab("Year") + ylab("Megalitres")
#time trend + seasonality, omit first period as dummy variable trap
fit.beer <- tslm(beer2 ~ trend + season) #since data is quarterly, it gives us quarterly seasonality
summary(fit.beer) #coefficients of seasonality is with reference to season 1
autoplot(beer2, series="Data") +
  forecast::autolayer(fitted(fit.beer), series="Fitted") +
  xlab("Year") + ylab("Megalitres") +
  ggtitle("Quarterly Beer Production")
cbind(Data=beer2, Fitted=fitted(fit.beer)) %>%
  as.data.frame() %>%
  ggplot(aes(x=Data, y=Fitted, colour=as.factor(cycle(beer2)))) +
  geom_point() +
  ylab("Fitted") + xlab("Actual values") +
  ggtitle("Quarterly beer production") +
  scale_colour_brewer(palette="Dark2", name="Quarter") +
  geom_abline(intercept=0, slope=1)
#Use fourier series for high frequency data, weekly and above
#fourier series use fewer predictor variables than we need to with dummy variables

fourier.beer <- tslm(beer2 ~ trend + fourier(beer2, K=2)) #first object defines the seasonality, K specifies how many pairs of sin and cos = m/2, define the smoothness of the cycle
summary(fourier.beer) #larger K gives smoother cycle

#in addition to seasonality, can use dummy for intervention variables
#spike variable is one event that we want to capture in the model/ handling an outlier
#eg, strike. 1 if it happens, 0 otherwise
#step variable is for before and after
#piecewise linear trend is to indicate the change in slope

easter.beer <- tslm(beer2 ~ trend + easter(beer2)) #easter function will compute the dummy variable for me
summary(easter.beer)

checkresiduals(fit.beer) #p=0.317 suggest there is no autocorrelation. Histogram shows not normal distribution
df <- as.data.frame(uschange)
df[,"Residuals"] <- as.numeric(residuals(fit.consMR))
p1 <- ggplot(df, aes(x=Income, y=Residuals)) + geom_point()
p2 <- ggplot(df, aes(x=Production, y=Residuals)) + geom_point()
p3 <- ggplot(df, aes(x=Savings, y=Residuals)) + geom_point()
p4 <- ggplot(df, aes(x=Unemployment, y=Residuals)) + geom_point()
gridExtra::grid.arrange(p1, p2, p3, p4, nrow=2)

fit <- tslm(elec ~ trend + season)
cbind(Fitted=fitted(fit), Residuals=residuals(fit)) %>%
  as.data.frame() %>%
  ggplot(aes(x=Fitted, y=Residuals)) + geom_point()
#This plot shows nonlinear trend and heteroscedastic pattern with variation increasing along the x axis

#black line shows line without outlier
#outlier can be due to measurement error, or it can be due to some event
#if measurement error, can omit. But otherwise, must include.
#Good to plot 2 different lines, with and without outlier

fit.consMR <- tslm(Consumption ~ Income + Production + Unemployment + Savings,
                   data=uschange)
summary(fit.consMR)

#spurrious regression - obviously not related other than the time trend - due to non stationary
aussies <- window(ausair, end=2011)
fit <- tslm(aussies ~ guinearice)
summary(fit)

#Selecting predictors
CV(fit.consMR)
#16 different models by removing each predictor. Selecting the lowest AICc
