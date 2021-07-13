library(fpp2)
library(readxl)
library(seasonal)
fnb19852020 <- read_excel("chained fnb.xlsx")
fnbts<-ts(fnb19852020[,2:6],start=c(1985,1),frequency=12)
autoplot(fnbts,facets=TRUE)
#Form training set
fnbtraining<-window(fnbts[,1],start=c(2005,1),end=c(2016,12))

##DECOMPOSITION##
#X11
fit2<-seas(fnbtraining,x11="")
autoplot(seasadj(fit2))
#SEATS (No BoxCox)
fit4<-seas(fnbtraining)
autoplot(seasadj(fit4))
#SEATS (W/ BoxCox)
lambda <- BoxCox.lambda(fnbtraining)
plot(BoxCox(fnbtraining,lambda))
fit3<-seas(BoxCox(fnbtraining,lambda))
autoplot(seasadj(fit3))

##EXPONENTIAL SMOOTHING##
fit5<-hw(fnbtraining,seasonal="additive")
fit6<-hw(fnbtraining,seasonal="multiplicative")
autoplot(fnbtraining)+
  forecast::autolayer(fit5$mean,series="HW additive")+
  forecast::autolayer(fit6$mean,series="HW multiplicative")+
  xlab("Year")+ylab("Index")+guides(colour=guide_legend(title="Forecast"))
accuracy(fit5) 
#MAE = 1.74
accuracy(fit6)
#MAE = 1.86

##ETS##
fit7<-ets(fnbtraining)
summary(fit7)
#ETS(M,N,A)
#MAE = 1.72
fit7%>%forecast(h=24)%>%autoplot()