#STARTING#
library(fpp2)
library(readxl)
library(seasonal)
library(tseries)
library(tfplot)
FSI_Chained <- read_excel("FSI Chained.xlsx")
View(FSI_Chained)
Population <- read_excel("Population.xlsx")
View(Population)
Changes_in_Employment <- read_excel("Changes in Employment.xlsx")
View(Changes_in_Employment)
Visitors <- read_excel("Visitors.xlsx")
View(Visitors)
#NAMING DATA#
#F&B Index#
FSI<-ts(FSI_Chained[,2:6],start=c(1985,1),frequency=12)
View(FSI)
fsi<-window(FSI[,1],start=c(2005,1))
fsirestaurants<-window(FSI[,2],start=c(2005,1))
fsitest<-window(fsi,end=c(2017))
fsirestaurantstest<-window(fsirestaurants,end=c(2017))
#Population Growth#
pop<-ts(Population[,2:3],start=1950,frequency=1)
View(pop)
popgrowth<-window(pop[,2],start=2005)
popgrowthtest<-window(pop[,2],start=2005,end=2017)
#Changes in Employment#
employ<-ts(Changes_in_Employment[,2:5],start=c(2004,1),frequency=4)
View(employ)
foodemploy<-window(employ[,4],start=c(2005,1))
servicesemploy<-window(employ[,2],start=c(2005,1))
overallemploy<-window(employ[,1],start=c(2005,1))
foodemploytest<-window(employ[,4],start=c(2005,1),end=(2017))
servicesemploytest<-window(employ[,2],start=c(2005,1),end=(2017))
overallemploytest<-window(employ[,1],start=c(2005,1),end=(2017))
#Tourism - Visitor Arrivals#
visitors<-ts(Visitors[,2],start=c(1978,1),frequency=12)
visitor<-window(visitors[,1],start=c(2005,1))
visitorrate<-percentChange(visitor)
visitorratetest<-window(visitorrate,end=(2017))

#TEST SET#
#144 Observations each
popgrowthtest2<-popgrowthtest/12
popgrowthtest2<-rep(popgrowthtest,each=12)
popgrowthtest2<-ts(popgrowthtest2,start=c(2005,2),end=c(2017,1),frequency=12)

servicesemploytest2<-servicesemploytest/3
servicesemploytest2<-rep(servicesemploytest2,each=4)
servicesemploytest2<-ts(servicesemploytest2,start=c(2005,2),end=c(2017,1),frequency=12)

overallemploytest2 <-overallemploytest/3
overallemploytest2<-rep(overallemploytest2,each=4)
overallemploytest2<-ts(overallemploytest2,start=c(2005,2),end=c(2017,1),frequency=12)

foodemploytest2<-foodemploytest/3
foodemploytest2<-rep(foodemploytest2,each=4)
foodemploytest2<-ts(foodemploytest2,start=c(2005,2),end=c(2017,1),frequency=12)

fsitest2<-window(fsitest,start=c(2005,2),end=c(2017,1))

fsirestuarantstest2<-window(fsirestaurantstest,start=c(2005,2),end=c(2017,1))

visitortest<-window(visitor,start=c(2005,2),end=c(2017,1))

poptest<-window(pop[,1],start=2005,end=2017)
poptest2<-rep(poptest,each=12)
poptest2<-ts(poptest2,start=c(2005,2),end=c(2017,1),frequency=12)

#REGRESSION MODELS#
fit1<-auto.arima(fsitest2,xreg=cbind(popgrowthtest2,foodemploytest2,visitorratetest))
summary(fit1)
fit2<-auto.arima(fsitest2,xreg=cbind(popgrowthtest2,servicesemploytest2,visitorratetest))
summary(fit2)
fit3<-auto.arima(fsitest2,xreg=cbind(popgrowthtest2,overallemploytest2,visitorratetest))
summary(fit3)

fit4<-auto.arima(fsirestuarantstest2,xreg=cbind(popgrowthtest2,foodemploytest2,visitorratetest))
summary(fit4)
fit5<-auto.arima(fsirestuarantstest2,xreg=cbind(popgrowthtest2,servicesemploytest2,visitorratetest))
summary(fit5)
fit6<-auto.arima(fsirestuarantstest2,xreg=cbind(popgrowthtest2,overallemploytest2,visitorratetest))
summary(fit6)

fit7<-auto.arima(fsitest2,xreg=cbind(popgrowthtest2,foodemploytest2,visitortest))
summary(fit7)
fit8<-auto.arima(fsitest2,xreg=cbind(popgrowthtest2,servicesemploytest2,visitortest))
summary(fit8)
fit9<-auto.arima(fsitest2,xreg=cbind(popgrowthtest2,overallemploytest2,visitortest))
summary(fit9)

fit10<-auto.arima(fsitest2,xreg=cbind(poptest2,foodemploytest2,visitorratetest))
summary(fit10)
fit11<-auto.arima(fsitest2,xreg=cbind(poptest2,servicesemploytest2,visitorratetest))
summary(fit11)
fit12<-auto.arima(fsitest2,xreg=cbind(poptest2,overallemploytest2,visitorratetest))
summary(fit12)

fit13<-auto.arima(fsitest2,xreg=cbind(poptest2,foodemploytest2,visitortest))
summary(fit13)
fit14<-auto.arima(fsitest2,xreg=cbind(poptest2,servicesemploytest2,visitortest))
summary(fit14)
fit15<-auto.arima(fsitest2,xreg=cbind(poptest2,overallemploytest2,visitortest))
summary(fit15)

#Testing with Seasonally Adjusted Data#
#popgrowthtest2, servicesemploytest2, visitortest
stl(popgrowthtest2,s.window="periodic")%>%autoplot()+ggtitle("STL, Periodic, Pop Growth")
stl(popgrowthtest2,s.window="periodic")%>%seasadj()%>%autoplot()+ggtitle("STL, Periodic, Pop Growth, SAdj")
SApopgrowthtest2<-stl(popgrowthtest2,s.window="periodic")%>%seasadj()

stl(servicesemploytest2,s.window=4)%>%autoplot()+ggtitle("STL, 4, Employment Changes (Services)")
stl(servicesemploytest2,s.window=4)%>%seasadj()%>%autoplot()+ggtitle("STL, 4, Employment Changes (Services), SAdj")
SAservicesemploytest2<-stl(servicesemploytest2,s.window=4)%>%seasadj()

stl(visitortest,s.window=2)%>%autoplot()+ggtitle("STL, 2, Visitor #")
stl(visitortest,s.window=2)%>%seasadj()%>%autoplot()+ggtitle("STL, 2, Visitor #")
SAvisitortest<-stl(visitortest,s.window=2)%>%seasadj()

stl(visitorratetest,s.window="periodic")%>%autoplot()+ggtitle("STL, 2, Visitor Rate")
stl(visitortest,s.window="periodic")%>%seasadj()%>%autoplot()+ggtitle("STL, 2, Visitor Rate")
SAvisitorratetest<-stl(visitorratetest,s.window="periodic")%>%seasadj()

fit8SA<-auto.arima(fsitest2,xreg=cbind(SApopgrowthtest2,SAservicesemploytest2,SAvisitortest))
summary(fit8SA)
fit8SAvisitor<-auto.arima(fsitest2,xreg=cbind(popgrowthtest2,servicesemploytest2,SAvisitortest))
summary(fit8SAvisitor)
fit8SAemploy<-auto.arima(fsitest2,xreg=cbind(popgrowthtest2,SAservicesemploytest2,visitortest))
summary(fit8SAemploy)
fit8SApop<-auto.arima(fsitest2,xreg=cbind(SApopgrowthtest2,servicesemploytest2,visitortest))
summary(fit8SApop)
fit8SAemployvisitor<-auto.arima(fsitest2,xreg=cbind(popgrowthtest2,SAservicesemploytest2,SAvisitortest))
summary(fit8SAemployvisitor)

#Trying diff#
SAvisitortest2<-stl(visitortest,s.window="periodic")%>%seasadj()
fit8SA2<-auto.arima(fsitest2,xreg=cbind(SApopgrowthtest2,SAservicesemploytest2,SAvisitortest2))
summary(fit8SA2)
fit8SAvisitor2<-auto.arima(fsitest2,xreg=cbind(popgrowthtest2,servicesemploytest2,SAvisitortest2))
summary(fit8SAvisitor2)
fit8SAvisitorEmployment<-auto.arima(fsitest2,xreg=cbind(popgrowthtest2,SAservicesemploytest2,SAvisitortest2))
summary(fit8SAvisitorEmployment)

#Moving on
Employment SA, Pop growth, Visitor
#Add in RSI#
RSI <- read_excel("RSI.xlsx")
View(RSI)
RSI<-ts(RSI[,2],start=c(1985,1),frequency=12)
RSIts<-window(RSI[,1],start=c(2005,2),end=c(2017,1))
#Employment SA, Pop growth, Visitor, RSI
fit16<-auto.arima(fsitest2,xreg=cbind(popgrowthtest2,SAservicesemploytest2,visitortest,rsits))
summary(fit16)
checkresiduals(fit16)
#Add in Non-SA RSI#
RSI2 <- read_excel("RSI2.xlsx")
RSI2<-ts(RSI2[,2],start=c(1985,1),frequency=12)
RSIts2<-window(RSI2[,1],start=c(2005,2),end=c(2017,1))
#Employment SA, Pop growth, Visitor, RSI (Non-SA)
fit17<-auto.arima(fsitest2,xreg=cbind(popgrowthtest2,SAservicesemploytest2,visitortest,rsits2))
summary(fit17)
checkresiduals(fit17)

#Employment SA, Pop growth, SA Visitor Rate, RSI (Non-SA)
fit18<-auto.arima(fsitest2,xreg=cbind(popgrowthtest2,SAservicesemploytest2,SAvisitorratetest,rsits2))
summary(fit18)
#Employment SA, Pop growth, SA Visitor Rate, RSI (SA)
fit19<-auto.arima(fsitest2,xreg=cbind(popgrowthtest2,SAservicesemploytest2,SAvisitorratetest,rsits))
summary(fit19)

#Employment SA (Food + Services), Pop growth, SA Visitor Rate, RSI (Non-SA)
stl(foodemploytest2,s.window="periodic")%>%autoplot()+ggtitle("STL, 4, Employment Changes (Food)")
stl(foodemploytest2,s.window="periodic")%>%seasadj()%>%autoplot()+ggtitle("STL, 4, Employment Changes (Food), SAdj")
SAfoodemploytest2<-stl(foodemploytest2,s.window="periodic")%>%seasadj()
fit20<-auto.arima(fsitest2,xreg=cbind(popgrowthtest2,SAservicesemploytest2,SAfoodemploytest2,SAvisitorratetest,rsits2))
summary(fit20)

#RSI (Non-SA)
fit21<-auto.arima(fsitest2,xreg=rsits2)
summary(fit21)

##TRYING WITH NET CHANGE DATA##
tester <- read_excel("Recruitment-Resignation.xlsx") %>% ts(c(2006,1), c(2019,4), 4)
netchangefnb<-window(tester[,2])
netchangefnbtest<-netchangefnb/3
netchangefnbtest<-rep(netchangefnbtest,each=12)
netchangefnbtest2<-ts(netchangefnbtest,,start=c(2006,1),end=c(2017,1),frequency=12)
popgrowthtest3<-ts(popgrowthtest2,start=c(2006,1),end=c(2017,1),frequency=12)
visitorratetest2<-window(visitorratetest,,start=c(2006,1),end=c(2017,1))
fsitest3<-window(fsi,start=c(2006,1),end=c(2017,1))

fitnetchange<-auto.arima(fsitest3,xreg=cbind(popgrowthtest3,netchangefnbtest2,visitorratetest2))
summary(fitnetchange)
checkresiduals(fitnetchange)

SApopgrowthtest3<-stl(popgrowthtest3,s.window="periodic")%>%seasadj()
SAvisitorratetest2<-stl(visitorratetest2,s.window="periodic")%>%seasadj()
fitnetchange2<-auto.arima(fsitest3,xreg=cbind(SApopgrowthtest3,netchangefnbtest2,SAvisitorratetest2))
summary(fitnetchange2)

rsitsnonsa<-window(rsi2[,1],start=c(2006,1),end=c(2017,1))
fitnetchange3<-auto.arima(fsitest3,xreg=cbind(SApopgrowthtest3,netchangefnbtest2,SAvisitorratetest2,rsitsnonsa))
summary(fitnetchange3)

netchangeservices<-window(tester[,1])
netchangeservicestest<-netchangeservices/3
netchangeservicestest<-rep(netchangeservicestest,each=12)
netchangeservicestest2<-ts(netchangeservicestest,,start=c(2006,1),end=c(2017,1),frequency=12)
fitnetchange4<-auto.arima(fsitest3,xreg=cbind(SApopgrowthtest3,netchangeservicestest2,SAvisitorratetest2,rsitsnonsa))
summary(fitnetchange4)
rsitssa<-window(rsi[,1],start=c(2006,1),end=c(2017,1))
fitnetchange5<-auto.arima(fsitest3,xreg=cbind(SApopgrowthtest3,netchangeservicestest2,SAvisitorratetest2,rsitssa))
summary(fitnetchange5)
fitnetchange6<-auto.arima(fsitest3,xreg=cbind(SApopgrowthtest3,netchangefnbtest2,SAvisitorratetest2,rsitssa))
summary(fitnetchange6)

fitnetchange7 <- Arima(fsitest3, xreg=cbind(SApopgrowthtest3,netchangeservicestest2,SAvisitorratetest2,rsitssa), order=c(2,1,0),seasonal=c(0,0,2))
summary(fitnetchange7)
fitnetchange8 <- Arima(fsitest3, xreg=cbind(SApopgrowthtest3,netchangefnbtest2,SAvisitorratetest2,rsitssa), order=c(2,1,0),seasonal=c(0,0,2))
summary(fitnetchange8)
checkresiduals(fitnetchange8)

###FORECASTING###
popgrowthtestset<-window(pop[,2],start=2017,end=2020)
poptestset2<-rep(popgrowthtestset,each=12)
poptestset2<-ts(poptestset2,start=c(2017,1),end=c(2020,1),frequency=12)

visitorratetestset<-window(visitorrate,start=2017,end=2020)
SAvisitorratetestset<-stl(visitorratetestset,s.window="periodic")%>%seasadj()

netchangefnbtestset<-netchangefnb/3
netchangefnbtestset<-rep(netchangefnbtestset,each=12)
netchangefnbtestset2<-ts(netchangefnbtestset,start=c(2017,1),end=c(2020),frequency=12)

rsitestset<-window(rsi2[,1],start=c(2017,1),end=c(2020))

fsitestset<-window(fsitestset,start=2017)

servicesemploytestset<-window(employ[,2],start=c(2017,1),end=2020)
servicesemploytestset2<-servicesemploytestset/3
servicesemploytestset2<-rep(servicesemploytestset2,each=4)
servicesemploytestset2<-ts(servicesemploytestset2,start=c(2017,1),end=c(2020),frequency=12)
SAservicesemploytestset2<-stl(servicesemploytestset2,s.window=4)%>%seasadj()

fcast1 <- forecast(fit18,h=length(fsitestset),xreg=cbind(SApoptestset2,SAservicesemploytestset2,SAvisitorratetestset,rsitestset))
fcast2 <- forecast(fitnetchange3,h=length(fsitestset),xreg=cbind(SApoptestset2,netchangefnbtestset2,SAvisitorratetestset,rsitestset))
autoplot(fsi)+ forecast::autolayer(fcast1$mean, series = "Fitted")
autoplot(fsi)+ forecast::autolayer(fcast2$mean, series = "Fitted")
autoplot(fsi)+ forecast::autolayer(fcast1$mean, series = "fit18")+forecast::autolayer(fcast2$mean, series = "fitnetchange3")