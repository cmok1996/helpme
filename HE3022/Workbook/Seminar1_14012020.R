setwd("C:/Users/cmok1/Desktop/Course material/Y3S2/HE3022/Workbook")
install.packages("fpp2")
library(fpp2)
tute1 <- read.csv("C:/Users/cmok1/Desktop/Course material/Y3S2/HE3022/Workbook/tute1.csv")
View(tute1)
head(tute1, n=10)
tute1[1:10,2] <- 0
head(tute1, n=10)
View(tute1)
tute1 <- read.csv("tute1.csv")
tute1 <- ts(tute1[,-1], start = 1981, frequency = 4) #remove first column
head(tute1)
plot(tute1)
plot(tute1[,"Sales"],xlab = "time",
       +      ylab = "Thousands")
library(fpp2)
S1 <- tute1[,"Sales"]
autoplot(S1) +
  + ggtitle("Sales") +
  + xlab("Year") + ylab("Thousands")
plot(Sales ~ AdBudget, data=tute1) #Sales on X-axis, to see correlation
plot(Sales ~ AdBudget, data=tute1) #Sales on y-axis, to see correlation
plot(Sales ~ GDP, data=tute1)
pairs(as.data.frame(tute1))
#Different combinations of correlations of the objects in tute1
#pair-wise plot
library(fpp2)
summary(tute1)
hist(tute1[,"Sales"])
hist(tute1[,"AdBudget"])
cor.test(tute1[,"Sales"],tute1[,"AdBudget"]) #Pearson product moment correlation
