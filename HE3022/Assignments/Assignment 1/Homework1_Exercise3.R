install.packages("readxl")
library(readxl)

#Load data
retaildata <- readxl::read_excel("retail.xlsx",skip=1)
#skip=1 because the Excel sheet has two header rows

#Select a time series from one of the columns - A3349873A
myts <- ts(retaildata[,"A3349873A"],
           frequency=12, start=c(1982,4))

#Construct plots
autoplot(myts)
ggseasonplot(myts)
ggsubseriesplot(myts)
ggAcf(myts)
