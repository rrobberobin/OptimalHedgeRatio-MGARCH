#OptimalHedgeRatio

rm(list=ls)
cat("\14")
library(readxl)

dataURL = "Data/eMini.xlsx"
data = read_excel(dataURL,skip=1)
withDates = cbind(as.Date(data$Date,"%Y-%m-%d"),data[,-1])
plot.ts(withDates[,2])
plot.ts(data[,2])

#OLS
lm(data)


#Garch

