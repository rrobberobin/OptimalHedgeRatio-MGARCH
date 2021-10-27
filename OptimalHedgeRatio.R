#OptimalHedgeRatio

rm(list=ls())
cat("\14")
library(readxl)

futuresURL = "Data/Weather.xlsx"
fName = names(read_excel(futuresURL, n_max=1))  #extract the name of the dataset
fData = read_excel(futuresURL, skip=2)

underlyingURL = "Data/eMini.xlsx"
uName = names(read_excel(underlyingURL, n_max=1))  #extract the name of the dataset
uData = read_excel(underlyingURL, skip=2)

merged = merge(uData, fData, by="Date")
omitted = na.omit(merged[,c(1,4,9)])  #choose only specific rows

library(xts)
plot(xts(omitted[,2:3],omitted$Date,yaxis.same=F, multi.panel=T))
plot(xts(omitted[,2],omitted$Date,yaxis.same=F, multi.panel=T))
plot(xts(omitted[,3],omitted$Date,yaxis.same=F, multi.panel=T))
change = lapply(as.vector(omitted[,2:3]), diff, lag=1)

#diff(as.matrix(omitted[,3],lag=1))
#uChange = diff(omitted$Price,lag=1)



#OLS
plot(unlist(omitted[2]), unlist(omitted[3]))
summary(lm(unlist(omitted[2]) ~ unlist(omitted[3])))

plot(unlist(change[1]), unlist(change[2]))
summary(lm(unlist(change[1]) ~ unlist(change[2])))


#Garch








