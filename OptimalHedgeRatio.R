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
omitted = na.omit(merged[,c(1,2,9)])  #choose only specific rows

u = unlist(omitted[,2])
f = unlist(omitted[,3])
dates = omitted$Date

#Just looking at the price data, we can see that it is non-stationary.
#So we choose returns data instead.
change = lapply(as.vector(omitted[,2:3]), diff, lag=1)
du = unlist(change[1])
df = unlist(change[2])    #check these. Not returns, they are price changes
dDates = dates[-1]

library(xts)
plot(xts(u,dates))
plot(xts(f,dates))
plot(xts(du,dDates))
plot(xts(df,dDates))

summary(u)
summary(f)
summary(du)
summary(df)


#diff(as.matrix(omitted[,3],lag=1))
#uChange = diff(omitted$Price,lag=1)


#Test for normality, heteroskedasticity, autocorrelation
#Multicollinearity? Prob not
#No multicollinearity if we have only one explanatory variable


#Corrections for the problems

#Let's check the distributions. Do they look normal?
hist(u, breaks=50, main = "Histogram of underlying prices")
hist(f, breaks=50, main = "Histogram of futures prices")
hist(du, breaks=50, main = "Histogram of underlying returns")   #these are actually price changes, not returns
hist(df, breaks=50, main = "Histogram of futures returns")



#OLS
plot(u, f)
model = lm(u ~ f)
summary(model)

plot(du, df)
dModel = lm(du ~ df)
summary(dModel)

dFlipped = lm(df ~ du)
summary(dFlipped)

#OLS correlation estimate
rho = dModel$coefficients[2,1]

#OLS Hedge
h = rho * sd(df)/sd(du)
h




#Residuals
res = model$residuals
dRes = dModel$residuals

#Normality of residuals. Do they look normal?
hist(res, breaks=50, main = "Histogram of price residuals")
hist(dRes, breaks=50, main = "Histogram of return residuals")

#Test for normality
library(moments)
jarque.test(res)  #Definitely not normal
jarque.test(dRes) #Not normal


#Remedies for normality. (Big sample size CLT, simulating more observations, logarithmic transform)
plot(log(1+du), log(1+df))
logModel = lm(log(1+du) ~ log(1+df))   #Check that it's the right log transformation. We want the correlation
logRes = logModel$residuals
hist(logRes, breaks=50, main = "Histogram of logreturns residuals")
jarque.test(logRes)
summary(logModel)


#Autocorrelation
acf(u)
acf(f)

acf(du) #Some autocorrelation
acf(df) #No autocorrelation
acf(df) #No autocorrelation

pacf(du)
pacf(df)

#Breusch-Godfrey (autocorrelation)
library(lmtest)
bgtest(dModel,252)  #test of order 252 based on days

#Ljung-box (autocorrelation)
Box.test(du,lag=9,type="Ljung-Box")
Box.test(df,lag=5,type="Ljung-Box")

#Augmented Dickey-Fuller (unit root)
library(fUnitRoots)
adfTest(du, lags = 9, type = c("nc"), title = NULL, description = NULL)
adfTest(df, lags = 5, type = c("nc"), title = NULL, description = NULL)





#Heteroskedasticity
library(lmtest)

#Breusch-Pagan (check also with White?)
bptest(dModel)  #p-value = 0.9401.  Can't reject null. We have homoskedasticity



#Correct for heteroskedasticity and autocorrelation (Andrews)
library(sandwich)
robust = coeftest(dModel, vcov=vcovHAC(dModel))
robust




#Univariate GARCH
library(rugarch)
uniVarSpec = ugarchspec(mean.model=list(armaOrder=c(0,0)),
                  variance.model=list(garchOrder=c(1,1),
                                      model="sGARCH"),
                  distribution.model="norm")

uniGarch <- ugarchfit(spec = uniVarSpec, data = f)
print(garch)


#Univariate GARCH hedge


#Bivariate GARCH
library(rmgarch)
biVarSpec = gogarchspec(uniVarSpec,uniVarSpec)

biGarch = gogarchfit(spec = biVarSpec, data = cbind(f,u))
print(biGarch)


#Bivariate GARCH hedge




#Fit VAR
#library(vars)
# VARselect(dtelefonica, lag.max=12)$selection
# fitvardtelefonica  <- VAR(dtelefonica,p=2)
# summary(fitvardtelefonica)

#Granger causality. Do we need to correct for the other price
#causality(fitvardtelefonica, cause = "dCDS")



#Information criteria
#round(AIC(reg1, reg2, logRet),2)
#round(BIC(reg1, reg2, logRet),2)

#Adjusted R squared



library(stargazer)


