#OptimalHedgeRatio

rm(list=ls())
cat("\14")
library(readxl)

#Futures data
futuresURL = "Data/Weather.xlsx"
fName = names(read_excel(futuresURL, n_max=1))  #extract the name of the dataset
fData = read_excel(futuresURL, skip=2)

#Underlying data
#underlyingURL = "Data/eMini.xlsx"
#underlyingURL = "Data/Corn.xlsx"
underlyingURL = "Data/SRWheat.xlsx"
uName = names(read_excel(underlyingURL, n_max=1))  #extract the name of the dataset
uData = read_excel(underlyingURL, skip=2)

#Merge the 2 datasets and exclude extra data
merged = merge(uData, fData, by="Date")
omitted = na.omit(merged[,c(1,2,20)])  #choose only data columns. And remove the empty rows

#Prices
u = unlist(omitted[,2])
f = unlist(omitted[,3])
dates = omitted$Date

#Price changes
change = lapply(as.vector(omitted[,2:3]), diff, lag=1)
du = unlist(change[1])
df = unlist(change[2])
dDates = dates[-1]

#Percentage changes (returns)
duPer = du/u[-length(u)]  #check theoretical settlement. Should we use the empty spaces instead?
dfPer = df/f[-length(f)]

#Combine all into one dataframe
priceAll = data.frame(dates,u,f)
dAll = data.frame(dDates,du,df,duPer,dfPer)
all = merge(priceAll, dAll, by.x="dates", by.y="dDates", all.x=T)

#Plotting
library(xts)
#dev.new()
#for(x in all) {plot(xts(x,dates))}
#Map(all,xts,dates)
plot(xts(u,dates))
plot(xts(f,dates))
plot(xts(du,dDates))
plot(xts(df,dDates))
plot(xts(duPer,dDates))
plot(xts(dfPer,dDates))
#Just looking at the price data, we can see that it is non-stationary.
#So we should probably use changes or percentage changes
#dev.off()


#Descriptive statistics
summary(all[,-1], digits=2)
library(pastecs)
stats <- stat.desc(all[, -1])
round(stats, 1)
library(stargazer)
stargazer(all,out = NULL)


#Let's check the distributions. Do they look normal?
hist(u, breaks=50, main = "Histogram of underlying prices")
hist(f, breaks=50, main = "Histogram of futures prices")
hist(du, breaks=50, main = "Histogram of underlying changes")
hist(df, breaks=50, main = "Histogram of futures changes")
hist(duPer, breaks=50, main = "Histogram of underlying returns")
hist(dfPer, breaks=50, main = "Histogram of futures returns")


#OLS
plot(u, f)
model = lm(u ~ f)
summary(model)

plot(du, df)
dModel = lm(du ~ df)
summary(dModel)

perModel = lm(duPer ~ dfPer)
summary(perModel)

#OLS Hedge ratio
h = dModel$coefficients[2]
h

#Same with correlations
rho = sqrt(summary(dModel)$r.squared)
rho
rhoCheck = cor(du,df)
rhoCheck

h = rho * sd(du)/sd(df)
h


#Test for normality, heteroskedasticity, autocorrelation
#No multicollinearity if we have only one explanatory variable
#If we have granger causality, we should include bothe prices and also check for collinearity
#Conduct corrections for some of the problems



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



# ARMA
ARMA1 <- arima(du, order=c(1,0,0))
ARMA2 <- arima(du, order=c(0,0,1))
ARMA3 <- arima(du, order=c(1,0,1))
ARMA9 <- arima(du, order=c(9,0,0)) # orders from acf and pacf

# AIC
ARMA1$aic
ARMA2$aic
ARMA3$aic
ARMA9$aic

# select ARMA9
ARMA9

# residuals and standardised residuals
# residuals <- model7$residuals
# w <- residuals/(sqrt(model7$sigma2))
# plot(w)
# acfw <- acf(w, lag.max=14)

# Forecasting (forecast with GARCH instead)
library(forecast)
forecast <- forecast(ARMA9, h=30)  #30 day forecast
plot(forecast)







#Univariate GARCH
library(rugarch)
uniVarSpec = ugarchspec(mean.model=list(armaOrder=c(0,0)),
                  variance.model=list(garchOrder=c(1,1),
                                      model="sGARCH"),
                  distribution.model="norm")

uniGarch <- ugarchfit(spec = uniVarSpec, data = f)
print(garch)


#Univariate GARCH hedge




#Fit VAR
#library(vars)
# VARselect(dtelefonica, lag.max=12)$selection
# fitvardtelefonica  <- VAR(dtelefonica,p=2)
# summary(fitvardtelefonica)

# roots(fitvartelefonica)
# 
# #install.packages("urca")
# library(urca)
# 
# citelefonica <- ca.jo(telefonica, type = "trace", ecdet = "const", K = 3)
# summary(citelefonica)
# 
# citelefonica = 1.0000000*CDS -0.9651852*CS -33.6102961
# plot(citelefonica, type="l")


#Granger causality. Do we need to correct for the other price
#causality(fitvardtelefonica, cause = "dCDS")




#Bivariate GARCH
library(rmgarch)
biVarSpec = gogarchspec(uniVarSpec,uniVarSpec)

biGarch = gogarchfit(spec = biVarSpec, data = cbind(f,u))
print(biGarch)


#Bivariate GARCH hedge








#Information criteria
# round(AIC(dModel, model, logModel),2)
# round(BIC(dModel, model, logModel),2)

#Adjusted R squared
# round(c(summary(dModel)$adj.r.squared, summary(model)$adj.r.squared, summary(logModel)$adj.r.squared), 4)



library(stargazer)


