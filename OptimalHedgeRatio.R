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
library(stargazer)
stargazer(all, 
          type = "text",
          out = "desc.html",
          title = "Descriptive statistics",
          style = "default", #"aer", "qje"
          flip = T,
          digits = 1,
          digits.extra = 0,
          median=T,
          add.lines = list(kurtosis(all[,-1],na.rm=T),skewness(all[,-1],na.rm=T))
          )


#Need to add these somehow
kurtosis(all[,-1],na.rm=T)
skewness(all[,-1],na.rm=T)


#More options for descriptives
# summary(all[,-1], digits=3)
# library(pastecs)
# stats <- stat.desc(all[, -1], norm=T)
# round(stats, 1)
# 
# library(vtable)
# st(all,
#    add.median=T,
#    out='csv',
#    file="desc"
# )
# library(psych)
# describe(all[,-1])



library(DescTools)
install.packages("RDCOMClient", repos = "http://www.omegahat.net/R")
library(RDCOMClient)
wrd = GetNewWrd(header = TRUE)
Desc(all[,-1], plotit = F, digits=1, verbose=1, wrd = wrd)





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

plot(duPer, dfPer)
perModel = lm(duPer ~ dfPer)
summary(perModel)

#Summary of all models
stargazer(model,dModel,perModel,
          type = "text",
          no.space = T,
          digits=5)

#OLS Hedge ratio
h = perModel$coefficients[2]
h

#Same with correlations
rho = sqrt(summary(perModel)$r.squared)
rho
rhoCheck = cor(duPer,dfPer) #Should be equal to this
rhoCheck

h = rho * sd(duPer)/sd(dfPer)
h

#Check
#onlyRecent = all[(all$dates == "2021-05-04"),]
onlyRecent = subset(all, dates > "2021-05-04")


cor(onlyRecent$duPer,onlyRecent$dfPer)
cor(onlyRecent$du,onlyRecent$df)
cor(onlyRecent$u,onlyRecent$f)






#Test for normality, heteroskedasticity, autocorrelation

#Residuals
res = model$residuals
dRes = dModel$residuals
perRes = perModel$residuals

#Normality of residuals. Do they look normal?
hist(res, breaks=50, main = "Histogram of price residuals")
hist(dRes, breaks=50, main = "Histogram of change residuals")
hist(perRes, breaks=50, main = "Histogram of return residuals")

#Test for normality
library(moments)
jarque.test(res)  #Not normal. The histogram definitely does not look normal
jarque.test(dRes) #Not normal
jarque.test(perRes) #Not normal


#Remedies for normality: (Big sample size CLT, simulating more observations, logarithmic transform)
plot(log(1+duPer), log(1+dfPer))
logModel = lm(log(1+duPer) ~ log(1+dfPer))    #Check if right model. Should I log both?
logRes = logModel$residuals
hist(logRes, breaks=50, main = "Histogram of logreturns residuals")
jarque.test(logRes)
summary(logModel)


#Autocorrelation
acf(u)
acf(f)

acf(du,lag.max=50) #Small autocorrelation at t+1
acf(df,lag.max=500) #Some autocorrelation periodically

acf(duPer,lag.max=50) #Small autocorrelation at t+1
acf(dfPer,lag.max=500) #Some autocorrelation periodically


pacf(u)
pacf(f)

pacf(du,lag.max=50) #Small partial autocorrelation at t+1
pacf(df,lag.max=500) #Some partial autocorrelation periodically

pacf(duPer,lag.max=50) #Small partial autocorrelation at t+1
pacf(dfPer,lag.max=500) #Some partial autocorrelation periodically


#Breusch-Godfrey (autocorrelation)
library(lmtest)
bgtest(perModel,252)  #test of order 252 based on days

#Ljung-box (autocorrelation)
Box.test(duPer,lag=9,type="Ljung-Box")
Box.test(dfPer,lag=5,type="Ljung-Box")

#Augmented Dickey-Fuller (unit root)
library(fUnitRoots)
adfTest(duPer, lags = 9, type = c("nc"), title = NULL, description = NULL)
adfTest(dfPer, lags = 5, type = c("nc"), title = NULL, description = NULL)


#Test for cointegration


#We should correct for these autocorrelations!



#Heteroskedasticity
library(lmtest)

#Breusch-Pagan (check also with White?)
bptest(dModel)  #p-value = 0.9401.  Can't reject null. We have homoskedasticity

#Correct for heteroskedasticity and autocorrelation (Andrews)
library(sandwich)
robust = coeftest(dModel, vcov=vcovHAC(dModel))
robust

#Multicollinearity
#No multicollinearity if we have only one explanatory variable
#If we have granger causality, we should include both prices and also check for collinearity
#Conduct corrections for some of the problems



# ARMA
ARMA1 = arima(du, order=c(1,0,0))
ARMA2 = arima(du, order=c(0,0,1))
ARMA3 = arima(du, order=c(1,0,1))
ARMA9 = arima(du, order=c(9,0,0)) # orders from acf and pacf

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
forecast = forecast(ARMA9, h=30)  #30 day forecast
plot(forecast)


fARMA = arima(f,order=c(1,0,0))
SARIMA = arima(f, 
                order=c(1,0,0),
                seasonal=list(order=c(0,1,0),period=252))
seasonalForecast = forecast(SARIMA, h=30)
ts.plot(seasonalForecast)






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


