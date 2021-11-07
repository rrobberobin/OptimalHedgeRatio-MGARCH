#OptimalHedgeRatio

rm(list=ls())
cat("\14")

#Import data ----
folder = "Data/"
source("Methods.R")
data = importData(folder)


#Data handling ----
#Choose which data we want to use
chooseTickers = c("CornChicago", "CornFuture", "WheatFuture")
chooseData = data[chooseTickers]

#Renaming the columns
renamedData = renameCols(chooseData)

#Merge the datasets and exclude extra columns
merged = Reduce(function(x, y) merge(x,y,by="Date"), renamedData)
filter = sapply(names(merged), grepl, pattern="Price|Date")
prices = na.omit(merged[filter])

#Prices
u = prices$CornChicago.Price
f = prices$CornFuture.Settlement.Price
f2 = prices$WheatFuture.Settlement.Price
dates = prices$Date

#Price changes
change = sapply(prices[-1], diff, lag=1)
du = change[,1]
df = change[,2]
df2 = change[,3]
dDates = dates[-1]

#Percentage changes (returns)
duPer = du/u[-length(u)]
dfPer = df/f[-length(f)]
df2Per = df2/f2[-length(f2)]

#Combine all into one dataframe
priceAll = data.frame(dates,u,f,f2)
dAll = data.frame(dDates,duPer,dfPer,df2Per)
all = merge(priceAll, dAll, by.x="dates", by.y="dDates", all.x=T)

dAllMatr = as.matrix(dAll[,-1])




#Plotting and data analysis ----

cor(u,f)
cor(du,df)
cor(duPer,dfPer)
cor(duPer,df2Per)



#Time series----

library(xts)
#for(x in all) {plot(xts(x,dates))}
#Map(all,xts,dates)
plot(xts(u,dates))
plot(xts(f,dates))
plot(xts(du,dDates))
plot(xts(df,dDates))
plot(xts(duPer,dDates))
plot(xts(dfPer,dDates))
#Just looking at the PRICE data, we can see that it is non-stationary.
#I.e. we should probably use changes or percentage changes (returns)


#Normality ----
#Let's check the distributions. Do they look normal?
hist(u, breaks=50, main = "Histogram of underlying prices")
hist(f, breaks=50, main = "Histogram of futures prices")
hist(du, breaks=50, main = "Histogram of underlying changes")
hist(df, breaks=50, main = "Histogram of futures changes")
hist(duPer, breaks=50, main = "Histogram of underlying returns")
hist(dfPer, breaks=50, main = "Histogram of futures returns")

#Autocorrelation ----

acfLength = sqrt(length(duPer))
acfLength

acf(u,acfLength)
acf(f,acfLength)

acf(du,lag.max=acfLength) #Small autocorrelation at t+1
acf(df,lag.max=acfLength) #Some autocorrelation periodically

acf(duPer,lag.max=acfLength) #Small autocorrelation at t+1
acf(dfPer,lag.max=acfLength) #Some autocorrelation periodically

pacf(u,acfLength)
pacf(f,acfLength)

pacf(du,lag.max=acfLength) #Small partial autocorrelation at t+1
pacf(df,lag.max=acfLength) #Some partial autocorrelation periodically

pacf(duPer,lag.max=acfLength) #Small partial autocorrelation at t+1
pacf(dfPer,lag.max=acfLength) #Some partial autocorrelation periodically

#Ljung-box (autocorrelation)
Box.test(duPer,lag=1,type="Ljung-Box")
Box.test(duPer,lag=2,type="Ljung-Box")
Box.test(duPer,lag=5,type="Ljung-Box")
Box.test(duPer,lag=30,type="Ljung-Box")
Box.test(duPer,lag=acfLength,type="Ljung-Box")
Box.test(dfPer,lag=1,type="Ljung-Box")
Box.test(dfPer,lag=2,type="Ljung-Box")
Box.test(dfPer,lag=5,type="Ljung-Box")
Box.test(dfPer,lag=30,type="Ljung-Box")
Box.test(dfPer,lag=acfLength,type="Ljung-Box")

#Unit-root ----
#Augmented Dickey-Fuller (unit root)
library(fUnitRoots)
adfTest(duPer, lags = 252, type = c("nc"), title = NULL, description = NULL)
adfTest(dfPer, lags = 252, type = c("nc"), title = NULL, description = NULL)
adfTest(df2Per, lags = 252, type = c("nc"), title = NULL, description = NULL)

#Test for cointegration






#Descriptive statistics ----

#Need to add these somehow
library(moments)
skew = c(skewness(all[,-1],na.rm=T))
kurt = c(kurtosis(all[,-1],na.rm=T))
kS = round(rbind(skew,kurt),2)
kurtSkew = data.frame(kS, row.names = c("Skewness","Kurtosis"))

library(stargazer)
stargazer(all,
          type = "text",
          out = "desc.html",
          title = "Descriptive statistics",
          style = "default", #"aer", "qje"
          flip = T,
          digits = 2,
          digits.extra = 0,
          median=T,
          add.lines = list(skew,kurt)
          )

write.csv(kS, "kurtSkew.csv")






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


# library(DescTools)
# if(!require(RDCOMClient)) {
#   install.packages("RDCOMClient", repos = "http://www.omegahat.net/R"); 
#   library(RDCOMClient)}
# wrd = GetNewWrd(header = TRUE)
# Desc(all[,-1], plotit = F, digits=1, verbose=1, wrd = wrd)




#Divide data into parts -----












#OLS ----

perModel = lm(duPer ~ dfPer)
summary(perModel)
plot(dfPer, duPer)
abline(perModel, col="red")



#OLS Hedge ratio ----

#Regular
OLSHedgeRatioBi = perModel$coefficients[2]
OLSHedgeRatioBi


#Cross
crossCov = cov(data.frame(duPer,dfPer,df2Per))
fCrossCov = crossCov[2:3,2:3]
ufCrossCov = crossCov[1,2:3]
OLSHedgeRatioTri = ufCrossCov %*% solve(fCrossCov)

#This is the same
crossModel = lm(duPer ~ dfPer + df2Per)
crossModel$coefficients[2:3]



#CorrelationCheck
rhoCheck = sqrt(summary(perModel)$r.squared)
rhoCheck
cor(duPer,dfPer)

#HedgeCheck
rhoCheck * sd(duPer)/sd(dfPer)





#Summary of OLS models ----
library(stargazer)
stargazer(model,dModel,perModel,
          title = "Hedge models",
          type = "text",
          out = "model.html",
          no.space = T,
          digits=1)


#Diagnostic tests----
#Test for normality, heteroskedasticity, autocorrelation

#Normality----

#Residuals
perRes = perModel$residuals
crossOLSRes = crossModel$residuals

#Normality of residuals. Do they look normal?
hist(res, breaks=50, main = "Histogram of price residuals")
hist(dRes, breaks=50, main = "Histogram of change residuals")
hist(perRes, breaks=100, main = "Histogram of return residuals")
hist(crossOLSRes, breaks=100, main = "Histogram of return residuals")


#Test for normality
library(moments)
jarque.test(res)  #Not normal. The histogram definitely does not look normal
jarque.test(dRes) #Not normal
jarque.test(perRes) #Not normal
JBOLS = jarque.test(perRes)$p.value #Not normal
jarque.test(crossOLSRes) #Not normal
JBCross = jarque.test(crossOLSRes)$p.value #Not normal


#Remedies for non-normality: (Big sample size CLT, simulating more observations, logarithmic transform)
plot(duPer, log(1+dfPer))
logModel = lm(duPer ~ log(1+dfPer))    #Check if right model. Should I log both?
loglogModel = lm(log(1+duPer) ~ log(1+dfPer))    #Check if right model. Should I log both?
logRes = logModel$residuals
loglogRes = loglogModel$residuals
hist(logRes, breaks=50, main = "Histogram of logreturns residuals")
hist(loglogRes, breaks=50, main = "Histogram of loglogreturns residuals")
jarque.test(logRes)
jarque.test(loglogRes)
summary(logModel)
summary(loglogModel)


logCrossModel = lm(duPer ~ log(1+dfPer)+log(1+df2Per))



standRes = logRes/(summary(logModel)$sigma)
hist(standRes, breaks=50)
jarque.test(standRes) #it's exactly the same as logRes



#Autocorrelation ----

#Breusch-Godfrey (autocorrelation)
library(lmtest)
bgtest(perModel,252)  #test of order 252 based on days
bgOLS = bgtest(perModel,252)$p.value  #test of order 252 based on days
bgtest(crossModel,252)  #test of order 252 based on days
bgCross = bgtest(crossModel,252)$p.value  #test of order 252 based on days


perResStandard = perRes/(summary(perModel)$sigma)
plot(perResStandard)
acf(perResStandard, lag.max=30)
Box.test(perResStandard,lag=1,type="Ljung-Box")

#We should correct for these autocorrelations!


#Heteroskedasticity ----
library(lmtest)

#Breusch-Pagan (check also with White?)
bptest(perModel)  #p-value = 0.9401.  Can't reject null. We have homoskedasticity
bpOLS = bptest(perModel)$p.value  #p-value = 0.9401.  Can't reject null. We have homoskedasticity
bptest(crossModel)  #p-value = 0.9401.  Can't reject null. We have homoskedasticity
bpCross = bptest(crossModel)$p.value  #p-value = 0.9401.  Can't reject null. We have homoskedasticity

#Correct for heteroskedasticity and autocorrelation (Andrews)
library(sandwich)
robustOLSModel = coeftest(perModel, vcov=vcovHAC(perModel))
robustOLSModel
robustCrossModel = coeftest(crossModel, vcov=vcovHAC(crossModel))
robustCrossModel





#Multicollinearity----

#BiModel has no multicollinearity because we have only one explanatory variable

#TriModel
library(car)
vif(crossModel)
#Vif rule of thumb: if vif>10, we have multicollinearity

cor(dfPer,df2Per)


#If we have granger causality, we could include both prices and also check for collinearity
#Conduct corrections for some of the problems----

#Using robust standard deviations
lm()

#Report OLS diagnostics----
JB = rbind(JBOLS,JBCross)
BG = rbind(bgOLS,bgCross)
BP = rbind(bpOLS,bpCross)

OLSDiagnostics = data.frame(JB,BG,BP, row.names=c("Regular hedge","Cross hedge"))
OLSDiagnostics=round(OLSDiagnostics,3)
write.csv(OLSDiagnostics,"OLSDiagnostics.csv")

#Timevarying models ----

#ARMA and SARIMA ----
ARMA0 = arima(duPer, order=c(0,0,0),include.mean=F)
ARMA1 = arima(duPer, order=c(1,0,0))
ARMA2 = arima(duPer, order=c(0,0,1))
ARMA3 = arima(duPer, order=c(1,0,1))
ARMA4 = arima(duPer, order=c(2,0,2))

library(forecast)
autoU = auto.arima(duPer)
autoU

# Choosing the best model
AIC(ARMA0,ARMA1,ARMA2,ARMA3,ARMA4,autoU)
BIC(ARMA0,ARMA1,ARMA2,ARMA3,ARMA4,autoU)


ARMAf0 = arima(dfPer, order=c(0,0,0),include.mean = F)
ARMAf1 = arima(dfPer, order=c(1,0,0),include.mean = F)
ARMAf2 = arima(dfPer, order=c(0,0,1),include.mean = F)
ARMAf3 = arima(dfPer, order=c(1,0,1),include.mean = F)
ARMAf4 = arima(dfPer, order=c(2,0,2),include.mean = F)

autoF = auto.arima(dfPer)
autoF

AIC(ARMAf0,ARMAf1,ARMAf2,ARMAf3,ARMAf4,autoF)
BIC(ARMAf0,ARMAf1,ARMAf2,ARMAf3,ARMAf4,autoF)


ARMAff0 = arima(df2Per, order=c(0,0,0),include.mean = F)
ARMAff1 = arima(df2Per, order=c(1,0,0),include.mean = F)
ARMAff2 = arima(df2Per, order=c(0,0,1),include.mean = F)
ARMAff3 = arima(df2Per, order=c(1,0,1),include.mean = F)
ARMAff4 = arima(df2Per, order=c(2,0,2),include.mean = F)

autoFF = auto.arima(df2Per)
autoFF

AIC(ARMAff0,ARMAff1,ARMAff2,ARMAff3,ARMAff4,autoFF)
BIC(ARMAff0,ARMAff1,ARMAff2,ARMAff3,ARMAff4,autoFF)


# residuals and standardised residuals
# residuals <- model7$residuals
# w <- residuals/(sqrt(model7$sigma2))
# plot(w)
# acfw <- acf(w, lag.max=14)

#Forecasting----
library(forecast)
forecast = forecast(ARMA9, h=30)  #30 day forecast
plot(forecast)


fARMA = arima(f,order=c(1,0,0))
SARIMA = arima(f, 
                order=c(1,0,0),
                seasonal=list(order=c(0,1,0),period=252))
seasonalForecast = forecast(SARIMA, h=252)
plot(seasonalForecast)






#Univariate GARCH ----
library(rugarch);library(rmgarch);
uniVarSpec = ugarchspec(variance.model=list(garchOrder=c(1,1),
                                            model="sGARCH"),
                        mean.model=list(armaOrder=c(0,0),
                                        include.mean = F),
                        distribution.model="norm")

uniVarSpec2 = ugarchspec(variance.model=list(garchOrder=c(2,2),
                                            model="sGARCH"),
                        mean.model=list(armaOrder=c(0,0),
                                        include.mean = F),
                        distribution.model="norm")

uniVarSpec3 = ugarchspec(variance.model=list(garchOrder=c(2,2),
                                             model="apARCH"),
                         mean.model=list(armaOrder=c(0,0),
                                         include.mean = F),
                         distribution.model="norm")

#apARCH seems better than gjrGARCH

uniGarch = ugarchfit(spec = uniVarSpec, 
                     data = duPer,
                     out.sample=50)

uniGarch2 = ugarchfit(spec = uniVarSpec2, 
                     data = duPer,
                     out.sample=50)

uniGarch3 = ugarchfit(spec = uniVarSpec3, 
                      data = duPer,
                      out.sample=50,solver="hybrid")


infocriteria(uniGarch);infocriteria(uniGarch2);infocriteria(uniGarch3)

#garch order (4,2) was good based on arch(4). But then again garch(2,2) and garch(1,1) seem also good (AIC,BIC,SIC,HQ)
print(uniGarch)


#UniGarch forecast ----
uniGarchForecast = ugarchforecast(uniGarch,
                                  n.ahead=30,
                                  out.sample = 10)
plot(uniGarchForecast, which=1)

#Univariate GARCH hedge----


#Test for GARCH residuals----
resGarch = as.vector(residuals(uniGarch))
hist(resGarch, breaks =100, freq = FALSE)
curve(dnorm(x,mean(resGarch),sd(resGarch)), add=TRUE)
jarque.test(resGarch)



norma = rnorm(100000)
hist(norma, breaks =100, freq = FALSE)
curve(dnorm(x, mean = mean(norma), sd = sd(norma)), add=TRUE)
jarque.test(norma)


#Manual model----
# for(n in 1:length(du)){
#   lm(var(du) ~ lag(var(du),1) + lag(var(df),1) + lag(cov(du,df),1) +
#      lag(residuals(du^2),1) + lag(residuals(df^2),1) + lag(residuals(du*df),1))
#   
#   lm(cov(du,df) ~ lag(var(du),1) + lag(var(df),1) + lag(cov(du*df),1) +
#      lag(residuals(du^2),1) + lag(residuals(df^2),1) + lag(residuals(du*df),1))
# }
#VAR----
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




#Bivariate GARCH ----
library(rmgarch)
biVarSpec = dccspec(multispec(replicate(2,uniVarSpec3)),
                    VAR=F,
                    lag = NULL,
                    lag.max = 30,
                    dccOrder = c(2,2),
                    model = "aDCC")

biVarSpec2 = dccspec(multispec(replicate(2,uniVarSpec3)),
                    VAR=F,
                    lag = NULL,
                    lag.max = 30,
                    dccOrder = c(2,2),
                    model = "DCC")

biGarch = dccfit(spec = biVarSpec, 
                     data = cbind(duPer,dfPer),
                     out.sample = 100)

biGarch2 = dccfit(spec = biVarSpec2, 
                 data = cbind(duPer,dfPer),
                 out.sample = 100)

infocriteria(biGarch); infocriteria(biGarch2)


biGarch
biGarchCor = rcor(biGarch)
biGarchCov = rcov(biGarch)
biGarchCov[,,1]

biGarchDates = head(dates,length(biGarchCor))
plot(xts(rcor(biGarch)[1,2,],biGarchDates))
plot(xts(rcor(biGarch)[1,3,],biGarchDates))
plot(xts(rcor(biGarch)[2,3,],biGarchDates))


#Trivariate GARCH ----
library(rmgarch)
triVarSpec = dccspec(multispec(replicate(3,uniVarSpec3)),
                    VAR=F,
                    lag = NULL,
                    lag.max = 30,
                    dccOrder = c(2,2,2), #check this
                    model = "aDCC")

triVarSpec2 = dccspec(multispec(replicate(3,uniVarSpec3)),
                     VAR=F,
                     lag = NULL,
                     lag.max = 30,
                     dccOrder = c(2,2,2), #check this
                     model = "DCC")

triGarch = dccfit(spec = triVarSpec, 
                 data = cbind(duPer,dfPer, df2Per),
                 out.sample = 100)

triGarch2 = dccfit(spec = triVarSpec2, 
                  data = dAllMatr,
                  out.sample = 100)

infocriteria(triGarch); infocriteria(triGarch2)


triGarchCor = rcor(triGarch)
triGarchCov = rcov(triGarch)
triGarchCov[,,1]

head(triGarch@model$sigma)

triGarchDates = head(dates,length(triGarchCor))
plot(xts(rcor(triGarch)[1,2,],biGarchDates))
plot(xts(rcor(triGarch)[1,3,],biGarchDates))
plot(xts(rcor(triGarch)[2,3,],biGarchDates))


#DCC Forecast----
#hist(residuals(biGarch))
triForecastLength = triGarch@model$modeldata$n.start
dccForecast = dccforecast(triGarch, n.roll=triForecastLength)
plot(dccForecast,which=1)
plot(dccForecast,which=2)
plot(dccForecast,which=3)
plot(dccForecast,which=4)
plot(dccForecast,which=5)
par(mar=c(4,4,4,4))
par(mar=c(2,2,2,2))

biForecastLength = biGarch@model$modeldata$n.start
bidccForecast = dccforecast(biGarch2, n.roll=biForecastLength)
plot(bidccForecast,which=1)
plot(bidccForecast,which=2)
plot(bidccForecast,which=3)
plot(bidccForecast,which=5)



# test_forecast()
# forecast::accuracy(dccForecast)
plot(forecast(triGarch2))



#Garch tests ----
library(matrixcalc)
matr = cov(cbind(u,f))
is.positive.semi.definite(matr)
matrD = cov(cbind(du,df))
is.positive.semi.definite(matrD)
matrDPer = cov(cbind(duPer,dfPer))
is.positive.semi.definite(matrDPer)


#Diagnostic tests----
#Test for normality, heteroskedasticity, autocorrelation, multicollinearity

#Normality----

#Residuals
triGarchRes = data.frame(triGarch@model$residuals)
triGarchResStd = data.frame(triGarch@mfit$stdresid)
maybeSame = data.frame(triGarch@model$residuals/triGarch@model$sigma)
sqrdResiduals = as.matrix(triGarchRes)^2
head(triGarchResStd)
head(triGarchRes)

biGarchRes = data.frame(biGarch@model$residuals)
sqrdResidualsBi = as.matrix(biGarchRes)^2


a = likelihood(object = "triGarch")
str(a)

#residuals(triGarch[])
skewness(triGarchResStd)
kurtosis(triGarchResStd)

library(car)
qqPlot(triGarchResStd[,1])

#Normality of residuals. Do they look normal?
sapply(triGarchRes,hist,breaks=50, main = "Histogram of GARCH residuals")
sapply(triGarchResStd,hist,breaks=50, main = "Histogram of GARCH residuals")
hist(triGarchResStd[,1], breaks=50)
hist(triGarchResStd[,2], breaks=50)
hist(triGarchResStd[,3], breaks=50)
hist(triGarchRes[,1], breaks=50)
hist(triGarchRes[,2], breaks=50)
hist(triGarchRes[,3], breaks=50)


#Test for normality
library(moments)
sapply(triGarchRes,jarque.test)  #Not normal. Leptocurtic
sapply(triGarchResStd,jarque.test) #Not normal


#ARCH effects test (volatility clustering)
perResSqrd = perRes^2
ARCHEffeOLS = summary(lm(perResSqrd ~ Lag(perResSqrd,1) +
                           Lag(perResSqrd,2) + 
                           Lag(perResSqrd,3) +
                           Lag(perResSqrd,4) +
                           Lag(perResSqrd,5)
))
ARCHEffeOLS
TRsqrdARCHOLS = length(perResSqrd)*ARCHEffeOLS$r.squared
pchisq(TRsqrdARCHOLS,df=5)
#We have effects


#After fixes
m=1
ARCHEffeTri = summary(lm(sqrdResiduals[,m] ~ Lag(sqrdResiduals[,m],1) +
                           Lag(sqrdResiduals[,m],2) + Lag(sqrdResiduals[,m],3) +
                           Lag(sqrdResiduals[,m],4) + Lag(sqrdResiduals[,m],5)
                         
))
ARCHEffeTri
TRsqrdARCHTri = length(sqrdResiduals[,m])*ARCHEffeTri$r.squared
pchisq(TRsqrdARCHTri,df=5)
#We have effects still for u

resLen = dim(sqrdResiduals)[2]

ARCHTri = c(ARCHTest(sqrdResiduals[,1]),
            ARCHTest(sqrdResiduals[,2]),
            ARCHTest(sqrdResiduals[,3])
            )
ARCHBi = c(ARCHTest(sqrdResidualsBi[,1]),
           ARCHTest(sqrdResidualsBi[,2]))


#Test for asymmetric volatility (levered effects)
library(Hmisc)
minusDummy2 = ifelse(Lag(perRes,1) < 0, 1, 0)
laggedResiduals2 = Lag(as.matrix(perRes),1)
laggedMinusDummy2 = minusDummy2*laggedResiduals2
laggedPlusDummy2 = (1-minusDummy2)*laggedResiduals2
engleNg2 = summary(lm(perResSqrd ~ minusDummy2 + laggedMinusDummy2 + laggedPlusDummy2))
engleNg2
TRsqrdEngle2 = length(perResSqrd)*engleNg2$r.squared
pchisq(TRsqrdEngle2,df=3)


#After fix
library(Hmisc)
minusDummy = ifelse(Lag(triGarchRes,1) < 0, 1, 0)
laggedResiduals = Lag(as.matrix(triGarchRes),1)
laggedMinusDummy = minusDummy*laggedResiduals
laggedPlusDummy = (1-minusDummy)*laggedResiduals

n=2
summary(lm(sqrdResiduals[,n] ~ minusDummy[,n]))
summary(lm(sqrdResiduals[,n] ~ laggedMinusDummy[,n]))
engleNg=summary(lm(sqrdResiduals[,n] ~ minusDummy[,n] + laggedMinusDummy[,n] + laggedPlusDummy[,n]))
engleNg
#A little bit of assymmetry even after grach model
TRsqrdEngle = length(sqrdResiduals[,n])*engleNg$r.squared
pchisq(TRsqrdEngle,df=3)
#Seems to be asymmetric for f

engleNgTri = c()
for(n in 1:resLen){
  ENModel = summary(lm(sqrdResiduals[,n] ~ 
                         minusDummy[,n] + 
                         laggedMinusDummy[,n] + 
                         laggedPlusDummy[,n]))
  TRsqrdEngle = length(sqrdResiduals[,n])*ENModel$r.squared
  p = pchisq(TRsqrdEngle,df=3)
  engleNgTri = cbind(engleNgTri,p)
}

source("Methods.R")
EngleNg(triGarchRes[,3])



#Autocorrelation ----

#Breusch-Godfrey (autocorrelation)
library(lmtest)
bgtest(perModel,5)  #test of order 252 based on days
bgtest(crossModel,5)  #test of order 252 based on days

n=3
par("mar"=c(4,4,4,4))
par(mfrow=c(1,1))
plot(triGarchResStd[n])
acf(triGarchResStd[n], lag.max=10)
acf(triGarchResStd[n], lag.max=30)
Box.test(triGarchResStd[n],lag=1,type="Ljung-Box")
Box.test(triGarchResStd[n],lag=2,type="Ljung-Box")
Box.test(triGarchResStd[n],lag=5,type="Ljung-Box")
Box.test(triGarchResStd[n],lag=30,type="Ljung-Box")
#We should correct for these autocorrelations!

boxFive= c()
for(n in 1:resLen){
  box = Box.test(triGarchResStd[n],lag=5,type="Ljung-Box")
  boxFive = cbind(boxFive,box$p.value)
  }

#Heteroskedasticity ----
library(lmtest)

#Breusch-Pagan (check also with White?)
bptest(perModel)  #p-value = 0.9401.  Can't reject null. We have homoskedasticity
summary(triGarch)
#lm(y[2:n] ~ 0 + xreg[2:n,] + y[1:(n-1)])


#Correct for heteroskedasticity and autocorrelation (Andrews)
library(sandwich)
robust = coeftest(dModel, vcov=vcovHAC(dModel))
robust

#Multicollinearity----

#BiModel has no multicollinearity because we have only one explanatory variable

#TriModel
library(car)
vif(triModel)
#Vif rule of thumb: if vif>10, we have multicollinearity

cor(dfPer,df2Per)


#If we have granger causality, we could include both prices and also check for collinearity
#Conduct corrections for the problems----

#Using robust standard deviations
#lm()

#Report diagnostics----
diag = round(rbind(boxFive,ARCHp,EngleNg),3)
diagnostics = data.frame(diag,
                  row.names=c("Box-Ljung, 5 lags", "ARCH test (LM?), 5 lags", "Engle-Ng"))
colnames(diagnostics) = c("u","f","f2")

write.csv(diagnostics, "diagnostics.csv")



#Multivariate hedge ratios (in-sample)----


inSampleSize = dim(triGarchCov)[3]


#Bivariate hedge ratio (in-sample)
biHedgeRatioInSample = biGarchCov[1,2,] / biGarchCov[2,2,]


#Trivariate hedge ratio (in-sample)
triVarHedgeIn = c()
for(n in 1:inSampleSize){
  fMatrix = as.matrix(triGarchCov[2:3,2:3,n])
  ufMatrix = as.matrix(triGarchCov[2:3,1,n])
  invF = solve(fMatrix)
  h = invF %*% ufMatrix
  triVarHedgeIn = cbind(triVarHedgeIn, h)
  }



#Multivariate hedge ratios (out-of-sample)----


outSampleSize = triGarch@model$modeldata$n.start
forecastCov = rcov(dccForecast)[[1]]
biForecastCov = rcov(bidccForecast)[[1]]


#Bivariate hedge ratio (out-of-sample)
biHedgeRatioOutSample = biForecastCov[1,2,] / biForecastCov[2,2,]


#Trivariate hedge ratio (out-of-sample)
#Hedge ratio formula: general matrix form. solve() is the inverse
triVarHedgeOut = c()
for(n in 1:outSampleSize){
  fMatrix = as.matrix(forecastCov[2:3,2:3,n])
  ufMatrix = as.matrix(forecastCov[2:3,1,n])
  invF = solve(fMatrix)
  h = invF %*% ufMatrix
  triVarHedgeOut = cbind(triVarHedgeOut, h)
}




#Hedge effectiveness (in-sample) ----
uIn = head(duPer,inSampleSize)
fIn = head(dfPer,inSampleSize)
f2In = head(df2Per,inSampleSize)
uInVar = var(uIn)

#Bivariate hedge effectiveness (in-sample)
fInBiOptimal = fIn * head(biHedgeRatioInSample,inSampleSize)
inHedgeBi = uIn-fInBiOptimal
inHedgeBiVar = var(inHedgeBi)
HEInBiVar = 1 - inHedgeBiVar/uInVar


#Trivariate hedge effectiveness (in-sample)
fInTriOptimal = fIn * head(triVarHedgeIn[1,],inSampleSize)
f2InTriOptimal = f2In * head(triVarHedgeIn[2,],inSampleSize)
inHedgeTri = uIn - fInTriOptimal - f2InTriOptimal
fInTriVariate = var(inHedgeTri)
HEInTriVar = 1 - fInTriVariate / uInVar

cat(HEInBiVar, HEInTriVar)
#TriVar seems slightly worse in-sample



#Hedge effectiveness (out-of-sample) ----

uOut = tail(duPer,outSampleSize)
fOut = tail(dfPer,outSampleSize)
f2Out = tail(df2Per,outSampleSize)
uOutVar = var(uOut)

#Bivariate hedge effectiveness (out-of-sample)
fOutBiOptimal = fOut * tail(biHedgeRatioOutSample,outSampleSize)
outHedgeBi = uOut - fOutBiOptimal
outHedgeBiVar = var(outHedgeBi)
HEOutbiVar = 1 - outHedgeBiVar/uOutVar


#Trivariate hedge effectiveness (out-of-sample)
fOutTriOptimal = fOut * head(triVarHedgeOut[1,],outSampleSize)
f2OutTriOptimal = f2Out * head(triVarHedgeOut[2,],outSampleSize)
outHedgeTri = uOut - fOutTriOptimal - f2OutTriOptimal
outHedgeTriVar = var(outHedgeTri)
HEOutTriVar = 1 - outHedgeTriVar / uOutVar

cat(HEOutbiVar, HEOutTriVar)
#TriVar seems slightly better out-of-sample


#OLS hedged position (normal)
OLSInHedgedBi = uIn - OLSHedgeRatioBi %*% fIn
OLSOutHedgedBi = uOut - OLSHedgeRatioBi %*% fOut


#OLS hedged position (cross)
OLSInHedgedTri = uIn - OLSHedgeRatioTri[1,]%*%rbind(fIn,f2In)
OLSOutHedgedTri = uOut - OLSHedgeRatioTri[1,]%*%rbind(fOut,f2Out)

#Naive
NaiveInHedged = uIn - fIn
NaiveOutHedged = uOut - fOut


#Hypothesis testing ----



#T-test in-sample
t1 = t.test(inHedgeTri,inHedgeBi)
#They are not statistically different

#T-test out-of-sample
t2 = t.test(outHedgeTri,outHedgeBi)
#They are not statistically different

#F-test in-sample
vt1 = var.test(inHedgeTri,inHedgeBi)
#They are not statistically different

#F-test out-of-sample
vt2 = var.test(outHedgeTri,outHedgeBi)
#They are not statistically different
#If I increase out-of-sample size, they will be become different?

pValues = c(t1$p.value,t2$p.value,vt1$p.value,vt2$p.value)


#Reporting----

inResults = t(rbind(uIn,NaiveInHedged,OLSInHedgedBi,OLSInHedgedTri,inHedgeBi,inHedgeTri))
outResults = t(rbind(uOut,NaiveOutHedged,OLSOutHedgedBi,OLSOutHedgedTri,outHedgeBi,outHedgeTri))

library(matrixStats)
vrsIn = colVars(inResults)
vrsOut = colVars(outResults)
vrsAnnualPercentages = rbind(vrsIn,vrsOut) * sqrt(252) * 100
vrs = round(vrsAnnualPercentages,3)

inHE = 1- vrsIn/uInVar
outHE = 1- vrsOut/uOutVar
HE = round(rbind(inHE,outHE),3)

meansIn = colMeans(inResults)
meansOut = colMeans(outResults)
meanAnnualPercentages = rbind(meansIn,meansOut) * 252 * 100
means = round(meanAnnualPercentages,3)

combined = rbind(vrs, means, HE)

results = data.frame(combined, 
                     row.names=c("In-sample variance","Out-of-sample variance",
                                 "In-sample return","Out-of-sample return", 
                                 "In-sample HE","Out-of-sample HE"))
colnames(results) = c("Unhedged","Naive","OLS","OLS cross","BiVariate GARCH","TriVariate GARCH")

write.csv(results, "results.csv")




# library(car)
# linearHypothesis()

#Information criteria
# round(AIC(dModel, model, logModel),2)
# round(BIC(dModel, model, logModel),2)

#Adjusted R squared
# round(c(summary(dModel)$adj.r.squared, 
#         summary(model)$adj.r.squared, 
#         summary(logModel)$adj.r.squared), 4)


#Summary of models----

#Robust SE

# cov1         = vcovHC(perModel, type = "HC3")
# cov2         = vcovHC(crossModel, type = "HC3")
cov1         = vcovHAC(perModel)
cov2         = vcovHAC(crossModel)
robustSE1    = sqrt(diag(cov1))
robustSE2    = sqrt(diag(cov2))

perModel$AIC <- AIC(perModel)
crossModel$AIC <- AIC(crossModel)
library(stargazer)
stargazer(perModel,crossModel,
          title = "OLS models",
          type = "text",
          out = "model.html",
          #no.space = T,
          digits=2,
          digits.extra=0,
          se = list(robustSE1, robustSE2),
          omit.stat = c("f"),
          keep.stat = c("n","rsq","adj.rsq","aic"))#,
          #add.lines=list(c("AIC", round(AIC(perModel),0), round(AIC(crossModel),0))))
          #keep.stat = c("all","aic","bic"))

stargazer(perModel,crossModel,biGarch,triGarch,
          title = "Hedge models",
          type = "text",
          out = "model.html",
          no.space = T,
          digits=1)

stargazer(biGarch,#triGarch,
          title = "Hedge models",
          type = "text",
          out = "model.html",
          no.space = T,
          digits=1)


require("magrittr")
modelLatex = {
  stargazer::stargazer(triGarch@mfit$matcoef, 
                       type = "text",
                       digits=2,
                       digits.extra=0,
                       title = "Parameter Estimates of the TriVariate Garch") %>% 
  gsub("Std. Error", "Rob. Std. Error", .) %>%  
  gsub("t value", "Rob. t value", .) %>%  
  gsub("mu", "$\\\\mu$", .) %>%
  gsub("alpha1", "$\\\\alpha$", .) %>%
  gsub("omega", "$\\\\omega$", .) %>%  
  gsub("beta1", "$\\\\beta$", .) %>%
  gsub("shape", "$\\\\nu$", .)  %>%
  writeLines("arch_output.html")
  }


