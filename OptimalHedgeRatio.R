#OptimalHedgeRatio

rm(list=ls())
cat("\14")

#Import data ----
folder = "Data/"
source("ImportData.R")
data = importData(folder)

#Futures data

#Underlying data
# underlyingURL = "Data/Corn.xlsx"
# uDataName = names(read_excel(underlyingURL, n_max=1))  #extract the name of the dataset
# uData = read_excel(underlyingURL, skip=2)


#Data handling ----
#Choose which data we want to use
chooseTickers = c("Corn", "CornFuture", "SRWheat")
chooseData = data[chooseTickers]

#Renaming the columns
renamedData = renameCols(chooseData)

#Merge the datasets and exclude extra columns
merged = Reduce(function(x, y) merge(x,y,by="Date"), renamedData)
filter = sapply(names(merged), grepl, pattern="Price|Date")
prices = merged[filter]

#Prices
u = prices$Corn.Price
f = prices$CornFuture.Settlement.Price
f2 = prices$SRWheat.Price
dates = prices$Date

#Price changes
change = sapply(prices[-1], diff, lag=1)
du = change[,1]
df = change[,2]
df2 = change[,3]
dDates = Date[-1]

#Percentage changes (returns)
duPer = du/u[-length(u)]
dfPer = df/f[-length(f)]
df2Per = df2/f2[-length(f2)]

#Combine all into one dataframe
priceAll = data.frame(dates,u,f)
dAll = data.frame(dDates,du,df,duPer,dfPer)
all = merge(priceAll, dAll, by.x="dates", by.y="dDates", all.x=T)





#Plotting ----
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

#Need to add these somehow
library(moments)
kurtosis(all[,-1],na.rm=T)
skewness(all[,-1],na.rm=T)
#Descriptive statistics ----
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
if(!require(RDCOMClient)) {
  install.packages("RDCOMClient", repos = "http://www.omegahat.net/R"); 
  library(RDCOMClient)}
wrd = GetNewWrd(header = TRUE)
Desc(all[,-1], plotit = F, digits=1, verbose=1, wrd = wrd)





#Let's check the distributions. Do they look normal?
hist(u, breaks=50, main = "Histogram of underlying prices")
hist(f, breaks=50, main = "Histogram of futures prices")
hist(du, breaks=50, main = "Histogram of underlying changes")
hist(df, breaks=50, main = "Histogram of futures changes")
hist(duPer, breaks=50, main = "Histogram of underlying returns")
hist(dfPer, breaks=50, main = "Histogram of futures returns")







#OLS ----
plot(f,u)
model = lm(u ~ f)
summary(model)
abline(model, col="red")

plot(df, du)
dModel = lm(du ~ df)
summary(dModel)
abline(dModel, col="red")

plot(dfPer, duPer)
perModel = lm(duPer ~ dfPer)
summary(perModel)
abline(perModel, col="red")


#NonLinearModels ----
nonLinearModel = lm(u ~ f + I(f^2))
nonLinearModel2 = lm(u ~ poly(f,6))
summary(nonLinearModel)
summary(nonLinearModel2)
lines(f,predict(nonLinearModel,data.frame(f)))

nonLinearModel3 = nls(u ~ f + I(f^2), start=list(f=1))
#lines(f,predict(nonLinearModel3),lty=2,col="red",lwd=3)

library(ggplot2)
qplot(f,u)+stat_smooth(method="lm", formula="y~poly(x,2)")
#ggplot(all, aes(x = f, y = nonLinearModel))

nonLinearModel4 = lm(u[-1] ~ f[-1] + I(df*f[-1]))
summary(nonLinearModel4)

nonLinearModel5 = lm(u[-1] ~ f[-1] + I(df^2))
summary(nonLinearModel5)





#Summary of all models ----
stargazer(model,dModel,perModel,
          type = "text",
          no.space = T,
          digits=5)

#OLS Hedge ratio ----
h = perModel$coefficients[2]
h

dModel$coefficients[2]
model$coefficients[2]


#Same with correlations
rho = sqrt(summary(model)$r.squared)
rho
rhoCheck = cor(duPer,dfPer) #Should be equal to this
rhoCheck


h = rho * sd(u)/sd(f)
h

cor(du,df2)


#Variance tests ----
library(riskR)
risk.hedge(u, f, alpha = c(0.05), beta = 2, p = 2)

library(matrixcalc)
fSqrd = f^2
matr = cov(cbind(u,f))
is.positive.semi.definite(matr)
library(NMOF)
round(minvar(var = matr),6)
twoAsset = minvar(var=cov(cbind(u,f)))
formatC(twoAsset[2]/twoAsset[1], format = "f", digits = 24)


#Check onlyRecent and NotZero ----
#onlyRecent = all[(all$dates == "2021-05-04"),]
onlyRecent = subset(all, dates > "2021-05-04")
plot(xts(onlyRecent$df,onlyRecent$dates))

plot(onlyRecent$f,onlyRecent$u)

notZero = all[dfPer!=0,]
dim(notZero)
plot(xts(notZero$dfPer,notZero$dates))
cor(notZero$duPer,notZero$dfPer)
cor(notZero$du,notZero$df)
cor(notZero$u,notZero$f)
cor(notZero$duPer,lag(notZero$dfPer,k=2))

cor(log(1+notZero$duPer),log(1+notZero$dfPer))



cor(onlyRecent$duPer,onlyRecent$dfPer)
cor(onlyRecent$du,onlyRecent$df)
cor(onlyRecent$u,onlyRecent$f)
plot(onlyRecent$f,onlyRecent$u)


cor(u,f)
cor(du,df)
cor(duPer,dfPer)









#Diagnostic tests
#Test for normality, heteroskedasticity, autocorrelation

#Normality----
#Residuals
res = model$residuals
dRes = dModel$residuals
perRes = perModel$residuals

#Normality of residuals. Do they look normal?
hist(res, breaks=50, main = "Histogram of price residuals")
hist(dRes, breaks=50, main = "Histogram of change residuals")
hist(perRes, breaks=100, main = "Histogram of return residuals")

#Test for normality
library(moments)
jarque.test(res)  #Not normal. The histogram definitely does not look normal
jarque.test(dRes) #Not normal
jarque.test(perRes) #Not normal


#Remedies for normality: (Big sample size CLT, simulating more observations, logarithmic transform)
plot(log(1+duPer), log(1+dfPer))
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


standRes = logRes/(summary(logModel)$sigma)
hist(standRes, breaks=50)
jarque.test(standRes) #it's exactly the same as logRes



#Autocorrelation ----
acf(u)
acf(f)

acf(du,lag.max=252) #Small autocorrelation at t+1
acf(df,lag.max=252) #Some autocorrelation periodically

acf(duPer,lag.max=252) #Small autocorrelation at t+1
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



#Heteroskedasticity ----
library(lmtest)

#Breusch-Pagan (check also with White?)
bptest(perModel)  #p-value = 0.9401.  Can't reject null. We have homoskedasticity

#Correct for heteroskedasticity and autocorrelation (Andrews)
library(sandwich)
robust = coeftest(dModel, vcov=vcovHAC(dModel))
robust

#Multicollinearity
#No multicollinearity if we have only one explanatory variable

#If we have granger causality, we should include both prices and also check for collinearity
#Conduct corrections for some of the problems



#ARMA ----
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
seasonalForecast = forecast(SARIMA, h=252)
plot(seasonalForecast)






#Univariate GARCH ----
library(rugarch)
uniVarSpec = ugarchspec(variance.model=list(garchOrder=c(1,1),
                                      model="sGARCH"),
                        mean.model=list(armaOrder=c(1,1)),
                        distribution.model="norm")

uniGarch = ugarchfit(spec = uniVarSpec, 
                     data = duPer,
                     out.sample=20)
print(uniGarch)
uniGarchForecast = ugarchforecast(uniGarch,
                                  n.ahead=30,
                                  out.sample = 0)
plot(uniGarchForecast, which=3)

#Univariate GARCH hedge


#Test for GARCH residuals
resGarch = as.vector(residuals(uniGarch))
hist(resGarch, breaks =100, freq = FALSE)
curve(dnorm(x,mean(resGarch),sd(resGarch)), add=TRUE)
jarque.test(resGarch)



norma = rnorm(100000)
hist(norma, breaks =100, freq = FALSE)
curve(dnorm(x, mean = mean(norma), sd = sd(norma)), add=TRUE)
jarque.test(norma)


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




#Bivariate GARCH ----
library(rmgarch)
biVarSpec = dccspec(multispec(replicate(3,uniVarSpec)),
                    VAR=T,
                    lag = 5,
                    lag.max = 30,
                    lag.criterion = c("AIC"),
                    dccOrder = c(1,1,1),
                    model = "DCC")



#VAR()

for(n in 1:length(du)){
  lm(var(du) ~ lag(var(du),1) + lag(var(df),1) + lag(cov(du,df),1) +
     lag(residuals(du^2),1) + lag(residuals(df^2),1) + lag(residuals(du*df),1))
  
  lm(cov(du,df) ~ lag(var(du),1) + lag(var(df),1) + lag(cov(du*df),1) +
     lag(residuals(du^2),1) + lag(residuals(df^2),1) + lag(residuals(du*df),1))
}


biGarch = dccfit(spec = biVarSpec, 
                     data = cbind(duPer,dfPer, df2Per),
                     out.sample = 252)
biGarch
biGarchCor = rcor(biGarch)
biGarchCov = rcov(biGarch)
biGarchCov[,,1]


biGarchSD = sigma(biGarch)

biGarchDates = head(dates,length(biGarchCor))
plot(xts(rcor(biGarch)[1,2,],biGarchDates))
plot(xts(rcor(biGarch)[1,3,],biGarchDates))
plot(xts(rcor(biGarch)[2,3,],biGarchDates))


#hist(residuals(biGarch))
dccForecast = dccforecast(biGarch, 190)
plot(dccForecast,which=1)
plot(dccForecast,which=2)
plot(dccForecast,which=3)
plot(dccForecast,which=5)




#Bivariate GARCH hedge ----
forecastCov = rcov(dccForecast)[[1]]
t = forecastCov[,,5]

# #One period
# library(NMOF)
# optimal = minvar(t, 
#                  wmin = -Inf, 
#                  wmax = Inf,
#                  groups = list(1,2:3),
#                  groups.wmin = c(1,-Inf),
#                  groups.wmax = c(1,Inf))
# optimal
# hedgeAsset1 = optimal[2]/optimal[1]
# hedgeAsset2 = optimal[3]/optimal[1]
# hedgeRatios = c(hedgeAsset1,hedgeAsset2)
# hedgeRatios


# #Multiple periods
# library(NMOF)
# optimalTimeVarying = sapply(forecastCov, 
#                             minvar, 
#                             groups = list(1,2:3), 
#                             groups.wmin = c(0.5,0), 
#                             groups.wmax = c(0.5,1))
# 
# forecastLength = dim(forecastCov[,,])[3]
# hedgeRatiosT = c()
# for(n in 1:forecastLength){
#   optimalT = minvar(forecastCov[,,n],
#                     wmin = -Inf, 
#                     wmax = Inf,
#                    groups = list(1,2:3),
#                    groups.wmin = c(1,-Inf),
#                    groups.wmax = c(1,Inf))
#   hedgeAsset1T = optimalT[2]/optimalT[1]
#   hedgeAsset2T = optimalT[3]/optimalT[1]
#   hedgeRatiosT = cbind(hedgeRatiosT,c(hedgeAsset1T,hedgeAsset2T))
# }
# 
# hedgeRatiosM = c()
# for(n in 1:forecastLength){
#   #hedge = lm()$coefficients[2]
# }

hedgeRatiosX = forecastCov[1,2,] / forecastCov[2,2,]

#Hedge ratio formula: general matrix form. Solve is the inverse
triVarHedge = c()
for(n in 1:forecastLength){
  fMatrix = as.matrix(forecastCov[2:3,2:3,n])
  ufMatrix = as.matrix(forecastCov[2:3,1,n])
  invF = solve(fMatrix)
  h = invF %*% ufMatrix
  triVarHedge = cbind(triVarHedge, h)
}


#Hedging effectiveness (out-of-sample) ----

outLength = biGarch@model$modeldata$n.start
duOut = tail(du,outLength)
dfOut = tail(df,outLength)
df2Out = tail(df2,outLength)
fCov = cov(dfOut,df2Out)
ufCov = 0



# test_forecast()
# forecast::accuracy(dccForecast)



triVarHE = c()
for(n in 1:forecastLength){
  fMatrix = as.matrix(cov(dfOut,df2Out)[n])
  ufMatrix = as.matrix(forecastCov[2:3,1,n])
  invF = solve(fMatrix)
  
  uMatrix = forecastCov[1,1,n]
  denominatorMatrix = t(ufMatrix) %*% invF %*% ufMatrix
  HE = denominatorMatrix/uMatrix
  triVarHE = cbind(triVarHE,HE)
}

biVarHE = c()
for(n in 1:forecastLength){
  fMatrix = as.matrix(forecastCov[2,2,n])
  ufMatrix = as.matrix(forecastCov[2,1,n])
  invF = solve(fMatrix)
  
  uMatrix = forecastCov[1,1,n]
  denominatorMatrix = t(ufMatrix) %*% invF %*% ufMatrix
  HE = denominatorMatrix/uMatrix
  biVarHE = cbind(biVarHE,HE)
}

#Hedging effectiveness (in-sample) ----

inSampleSize = dim(biGarchCov)[3]

#TriVarHedge (in-sample)
triVarHedgeIn = c()
for(n in 1:inSampleSize){
  fMatrix = as.matrix(biGarchCov[2:3,2:3,n])
  ufMatrix = as.matrix(biGarchCov[2:3,1,n])
  invF = solve(fMatrix)
  h = invF %*% ufMatrix
  triVarHedgeIn = cbind(triVarHedgeIn, h)
}








triVarHEin = c()
for(n in 1:inSampleSize){
  fMatrix = as.matrix(biGarchCov[2:3,2:3,n])
  ufMatrix = as.matrix(biGarchCov[2:3,1,n])
  invF = solve(fMatrix)
  
  uMatrix = biGarchCov[1,1,n]
  denominatorMatrix = t(ufMatrix) %*% invF %*% ufMatrix
  HE = denominatorMatrix/uMatrix
  triVarHEin = cbind(triVarHEin,HE)
}


biVarHEin = c()
for(n in 1:inSampleSize){
  fMatrix = as.matrix(biGarchCov[2,2,n])
  ufMatrix = as.matrix(biGarchCov[2,1,n])
  invF = solve(fMatrix)
  
  uMatrix = biGarchCov[1,1,n]
  denominatorMatrix = t(ufMatrix) %*% invF %*% ufMatrix
  HE = denominatorMatrix/uMatrix
  biVarHEin = cbind(biVarHEin,HE)
}



#Should have different models, not the same biGarchCov.
#Instead biGarchCov and TriGarchCov
hedgeRatiosInSample = biGarchCov[1,2,] / biGarchCov[2,2,]


inLength = length(du) - outLength
duIn = head(du,inLength)
duInVar = var(duIn)
dfIn = head(df,inLength)
dfInBiOptimal = dfIn * head(hedgeRatiosInSample,inLength)
uf = duIn - dfInBiOptimal
dfInBiVariate = var(duIn-dfInBiOptimal)
HEinBiVar = 1 - dfInBiVariate/duInVar



dfIn2 = head(df2,inLength)
dfInTriVariate = head(df,inLength) * head(triVarHedgeIn[1,],inLength)
df2InTriVariate = head(df2,inLength) * head(triVarHedgeIn[2,],inLength)
#fMatrixVar = cov(dfInTriVariate,dfIn2TriVariate)

#fMatr = cbind(dfIn, dfIn2)
#fOptimalMatr = fMatr * t(triVarHedge)
#fMatrVar = var(fOptimalMatr)
# fCov = cov(dfIn,dfIn2)
# ufCov = 0
ufMatr = duIn - dfInTriVariate - df2InTriVariate
ufVar = var(dfMatr)
HEinTriVar = 1 - ufVar / duInVar

cat(HEinBiVar, HEinTriVar)
#HEinTriVar slightly worse?????




#Hypothesis testing ----
biVarHEMean = mean(biVarHE)
triVarHEMean = mean(triVarHE)
triVarHEMean-biVarHEMean


#T-test in
t.test(triVarHEin,biVarHEin)
#They are different


#T-test in
t.test(ufMatr,uf)
#They are not statistically different


#T-test out
t.test(triVarHE,biVarHE)
#They are different




#F-test for variances
var.test(uf, ufMatr)
#They are not statistically different



# library(car)
# linearHypothesis()

#Information criteria
# round(AIC(dModel, model, logModel),2)
# round(BIC(dModel, model, logModel),2)

#Adjusted R squared
# round(c(summary(dModel)$adj.r.squared, summary(model)$adj.r.squared, summary(logModel)$adj.r.squared), 4)


#Report models
library(stargazer)




