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

u = unlist(omitted[,2])
f = unlist(omitted[,3])
dates = omitted$Date

change = lapply(as.vector(omitted[,2:3]), diff, lag=1)
du = unlist(change[1])
df = unlist(change[2])

library(xts)
plot(xts(u,dates))
plot(xts(f,dates))


#diff(as.matrix(omitted[,3],lag=1))
#uChange = diff(omitted$Price,lag=1)


#Test for normality, heteroskedasticity, autocorrelation
#Multicollinearity? Prob not




#OLS
plot(u, f)
summary(lm(u ~ f))

plot(du, df)
model = summary(lm(du ~ df))
summary(lm(df ~ du)) #flipped

#OLS correlation estimate
rho = model$coefficients[2,1]

#OLS Hedge
h = rho * sd(df)/sd(du)
h


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


