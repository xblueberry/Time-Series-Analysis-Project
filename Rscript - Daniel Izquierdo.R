rm(list=ls())

#libraries
library(readxl)
library(CADFtest)
library(forecast)
library(vars)
library(urca)
library(xtable)

#Import data
Trade<- read_excel("C:/Users/Daniel/Desktop/Trade_of_Goods.xlsx")

attach(Trade)

####UNIVARIATE ANALYSIS####

#Declare time series
exp_ts<-ts(`Goods, Value of Exports, National Currency`,frequency=4,start=c(1960,1))
exp_ts
ts.plot(exp_ts,ylab="Value of exports")

#log-differences
logexp_ts<-log(exp_ts)
ts.plot(logexp_ts)
dlogexp_ts<-diff(log(exp_ts))
ts.plot(dlogexp_ts,ylab="diff (log (Value of exports))")
acf(dlogexp_ts)

#correct for seasonality
monthplot(dlogexp_ts, ylab="diff (log (Value of exports))")
dslogexp_ts<-diff(diff(log(exp_ts)),lag=4)
ts.plot(dslogexp_ts,ylab="diff_season (diff (log (Value of exports)))")

#Seems like a structural break: subset 1990-2017

detach(Trade)
Trade<-Trade[121:230,]
attach(Trade)

exp_ts<-ts(`Goods, Value of Exports, National Currency`,frequency=4,start=c(1990,1))
ts.plot(exp_ts,ylab="Value of exports")

logexp_ts<-log(exp_ts)
ts.plot(logexp_ts)
#test if deterministic or stochastic trend
max.lag<-round(sqrt(length(exp_ts))) #10
CADFtest(logexp_ts, type= "trend", criterion= "BIC", max.lag.y=max.lag)

dlogexp_ts<-diff(log(exp_ts))
ts.plot(dlogexp_ts,ylab="diff (log (Value of exports))")

#correct for seasonality
monthplot(dlogexp_ts,ylab="diff (log (Value of exports))")
seasonplot(dlogexp_ts,ylab="diff (log (Value of exports))",main="")
dslogexp_ts<-diff(diff(log(exp_ts)),lag=4)
ts.plot(dslogexp_ts,ylab="diff_season (diff (log (Value of exports)))")

#unit root test
max.lag<-round(sqrt(length(dslogexp_ts))) #10
CADFtest(dslogexp_ts, type= "drift", criterion= "BIC", max.lag.y=max.lag)

#correlograms
acf(dslogexp_ts,main="")
pacf(dslogexp_ts,main="")


#SARIMA modeling 
max.lag<-round(sqrt(length(logexp_ts)))

#MA model
fit_ma<-arima(logexp_ts,order=c(0,1,0),seasonal=list(order=c(0,1,1)))
fit_ma
acf(fit_ma$residuals)              
pacf(fit_ma$residuals)
Box.test(fit_ma$residuals,lag=max.lag,type="Ljung-Box")
BIC(fit_ma)

#AR model
fit_ar<-arima(logexp_ts,order=c(0,1,0),seasonal=list(order=c(3,1,0)))
fit_ar
acf(fit_ar$residuals)              
pacf(fit_ar$residuals)
Box.test(fit_ar$residuals,lag=max.lag,type="Ljung-Box")
BIC(fit_ar)

#ARMA model
fit_arma<-arima(logexp_ts,order=c(0,1,0),seasonal=list(order=c(2,1,1)))
fit_arma
acf(fit_arma$residuals)              
pacf(fit_arma$residuals)
Box.test(fit_arma$residuals,lag=max.lag,type="Ljung-Box")
BIC(fit_arma)


#Prediction

#ARMA model

myforecastARMA<-predict(fit_arma,n.ahead=8); myforecastARMA
expected<-myforecastARMA$pred; expected

#CI
lower<-myforecastARMA$pred-qnorm(0.975)*myforecastARMA$se
upper<-myforecastARMA$pred+qnorm(0.975)*myforecastARMA$se

ts.plot(logexp_ts,xlim=c(2005,2020),ylim=c(10,12),ylab="log (Value of exports)")
lines(expected,col="red")
lines(lower,col="blue")
lines(upper,col="blue")

#MA model

myforecastMA<-predict(fit_ma,n.ahead=8)
expected<-myforecastMA$pred
lower<-myforecastMA$pred-qnorm(0.975)*myforecastMA$se
upper<-myforecastMA$pred+qnorm(0.975)*myforecastMA$se
cbind(lower,expected,upper)
ts.plot(logexp_ts,xlim=c(2005,2020),ylim=c(10,12))
lines(expected,col="red")
lines(lower,col="blue")
lines(upper,col="blue")

#Compare forecasts: MAE and RMSE of best 2 BIC models
y<-logexp_ts
S=round(0.75*length(y))
h=1
error1.h<-c() #SARIMA(0,1,0)(0,1,1)
for (i in S:(length(y)-h))
{
  mymodel.sub<-arima(y[1:i], order = c(0,1,0),seasonal=c(0,1,1))
  predict.h<-predict(mymodel.sub,n.ahead=h)$pred[h]
  error1.h<-c(error1.h,y[i+h]-predict.h)
}
error2.h<-c() #SARMA(0,1,0)(2,1,1)
for (i in S:(length(y)-h))
{
  mymodel.sub<-arima(y[1:i], order = c(0,1,0),seasonal=c(2,1,1))
  predict.h<-predict(mymodel.sub,n.ahead=h)$pred[h]
  error2.h<-c(error2.h,y[i+h]-predict.h)
}
#MAE
MAE1 <- mean(abs(error1.h)); MAE1 #SARIMA(0,1,0)(0,1,1)
MAE2 <- mean(abs(error2.h)); MAE2 #SARMA(0,1,0)(2,1,1)

#RMSE
sqrt(mean(error1.h^2))
sqrt(mean(error2.h^2))

#MAPE
rerror.h<-c() 
for (i in S:(length(y)-h))
{
  mymodel.sub<-arima(y[1:i], order = c(0,1,0),seasonal=c(2,1,1)) #choose model
  predict.h<-predict(mymodel.sub,n.ahead=h)$pred[h]
  rerror.h<-c(rerror.h, (y[i+h]-predict.h)/y[i+h])
}
MAPE <- mean(abs(rerror.h)); MAPE 

#Debold-Mariano test
dm.test(error1.h,error2.h,h=h,power=1)
dm.test(error1.h,error2.h,h=h,power = 2)



#####MULTIVARIATE ANALYSIS####

#import TS
imp_ts<-ts(`Goods, Value of Imports, CIF, National Currency`,frequency=4,start=c(1990,1))
ts.plot(exp_ts,imp_ts,col=c("blue","red"))
legend("bottomright",legend=c("Exports","imports"),col=c("blue","red"),lty = c(1,1))

#log-differences
logimp_ts<-log(imp_ts)
ts.plot(logexp_ts,logimp_ts,col=c("blue","red"))
dlogimp_ts<-diff(log(imp_ts))
ts.plot(dlogexp_ts,dlogimp_ts,col=c("blue","red"))

#correct for seasonality
monthplot(dlogimp_ts)
dslogimp_ts<-diff(diff(log(imp_ts)),lag=4)
ts.plot(dslogexp_ts,dslogimp_ts,col=c("blue","red"))
legend("bottomright",legend=c("Exports","imports"),col=c("blue","red"),lty = c(1,1))

#unit root test for imports
max.lag<-round(sqrt(length(dslogimp_ts))) #10
CADFtest(dslogimp_ts, type= "drift", criterion= "BIC", max.lag.y=max.lag)

#test I(1)
CADFtest(dlogexp_ts, type= "drift", criterion= "BIC", max.lag.y=max.lag)
CADFtest(dlogimp_ts, type= "drift", criterion= "BIC", max.lag.y=max.lag)


#Linear regression

fit<-lm(dslogexp_ts~dslogimp_ts)
summary(fit)
xtable(fit)
ts.plot(fit$residuals,ylab="Residuals",xlab="Index")
acf(fit$residuals,main="")
pacf(fit$residuals)
Box.test(fit$residuals, lag = max.lag, type = "Ljung-Box")


#DL(4)

lag <- 4
n <- length(dslogexp_ts)
dslogexp.0 <- dslogexp_ts[(lag+1):n]
dslogimp.0 <- dslogimp_ts[(lag+1):n]
dslogimp.1 <- dslogimp_ts[lag:(n-1)]
dslogimp.2 <- dslogimp_ts[(lag-1):(n-2)]
dslogimp.3 <- dslogimp_ts[(lag-2):(n-3)]
dslogimp.4 <- dslogimp_ts[(lag-3):(n-4)]
fit_dlm <- lm(dslogexp.0 ~ dslogimp.0+dslogimp.1+dslogimp.2+dslogimp.3+dslogimp.4)
summary(fit_dlm)
ts.plot(fit_dlm$residuals,ylab="Residuals")
acf(fit_dlm$residuals,main="")
Box.test(fit_dlm$residuals, lag = max.lag, type = "Ljung-Box")


#ADL(4)

dslogexp.1 <- dslogexp_ts[lag:(n-1)]
dslogexp.2 <- dslogexp_ts[(lag-1):(n-2)]
dslogexp.3 <- dslogexp_ts[(lag-2):(n-3)]
dslogexp.4 <- dslogexp_ts[(lag-3):(n-4)]
fit_adlm <- lm(dslogexp.0 ~ dslogexp.1+dslogexp.2+dslogexp.3+dslogexp.4
               +dslogimp.1+dslogimp.2+dslogimp.3+dslogimp.4)

summary(fit_adlm)
ts.plot(fit_adlm$residuals,ylab="Residuals")
acf(fit_adlm$residuals,main="")
Box.test(fit_adlm$residuals, lag = max.lag, type = "Ljung-Box")


#Granger-Causality
fit_adlm_nox <- lm(dslogexp.0 ~ dslogexp.1+dslogexp.2+dslogexp.3+dslogexp.4)
anova(fit_adlm,fit_adlm_nox)
xtable(anova(fit_adlm,fit_adlm_nox))


#VAR

dslogTrade<-data.frame(dslogexp_ts,dslogimp_ts)
names(dslogTrade)<-c("dslogEXP","dslogIMP")
attach(dslogTrade)

VARselect(dslogTrade,lag.max=10,type="const")

#VAR(1)
fit_var1<-VAR(dslogTrade,type="const",p=1)
summary(fit_var1)

#VAR(4)
fit_var4<-VAR(dslogTrade,type="const",p=4)
summary(fit_var4)
var4_residuals<-resid(fit_var4)
acf(var4_residuals[,1],main="dslogEXP")
acf(var4_residuals[,2],main="dslogIMP")
ccf(var4_residuals[,1],var4_residuals[,2],main="")

test_bg<- serial.test(fit_var4, lags.pt=10, type = "BG"); test_bg

#impulse response function
irf_var4<-irf(fit_var4,ortho=FALSE,boot=TRUE)
plot(irf_var4)

#VAR prediction
myforecastVAR<-predict(fit_var4,n.ahead=8)
## dslogEXP
dslogEXP_ts_forecast<-ts(myforecastVAR$fcst$dslogEXP[,1], frequency=4,start=c(2017,3))
dslogEXP_ts_lower<-ts(myforecastVAR$fcst$dslogEXP[,2],frequency=4,start=c(2017,3))
dslogEXP_ts_upper<-ts(myforecastVAR$fcst$dslogEXP[,3],frequency=4,start=c(2017,3))
ts.plot(dslogEXP,dslogEXP_ts_forecast,dslogEXP_ts_lower,dslogEXP_ts_upper,col=c("black","blue","red","red"),
        xlim=c(2005,2020),ylab="dslogEXP")
## dslogIMP
dslogIMP_forecast<-ts(myforecastVAR$fcst$dslogIMP[,1],frequency=4,start=c(2017,3))
dslogIMP_lower<-ts(myforecastVAR$fcst$dslogIMP[,2],frequency=4,start=c(2017,3))
dslogIMP_upper<-ts(myforecastVAR$fcst$dslogIMP[,3],frequency=4,start=c(2017,3))
ts.plot(dslogIMP,dslogIMP_forecast,dslogIMP_lower,dslogIMP_upper,col=c("black","blue","red","red"), 
        xlim=c(2005,2020),ylab="dslogIMP")


#Cointegration: Engle-Granger
coint1 <- lm(logexp_ts ~ logimp_ts)
summary(coint1)
plot.ts(coint1$res)
CADFtest(coint1$res, type="drift", criterion="BIC", max.lag.y=10)
coint2 <- lm(logimp_ts ~ logexp_ts)
summary(coint2)
plot.ts(coint2$res)
CADFtest(coint2$res, type="drift", criterion="BIC", max.lag.y=10)

#Cointegration: johansen test
logTrade<-data.frame(logexp_ts,logimp_ts)
names(logTrade)<-c("logEXP","logIMP")

VARselect(logTrade,lag.max=10,type="const",season = 4)

trace_test<-ca.jo(logTrade,type="trace",K=5,ecdet="const",spec="transitory",season=4)
summary(trace_test)
maxeigen_test<-ca.jo(logTrade,type="eigen",K=5,ecdet="const",spec="transitory",season=4)
summary(maxeigen_test)


#Persistence in volatility?
library(fGarch)
acf((fit_ma$residuals)^2)
acf((fit_ar$residuals)^2)
acf((fit_arma$residuals)^2)

