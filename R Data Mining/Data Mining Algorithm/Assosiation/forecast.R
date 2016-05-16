library(tseries)
air<-AirPassengers
ts.plot(air)
acf(air)
pacf(air)

x<-decompose(air)
plot(x)
plot(x$seasonal)
air.fit <- arima(air,order=c(0,1,1), seasonal=list(order=c(0,1,1), period=12))
tsdiag(air.fit)
library(forecast)

air.forecast <- forecast(air.fit,12)
summary(air.forecast)
plot.forecast(air.forecast)



require(graphics)
(m<-HoltWinters(co2))
plot(m)
plot(fitted(m))

library(forecast)
library(tseries)
ohhist <- read.csv("ohhist.csv", sep = ",")
ts.plot(ohhist)
auto.arima(ohhist,ic=c('bic'))
ohhista<- arima(ohhist,order=c(0,0,0))
ohhistf <- forecast.Arima(ohhista,6)
ohhistf



