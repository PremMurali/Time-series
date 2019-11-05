setwd("C:/Workspace/Data")
library(tseries)
library(forecast)
plot(AirPassengers)
AirPassengers
class(AirPassengers)
abline(reg=lm(AirPassengers~time(AirPassengers)))
kpss.test(AirPassengers)
plot(log(AirPassengers))
plot(diff(log(AirPassengers)))
abline(h=0)
kpss.test(diff(log(AirPassengers)))
acf(diff(log(AirPassengers)))
pacf(diff(log(AirPassengers)))
model=arima(log(AirPassengers),c(2,1,1),seasonal = list(order=c(2,1,1),period=12))
pred=predict(model,n.ahead = 12*1)
predf=2.718^pred$pred
predf
