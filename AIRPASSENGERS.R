AirPassengers
AirPassengers=data.frame(AirPassengers)
AirPassengers
AirPassengers1=AirPassengers[-c(1:84),]
#Exponential Smoothing for Airpassengers -- last five years
fiveses=ts(AirPassengers1,start = c(1956,1),end = c(1960,12),frequency = 12)
fiveses
airpass=HoltWinters(fiveses,beta = F,gamma = F)
plot(airpass)
fairpass=forecast(airpass,5)
plot(fairpass)
accuracy(fairpass)
shapiro.test(fairpass$residuals)
Box.test(fairpass$residuals,type = "Ljung-Box")
#ARIMA for Airpassengers -- last five years
library(tseries)
plot(fiveses)
abline(reg=lm(fiveses~time(fiveses)))
plot(diff(log(fiveses)))
pacf(diff(log(fiveses)))
acf(diff(log(fiveses)))
model=arima(log(fiveses),c(1,1,1),seasonal =list(order=c(1,1,1),period=12))
predict=predict(model,n.ahead = 12*1)
predictf=2.718^predict$pred
predictf
accuracy(model)