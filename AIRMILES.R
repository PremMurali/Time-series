shoesales=read.csv(file.choose())
shoesales=shoesales$No..of.Pairs
library(tseries)
library(forecast)
shoesales=ts(shoesales,start = c(2011,1),end = c(2015,12), frequency = 12)
class(shoesales)
plot(shoesales)
kpss.test(shoesales)
abline(reg = lm(shoesales~time(shoesales)))
kpss.test(shoesales)
plot(diff(log(shoesales)))
kpss.test(diff(log(shoesales)))
pacf(diff(log(shoesales)))
acf(diff(log(shoesales)))
model=arima(log(shoesales),c(0,1,0),seasonal =list(order=c(0,1,0),period=12))
predict=predict(model,n.ahead = 12*1)
predictf=2.718^predict$pred
predictf
accuracy(model)

class(nhtemp)
plot(nhtemp)
modelses=HoltWinters(nhtemp,beta = F, gamma = F)
modelses
plot(modelses)
fses=forecast(modelses,5)
fses
plot(fses)
accuracy(fses)
shapiro.test(fses$residuals)
Box.test(fses$residuals,type = "Ljung-Box")

#Simple Exponential Smoothing for airpassengers
sesairpass=HoltWinters(AirPassengers,beta=F,gamma = F)
plot(sesairpass)
fairpass1=forecast(sesairpass,12)
plot(fairpass)
fairpass
accuracy(fairpass)
shapiro.test(fairpass$residuals)
Box.test(fairpass$residuals,type = "Ljung-Box")

#Holts Trend Model
plot(airmiles)
modeltrend=HoltWinters(airmiles,gamma = F)
modeltrend
plot(modeltrend)
ftrend=forecast(modeltrend,5)
plot(ftrend)
accuracy(ftrend)

AirPassengers
airtrend=HoltWinters(AirPassengers,gamma = F)
airtrend
plot(airtrend)
fairtrend=forecast(airtrend,12)
plot(fairtrend)
accuracy(fairtrend)

AirPassengers1=data.frame(AirPassengers)
AirPassengers
AirPassengers1=AirPassengers1[-c(1:84),]
Airholt=ts(AirPassengers1,start = c(1956,1),end = c(1960,12),frequency = 12)
Airholt1=HoltWinters(Airholt,gamma = F)
plot(Airholt1)
Airforecast=forecast(Airholt1,12)
plot(Airforecast)
accuracy(Airforecast)

#Holt Winter seasonal Model
airseason=HoltWinters(AirPassengers)
airseason
plot(airseason)
fairseason=forecast(airseason,12)
plot(fairseason)
accuracy(fairseason)
airseason1=HoltWinters(Airholt)
plot(airseason1)
fairseason1=forecast(airseason1,12)
plot(fairseason1)
accuracy(fairseason1)
seasonplot(AirPassengers,col=rainbow(12))
decom=decompose(AirPassengers)
decom
plot(decom)
plot(AirPassengers,lwd=2)
lines(decom$trend+decom$random,col=2,lwd=2)
lines(decom$trend,col=3,lwd=2)
lines(decom$seasonal,col=5,lwd=3)
