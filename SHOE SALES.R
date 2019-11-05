library(tseries)
library(forecast)
shoesales=read.csv(file.choose())
shoesales=shoesales$No..of.Pairs
class=ts(shoesales,start = c(2011,1),end = c(2015,12),frequency = 12)
class
plot(class)
abline(reg=lm(class~time(class)))
kpss.test(diff(diff(class)))
plot(diff(diff(class)))
abline(h=0)
pacf(diff(diff(log(class))))
acf(diff(diff(log(class))))
model=arima(log(class),c(4,2,1),seasonal = list(order=c(1,1,1),period=12))
model
predict=predict(model,n.ahead =5)
predictf=2.718^predict$pred
predictf
accuracy(predictf,model)

#Simple Exponential Smoothing
modelses=HoltWinters(class,beta = F, gamma = F)
modelses
fses=forecast(modelses,5)
fses
plot(fses)
accuracy(fses)
shapiro.test(fses$residuals)
Box.test(fses$residuals,type = "Ljung-Box")


#Holt Trend Model
modeltrend=HoltWinters(class,gamma = F)
modeltrend
ftrend=forecast(modeltrend,5)
ftrend
plot(ftrend)
accuracy(ftrend)

#Holtseasonal Model
modelseason=HoltWinters(class)
modelseason
fseason=forecast(modelseason,5)
fseason
plot(fseason)
accuracy(fseason)
seasonplot(class,col=rainbow(12))
decomp=decompose(class,type = "multiplicative")
decomp
plot(decomp)
plot(class,lwd=2)
lines(decomp$trend,col=2,lwd=2)
lines(decomp$seasonal+decomp$random,col=3,lwd=2)
