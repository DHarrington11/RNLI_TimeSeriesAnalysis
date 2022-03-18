#=========Data=========================
library(tseries)
data = read.csv("D:\\RNLI_Callouts.csv", header=T)
callouts= data$value
plot(comp_ts, ylab="No. Callouts",
     type='l',
     xlab="Time",
     main="RNLI Callouts UK&Ireland", lwd=3)
comp_ts = ts(callouts, start=2008, frequency=12)
##============Train/Test Sets====================
length(callouts)
callouts_train = callouts[1:120]
callouts_test = callouts[120:155]
length(callouts_test)

##==============Look at Data ====================
rnli = ts(callouts_train, start=2008, frequency=12)
rnli_test = ts(callouts_test, start=2018, frequency = 12)
plot(decompose(rnli))#->use*****
plot(rnli, ylab="No. Callouts",
     xlab="Time",
     main="RNLI Callouts UK&Ireland", lwd=3)
adf.test(rnli, alternative="stationary")
?adf.test
#==============ACF & PACF=========================
par(mfrow=c(4,2))

acf(rnli)
pacf(rnli)
rnli.d=diff(rnli)
acf(rnli.d)
pacf(rnli.d)
rnli.d1 = diff(rnli, lag=12)
acf(rnli.d1)
pacf(rnli.d1)
rnli.d2 = diff(rnli.d,lag=12) #d=2
acf(rnli.d2)
pacf(rnli.d2)
#=========================================
arima(rnli, order=c(0,1,0), seasonal=list(order=c(0,1,0), period=12))#aic 1356.48
arima(rnli, order=c(1,0,0), seasonal=list(order=c(0,0,0), period=12))#AIC 1362.29
arima(rnli, order=c(0,0,0), seasonal=list(order=c(0,1,0), period=12))# AIC 1307.19

#looking at AR components => PACF 
arima(rnli, order=c(1,0,0), seasonal=list(order=c(1,1,0), period=12)) #AIC 1346.39
arima(rnli, order=c(1,0,0), seasonal=list(order=c(2,1,0), period=12))#AIC 1331.33
arima(rnli, order=c(2,0,0), seasonal=list(order=c(2,1,0), period=12))#AIC 1332.56

#Looking at MA components => ACF 
arima(rnli, order=c(0,0,0), seasonal=list(order=c(0,1,2), period=12)) #AIC 1316.64
arima(rnli, order=c(0,0,1), seasonal=list(order=c(0,1,1), period=12)) #AIC 1315.29
arima(rnli, order=c(0,0,1), seasonal=list(order=c(0,1,2), period=12)) #AIC 1316.78
arima(rnli, order=c(0,0,2), seasonal=list(order=c(0,1,2), period=12)) #AIC 1318.56
#Combining 
model = arima(rnli, order=c(1,0,1), seasonal=list(order=c(2,1,2))) #AIC 1318.17
model1 = arima(rnli, order=c(2,0,1), seasonal=list(order=c(2,1,2))) #AIC 1318.17
tsdiag(model)
?tsdiag
#=================Residuals==================
par(mfrow=c(1,3))
cpgram(rnli.d)

cpgram(rnli.d1)

cpgram(rnli.d2)

cpgram(residuals(model), main = "SARIMA(1,0,1)x(2,1,2)")
Box.test(residuals(model), lag=1, type="Ljung-Box")
Box.test(residuals(model), lag=4, type="Ljung-Box")
#===============Forecasts==================
rnli.pred = predict(model, n.ahead = 36)
preds = c(rnli.pred$pred)

par(mfrow=c(2,1))
plot(rnli, xlim = c(2008,2021),
     type='l',
     ylab = 'No. Callouts',
     main="RNLI Callouts IrL&GB",
     lwd=2)
lines(rnli.pred$pred,col="red",lwd=1)
#lines(rnli_test,col="blue",lwd=2)
lines(rnli.pred$pred+2*rnli.pred$se,col="green",lty=3)


lines(rnli.pred$pred-2*rnli.pred$se,col="blue",lty=3)
legend("topleft",
       c("Predictions", "Upper Bound","Lower Bound" ),
       fill=c("red", "green", "blue"))

plot(rnli, xlim = c(2008,2021),
     type='l',
     ylab = 'No. Callouts',
     main="RNLI Callouts IrL&GB",
     lwd=2)
lines(rnli.pred$pred,col="red",lwd=1)
lines(rnli_test,col="blue",lwd=2)
legend("topleft",
       c("Predictions","True Values"),
       fill=c("red","blue"))

plot(rnli_test,col="blue",lwd=2,ylim=c(300,1400),main="RNLI Callouts True Values 2018-2021",xlab="Year",ylab="Callouts")
lines(rnli.pred$pred+2*rnli.pred$se,col="green",lty=3)
lines(rnli.pred$pred-2*rnli.pred$se,col="red",lty=3)
legend("topleft",
       c("True Value","Preds. Upper Bound", "Preds. Lower Bound"),
       fill=c("blue","green","red"))
#==============Loss===========================
preds = c(rnli.pred$pred)
actual = c(rnli_test)
cor(preds, actual)
difference = round(rnli.pred$pred) - rnli_test
cbind(rnli.pred$pred,rnli_test,difference)
max(abs(difference))
sqrt(mean((difference**2)/length(difference)))
#MAY 2020
#December 2019 
#Investigate 
weather_data = read.csv("D:\\MLY1075.csv",header=T)
wind = weather_data$wdsp
wind_data = ts(wind[41:196],start=2008,frequency=12)
wind_data
plot(wind_data, xlim=c(2008,2021),
     ylab="Wind Speed",
     xlab="Time")
temp = weather_data$maxtp
temp_data = ts(temp[41:196], start=2008, frequency=12)
Dec = c(12,24,36,48,60,72,84,96,108,120,132,144,156)
May = c(5,17,29,41,53,65,77,89,101,113,125,137,149)
#==============May2020 Investigation===================
plot(temp_data[May],
     ylab="Average Temperature",
     xlab="Year",
     lwd=2,
     type='l',
     main="Average Temperature in May @Roches Point"
     )
plot(wind[May], 
      lwd=2, col="red", type='ol', main="Average Wind Speed",
     ylab="Wind Speed", xlab="Years")
#=================Dec 2019 Investigation===============
plot(temp_data[Dec], 
     ylab="avergae Temperature",
     xlab="Year",
     lwd=2,
     type="ol")
plot(wind_data[Dec],
     ylab="Wind Speed",
     xlab="Year",
     lwd=2,
     type="ol", 
     main="Wind Speed")
gusts = weather_data$maxgt
gust_ts= ts(gusts[41:196], start=2008, frequency=12)
plot(gust_ts[Dec],
     ylab= "Max Wind Speed (Knots)",
     xlab="Year",
     lwd=2,
     type="ol", 
     main="Yearly Max wind Speed in December @ Roches Point")
abline(v=12)

par(mfrow=c(2,1))
weather_meant = ts(weather_data$meant[41:196], start=2008, frequency=12)
plot(comp_ts/max(comp_ts), lwd=2, type='l',
     ylab=" ",
     xlab="Year",
     main="Mean Temperature vs RNLI Callouts")
lines(weather_meant/max(weather_meant),
      col="blue", 
      lwd=1.5,
      lty=2)
legend("topleft",
       c("Temp","Callouts"),
       fill=c("blue","black"))

plot(comp_ts/max(comp_ts), lwd=2, type='l',
     ylab = "",
     xlab="Year",
     main="Mean Wind vs RNLI Callouts")
lines(wind_data/max(wind_data), col="blue", lwd=1.5, lty=2)
legend("topleft",
       c("Wind","Callouts"),
       fill=c("blue","black"))
wind_data[is.na(wind_data)] <- 0

plot(comp_ts/max(comp_ts), lwd=2, type='l',
     ylab = "",
     xlab="Year",
     main="Max Gusts vs RNLI Callouts")
lines(gust_ts/max(gust_ts), col="blue", lwd=1.5, lty=2)
