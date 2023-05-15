#if any code is not working use "dev.off()"
dev.off()
#Install the required packages and load them.
install.packages("quantmod")
install.packages("shiny")
install.packages("forecast")
installed.packages("tseries")
install.packages("timeSeries")
install.packages("zoo")
install.packages("xts")
library(quantmod)
library(shiny)
library(forecast)
library(tseries)
library(timeSeries)
library(zoo)
library(xts)
#To get the data for any share listed on NSE stock exchange
a=getSymbols('TCS.NS', from= '2016-12-31', to='2021-12-31')
#We need to further confirm the data is form xts and zoo for time series.
class(TCS.NS)
#Filtering the data to obtain only the necessary column
TCS.NS_CLose_Prices = TCS.NS[,4]
#To get a raw idea of how the stock has performed
#over the past few years we can chart it can get a better insight.
plot(TCS.NS_CLose_Prices)
class(TCS.NS_CLose_Prices)
#For further graphs we can use par to split the screen into sections.
par(mfrow=c(1,2))
#Calculation of ACF is necessary as it estimates of the autocovariance or autocorrelation function.
acf(TCS.NS_CLose_Prices,main="ACF for Differenced Series")
# Calculation of PACF is necessary for finding the partial auto correlation between the series and lags.
pacf(TCS.NS_CLose_Prices,main="PACF for Differenced Series")
#test value
print(adf.test(TCS.NS_CLose_Prices))
kpss.test(TCS.NS_CLose_Prices, null="Trend")
auto.arima(TCS.NS_CLose_Prices,seasonal=FALSE) #AICc=12272.75   BIC=12282

fitA=auto.arima(TCS.NS_CLose_Prices,seasonal = FALSE)
tsdisplay(residuals(fitA),lag.max = 40,main='(Auto Arima)Model Residuals')
dev.off()
fitB=arima(TCS.NS_CLose_Prices,order = c(1,2,3))
tsdisplay(residuals(fitB),lag.max = 40,main='(1,2,3)Model Residuals')

fitC=arima(TCS.NS_CLose_Prices,order = c(1,1,1))
tsdisplay(residuals(fitC),lag.max = 40,main="(1,1,1)Model Residuals")

fitD=arima(TCS.NS_CLose_Prices,order = c(1,4,2))
tsdisplay(residuals(fitD),lag.max = 40,main="(1,4,2)Model residuals")

fitE=arima(TCS.NS_CLose_Prices,order = c(3,2,1))
tsdisplay(residuals(fitD),lag.max = 40,main="(3,2,1)Model residuals")

fitF=arima(TCS.NS_CLose_Prices,order = c(5,2,3))
tsdisplay(residuals(fitE),lag.max = 40,main="(5,2,1)Model residuals")

fitG=arima(TCS.NS_CLose_Prices,order = c(3,2,5))
tsdisplay(residuals(fitF),lag.max = 40,main="(3,2,5)Model residuals")

fitH=arima(TCS.NS_CLose_Prices,order = c(2,3,1))
tsdisplay(residuals(fitG),lag.max = 40,main="(2,4,1)Model residuals")

#plotting
par(mfrow=c(2,2))
term=90
fcast1=forecast(fitA,h=term)
plot(fcast1)
fcast2=forecast(fitB,h=term)
plot(fcast2)
fcast3=forecast(fitC,h=term)
plot(fcast3)
fcast4=forecast(fitD,h=term)
plot(fcast4)
fcast5=forecast(fitE,h=term)
plot(fcast5)
fcast6=forecast(fitF,h=term)
plot(fcast6)
fcast7=forecast(fitG,h=term)
plot(fcast7)
fcast8=forecast(fitH,h=term)
plot(fcast8)

accuracy(fcast1) #98.862839
accuracy(fcast2) #98.864638
accuracy(fcast3) #98.861027
accuracy(fcast4) #98.541956
accuracy(fcast5) #98.863145
accuracy(fcast6) #98.861908
accuracy(fcast7) #98.868448
accuracy(fcast8) #98.644234
max(98.862839,98.864638,
    98.861027,98.541956,
    98.863145,98.861908,
    98.868448,98.644234)
save.image("C:/USERP/R/Arima kaggle.RData")
TCS.NS - fit$residual
fit=arima(TCS.NS_CLose_Prices,order=c(3,2,5),seasonal = c(3,2,5))
fit
View(TCS.NS_CLose_Prices - fit$residual)
#test
b=getSymbols('TCS.NS', from= '2021-12-31', to='2022-03-31')
