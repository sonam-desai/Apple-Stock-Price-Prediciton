##### PROJECT : Prediction of Apple Stock Price
    
library(tseries)
library(TSA)
library(lmtest)

#Read Apple Stock Data from file
rm(list=ls())
data = read.csv("C://Users//sonam//OneDrive - San Diego State University (SDSU.EDU)//Sem 2//TSAF//Project//APPLE-3.csv")
head(data)
n = data$Adj.Close
Date = as.Date(data$Date, tryFormats = "%m/%d/%Y") 

##Time Series Plot
plot(Date, n, type="l",main="Apple Stock price")
x = ts(n)
plot(Date, x, type="l")
acf(x)
pacf(x)

##Log Transformation
logx = log(x)
plot(Date,logx, type="l",main="log of Stock price")
acf(logx)
pacf(logx)
adf.test(logx)

##Difference of Log Transformation
y=diff(logx)
plot(y, type="l",main="diff of log of Stock price")
adf.test(y)
acf(y)
pacf(y)
eacf(y)

######## MODEL SELECTION ########
######### ARMA(2,2) Model 
y.arma22=arima(y, order = c(2, 0, 2))
y.arma22
coeftest(y.arma22)

#Box-Ljung Test
Box.test(y.arma22$residuals, lag = 12, type = 'Ljung')
aic22 = y.arma22$aic
aic22

# stationarity Check
polyroot(c(1, -y.arma22$coef[1:2]))
abs(polyroot(c(1, -y.arma22$coef[1:2]))) #roots are outside the unit cycle. greater than 1.


######## ARMA(1,9) Model
y.arma19 = arima(y, order = c(1, 0, 9))
y.arma19
coeftest(y.arma19) #####z test for the coefficients

#Box-Ljung Test
Box.test(y.arma19$residuals, lag = 12, type = 'Ljung')

# stationarity Check
polyroot(c(1, -y.arma19$coef[1]))
abs(polyroot(c(1, -y.arma19$coef[1])))

aic19 = y.arma19$aic
aic19

####### ARMA(3,3) Model
y.arma33=arima(y, order = c(3, 0, 3))
y.arma33
coeftest(y.arma33)
y.arma33=arima(y, order = c(3, 0, 3), fixed = c(NA,0,0,NA,0,0), include.mean = F)
y.arma33

Box.test(y.arma33$residuals, lag = 12, type = 'Ljung')

aic22 = y.arma33$aic
aic22


### Rolling Forecasting
source("C://Users//sonam//OneDrive - San Diego State University (SDSU.EDU)//Sem 2//TSAF//Project//rolling.forecast.R")
rolling.forecast(y, 5, length(n)-50, c(1,0,9))
rolling.forecast(y, 5, length(n)-50, c(2,0,2))
rolling.forecast(y, 5, length(n)-50, c(3,0,3))

###Prediction for returns - ARMA(1,9) Model
pp = predict(y.arma19, 5)
nn = length(y)	#length of your data
nt = 5	#forecast horizon
nb = 30	#number of data points you want to plot
tt = (nn-nb):nn	#indexes of data points you want to plot
xxx = y[tt]		#data you want to plot
rr = range(c(xxx, pp$pred+2*pp$se, pp$pred-2*pp$se))	#find the minimum and maximum y values in your plot
par(mfrow = c(1, 1))
plot(tt, xxx, pch=3, xlim=c(nn-nb, nn+nt), ylim=rr, main='APPLE stock return', ylab='Return', xlab='Time')	
lines(tt, xxx)	#observed values
points(nn+1:nt, pp$pred, pch=2, col='red', type='o')	#predicted values
lines(nn+1:nt, pp$pred+2*pp$se, lty=2, col='red')	#upper bound of predicted interval
lines(nn+1:nt, pp$pred-2*pp$se, lty=2, col='red')	#lower bound of predicted interval

y.arma19.1 = arima(logx, order = c(1,0,9))
new.price = c(131.25, 131.30, 132.64, 131.34, 131.81)

###Prediction for Stock
pp = predict(y.arma19.1, 5)
nn = length(logx)	#length of your data
nt = 5	#forecast horizon
nb = 30	#number of data points you want to plot
tt = (nn-nb):nn	#indexes of data points you want to plot
xxx = n[tt]		#data you want to plot
pred = exp(pp$pred)   #prediction
pred.upp = exp(pp$pred+2*pp$se)  #upper bound for prediction
pred.low = exp(pp$pred-2*pp$se)  #lower bound for prediction
rr = range(c(xxx, pred, pred.upp, pred.low))	#find the minimum and maximum y values in your plot
par(mfrow = c(1, 1))
print(xxx)
print(tt)
plot(tt, xxx, pch=3, xlim=c(nn-nb,nn+nt), ylim=rr, main='APPLE stock price', ylab='Return', xlab='Time')	
lines(tt, xxx)	#observed values
points(nn+1:nt, pred, pch=2, col='red', type='o')	#predicted values
lines(nn+1:nt, pred.upp, lty=2, col='red')	#upper bound of predicted interval
lines(nn+1:nt, pred.low, lty=2, col='red')	#lower bound of predicted interval
points(nn+1:nt, new.price[1:5], pch=1, col='black', type='o')
legend.text = c("Actual value", "Prediction")
legend("bottomleft", legend.text, col=1:2, pch=1:2, lty=rep(1,2))


