library(forecast)
library(xlsx)

dat <- read.xlsx("SouvenirSales.xlsx",sheetName="SouvenirSales")
dat.ts <- ts(dat[,2],start=c(1995,1),frequency=12)
plot(dat.ts)

train <- window(dat.ts,end=c(2000,12))
valid <- window(dat.ts,start=c(2001,1))

fit1 <- tslm(train~trend+season,lambda=0)
plot(forecast(fit1,h=13))

# Forecast January 2002
fc.lm <- forecast(fit1,h = 13)$mean

# Now conduct ARIMA on the residuals
er.train <- resid(fit1)
plot(er.train)
abline(h=0)

fit2 <- Arima(er.train,order=c(2,0,0))

finresid <-resid(fit2)
plot(finresid)
abline(h=0)

fc.lm.ar <- fc.lm + forecast(fit2,h=13)$mean

# back transform predictions
exp(fc.lm)[13]
exp(fc.lm.ar)[13]



### Now use Arima and SArima
aa1 <- auto.arima(train,seasonal = T,lambda = 0)
fc.aa1 <- forecast(aa1,h=12)
fc.res <- valid - fc.aa1$mean

# fit suggested model with all the data
ss.fit <- Arima(dat.ts,order = c(2,0,0), seasonal = c(0,1,1), lambda = 0, include.drift = T)
fc.arima <- forecast(ss.fit,h=1)
plot(fc.arima)

fc.arima$mean


## Which appears to be better?
# lm and arima
jpeg("lm-arima-sales-mod.jpg",height=960)
par(mfrow=c(2,1))
plot(dat.ts, main="lm-Arima Combo Predictions and Residuals")
lines(fitted(fit1),col="blue")
lines(forecast(fit1,h=12)$mean,col="red")
dat.res <- log(dat.ts) - log(c(fitted(fit1),forecast(fit1,h=12)$mean))
plot(dat.res)
abline(h=0)
dev.off()

# sarima
jpeg("sarima-sales-mod.jpg",height=960)
par(mfrow=c(2,1))
plot(dat.ts, main="Seasonal Arima Predictions and Residuals")
lines(fitted(aa1),col="blue")
lines(forecast(aa1,h=12)$mean,col="red")
dat.res <- log(dat.ts) - log(c(fitted(aa1),forecast(aa1,h=12)$mean))
plot(dat.res)
abline(h=0)
dev.off()
