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
