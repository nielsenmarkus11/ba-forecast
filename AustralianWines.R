library(forecast)

dat <- read.csv('AustralianWines.csv', colClasses=c("character",rep("numeric",6)),na.strings="*")

dat.ts <- ts(dat[,-1],start=c(1980,1),frequency=12)

plot(dat.ts)

# Forecast using naive and seasonal naive methods
for (i in 1:dim(dat.ts)[2]){
  print(paste0("Naive Forecast for ",colnames(dat.ts)[i]))
  print(naive(dat.ts[,i],2))
  print(paste0("Seasonal Naive Forecast for ",colnames(dat.ts)[i]))
  print(snaive(dat.ts[,i],2))
}

