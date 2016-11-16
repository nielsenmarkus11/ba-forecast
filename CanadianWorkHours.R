# READ in the data
dat <- read.csv("CanadianWorkHours.csv")
plot(dat$Year,dat$Hours,type='l')

# find suitable trend shape
library(ggplot2)
library(gridExtra)

p1 <- ggplot(dat,aes(Year,Hours)) + geom_line() + stat_smooth(method="lm", se = FALSE) + labs(title="Linear Trend")
p2 <- ggplot(dat,aes(Year,Hours)) + geom_line() + stat_smooth(method="lm",formula=y~ poly(x,2), se = FALSE) + labs(title="Quadratic Trend")
p3 <- ggplot(dat,aes(Year,Hours)) + geom_line() + stat_smooth(method="lm",formula=y~ poly(x,3), se = FALSE) + labs(title="3rd Degree Polynomial Trend")
jpeg('CWH-trend-evaluation.jpg',height=800)
grid.arrange(p1,p2,p3)
dev.off()

# partition into training/validation
train <- dat[dat$Year<1997,]
valid <- dat[dat$Year>=1997,]

# fit regression model
myfit <- lm(Hours~poly(Year,3),data=train)
summary(myfit)

# generate predictions and overlay on original plot
preds<-predict(myfit,se.fit=T, interval = "confidence")
train$pred <- preds$fit[,1] 
train$lower <- preds$fit[,2]
train$upper <- preds$fit[,3]

preds<-predict(myfit,newdat=valid,se.fit = T,interval = "prediction")
valid$pred <- preds$fit[,1] 
valid$lower <- preds$fit[,2]
valid$upper <- preds$fit[,3]

jpeg('CWH-predictions.jpg')
plot(dat$Year,dat$Hours,type='l',ylim=c(min(dat$Hours),max(valid$upper)),
     main="Canadian Working Hours\n3rd Degree Polinomial Trend",
     xlab = "Hours",ylab = "Year")
lines(train$Year,train$pred,col='blue')
lines(train$Year,train$lower,col='blue',lty=2)
lines(train$Year,train$upper,col='blue',lty=2)
lines(valid$Year,valid$pred,col='red')
lines(valid$Year,valid$lower,col='red',lty=2)
lines(valid$Year,valid$upper,col='red',lty=2)
dev.off()

# fit new regression model
myfit <- lm(Hours~poly(Year,2),data=train)
summary(myfit)


# generate new predictions and overlay on original plot
preds<-predict(myfit,se.fit=T, interval = "confidence")
train$pred <- preds$fit[,1] 
train$lower <- preds$fit[,2]
train$upper <- preds$fit[,3]

preds<-predict(myfit,newdat=valid,se.fit = T,interval = "prediction")
valid$pred <- preds$fit[,1] 
valid$lower <- preds$fit[,2]
valid$upper <- preds$fit[,3]

jpeg('CWH-predictions-new.jpg')
plot(dat$Year,dat$Hours,type='l',ylim=c(min(valid$lower),max(train$upper)),
     main="Canadian Working Hours\nQuadratic Trend",
     xlab = "Hours",ylab = "Year")
lines(train$Year,train$pred,col='blue')
lines(train$Year,train$lower,col='blue',lty=2)
lines(train$Year,train$upper,col='blue',lty=2)
lines(valid$Year,valid$pred,col='red')
lines(valid$Year,valid$lower,col='red',lty=2)
lines(valid$Year,valid$upper,col='red',lty=2)
dev.off()

