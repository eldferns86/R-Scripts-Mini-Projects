rm(list = ls())
setwd("C:/Users/Dell/Downloads/R Studio")

library(forecast)
Search.data <- read.csv("Kaggle_SearchTerm.csv")


Search.ts <- ts(Search.data$analytics, 
                   start = c(2014, 51), end = c(2019, 51), freq = 52)

nValid <- 78
nTrain <- length(Search.ts) - nValid

train.ts <- window(Search.ts, start = c(2014, 51), end = c(2014, nTrain))
valid.ts <- window(Search.ts, start = c(2014, nTrain + 1), 
                   end = c(2014, nTrain + nValid))


naive.pred <- naive(train.ts, h = nValid)
snaive.pred <- snaive(train.ts, h = nValid)
accuracy(naive.pred, valid.ts)
accuracy(snaive.pred, valid.ts)

#Linear trend model
train.lm <- tslm(train.ts ~ trend)
train.lm.pred <- forecast(train.lm, h = nValid, level = 0)

summary(train.lm)


plot(train.lm.pred, ylim = c(30, 120),  ylab = "Analytics", xlab = "Time", 
     bty = "l", xaxt = "n", xlim = c(2014,2020), main = "", flty = 2)
axis(1, at = seq(2014, 2020, 1), labels = format(seq(2014, 2020, 1)))
lines(train.lm.pred$fitted, lwd = 2, col = "blue")
lines(valid.ts)

#Non linear trend
train.lm.poly.trend <- tslm(train.ts ~ trend + I(trend^2))
summary(train.lm.poly.trend)
train.lm.poly.trend.pred <- forecast(train.lm.poly.trend, h = nValid, level = 0)


plot(train.lm.poly.trend.pred, ylim = c(30, 120),  ylab = "Analytics", xlab = "Time", 
     bty = "l", xaxt = "n", xlim = c(2014,2020), main = "", flty = 2)
axis(1, at = seq(2014, 2020, 1), labels = format(seq(2014, 2020, 1)))
lines(train.lm.poly.trend.pred$fitted, lwd = 2, col = "blue")
lines(valid.ts)



plot(train.lm.poly.trend$residuals, ylim = c(-50, 50),  ylab = "Forecast Errors", 
     xlab = "Time", bty = "l", xaxt = "n", xlim = c(2014,2020), main = "")
axis(1, at = seq(2014,2020, 1), labels = format(seq(2014,2020, 1)))
lines(valid.ts - train.lm.poly.trend.pred$mean, lwd = 1)



#Polynomial trend + season
train.lm.trend.season <- tslm(train.ts ~ trend + I(trend^2) + season)
summary(train.lm.trend.season)



train.lm.trend.season.pred <- forecast(train.lm.trend.season, h = nValid, level = 0)
plot(train.lm.trend.season.pred, ylim = c(30, 120),  ylab = "Analytics", 
     xlab = "Time", bty = "l", xaxt = "n", xlim = c(2014,2020), main = "", flty = 2)
axis(1, at = seq(2014,2020, 1), labels = format(seq(2014,2020, 1))) 
lines(train.lm.trend.season.pred$fitted, lwd = 2)
lines(valid.ts)

#Smoothing
residuals.ts <- train.lm.trend.season$residuals


ses <- ets(residuals.ts, model = "ANN", alpha = 0.2)
ses.pred <- forecast(ses, h = nValid, level = 0)



plot(ses.pred, ylim = c(-50, 50),  ylab = "Analytics", xlab = "Time", 
     bty = "l", xaxt = "n", xlim = c(2014,2020), main = "", flty = 2)
#lines(train.lm.trend.season.pred$fitted, lwd = 2, col = "blue")
axis(1, at = seq(2014,2020, 1), labels = format(seq(2014,2020, 1)))
lines(ses.pred$fitted, lwd = 2, col = "blue")




#Try hrwin

Ys.data <- read.csv("YellowStone.csv")


Ys.ts <- ts(Ys.data$Recreation, 
                start = c(1986, 1), end = c(2016, 9), freq = 12)

nValid <- 60
nTrain <- length(Ys.ts) - nValid

train.ts <- window(Ys.ts, start = c(1986, 1), end = c(1986, nTrain))
valid.ts <- window(Ys.ts, start = c(1986, nTrain + 1), 
                   end = c(1986, nTrain + nValid))



hwin <- ets(train.ts, model = "MAA")

hwin.pred <- forecast(hwin, h = nValid, level = 0)


plot(hwin.pred, ylim = c(6000, 100000),  ylab = "Visits", xlab = "Time", 
     bty = "l", xaxt = "n", xlim = c(1985,2017), main = "", flty = 2)
axis(1, at = seq(1985,2017, 1), labels = format(seq(1985,2017, 1)))
lines(hwin.pred$fitted, lwd = 2, col = "blue")
lines(valid.ts)


accuracy(hwin.pred, valid.ts)
