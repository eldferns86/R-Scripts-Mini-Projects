rm(list = ls())
setwd("C:/Users/Dell/Downloads/R Studio")

library(forecast)
Amtrak.data <- read.csv("Amtrak.csv")

# create time series object using ts()
# ts() takes three arguments: start, end, and freq. 
# with monthly data, the frequency of periods per season is 12 (per year). 
# arguments start and end are (season number, period number) pairs. 
# here start is Jan 1991: start = c(1991, 1); end is Mar 2004: end = c(2004, 3).
ridership.ts <- ts(Amtrak.data$Ridership, 
                   start = c(1991, 1), end = c(2004, 3), freq = 12)

# plot the series
plot(ridership.ts, xlab = "Time", ylab = "Ridership (in 000s)", ylim = c(1300, 2300))

# Zoom In: use window() to create a new, shorter time series of ridership.ts
# for the new three-year series, start time is Jan 1997 and end time is Dec 1999
ridership.ts.3yrs <- window(ridership.ts, start = c(1997, 1), end = c(1999, 12))
par(mfrow = c(2, 1))
plot(ridership.ts.3yrs, xlab = "Time", ylab = "Ridership (in 000s)", 
     ylim = c(1300, 2300))
plot(ridership.ts, xlab = "Time", ylab = "Ridership (in 000s)", ylim = c(1300, 2300))


# fit a linear regression model to the time series
ridership.lm <- tslm(ridership.ts ~ trend + I(trend^2))

# overlay the fitted values of the linear model
lines(ridership.lm$fitted, lwd = 2)


# partition the data
nValid <- 36
nTrain <- length(ridership.ts) - nValid

train.ts <- window(ridership.ts, start = c(1991, 1), end = c(1991, nTrain))
valid.ts <- window(ridership.ts, start = c(1991, nTrain + 1), 
                   end = c(1991, nTrain + nValid))

#  generate the naive and seasonal naive forecasts
naive.pred <- naive(train.ts, h = nValid)
snaive.pred <- snaive(train.ts, h = nValid)

# plot forecasts and actuals in the training and validation sets
par(mfrow = c(1, 1))

plot(train.ts, ylim = c(1300, 2600),  ylab = "Ridership", xlab = "Time", bty = "l", 
     xaxt = "n", xlim = c(1991,2006.25), main = "")
axis(1, at = seq(1991, 2006, 1), labels = format(seq(1991, 2006, 1)))
lines(naive.pred$mean, lwd = 2, col = "blue", lty = 1)
lines(snaive.pred$mean, lwd = 2, col = "blue", lty = 1)
lines(valid.ts, col = "grey20", lty = 3)
lines(c(2004.25 - 3, 2004.25 - 3), c(0, 3500)) 
lines(c(2004.25, 2004.25), c(0, 3500))
text(1996.25, 2500, "Training")
text(2002.75, 2500, "Validation")
text(2005.25, 2500, "Future")

# Prediction Accuracy
accuracy(naive.pred, valid.ts)
accuracy(snaive.pred, valid.ts)


#########################
# Trend and Seasonality
#########################

# produce a model with only linear trend 
ridership.lm <- tslm(ridership.ts ~ trend)

# plot the series
plot(ridership.ts, xlab = "Time", ylab = "Ridership", ylim = c(1300,2300),
     bty = "l")
lines(ridership.lm$fitted, lwd = 2)

# fit linear trend model to training set and create forecasts
train.lm <- tslm(train.ts ~ trend)
train.lm.pred <- forecast(train.lm, h = nValid, level = 0)

summary(train.lm)

# Plot forecasted values and residuals
par(mfrow = c(1, 1))
plot(train.lm.pred, ylim = c(1300, 2600),  ylab = "Ridership", xlab = "Time", 
     bty = "l", xaxt = "n", xlim = c(1991,2006.25), main = "", flty = 2)
axis(1, at = seq(1991, 2006, 1), labels = format(seq(1991, 2006, 1)))
lines(train.lm.pred$fitted, lwd = 2, col = "blue")
lines(valid.ts)
plot(train.lm.pred$residuals, ylim = c(-420, 500),  ylab = "Forecast Errors", 
     xlab = "Time", bty = "l", xaxt = "n", xlim = c(1991,2006.25), main = "")
axis(1, at = seq(1991, 2006, 1), labels = format(seq(1991, 2006, 1)))
lines(valid.ts - train.lm.pred$mean, lwd = 1)


# fit exponential trend using tslm() with argument lambda = 0 
train.lm.expo.trend <- tslm(train.ts ~ trend, lambda = 0)
train.lm.expo.trend.pred <- forecast(train.lm.expo.trend, h = nValid, level = 0)
summary(train.lm.expo.trend)

# fit linear trend using tslm() with argument lambda = 1 (no transform of y)
train.lm.linear.trend <- tslm(train.ts ~ trend, lambda = 1)
train.lm.linear.trend.pred <- forecast(train.lm.linear.trend, h = nValid, level = 0)

plot(train.lm.expo.trend.pred, ylim = c(1300, 2600),  ylab = "Ridership", 
     xlab = "Time", bty = "l", xaxt = "n", xlim = c(1991,2006.25), main = "", flty = 2)
lines(train.ts)
axis(1, at = seq(1991, 2006, 1), labels = format(seq(1991, 2006, 1)))
lines(train.lm.expo.trend.pred$fitted, lwd = 2, col = "blue")  # Added in 6-5
lines(train.lm.linear.trend.pred$fitted, lwd = 2, col = "black", lty = 3)
lines(train.lm.linear.trend.pred$mean, lwd = 2, col = "black", lty = 3)
lines(valid.ts)


# fit quadratic trend using function I(), which treats an object "as is".
train.lm.poly.trend <- tslm(train.ts ~ trend + I(trend^2))
summary(train.lm.poly.trend)
train.lm.poly.trend.pred <- forecast(train.lm.poly.trend, h = nValid, level = 0)

par(mfrow = c(1,1))
plot(train.lm.poly.trend.pred, ylim = c(1300, 2600),  ylab = "Ridership", 
     xlab = "Time", bty = "l", xaxt = "n", xlim = c(1991,2006.25), main = "", flty = 2)
axis(1, at = seq(1991, 2006, 1), labels = format(seq(1991, 2006, 1))) 
lines(train.lm.poly.trend$fitted, lwd = 2)
lines(valid.ts)

plot(train.lm.poly.trend$residuals, ylim = c(-400, 550),  ylab = "Forecast Errors", 
     xlab = "Time", bty = "l", xaxt = "n", xlim = c(1991,2006.25), main = "")
axis(1, at = seq(1991, 2006, 1), labels = format(seq(1991, 2006, 1)))
lines(valid.ts - train.lm.poly.trend.pred$mean, lwd = 1)

par(mfrow = c(1,1))

# include season as a predictor in tslm(). Here it creates 11 dummies, 
# one for each month except for the first season, January.  
train.lm.season <- tslm(train.ts ~ season)
summary(train.lm.season)
train.lm.season <- tslm(train.ts ~ season,lambda = 0)
summary(train.lm.season)


# Both season and quadratic trend
train.lm.trend.season <- tslm(train.ts ~ trend + I(trend^2) + season)
summary(train.lm.trend.season)
train.lm.trend.season.pred <- forecast(train.lm.trend.season, h = nValid, level = 0)
plot(train.lm.trend.season.pred, ylim = c(1300, 2600),  ylab = "Ridership", 
     xlab = "Time", bty = "l", xaxt = "n", xlim = c(1991,2006.25), main = "", flty = 2)
axis(1, at = seq(1991, 2006, 1), labels = format(seq(1991, 2006, 1))) 
lines(train.lm.trend.season.pred$fitted, lwd = 2)
lines(valid.ts)


# Autocorrelation
ridership.24.ts <- window(train.ts, start = c(1991, 1), end = c(1991, 24))
Acf(ridership.24.ts, lag.max = 12, main = "")


# fit AR(1) model to training residuals
# use Arima() in the forecast package to fit an ARIMA model 
# (that includes AR models); order = c(1,0,0) gives an AR(1).
train.res.arima <- Arima(train.lm.trend.season$residuals, 
                         order = c(1,0,0))
valid.res.arima.pred <- forecast(train.res.arima, h = nValid, level = 0)
summary(train.res.arima)

plot(train.lm.trend.season$residuals, ylim = c(-250, 250),  ylab = "Residuals", 
     xlab = "Time", bty = "l", xaxt = "n", xlim = c(1991,2006.25), main = "")
axis(1, at = seq(1991, 2006, 1), labels = format(seq(1991, 2006, 1)))
lines(valid.res.arima.pred$fitted, lwd = 2, col = "blue")

# Predictability: Random Walk
sp500.df <- read.csv("SP500.csv")

sp500.ts <- ts(sp500.df$Close, start = c(1995, 5), end = c(2003, 8), freq = 12)
sp500.arima <- Arima(sp500.ts, order = c(1,0,0))
sp500.arima

#################
# Smoothing
#################

library(zoo)

# centered moving average with window order = 12
ma.centered <- ma(ridership.ts, order = 12)

# trailing moving average with window k = 12
# in rollmean(), use argument align = right to calculate a trailing moving average.
ma.trailing <- rollmean(ridership.ts, k = 12, align = "right")

# generate a plot 
plot(ridership.ts, ylim = c(1300, 2200),  ylab = "Ridership", 
     xlab = "Time", bty = "l", xaxt = "n", 
     xlim = c(1991,2004.25), main = "")
axis(1, at = seq(1991, 2004.25, 1), labels = format(seq(1991, 2004.25, 1)))
lines(ma.centered, lwd = 2)
lines(ma.trailing, lwd = 2, lty = 2)
legend(1994,2200, c("Ridership","Centered Moving Average", "Trailing Moving Average"), 
       lty=c(1,1,2), lwd=c(1,2,2), bty = "n")  


# moving average on training
ma.trailing <- rollmean(train.ts, k = 12, align = "right")

# obtain the last moving average in the training period
last.ma <- tail(ma.trailing, 1)

# create forecast based on last MA
ma.trailing.pred <- ts(rep(last.ma, nValid), start = c(1991, nTrain + 1), 
                       end = c(1991, nTrain + nValid), freq = 12)

# plot the series
plot(train.ts, ylim = c(1300, 2600),  ylab = "Ridership", xlab = "Time", bty = "l", 
     xaxt = "n", xlim = c(1991,2006.25), main = "")
axis(1, at = seq(1991, 2006, 1), labels = format(seq(1991, 2006, 1)))
lines(ma.trailing, lwd = 2, col = "blue") 
lines(ma.trailing.pred, lwd = 2, col = "blue", lty = 2)
lines(valid.ts)


# get residuals of the season_quardratic_trend model
residuals.ts <- train.lm.trend.season$residuals

# run simple exponential smoothing
# use ets() with model = "ANN" (additive error (A), no trend (N), no seasonality (N)) 
# and alpha = 0.2 to fit simple exponential smoothing.
ses <- ets(residuals.ts, model = "ANN", alpha = 0.2)
ses.pred <- forecast(ses, h = nValid, level = 0)

plot(ses.pred, ylim = c(-250, 300),  ylab = "Ridership", xlab = "Time", 
     bty = "l", xaxt = "n", xlim = c(1991,2006.25), main = "", flty = 2)
#lines(train.lm.trend.season.pred$fitted, lwd = 2, col = "blue")
axis(1, at = seq(1991, 2006, 1), labels = format(seq(1991, 2006, 1)))
lines(ses.pred$fitted, lwd = 2, col = "blue")
#lines(valid.ts)
### Note: the forecast is a fixed-level forecast


# run Holt-Winters exponential smoothing
# use ets() with option model = "MAA" to fit Holt-Winter's exponential smoothing 
# with multiplicative error, additive trend, and additive seasonality. 
hwin <- ets(train.ts, model = "MAA")

# create predictions
hwin.pred <- forecast(hwin, h = nValid, level = 0)

# plot the series
plot(hwin.pred, ylim = c(1300, 2600),  ylab = "Ridership", xlab = "Time", 
     bty = "l", xaxt = "n", xlim = c(1991,2006.25), main = "", flty = 2)
axis(1, at = seq(1991, 2006, 1), labels = format(seq(1991, 2006, 1)))
lines(hwin.pred$fitted, lwd = 2, col = "blue")
lines(valid.ts)

accuracy(hwin.pred, valid.ts)
accuracy(train.lm.trend.season.pred, valid.ts)

hwin
