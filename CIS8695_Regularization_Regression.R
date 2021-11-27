rm(list = ls())
setwd("C:/Users/Dell/Downloads/R Studio")

car.df <- read.csv("CSV_ToyotaCorolla.csv")
# select variables for regression
selected.var <- c("Price","Age_08_04","KM","Fuel_Type","HP","Met_Color",
                  "Automatic","cc","Doors", "Quarterly_Tax","Weight")

# partition data
set.seed(1)  # set seed for reproducing the partition
train.index <- sample(c(1:dim(car.df)[1]), dim(car.df)[1]*0.6)  
train.df <- car.df[train.index, selected.var]
valid.df <- car.df[-train.index, selected.var]

# install.packages("glmnet")
library(glmnet)

x <- as.matrix(train.df[,c("Age_08_04","KM","HP","Met_Color",
                            "Automatic","cc","Doors", "Quarterly_Tax","Weight")])
y <- train.df$Price

RidgeMod <- glmnet(x, y, alpha=0, nlambda=100, lambda.min.ratio=0.0001)
summary(RidgeMod)
plot(RidgeMod,xvar="lambda",label=TRUE)

# Or, 
library(plotmo)
plot_glmnet(RidgeMod)


# with k-fold cross-validation to select the best lamda
CvRidgeMod <- cv.glmnet(x, y, alpha=0, nlambda=100, lambda.min.ratio=0.0001)
plot(CvRidgeMod)

# There are two vertical lines: one is at the minimum, and the other vertical line is within one standard error of the minimum. 
# The second line is a slightly more restricted model that does almost as well as the minimum, 
# and sometimes, we'll go for that.

best.lambda <- CvRidgeMod$lambda.min
best.lambda

# obtain coefficients of the best model
predict(RidgeMod, s=best.lambda, type="coefficients")[1:10, ]
# or, an alternative way
coef(CvRidgeMod, s = "lambda.min")


# Use Ridge estimates for prediction
library(forecast)
x_pred <- as.matrix(valid.df[,c("Age_08_04","KM","HP","Met_Color",
                           "Automatic","cc","Doors", "Quarterly_Tax","Weight")])
Ridge.pred <- predict(CvRidgeMod, s = best.lambda, newx = x_pred)
accuracy(as.vector(Ridge.pred), valid.df$Price)


# Lasso 
LassoMod <- glmnet(x, y, alpha=1, nlambda=100, lambda.min.ratio=0.0001)
plot(LassoMod,xvar="norm",label=TRUE)

plot_glmnet(LassoMod)

CvLassoMod <- cv.glmnet(x, y, alpha=1, nlambda=100, lambda.min.ratio=0.0001)

plot(CvLassoMod)

best.lambda <- CvLassoMod$lambda.min
best.lambda

coef(CvLassoMod, s = "lambda.min")

# Use Lasso estimates for prediction
Lasso.pred <- predict(CvLassoMod, s = best.lambda, newx = x_pred)
accuracy(as.vector(Lasso.pred), valid.df$Price)


