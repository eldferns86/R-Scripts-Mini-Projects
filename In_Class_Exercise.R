rm(list = ls())
setwd("C:/Users/Dell/Downloads/R Studio")

library(e1071)
library(caret)

#install.packages("MASS")
library(MASS)

data(Pima.tr)
data(Pima.te)
pima<-rbind(Pima.tr,Pima.te)

pima.scale<-data.frame(scale(pima[,-8]))
pima.scale$type<-pima$type


train.index <- sample(c(1:dim(pima.scale)[1]), dim(pima.scale)[1]*0.6)  
train.df <- pima.scale[train.index, ]
valid.df <- pima.scale[-train.index, ]

# NB
pima.nb <- naiveBayes(type~., data=train.df)
pima.nb

pred.nb <- predict(pima.nb, newdata = valid.df)
confusionMatrix(as.factor(pred.nb), valid.df$type)

#SVM
pima.svm <- svm (type~., data = train.df, kernel ="linear", cost=100)

pred.svm <- predict(pima.svm, newdata = valid.df)
confusionMatrix(as.factor(pred.svm), valid.df$type)

pima.linear.tune <- tune.svm(type~., data=train.df, kernel = "linear", cost=c(0.001, 0.01, 0.1, 1, 5, 10, 20, 50, 100))
summary(pima.linear.tune)


set.seed(502)
grid1 <- expand.grid(.k = seq(2, 20, by = 1))
control <- trainControl(method = "cv")
knn.train <- train(type ~ ., data = train.df,
                   method = "knn",
                   trControl = control,
                   tuneGrid = grid1)
knn.train



pred.knn <- predict(knn.train, newdata = valid.df)
confusionMatrix(as.factor(pred.knn),as.factor(valid.df$type))


set.seed(123)
kknn.train <- train(type ~ ., data = train.df, kmax = 25, 
                         distance = 2, 
                         kernel = c("rectangular", "triangular", "epanechnikov"))
plot(kknn.train)
kknn.train

kknn.pred <- predict(kknn.train, newdata = valid.df)
confusionMatrix(as.factor(kknn.pred), as.factor(valid.df$type))
