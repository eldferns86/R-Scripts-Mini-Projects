rm(list = ls())
setwd("C:/Users/Dell/Downloads/R Studio")
delays.df <- read.csv("CSV_FlightDelays.csv")

# Creating dummies
delays.df$Weekend <- delays.df$DAY_WEEK %in% c(6, 7)
delays.df$CARRIER_CO_MQ_DH_RU <- delays.df$CARRIER %in% c("CO", "MQ", "DH", "RU")
delays.df$MORNING <- delays.df$CRS_DEP_TIME.1 %in% c(6, 7, 8, 9)
delays.df$NOON <- delays.df$CRS_DEP_TIME.1 %in% c(10, 11, 12, 13)
delays.df$AFTER2P <- delays.df$CRS_DEP_TIME.1 %in% c(14, 15, 16, 17, 18)
delays.df$EVENING <- delays.df$CRS_DEP_TIME.1 %in% c(19, 20)

selected.var <- c("Flight.Status", "MORNING", "NOON", "AFTER2P", "EVENING", 
                  "CARRIER_CO_MQ_DH_RU","DEST","DISTANCE","Weather",
                  "Weekend")

# partition
set.seed(1)  
train.index <- sample(c(1:dim(delays.df)[1]), dim(delays.df)[1]*0.6)  
train.df <- delays.df[train.index, selected.var]
valid.df <- delays.df[-train.index, selected.var]

default.ct <- rpart(Flight.Status ~ ., data = train.df, method = "class")
summary(default.ct)


# plot using prp()
prp(default.ct, type = 5, extra = 101,  clip.right.lab = FALSE, 
    box.palette = "GnYlRd", leaf.round = 5, 
    branch = .3, varlen = -10, space=0) 


# Look at decision rules
rpart.rules(default.ct, cover = TRUE)


# Grow a full tree, 
deeper.ct <- rpart(Flight.Status ~ ., data = train.df, method = "class", 
                   cp = 0, minsplit = 1)


# count number of leaves
length(deeper.ct$frame$var[deeper.ct$frame$var == "<leaf>"])
# plot tree
prp(deeper.ct, type = 1, extra = 1, under = TRUE, split.font = 1, varlen = -10, 
    box.col=ifelse(deeper.ct$frame$var == "<leaf>", 'gray', 'white'))  




cv.ct <- rpart(Flight.Status ~ ., data = train.df, method = "class", 
               cp = 0.00001, minsplit = 5, xval = 5)


printcp(cv.ct)



pruned.ct <- prune(cv.ct, 
                   cp = cv.ct$cptable[which.min(cv.ct$cptable[,"xerror"]),"CP"])
length(pruned.ct$frame$var[pruned.ct$frame$var == "<leaf>"])
prp(pruned.ct, type = 5, extra = 101, cex=0.6, box.palette = "GnYlRd", 
    branch = .3, varlen = -10, space=0, yspace=0) 



library(randomForest)
rf <- randomForest(as.factor(Flight.Status) ~ ., data = train.df, ntree = 500, 
                   mtry = 4, nodesize = 5, importance = TRUE)  

varImpPlot(rf, type = 1)


#Boosting Tree
library(adabag)

train.df$Flight.Status <- as.factor(train.df$Flight.Status)

set.seed(1)
boost <- boosting(Flight.Status ~ ., data = train.df)
boost$trees[100]
pred <- predict(boost, valid.df)
confusionMatrix(as.factor(pred$class), as.factor(valid.df$Flight.Status))



