rm(list = ls())
#setwd("C:/Users/lxue5/Dropbox/2019Sp/CIS8695/5 Dimension Reduction")
setwd("C:/Users/Dell/Downloads/R Studio")


#install.packages("RSNNS")

library(caret)
library(NeuralNetTools)
library(RSNNS)
library(ggplot2)
library(tidyverse)
library(pROC)
library(reshape2)


churn<-read.csv("WA_Fn-UseC_-Telco-Customer-Churn.csv")

churn <- na.omit(churn)
na_churn<-rep(0,21)
for(i in 1:21){
  na_churn[i]<-sum(is.na(churn[,i]))
}
cols<-as.character(names(churn))
na_churn<-data.frame(na_churn,cols)
na_churn$cols<-as.character(na_churn$cols)

ggplot(data = na_churn) +
  aes(x = cols, weight = na_churn) +
  geom_bar(fill = "#0c4c8a") +
  labs(x = "Variable",
       y = "NA values") +
  theme_minimal() +
  coord_flip()


dum<-dummyVars(~.,data=churn[,2:21])
churn_full<-data.frame(predict(dum,newdata=churn))
churn_full$Churn.No<-NULL
churn_full$Churn.Yes<-NULL
churn_full$Churn<-churn$Churn
churn_full<-churn_full[sample(1:nrow(churn_full),length(1:nrow(churn_full))),1:ncol(churn_full)]
churn_names<-names(churn_full[1:45])
tgt<-decodeClassLabels(churn_full$Churn)
churn_full<-splitForTrainingAndTest(x=churn_full[1:45],y=tgt,ratio=0.25)
churn_full<-normTrainingAndTestSet(churn_full,type='0_1')



model<-mlp(x=churn_full$inputsTrain,y=churn_full$targetsTrain,size=5,
           learnFuncParams=c(0.005,0),inputsTest=churn_full$inputsTest,
           targetsTest=churn_full$targetsTest,maxit=240)


pred<-predict(model,newdata=churn_full$inputsTest)
errorit<-data.frame(iteration=seq(1:240),IterativeTestError=model$IterativeTestError)

ggplot(data=errorit,aes(x=iteration,y=IterativeTestError))+geom_line()

plotROC(pred,churn_full$targetsTest)

RSNNS::
confusionMatrix(as.factor(churn_full$targetsTest),as.factor(pred))

