rm(list = ls())
setwd("C:/Users/Dell/Downloads/R Studio")

# options(repos = c(CRAN = "http://cran.rstudio.com"))
# install.packages("sentimentr")
# install.packages("tm")
library(sentimentr)
library(tm)
library(e1071)
library(caret)

data_insults <- read.csv("kaggle_insults_1.csv",header=TRUE)

insult_comment<-as.vector(data_insults[,"Comment"])
insult_score<-as.vector(data_insults[,"Insult"])

clean.data = function(text) {
  
  # delete @ word and remove possible re-tweet entries
  text = gsub("(RT|via)((?:\\b\\W*@\\w+)+)", "", text)
  text = gsub("@\\w+", "", text)
  # delete punctuation and remove digits 0-9
  text = gsub("[[:punct:]]", "", text )
  text = gsub("[[:digit:]]", "", text)
  # delete html links and unnecessary tabs,spaces
  text = gsub("http\\w+", "", text)
  text = gsub("[ \t]{2,}", "", text)
  text = gsub("^\\s+|\\s+$", "", text)
  
  return(text)
}

# Clean text
insult_comment<-clean.data(insult_comment)

handle.error = function(x)
{
  # create missing value
  y = NA
  # tryCatch error
  try_error = tryCatch(tolower(x), error=function(e) e)
  # if not an error
  if (!inherits(try_error, "error"))
    y = tolower(x)
  # result
  return(y)
}

# Turn to lower-case
insult_comment = sapply(insult_comment, handle.error)

# remove NAs and nullifying the column names
insult_comment = insult_comment[!is.na(insult_comment)]
insult_comment[1:10]
names(insult_comment) = NULL


# builds a document term matrix
insult_corpus <- Corpus(VectorSource(insult_comment))
insult_dtm <- DocumentTermMatrix(insult_corpus)
dim(insult_dtm)

# removing words that do not occur in at least 1 percent of those documents
insult_dtm <- removeSparseTerms(x = insult_dtm, sparse = 0.99)
dim(insult_dtm)
inspect(insult_dtm[1:10,1:20])

# Convert all entries to binary
insult_dtm <- weightBin(insult_dtm)

dim(insult_dtm)

# Perform PCA
pcs.cor <- prcomp(insult_dtm)
summary(pcs.cor)
scores.cor <- pcs.cor$x[,1:26]
data_pcs<-data.frame(data.frame(insult_score),scores.cor)

set.seed(4232)
train.index <- sample(nrow(data_pcs), nrow(data_pcs)*0.7)  
train.df <- data_pcs[train.index, ]
valid.df <- data_pcs[-train.index, ] 

# Building a Naive Bayes model
nb_insult_pca <- naiveBayes(as.factor(insult_score)~., 
                            data=train.df)

# Prediction
insult_valid_pred <- predict(nb_insult_pca, valid.df)
confusionMatrix(as.factor(insult_valid_pred), 
                as.factor(valid.df$insult_score))


# Without PCA
data_terms<-data.frame(data.frame(insult_score),
                       as.data.frame(as.matrix(insult_dtm)))

set.seed(4232)
train.index <- sample(nrow(data_terms), nrow(data_terms)*0.7)  
train.df.term <- data_terms[train.index, ]
valid.df.term <- data_terms[-train.index, ] 

# Naive Bayes Model based on original 
nb_insult_term <- naiveBayes(as.factor(insult_score)~., 
                            data=train.df.term)

# Prediction
insult_valid_term_pred <- predict(nb_insult_term, valid.df.term)
confusionMatrix(as.factor(insult_valid_term_pred), 
                as.factor(valid.df.term$insult_score))




