library(datasets)
library(arules)
library(arulesViz)
library(ggplot2)
library(dplyr)
library(rpart)
library(rpart.plot)
library(TH.data)
library(ISLR2)
library(lattice)
library(stats)
library(rattle)
library(RColorBrewer)
library(caret)
library(ROCR)
library(tidyverse)  
library(cluster)  
library(factoextra) 
library(gridExtra)
library(NbClust)
library(dendextend)
library(class)
library(ClustOfVar)
library(MASS)
library(kableExtra)
library(partykit)
library(dbscan)
#library(knitr)

#import data
churn <- read.csv("https://github.com/sjsimmo2/DataMining-Fall/raw/master/TelcoChurn.csv", header = TRUE)

head(churn)

#Check the variable types
str(churn)

#Check for rows with missing values and store them in a table
missing <- churn[!complete.cases(churn$TotalCharges),]

#set seed so we all get the same training, validation, train data set
set.seed(1905)

#take 60/30/10 sample for train, validation, and test
ss <- sample(1:3,size=nrow(churn),replace=TRUE,prob=c(0.6,0.3,0.1))
train <- churn[ss==1,]
validation <- churn[ss==2,]
test <- churn[ss==3,]

#impute missing values with median and create missing variable

#Repeat for validation and test using the training median
train$missing[is.na(train$TotalCharges)] <- 1
train$missing[is.na(train$missing)] <- 0
train$missing <- as.character(train$missing)

validation$missing[is.na(validation$TotalCharges)] <- 1
validation$missing[is.na(validation$missing)] <- 0
validation$missing <- as.character(validation$missing)

test$missing[is.na(test$TotalCharges)] <- 1
test$missing[is.na(test$missing)] <- 0
test$missing <- as.character(test$missing)

median <- median(train$TotalCharges, na.rm = T)

train$TotalCharges[is.na(train$TotalCharges)] <- median
validation$TotalCharges[is.na(validation$TotalCharges)] <- median
test$TotalCharges[is.na(test$TotalCharges)] <- median



train[!complete.cases(train),]
str(train)

#Create classification tree
class.tree = rpart(Churn ~ . - customerID, data=train, method='class',
                parms = list(split='gini'))
summary(class.tree)


print(class.tree)

#Graph of the tree
rpart.plot(class.tree)

#Misclassification rate of training and validation
tscores = predict(class.tree,type='class')
scores = predict(class.tree, validation, type='class')

##Training misclassification rate:
sum(tscores!=train$Churn)/nrow(train)

### validation data:
sum(scores!=validation$Churn)/nrow(validation)

#################################################################

#Recursive partitioning tree

#need character variables as factors, so create new data set
c.train <- train

c.train[sapply(c.train, is.character)] <- lapply(c.train[sapply(c.train, is.character)], 
                                             as.factor)
c.train$customerID <- as.character(c.train$customerID)

c.train$SeniorCitizen <- as.factor(c.train$SeniorCitizen)

str(c.train)

#run model
c.tree <- ctree(Churn ~ . - customerID, data=c.train)


c.tree
plot(c.tree)

#Check Misclassification rates
c.tscores = predict(c.tree,type='response')
c.scores = predict(c.tree, validation, type='response')

##Training misclassification rate:
sum(c.tscores!=train$Churn)/nrow(train)

### validation data:
sum(c.scores!=validation$Churn)/nrow(validation)
