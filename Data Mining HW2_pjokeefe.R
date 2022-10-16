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
library(Amelia)
#library(knitr)

#import data
churn <- read.csv("https://github.com/sjsimmo2/DataMining-Fall/raw/master/TelcoChurn.csv", header = TRUE)

head(churn)

#Check the variable types
str(churn)

#Check for rows with missing values and store them in a table
missing <- churn[!complete.cases(churn$TotalCharges),]

#Change all categorical variables to factors except CustomerID
churn[sapply(churn, is.character)] <- lapply(churn[sapply(churn, is.character)], 
                                       as.factor)
churn$customerID <- as.character(churn$customerID)

churn$SeniorCitizen <- as.factor(churn$SeniorCitizen)

str(churn)

#set seed so we all get the same training, validation, train data set
set.seed(1905)

#take 60/30/10 sample for train, validation, and test
ss <- sample(1:3,size=nrow(churn),replace=TRUE,prob=c(0.6,0.3,0.1))
train <- churn[ss==1,]
validation <- churn[ss==2,]
test <- churn[ss==3,]

#impute missing values with median and create missing variable
train$missing[is.na(train$TotalCharges)] <- 1
train$missing[is.na(train$missing)] <- 0
train$missing <- as.factor(train$missing)
train$missing[train$missing == 1]
train$TotalCharges[is.na(train$TotalCharges)] <- median(train$TotalCharges, na.rm = T)

train[!complete.cases(train),]
str(train)

#Create classification tree
class.tree = rpart(Churn ~ . - customerID, data=train, method='class',
                parms = list(split='gini'))
summary(class.tree)
