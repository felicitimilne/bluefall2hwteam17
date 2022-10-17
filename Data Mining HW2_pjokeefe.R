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
library(ROCR)
#library(knitr)

#import data
churn <- read.csv("https://github.com/sjsimmo2/DataMining-Fall/raw/master/TelcoChurn.csv", header = TRUE)

head(churn)

#Check the variable types
str(churn)

#Check for rows with missing values and store them in a table
missing <- churn[!complete.cases(churn$TotalCharges),]

#set seed so we all get the same training, test, train data set
set.seed(1905)

#take 80/20 sample for train and test
ss <- sample(1:2,size=nrow(churn),replace=TRUE,prob=c(0.8,0.2))
train <- churn[ss==1,]
test <- churn[ss==2,]


#impute missing values with median and create missing variable

#Repeat for test and test using the training median
train$missing[is.na(train$TotalCharges)] <- 1
train$missing[is.na(train$missing)] <- 0

train$missing <- as.factor(train$missing)
train$missing[train$missing == 1]
train$TotalCharges[is.na(train$TotalCharges)] <- median(train$TotalCharges, na.rm = T)

train$missing <- as.character(train$missing)


test$missing[is.na(test$TotalCharges)] <- 1
test$missing[is.na(test$missing)] <- 0
test$missing <- as.character(test$missing)


train$TotalCharges[is.na(train$TotalCharges)] <- 0
test$TotalCharges[is.na(test$TotalCharges)] <- 0


train[!complete.cases(train),]
str(train)

#Create classification tree
class.tree = rpart(Churn ~ gender + SeniorCitizen + Partner + Dependents + tenure + PhoneService + MultipleLines + InternetService + OnlineSecurity + OnlineBackup + DeviceProtection + TechSupport + StreamingTV + StreamingMovies + Contract + PaperlessBilling + PaymentMethod + MonthlyCharges + TotalCharges + missing, 
                   data=train, method='class', parms = list(split='gini'))
summary(class.tree)


print(class.tree)

#Graph of the tree
rpart.plot(class.tree)

#Misclassification rate of training and test
tscores = predict(class.tree,type='class')
scores = predict(class.tree, test, type='class')

##Training misclassification rate:
sum(tscores!=train$Churn)/nrow(train)

### test data:
sum(scores!=test$Churn)/nrow(test)

#plot ROC Curve
tscores.prob <- predict(class.tree,test,type="prob")


pred_val <-prediction(tscores.prob[,2],test$Churn)

auroc<-performance(pred_val, measure = "auc")@y.values
auroc[[1]]

perf <- performance(pred_val, measure = "tpr", x.measure = "fpr")
perf.df <- as.data.frame(c(perf@x.values, perf@y.values))
colnames(perf.df) <- c("x.values", "y.values")
roc <- ggplot(data = perf.df, aes(x = x.values, y = y.values)) + geom_line(color = "dodgerblue3") + 
        labs(x = "True Positive Rate", y = "False Positive Rate", title = "ROC Curve of Classification Tree", hjust = 0.5) + 
        theme(plot.title = element_text(hjust = 0.5)) + geom_abline(slope = 1, intercept = 0, linetype = "dashed", color = "gray57") +
        annotate("text", x = 0.5, y = 0.15, size = 6, label = paste("AUROC =", auroc[[1]]))
roc


roc <- plot(perf, lwd = 3, col = "dodgerblue3", 
     main = "ROC Curve of Classification Tree",
     sub = paste("AUROC =", round(auroc[[1]], 4)),
     xlab = "True Positive Rate",
     ylab = "False Positive Rate")
abline(a = 0, b = 1, lty = 3)


#################################################################

#Recursive partitioning tree

#need character variables as factors, so create new data set
c.train <- train

c.train[sapply(c.train, is.character)] <- lapply(c.train[sapply(c.train, is.character)], 
                                             as.factor)
c.train$customerID <- as.character(c.train$customerID)

c.train$SeniorCitizen <- as.factor(c.train$SeniorCitizen)

c.test <- test

c.test[sapply(c.test, is.character)] <- lapply(c.test[sapply(c.test, is.character)], 
                                                 as.factor)
c.test$customerID <- as.character(c.test$customerID)

c.test$SeniorCitizen <- as.factor(c.test$SeniorCitizen)

str(c.train)

#run model
c.tree <- ctree(Churn ~ gender + SeniorCitizen + Partner + Dependents + tenure + PhoneService + MultipleLines + InternetService + OnlineSecurity + OnlineBackup + DeviceProtection + TechSupport + StreamingTV + StreamingMovies + Contract + PaperlessBilling + PaymentMethod + MonthlyCharges + TotalCharges + missing, 
                data=c.train, mincriterion = 0.99995)

#prune(c.tree, cp = 0.05)
c.tree
summary(c.tree)
plot(c.tree)

#Check Misclassification rates
c.tscores = predict(c.tree,type='response')
c.scores = predict(c.tree, c.test, type='response')

##Training misclassification rate:
sum(c.tscores!=train$Churn)/nrow(train)

### test data:
sum(c.scores!=c.test$Churn)/nrow(c.test)

#plot ROC Curve
c.tscores.prob <- predict(c.tree, c.test, type="prob")


c.pred_val <-prediction(c.tscores.prob[,2],c.test$Churn)

c.auroc<-performance(c.pred_val, measure = "auc")@y.values
c.auroc[[1]]

c.perf <- performance(c.pred_val, measure = "tpr", x.measure = "fpr")
cperf.df <- as.data.frame(c(c.perf@x.values, c.perf@y.values))
colnames(cperf.df) <- c("x.values", "y.values")
c.roc <- ggplot(data = cperf.df, aes(x = x.values, y = y.values)) + geom_line(color = "dodgerblue3") + 
        labs(x = "True Positive Rate", y = "False Positive Rate", title = "ROC Curve of Recursive Partitioning Tree", hjust = 0.5) + 
        theme(plot.title = element_text(hjust = 0.5)) + geom_abline(slope = 1, intercept = 0, linetype = "dashed", color = "gray57") +
        annotate("text", x = 0.5, y = 0.15, size = 6, label = paste("AUROC =", c.auroc[[1]]))
c.roc
plot(c.perf, lwd = 3, col = "dodgerblue3", 
     main = "ROC Curve of Recursive Partitioning Tree",
     sub = paste("AUROC =", round(c.auroc[[1]], 4)),
     xlab = "True Positive Rate",
     ylab = "False Positive Rate")
abline(a = 0, b = 1, lty = 3)

grid.arrange(roc, c.roc, nrow = 1)
