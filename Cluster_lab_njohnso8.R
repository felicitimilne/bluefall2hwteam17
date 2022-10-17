library(teens4sets)
library(arules)
library(arulesViz)
library(ggplot2)
library(dplyr)
library(rpart)
library(rpart.plot)
library(TH.teens4)
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
library(Hmisc)

#Check for rows with missing values and store them in a table
missing <- teens4[!complete.cases(teens4),]

teens4$gender <- as.character(teens4$gender)

teens4$gender[is.na(teens4$gender)] <- 'missing'
teens4$gender <- as.factor(teens4$gender)

str(teens4)
hist.data.frame(teens4)

for (i in 3:ncol(teens4)) {
  hist(teens4[,i])
}

teens4_norm = teens4

for (i in 3:ncol(teens4)) {
  teens4_norm[, i] <- teens4[,i] / max(teens4[, i])
}

summary(teens4_norm)

for (i in 3:ncol(teens4)) {
  hist(teens4_norm[,i])
}

kmeans(teens4_norm, k = 2)

