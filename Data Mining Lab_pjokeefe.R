
library(arules)
library(arulesViz)
library(ggplot2)
library(dplyr)
library(rpart)
library(rpart.plot)
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

load("C:/Users/cardu/Downloads/TeenSNS4.RData")

#Check for rows with missing values and store them in a table
missing <- teens4[!complete.cases(teens4),]

teens4$gender <- as.character(teens4$gender)

teens4$gender[is.na(teens4$gender)] <- 'missing'

teens4 <- teens4 %>% mutate(gender = ifelse(gender == 'M',1,ifelse(gender =='F',2,3)))

str(teens4)
hist.data.frame(teens4)

for (i in 3:ncol(teens4)) {
  hist(teens4[,i])
}

for (i in 3:ncol(teens4)) {
  print(IQR(teens4[,i]) *1.5)
}
summary(teens4)

teens4_norm = teens4

for (i in 3:ncol(teens4)) {
  teens4_norm[, i] <- teens4[,i] / max(teens4[, i])
}

summary(teens4_norm)

for (i in 3:ncol(teens4)) {
  hist(teens4_norm[,i])
}

test <- kmeans(teens4_norm, centers = 6, nstart = 50)
test$withinss

fviz_cluster(test, data = teens4_norm)


set.seed(12964)

fviz_nbclust(teens4_norm, kmeans, method = "silhouette",k.max = 5)

NbClust(teens4_norm,method="kmeans",min.nc=2,max.nc = 5)
