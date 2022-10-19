library(purrr)
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

#histograms of all variables
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



#set seed for consistent results
set.seed(12964)

#calculate within SS for up to 16 clusters and find elbow
withinss <- NULL
for (i in 2:16) {
  test1 <- kmeans(teens4_norm, centers = i, nstart = 50)
  print(sum(test1$withinss))
  withinss <- c(withinss, sum(test1$withinss))
}

plot(withinss)


#Chose center of 9 due to elbow plot
test <- kmeans(teens4_norm, centers = 9, nstart = 50)

print(sum(test$withinss))

fviz_cluster(test, data = teens4_norm)

#Assign each observation to a cluster
profile.kmeans <- cbind(teens4_norm,test$cluster)

#Get summary statistic for each cluster
clusters <- profile.kmeans %>%
  split(test$cluster) %>%
  map(summary)

clusters$`8`
