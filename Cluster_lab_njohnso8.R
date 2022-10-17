library(tidyv)
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

teens4_gender <- teens4 %>% mutate(gender = ifelse(is.na(gender), 0, ifelse(gender == "F", 1, 0)))

str(teens4)
hist.data.frame(teens4)

for (i in 3:ncol(teens4)) {
  hist(teens4[,i])
}

teens4_norm = teens4_gender

for (i in 3:ncol(teens4)) {
  teens4_norm[, i] <- teens4[,i] / max(teens4[, i])
}

summary(teens4_norm)

for (i in 3:ncol(teens4)) {
  hist(teens4_norm[,i])
}

teens4_scale <- scale(teens4_gender)

fviz_nbclust(teens4_scale, kmeans, method = "wss", k.max = 5)

k1_list = c()
k1_df <- data.frame()
colnames(k1_df) <- c("Clusters", "WSS")
for (i in 11:16) {
  k1 <- kmeans(teens4_scale, centers = i, nstart = 100)
  k1_list <- append(k1_list, k1)
  k1_df <- rbind(k1_df, c(i, k1$tot.withinss))
}

ggplot(data = k1_df, aes(x = Clusters, y = WSS)) + geom_line()


teen_pca <- prcomp(teens4_gender, scale = F)
tp_data <- cbind.data.frame(teen_pca$x[,1], teen_pca$x[,2], teen_pca$x[,3], teen_pca$x[,4], teen_pca$x[,5], as.factor(k1$cluster))
colnames(tp_data) <- c("PCA1", "PCA2", "PCA3", "PCA4", "PCA5", "cluster")

tp_data_grp <- tp_data %>% group_by(cluster) %>% summarise(mean(PCA1), mean(PCA2), mean(PCA3), mean(PCA4), mean(PCA5))

tp_data <- tp_data[-which(tp_data$PCA2 == min(tp_data$PCA2))]
