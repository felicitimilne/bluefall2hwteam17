##Patrick's code
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


order <- read.csv("https://github.com/sjsimmo2/DataMining-Fall/blob/master/orderData.csv?raw=true", header = TRUE)

head(order)

order$OrderSeat <- paste(order$orderNo, order$seatNo)
order$type <- rep(c('meat', 'wine', 'side'), times= (nrow(order)/3))


wideorder<- order %>%
  pivot_wider(names_from = type, values_from = item)

##Josh's code
