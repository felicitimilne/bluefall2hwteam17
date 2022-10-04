#code for data mining homework 1

# libraries
library(dplyr)
library(tidyr)
library(arules)
library(seasonal)
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
#library(caret)
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

#load data
df <- read.csv("https://github.com/sjsimmo2/DataMining-Fall/blob/master/orderData.csv?raw=true", header = TRUE)

#reformat data to wide (thank you Patrick!)
df$OrderSeat <- paste(df$orderNo, df$seatNo)
df$type <- rep(c('meat', 'wine', 'side'), times= (nrow(df)/3))
wideorder <- df %>%
  pivot_wider(names_from = type, values_from = item)

wideorder <- mutate(wideorder, personOrder = paste(meat, wine, side)) 

trans.data <- wideorder %>% 
  dplyr::select('OrderSeat', 'personOrder')

#association analysis
inspect(trans.data) #keep getting error with inspect
