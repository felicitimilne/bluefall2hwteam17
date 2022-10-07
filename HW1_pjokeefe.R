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
library(stringr)
#library(knitr)


order <- read.csv("https://github.com/sjsimmo2/DataMining-Fall/blob/master/orderData.csv?raw=true", header = TRUE)

head(order)

order$OrderSeat <- str_remove_all(paste(str_remove_all(order$orderNo, " "), str_remove_all(order$seatNo, " ")), " ")

order$type <- rep(c('meat', 'wine', 'side'), times= (nrow(order)/3))

#Filter to meats only
meat <- order %>%
  filter(type == 'meat')

meat <- meat[,c('item', 'OrderSeat')]

graph <- meat %>%
  group_by(item) %>%
  count() %>%
  head(5)

  
ggplot(graph, aes(x=reorder(item, -n), y = n)) +
  geom_bar(stat = 'identity', fill = "brown2") +
  xlab("Main Entree Name") +
  ylab("Count of Meats Sold") + 
  ggtitle("Top 5 Most Frequently Purchased Meats") +
  theme(plot.title = element_text(hjust = 0.5))


#Filter to wines only
wine <- order %>%
  filter(type == 'wine')

wine <- wine[,c('item', 'OrderSeat')]

graph1 <- wine %>%
  group_by(item) %>%
  count() %>%
  head(5)


ggplot(graph1, aes(x=reorder(item, -n), y = n)) +
  geom_bar(stat = 'identity', fill = "darkorchid2") +
  xlab("Wine Name") +
  ylab("Count of Wines Sold") + 
  ggtitle("Top 5 Most Frequently Purchased Wines") +
  theme(plot.title = element_text(hjust = 0.5))

#Filter to sides only
side <- order %>%
  filter(type == 'side')

side <- side[,c('item', 'OrderSeat')]

graph2 <- side %>%
  group_by(item) %>%
  count() %>%
  head(5)


ggplot(graph2, aes(x=reorder(item, -n), y = n)) +
  geom_bar(stat = 'identity', fill = "darkolivegreen4") +
  xlab("Side Name") +
  ylab("Count of Sides Sold") + 
  ggtitle("Top 5 Most Frequently Purchased Sides") +
  theme(plot.title = element_text(hjust = 0.5))

#Item transactions of all items
temp <- as(split(order$item, order$OrderSeat), "transactions")

inspect(temp)


temp@itemInfo$labels

itemFrequencyPlot(temp, topN=5, type = "absolute", xlab = "Top 5 Most Frequently Purchased Items"
                  , ylab = "Count of Items Sold")

rules <- apriori(temp, parameter = list(supp=0.001, conf=0.001, target = "rules"), appearance = list(default = "lhs", rhs = "Salmon"))

rules <- sort(rules, by = "lift", decreasing = TRUE)

inspect(head(rules))

##################################################################################


#Reorder the data to wide
wideorder <- order %>%
  pivot_wider(names_from = type, values_from = item)

wideorder %>% select(OrderSeat)

wideorder <- wideorder[,c('OrderSeat','meat', 'wine', 'side')]

order2 <- order[,c('OrderSeat', 'item')]

test <- as(split(order$item, temp.dat$OrderSeat), "transactions")
inspect(test)
