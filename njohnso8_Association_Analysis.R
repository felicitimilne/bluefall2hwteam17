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

order_freq <- order %>% count(item) %>% arrange(desc(n))
ggplot(data = order, aes(x = reorder(item, item, function(x) - length(x)))) + geom_bar() + 
  theme(axis.text.x=element_text(angle = -80, hjust = 0), plot.title = element_text(hjust = 0.5)) +
  labs(x = "Item", y = "Frequency", title = "Descending Order of Item Frequency")

wideorder<- order %>%
  pivot_wider(names_from = type, values_from = item)

meat_list <- unique(wideorder$meat)
wine_list <- unique(wideorder$wine)
side_list <- unique(wideorder$side)

ggplot(data = wideorder, aes(x = reorder(meat, meat, function(x) - length(x)))) + geom_bar() + 
  theme(axis.text.x=element_text(angle = -60, hjust = 0), plot.title = element_text(hjust = 0.5)) +
  labs(x = "Item", y = "Frequency", title = "Descending Order of Meat Frequency")

ggplot(data = wideorder, aes(x = reorder(wine, wine, function(x) - length(x)))) + geom_bar() + 
  theme(axis.text.x=element_text(angle = -70, hjust = 0), plot.title = element_text(hjust = 0.5)) +
  labs(x = "Item", y = "Frequency", title = "Descending Order of Wine Frequency")

ggplot(data = wideorder, aes(x = reorder(side, side, function(x) - length(x)))) + geom_bar() + 
  theme(axis.text.x=element_text(angle = -60, hjust = 0), plot.title = element_text(hjust = 0.5)) +
  labs(x = "Item", y = "Frequency", title = "Descending Order of Side Frequency")

trans.order <- as(split(order$item, order$OrderSeat), "transactions")
# order.rules <- trans.order %>% apriori(parameter = list(supp = 0.01, conf = 0.25, target = "rules")) %>% sort(by = "confidence", decreasing = TRUE)
# order.rules.df <- as.data.frame(inspect(order.rules))
# order.rules.df <- order.rules.df %>% dplyr::select(lhs, rhs, support, confidence, coverage, lift, count) %>%
#   filter(lhs != "{}") %>% arrange(by = desc(confidence))

# plot(order.rules)
# ggplot(data = order.rules.df)
# plot(head(order.rules, n = 10), method = "graph", engine = "htmlwidget")

order.rules.meat <- trans.order %>% apriori(parameter = list(supp = 0.008, conf = 0.15, target = "rules"), appearance = list(default = "rhs", lhs = unique(wideorder$meat))) %>% sort(by = "lift", decreasing = TRUE)
order.rules.meat.df <- as.data.frame(inspect(order.rules.meat)) 
list = str_replace(str_replace(order.rules.meat.df$rhs, "\\{", ""), "\\}", "")
index = c()
for (i in 1:length(list)) { 
  if (list[i] %in% unique(wideorder$wine)) {
    index = append(index, i)
  }
}
order.rules.meat.df <- order.rules.meat.df[index,]

unique.meats.df.lift <- order.rules.meat.df[match(unique(order.rules.meat.df$lhs), order.rules.meat.df$lhs),]
unique.meats.df.lift <- unique.meats.df.lift[1:length(unique(wideorder$meat)),]

plot(order.rules.meat.df)
plot(unique.meats.df)
plot(unique.meats.df.lift)

# plot(order.rules.fm)
# 
# order.rules.bs <- trans.order %>% apriori(parameter = list(supp = 0.01, conf = 0.25, target = "rules"), appearance = list(default = "lhs", rhs = "Blackstone Merlot")) %>% sort(by = "confidence", decreasing = TRUE)
# bs_plot <- plot(order.rules.bs)

