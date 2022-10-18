ta_metrics <- read.csv("/Users/noahjohnson/Downloads/bluefall2hwteam17/yearly_sent_metrics.csv")

library(tidyverse)
library(ggplot2)
install.packages(gghighlight)
library(gghighlight)

ta_metrics <- ta_metrics %>% mutate(Avg.Anger = -1 * Avg.Anger, Avg.Disgust = -1 * Avg.Disgust, 
           Avg.Fear = -1 * Avg.Fear, Avg.Neg = -1 * Avg.Neg, Avg.Sadness = -1 * Avg.Sadness) %>% 
  rename(Neg.Anger = Avg.Anger, Neg.Disgust = Avg.Disgust, Neg.Fear = Avg.Fear, Neg.Negative = Avg.Neg, Neg.Sadness = Avg.Sadness, 
         Pos.Anticipation = Avg.Antic, Pos.Joy = Avg.Joy, Pos.Positive = Avg.Pos, Pos.Surprise = Avg.Surprise, Pos.Trust = Avg.Trust)

#ta_metrics <- ta_metrics %>% mutate(Avg.Exp.Valence = Neg.Anger + Pos.Anticipation + Neg.Disgust + Neg.Fear + Pos.Joy + Neg.Negative + Pos.Positive + Neg.Sadness + Pos.Surprise + Pos.Trust)
  
ggplot(data = ta_metrics) + geom_line(aes(x = Year, y = Avg.Exp.Valence), color = "#648fff") + 
  scale_x_continuous(breaks = seq(min(ta_metrics$Year),max(ta_metrics$Year),by=1)) + 
  geom_line(aes(x = Year, y = rep(0, nrow(ta_metrics))), color = "#ffb000", linetype = "dashed") + 
  geom_point(aes(x = 2019, y = Avg.Exp.Valence[14]), color = "#dc267f")

tam_avg <- data.frame()
for (i in 9:19) {
  tam_avg <- rbind(tam_avg, c(colnames(ta_metrics)[i], mean(ta_metrics[,i]), "Overall Average"))
}
colnames(tam_avg) <- c("Metric", "Value", "Type")

tam_long <- ta_metrics %>% pivot_longer(cols = -1, names_to = "Metric") 
tam_2019 <- tam_long %>% filter(X == 13) %>% dplyr::select(-X) %>% 
  mutate(Type = "2019") %>% rename(Value = value)
tam_2019 <- tam_2019[8:18,]
tam_2019 <- rbind(tam_2019, data.frame(tam_avg))
tam_2019 <- tam_2019 %>% mutate(Value = as.numeric(Value))

ggplot(data = tam_2019) + geom_bar(aes(x = Metric, y = Value , fill = Type), position = "dodge", stat = "identity") + 
  scale_x_discrete(labels = c("Valence", "Anger", "Disgust", "Fear", "Negativity", "Sadness", "Anticipation", "Joy", "Positivity", "Surprise", "Trust")) + 
  ylim(c(-0.2, 0.2)) + theme(axis.text.x = element_text(angle = 45, vjust = 0.5))

tam_2020 <- tam_long %>% filter(X == 14) %>% dplyr::select(-X) %>% 
  mutate(Type = "2020") %>% rename(Value = value)
tam_2020 <- tam_2020[8:18,]

tam_2021 <- tam_long %>% filter(X == 15) %>% dplyr::select(-X) %>% 
  mutate(Type = "2021") %>% rename(Value = value)
tam_2021 <- tam_2021[8:18,]

tam_2019_20_21 <- rbind(tam_2019, tam_2020, tam_2021, data.frame(tam_avg))
tam_2019_20_21 <- tam_2019_20_21 %>% mutate(Value = as.numeric(Value))

ggplot(data = tam_2019_20_21) + geom_bar(aes(x = Metric, y = Value , fill = Type), position = "dodge", stat = "identity") + 
  geom_segment(aes(x = 1, xend = 1, y = -0.25, yend = 0.25), color = "#bbbbbb") +
  scale_x_discrete(labels = c("Valence", "Anger", "Disgust", "Fear", "Negativity", "Sadness", "Anticipation", "Joy", "Positivity", "Surprise", "Trust")) + 
  scale_fill_manual(labels = c("2019", "2020", "2021", "Overall Average"), values = c("#648fff", "#ffb000", "#dc267f", "#888888")) +
  ylim(c(-0.2, 0.2)) + theme(axis.text.x = element_text(angle = 45, vjust = 0.5), legend.title = element_text(hjust = 0.5), plot.title = element_text(hjust = 0.5)) + 
  labs(x = "Emotion", y = "Average Value", title = "Late-Year Emotion Analysis")

from_2012 <- ta_metrics %>% filter(Year >= 2012)

from_2012_long <- from_2012 %>% mutate(Avg.Exp.Valence = Avg.Exp.Valence + 0.5) %>% dplyr::select(c(2, 19, 21:22)) %>% pivot_longer(cols = -1, names_to = "Metric")

to_2012 <- ta_metrics %>% filter(Year <= 2011)

to_2012_long <- to_2012 %>% mutate(Avg.Exp.Valence = Avg.Exp.Valence + 0.5) %>% dplyr::select(c(2, 19, 21)) %>% pivot_longer(cols = -1, names_to = "Metric")

long_sent <- ta_metrics %>% mutate(Avg.Exp.Valence = Avg.Exp.Valence + 0.5) %>% dplyr::select(c(2, 19, 21)) %>% pivot_longer(cols = -1, names_to = "Metric")

ggplot(data = long_sent, aes(x = Year, y = value, color = Metric)) + geom_line() + 
  scale_y_continuous(sec.axis = sec_axis(~.-0.5, name = "Expanded Valence")) +
  theme(legend.title = element_text(hjust = 0.5), plot.title = element_text(hjust = 0.5)) + 
  scale_color_manual(labels = c("Expanded Valence", "Global Sentiment Indicator"), values = c("#648fff", "#ffb000")) +
  labs(x = "Year", y = "Combined Sentiment Index", col = "Sentiment Metric", title = "Popular Song Sentiment vs Global Economic Sentiment")


ggplot(data = to_2012_long, aes(x = Year, y = value, color = Metric)) + geom_line() + 
  scale_y_continuous(sec.axis = sec_axis(~.-0.5, name = "Expanded Valence")) +
  theme(legend.title = element_text(hjust = 0.5), plot.title = element_text(hjust = 0.5)) + 
  scale_color_manual(labels = c("Expanded Valence", "Global Sentiment Indicator"), values = c("#648fff", "#ffb000")) +
  labs(x = "Year", y = "Combined Sentiment Index", col = "Sentiment Metric", title = "Popular Song Sentiment vs Global Economic Sentiment until 2012")

twitter_only <- from_2012_long %>% filter(Metric != "Avg.GS.Indicator")

ggplot(data = twitter_only, aes(x = Year, y = value, color = Metric)) + geom_line() + 
  scale_y_continuous(sec.axis = sec_axis(~.-0.5, name = "Expanded Valence")) +
  theme(legend.title = element_text(hjust = 0.5), plot.title = element_text(hjust = 0.5)) + 
  scale_color_manual(labels = c("Expanded Valence", "Twitter Sentiment"), values = c("#648fff", "#dc267f")) +
  labs(x = "Year", y = "Combined Sentiment Index", col = "Sentiment Metric", title = "Popular Song Sentiment vs Twitter Sentiment")
  

comb_metr_df <- data.frame(rbind(to_2012_long, twitter_only)) %>% mutate(Metric2 = ifelse(Metric == "Avg.Exp.Valence", Metric, "Sent.Index"))

ggplot(data = comb_metr_df, aes(x = Year, y = value, color = interaction(Metric, Metric2))) + geom_line() + 
  scale_y_continuous(sec.axis = sec_axis(~.-0.5, name = "Expanded Valence")) +
  theme(legend.title = element_text(hjust = 0.5), plot.title = element_text(hjust = 0.5)) + 
  geom_segment(aes(x = 2011, xend = 2012, y = comb_metr_df["value"][12,] + 0.001, yend = comb_metr_df["value"][14,] + 0.001), color = "#888888", linetype = 2) +
  scale_color_manual(labels = c("Expanded Valence", "Global Sentiment Indicator", "Twitter Sentiment"), values = c("#648fff", "#ffb000", "#dc267f")) +
  labs(x = "Year", y = "Combined Sentiment Index", col = "Sentiment Metric", title = "Popular Song Sentiment vs Combined Global Sentiment")






       