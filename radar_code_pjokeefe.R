library(tidyverse)


tennis <- read.csv("https://github.com/felicitimilne/bluefall2hwteam17/raw/main/match_stats.csv")


tennis$winners_norm <- ((tennis$winners-min(tennis$winners))/(max(tennis$winners)-min(tennis$winners)))

tennis$forehand_norm <- ((tennis$forehand_winner-min(tennis$forehand_winner))/(max(tennis$forehand_winner)-min(tennis$forehand_winner)))

tennis$backhand_norm <- ((tennis$backhand_winner-min(tennis$backhand_winner))/(max(tennis$backhand_winner)-min(tennis$backhand_winner)))

tennis$errors_forced_norm <- ((tennis$errors_forced-min(tennis$errors_forced))/(max(tennis$errors_forced)-min(tennis$errors_forced)))

tennis$unforced_errors_norm <- ((tennis$unforced_errors-min(tennis$unforced_errors))/(max(tennis$unforced_errors)-min(tennis$unforced_errors)))

tennis$aces_norm <- ((tennis$aces-min(tennis$aces))/(max(tennis$aces)-min(tennis$aces)))


group <- tennis %>%
  group_by(player) %>%
  summarise(winners_norm = mean(winners_norm),
            forehand_norm = mean(forehand_norm),
            errors_forced_norm = mean(errors_forced_norm),
            backhand_norm = mean(backhand_norm),
            unforced_errors_norm = mean(unforced_errors_norm),
            aces_norm = mean(aces_norm))

write.csv(tennis, file = "C:/HWTeam2Git/bluefall2hwteam17/radar_tennis.csv")


write.csv(group, file = "C:/HWTeam2Git/bluefall2hwteam17/radar_tennis_test.csv")
