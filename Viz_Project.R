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


##loading ncsu tennis singles stats
ncsu_stats <- read.csv("https://raw.githubusercontent.com/felicitimilne/bluefall2hwteam17/main/NcstateSinglesStatReport.csv")

#removing white space to avoid same player registering as two different players
ncsu_stats$player <- trimws(ncsu_stats$player)

################# EDA & data cleaning ####################

#creating player list
players <- data.frame(player = unique(ncsu_stats$player)) %>% arrange(player)

#sub-setting career matches and points
career_matches_played <- ncsu_stats %>% group_by(player) %>% summarize(matches = length(unique(matchId))) %>% arrange(player)

career_points_played <- ncsu_stats %>% count(player) %>% arrange(player)


#sub-setting each season's matches and points





# temp variable for testing
# test <- ncsu_stats %>% count(player)
# 
# temp function for EDA within various dfs
# unique(test$errorType)
# 
# max(ncsu_stats$stopTime)
# min(ncsu_stats$startTime)
# 
# variables with single option (provide no info) = matchType, adType (there is no "advantage" in college), opp2, setTo, player2, 
# unique(ncsu_stats$errorType)
# 
# removing useless columns
# ncsu_stats <- subset(ncsu_stats, select = -c(matchType,adType,opp2,matchLink, stopTime, startTime, setTo, player2))


############ Goal 1.1:Create career and season cumulative stats profile for each NCSU player using all 17 metrics. ##############

#### initializing cumulative stat tables ####

#career
player_metrics_career <- players
player_metrics_career$matches <- career_matches_played$matches
player_metrics_career$points_played <- career_points_played$n

#season



### Desired stats: ###

#1

#career
Total_points_won <- ncsu_stats %>% filter(pointWonBy == 0)
total_points_won_count <- Total_points_won %>% group_by(player) %>% tally() %>% arrange(player)

player_metrics_career$points_won <- total_points_won_count$n

#16332/30173

#season



#2

#career
Winners <- ncsu_stats %>% filter(outcome == "Winner" & pointWonBy == 0)
winners_count <- Winners %>% group_by(player) %>% tally() %>% arrange(player)

player_metrics_career$winners <- winners_count$n

  #3687 ncsu / 6684 total

#season



#3

#career
Errors_forced <- ncsu_stats %>% filter(outcome == "ForcedError" & pointWonBy == 0)
errors_forced_count <- Errors_forced %>% group_by(player) %>% tally() %>% arrange(player)

player_metrics_career$errors_forced <-errors_forced_count$n

#season




#4

#career
Unforced_errors <- ncsu_stats %>% filter(outcome == "UnforcedError" & pointWonBy == 1)
unforced_errors_count <- Unforced_errors %>% group_by(player) %>% tally() %>% arrange(player)

player_metrics_career$unforced_errors <- unforced_errors_count$n

#season



#5

#career
Break_points <- ncsu_stats %>% filter(breakPoint == TRUE & pointWonBy == 0)
break_points_count <- Break_points %>% group_by(player) %>% tally() %>% arrange(player)

player_metrics_career$break_points <- break_points_count$n

  #2474 ncsu / 4510 total
  
#season



#6

#career
Aces <- ncsu_stats %>% filter(outcome == "Ace" & pointWonBy == 0)
aces_count <- Aces %>% group_by(player) %>% tally() %>% arrange(player)

player_metrics_career$aces <- aces_count$n

  ##compare to opponents! simply divide by num obs without filtering server

#season
  


#7

#career
Double_faults <- ncsu_stats %>% filter(outcome == "Fault" & pointWonBy == 1)
double_faults_count <- Double_faults %>% group_by(player) %>% tally() %>% arrange(player)

player_metrics_career$double_faults <- double_faults_count$n

  #849 ncsu /1755 total

#season



#8

#career
First_serve_in <- ncsu_stats %>% filter(firstServeIn == TRUE & server == 0)
first_serve_in_count <- First_serve_in %>% group_by(player) %>% tally() %>% arrange(player)

player_metrics_career$first_serves_in <- first_serve_in_count$n

#First_serve_in <- ncsu_stats %>% filter(firstServeIn == TRUE)

    ##~60% of observations -- compare other percentages to this group
  
#season



#9

#career
First_serves_won <- ncsu_stats %>% filter(firstServeIn == TRUE & server == 0 & pointWonBy == 0)
first_serves_won_count <- First_serves_won %>% group_by(player) %>% tally() %>% arrange(player)

player_metrics_career$first_serves_won <- first_serves_won_count$n

  ##compare to first serve in
  
#season




#10

#career
Second_serve_in <- ncsu_stats %>% filter(firstServeIn == FALSE & server == 0 & outcome != "Fault")
second_serve_in_count <- Second_serve_in %>% group_by(player) %>% tally() %>% arrange(player)

player_metrics_career$second_serves_in <- second_serve_in_count$n

#season



#11

#career
Second_serves_won <- ncsu_stats %>% filter(firstServeIn == FALSE & server == 0 & pointWonBy == 0)
second_serves_won_count <- Second_serves_won %>% group_by(player) %>% tally() %>% arrange(player)

player_metrics_career$second_serves_won <- second_serves_won_count$n

#season




#12

#career
First_serve_returns <- ncsu_stats %>% filter(firstServeIn == TRUE & server == 1 & returnInPlay == TRUE)
first_serve_returns_count <- First_serve_returns %>% group_by(player) %>% tally() %>% arrange(player)

player_metrics_career$first_serve_returns <- first_serve_returns_count$n

  #7977 first serve returns / 9609 first serves in faced
  
#season



#13

#career
Second_serve_returns <- ncsu_stats %>% filter(firstServeIn == FALSE & server == 1 & returnInPlay == TRUE)
second_serve_returns_count <- Second_serve_returns %>% group_by(player) %>% tally() %>% arrange(player)

player_metrics_career$second_serve_returns <- second_serve_returns_count$n

#season



#14

#career
Short_rallies_won <- ncsu_stats %>% filter(rallyLength < 5 & pointWonBy == 0)
short_rallies_won_count <- Short_rallies_won %>% group_by(player) %>% tally() %>% arrange(player)
    
player_metrics_career$short_rallies_won <- short_rallies_won_count$n

#compare to total short rallies
  
#season



#15

#career
Medium_rallies_won <- ncsu_stats %>% filter(rallyLength >= 5 & rallyLength < 9  & pointWonBy == 0)
medium_rallies_won_count <- Medium_rallies_won %>% group_by(player) %>% tally() %>% arrange(player)

player_metrics_career$medium_rallies_won <- medium_rallies_won_count$n

    #compare to total rallies of medium length
  
#season



#16

#career
Long_rallies_won <- ncsu_stats %>% filter(rallyLength > 8 & pointWonBy == 0)
long_rallies_won_count <- Long_rallies_won %>% group_by(player) %>% tally() %>% arrange(player)

player_metrics_career$long_rallies_won <- long_rallies_won_count$n

#season



#17

#career
Service_games_won <- ncsu_stats %>% filter(server == 0 & pointWonBy == 0)
service_games_won_count <- Service_games_won %>% group_by(player) %>% tally() %>% arrange(player)

player_metrics_career$service_games_won <- service_games_won_count$n

    #8749 ncsu / 15045 total

#season




## adding row for team totals ##
player_metrics_career <- player_metrics_career %>% bind_rows(summarise_all(., ~if(is.numeric(.)) sum(.) else "Total"))

## write to csv ##





  ####creating player career stats data frame####

# player_metrics_career <- players
# player_metrics_career$matches <- career_matches_played$matches
# player_metrics_career$points_played <- career_points_played$n

# player_metrics_career$points_won <- total_points_won_count$n
# player_metrics_career$winners <- winners_count$n
# player_metrics_career$errors_forced <-errors_forced_count$n
# player_metrics_career$unforced_errors <- unforced_errors_count$n
# player_metrics_career$break_points <- break_points_count$n
# player_metrics_career$aces <- aces_count$n
# player_metrics_career$double_faults <- double_faults_count$n
# player_metrics_career$first_serves_in <- first_serve_in_count$n
# player_metrics_career$first_serves_won <- first_serves_won_count$n
# player_metrics_career$second_serves_in <- second_serve_in_count$n
# player_metrics_career$second_serves_won <- second_serves_won_count$n
# player_metrics_career$first_serve_returns <- first_serve_returns_count$n
# player_metrics_career$second_serve_returns <- second_serve_returns_count$n
# player_metrics_career$short_rallies_won <- short_rallies_won_count$n
# player_metrics_career$medium_rallies_won <- medium_rallies_won_count$n
# player_metrics_career$long_rallies_won <- long_rallies_won_count$n
# player_metrics_career$service_games_won <- service_games_won_count$n

#sum(winners_count$n)

#library(help = "base")

## Goal 1.2: Visualize key metric averages for both season and career. ##
#Tableau???


## Goal 2.1: Create a team leaderboard for each stat based on averages in each metric. ##

### Desired stats: ###

career_matches_played <- ncsu_stats %>% group_by(player) %>% summarize(matches = length(unique(matchId))) %>% arrange(player)

career_points_played <- ncsu_stats %>% count(player) %>% arrange(player)

#1

#career
L1 <- Total_points_won %>% group_by(player) %>% tally() %>% arrange(player) %>% rename("Career Points Won" = "n")
L1$match_avg <- L1[2]/career_matches_played$matches
L1$point_avg <- L1[2]/career_points_played$n
arrange(desc(L1$point_avg))

#season

?rename

#2

#career
Leaderboard_winners_career <- Winners %>% group_by(player) %>% tally() %>% arrange(desc(n)) %>% rename("Career Winners" = "n")

#season



#3

#career
Leaderboard_errors_forced_career <- Errors_forced %>% group_by(player) %>% tally() %>% arrange(desc(n)) %>% rename("Career Errors Forced" = "n")

#season




#4

#career
Leaderboard_unforced_errors_career <- Unforced_errors %>% group_by(player) %>% tally() %>% arrange(n) %>% rename("Career Unforced Errors" = "n")

#season



#5

#career
Leaderboard_break_points_career <- Break_points %>% group_by(player) %>% tally() %>% arrange(desc(n)) %>% rename("Career Break Points" = "n")

#season



#6

#career
Leaderboard_aces_career <- Aces %>% group_by(player) %>% tally() %>% arrange(desc(n)) %>% rename("Career Aces" = "n")

#season



#7

#career
Leaderboard_double_faults_career <- Double_faults %>% group_by(player) %>% tally() %>% arrange(n) %>% rename("Career Faults" = "n")


#season



#8

#career
Leaderboard_first_serve_in_career <- First_serve_in %>% group_by(player) %>% tally() %>% arrange(desc(n)) %>% rename("Career First Serves In" = "n")

#season



#9

#career
Leaderboard_first_serves_won_career <- First_serves_won %>% group_by(player) %>% tally() %>% arrange(desc(n)) %>% rename("Career First Serves Won" = "n")

#season




#10

#career
Leaderboard_second_serve_in_career <- Second_serve_in %>% group_by(player) %>% tally() %>% arrange(desc(n)) %>% rename("Career Second Serves In" = "n")

#season



#11

#career
Leaderboard_second_serves_won_career <- Second_serves_won %>% group_by(player) %>% tally() %>% arrange(desc(n)) %>% rename("Career Second Serves Won" = "n")

#season




#12

#career
Leaderboard_first_serve_returns_career <- First_serve_returns %>% group_by(player) %>% tally() %>% arrange(desc(n)) %>% rename("Career First Serves Returned" = "n")

#season



#13

#career
Leaderboard_second_serve_returns_career <- Second_serve_returns %>% group_by(player) %>% tally() %>% arrange(desc(n)) %>% rename("Career Second Serves Returned" = "n")

#season



#14

#career
Leaderboard_short_rallies_won_career <- Short_rallies_won %>% group_by(player) %>% tally() %>% arrange(desc(n)) %>% rename("Career Short Rallies Won" = "n")

#season



#15

#career
Leaderboard_medium_rallies_won_career <- Medium_rallies_won %>% group_by(player) %>% tally() %>% arrange(desc(n)) %>% rename("Career Med Rallies Won" = "n")

#season



#16

#career
Leaderboard_long_rallies_won_career <- Long_rallies_won %>% group_by(player) %>% tally() %>% arrange(desc(n)) %>% rename("Career Long Rallies Won" = "n")

#season



#17

#career
Leaderboard_service_games_won_career <- Service_games_won %>% group_by(player) %>% tally() %>% arrange(desc(n)) %>% rename("Career Service Games Won" = "n")

#season



## Goal 2.2: Visualize which players are stronger in each area. ##

#Tableau????



##Goal 3.1: Create a stats profile for each player based on wins vs losses. ##




##Goal 3.2: Attempt to identify key trends that are different in wins vs losses that might be predictors of outcome. ##




##Goal 4: Visualize trends over time for the key metrics for each player (start with ncsu, but maybe include opp?). ##
##For example, trend line of a player's first serve % over their recorded matches. ##




##Goal 5: Repeat of goal #4 for team as a whole. ##




##Goal 6.1: Shows metric vs stats(?) relative to indoor vs outdoor conditions. ##




##Goal 6.2: Identify significant differences in any of the key metrics based on conditions, if any exist. ##




##Goal 7: Create a plus/minus ratio for each player. Use Aggressive error margin formula. ##




##Goal 8: Develop a First serve performance rating metric. Using first serve rating formula. ##




##Goal 9: Develop a simple UI to quickly update CSV and identify real time trends as the season progresses. ##




