#!/usr/bin/env python3
# -*- coding: utf-8 -*-
"""
Created on Fri Oct 21 12:44:03 2022

@author: josiahhull
"""

# import libraries
import pandas as pd
import csv

# load data and create dataframe
rawdata = pd.read_csv("https://raw.githubusercontent.com/felicitimilne/bluefall2hwteam17/main/NcstateSinglesStatReport.csv")

indoor_outdoor = pd.read_csv("https://raw.githubusercontent.com/felicitimilne/bluefall2hwteam17/main/Indoors%20and%20Outdoors.csv")

# make clean dataset
cleandata = rawdata

# remove white space from player name
cleandata['player'] = cleandata['player'].str.strip()

# remove errors in rally length(those that do not match with the time duration
##### team - how should we deal with this? 
##### my idea is checking the distribution of rally lengths, checking all rally lengths above a certain (unlikely) number, then cross-check with point time duration and the actual footage on Cizr.

# take care of instances where nc state players play against each other - are they counted separately/are they reflected in the data?
# "coach secker: Count them for each of our players as normal. They are counting matches in a players record."
##### team - how should we deal with this?



# create seasons function

# if month is 1,2,3,4,5,6 --> current season, take year value 
# if month is 7,8,9,10,11,12 --> next season, year value + 1

def find_season(date):
    if int(date.split('/')[0]) < 7:
        return int(date.split('/')[2])
    else:
        return int(date.split('/')[2]) + 1

# adding season column to data

cleandata.insert(loc = 3,
          column = 'season',
          value = '')

cleandata['season'] = cleandata['date'].apply(find_season) 


# create list of player names
players = cleandata['player'].unique()


### Create functions ###

def get_player_name(df, match_id):
    return df[(df['matchId'] == match_id)]['player'].iloc[0]

def get_opp_name(df, match_id):
    return df[(df['matchId'] == match_id)]['opp'].iloc[0]

def get_season(df, match_id):
    return df[(df['matchId'] == match_id)]['season'].iloc[0]


################function incomplete (match result, indoor/outdoor)#######################
def get_match_result(df, match_id):
    return df[(df['matchId'] == match_id)]['season'].iloc[0]
################################function incomplete#######################


def indoors(match_id):
    if indoor_outdoor[(indoor_outdoor['matchId'] == match_id)]['Indoors'].iloc[0] == "Indoors":
        return True
    else:
        return False

def total_points_won(df, match_id):
    return len(df[(df['matchId'] == match_id) & (df['pointWonBy'] == 0)])

def winners(df, match_id):
    return len(df[(df['matchId'] == match_id) & (df['pointWonBy'] == 0) & (df['outcome'] == 'Winner')])

def forehand_winners(df, match_id):
    return len(df[(df['matchId'] == match_id) & (df['pointWonBy'] == 0) & (df['outcome'] == 'Winner') & (df['shotType'] == 'Forehand')])

def backhand_winners(df, match_id):
    return len(df[(df['matchId'] == match_id) & (df['pointWonBy'] == 0) & (df['outcome'] == 'Winner') & (df['shotType'] == 'Backhand')])

def errors_forced(df, match_id): 
    return len(df[(df['matchId'] == match_id) & (df['pointWonBy'] == 0) & (df['outcome'] == 'ForcedError')])

def unforced_errors(df, match_id): 
    return len(df[(df['matchId'] == match_id) & (df['pointWonBy'] == 1) & (df['outcome'] == 'UnforcedError')])

#add error type?

def unforced_errors_forehand(df, match_id):     #added by Josh
    return len(df[(df['matchId'] == match_id) & (df['pointWonBy'] == 1) & (df['outcome'] == 'UnforcedError') & (df['shotType'] == 'Forehand')])

def unforced_errors_backhand(df, match_id):     #added by Josh
    return len(df[(df['matchId'] == match_id) & (df['pointWonBy'] == 1) & (df['outcome'] == 'UnforcedError') & (df['shotType'] == 'Backhand')])

def break_points_total(df, match_id):
    return len(df[(df['matchId'] == match_id) & (df['breakPoint'] == True) & (df['server'] == 1)])

def break_points_won(df, match_id):
    return len(df[(df['matchId'] == match_id) & (df['breakPoint'] == True) & (df['server'] == 1) & (df['pointWonBy'] == 0)])

def aces(df, match_id):
    return len(df[(df['matchId'] == match_id) & (df['outcome'] == 'Ace') & (df['server'] == 0)])

def double_faults(df, match_id):
    return len(df[(df['matchId'] == match_id) & (df['outcome'] == 'Fault') & (df['server'] == 0)])

def total_serves(df, match_id):
    return len(df[(df['matchId'] == match_id) & (df['server'] == 0)])

#add total_first_serves???

def first_serves_in(df, match_id):
    return len(df[(df['matchId'] == match_id) & (df['server'] == 0) & (df['firstServeIn'] == True)])

def first_serve_in_percentage(df, match_id): 
    return first_serves_in(df, match_id) / total_serves(df, match_id)

def first_serves_won(df, match_id):     #added by Josh
    return len(df[(df['matchId'] == match_id) & (df['server'] == 0) & (df['firstServeIn'] == True) & (df['pointWonBy'] == 0)])

def first_serves_won_percentage(df, match_id): 
    return len(df[(df['matchId'] == match_id) & (df['server'] == 0) & (df['firstServeIn'] == True) & (df['pointWonBy'] == 0)]) / first_serves_in(df, match_id)

def total_second_serves(df, match_id):
    return total_serves(df, match_id) - first_serves_in(df, match_id)

def second_serves_in(df, match_id):
    return total_second_serves(df, match_id) - len(df[(df['matchId'] == match_id) & (df['server'] == 0) & (df['firstServeIn'] == False) & (df['outcome'] == 'Fault')])

def second_serves_in_percentage(df, match_id):
    return second_serves_in(df, match_id) / total_second_serves(df, match_id)

def second_serves_won(df, match_id):    #added by Josh
    return len(df[(df['matchId'] == match_id) & (df['server'] == 0) & (df['firstServeIn'] == False) & (df['pointWonBy'] == 0)])

def second_serves_won_percentage(df, match_id):
    return len(df[(df['matchId'] == match_id) & (df['server'] == 0) & (df['firstServeIn'] == False) & (df['pointWonBy'] == 0)]) / total_second_serves(df, match_id)

def first_serve_returns(df, match_id):  #added by Josh
    return len(df[(df['matchId'] == match_id) & (df['returner'] == 0) & (df['firstServeIn'] == True) & (df['returnInPlay'] == True)])

def first_serve_returns_percentage(df, match_id):
    return len(df[(df['matchId'] == match_id) & (df['returner'] == 0) & (df['firstServeIn'] == True) & (df['returnInPlay'] == True)]) / len(df[(df['matchId'] == match_id) & (df['returner'] == 0) & (df['firstServeIn'] == True)])

def second_serve_returns(df, match_id):     #added by Josh
       return len(df[(df['matchId'] == match_id) & (df['returner'] == 0) & (df['firstServeIn'] == False) & (df['outcome'] != "Fault") & (df['returnInPlay'] == True)])

def second_serve_returns_percentage(df, match_id):
       return len(df[(df['matchId'] == match_id) & (df['returner'] == 0) & (df['firstServeIn'] == False) & (df['outcome'] != "Fault") & (df['returnInPlay'] == True)]) / len(df[(df['matchId'] == match_id) & (df['returner'] == 0) & (df['firstServeIn'] == False) & (df['outcome'] != "Fault")])

def short_rallies_won(df, match_id):    #added by Josh
       return len(df[(df['matchId'] == match_id) & (df['rallyLength'] < 5) & (df['pointWonBy'] == 0)])

def short_rallies_won_percentage(df, match_id):
       return len(df[(df['matchId'] == match_id) & (df['rallyLength'] < 5) & (df['pointWonBy'] == 0)]) / len(df[(df['matchId'] == match_id) & (df['rallyLength'] < 5)])

def medium_rallies_won(df, match_id):   #added by Josh
       return len(df[(df['matchId'] == match_id) & (df['rallyLength'] >= 5) & (df['rallyLength'] < 9) & (df['pointWonBy'] == 0)])

def medium_rallies_won_percentage(df, match_id):
       return len(df[(df['matchId'] == match_id) & (df['rallyLength'] >= 5) & (df['rallyLength'] < 9) & (df['pointWonBy'] == 0)]) / len(df[(df['matchId'] == match_id) & (df['rallyLength'] >= 5) & (df['rallyLength'] < 9)])

def long_rallies_won(df, match_id):     #added by Josh
       return len(df[(df['matchId'] == match_id) & (df['rallyLength'] > 8) & (df['pointWonBy'] == 0)])

def long_rallies_won_percentage(df, match_id):
       return len(df[(df['matchId'] == match_id) & (df['rallyLength'] > 8) & (df['pointWonBy'] == 0)]) / len(df[(df['matchId'] == match_id) & (df['rallyLength'] > 8)])

def service_games_total(df, match_id):
    return len(pd.unique(df[(df['matchId'] == match_id) & (df['server'] == 0) & (df['tiebreaker'] == False)]['game']))

def service_games_won(df, match_id):
    return len(pd.unique(df[(df['matchId'] == match_id) & (df['server'] == 0) & (df['tiebreaker'] == False) & (df['gameWonBy'] == 0)]['game']))

def service_games_won_percentage(df, match_id):
    return service_games_won(df, match_id) / service_games_total(df, match_id)

# aggressive error margin formula = (Winner + Forced Errors)-unforced errors
# josh added aces and faults --> aem formula = (Aces + winners + forced errors) - (unforced errors + faults)
def aggressive_error_margin(df, match_id):
    return (len(df[((df['matchId'] == match_id) & (df['pointWonBy'] == 0)) & ((df['outcome'] == 'Ace') | (df['outcome'] == 'Winner') | (df['outcome'] == 'ForcedError'))])) - (len(df[(df['matchId'] == match_id) & (df['pointWonBy'] == 1) & ((df['outcome'] == 'Fault') | df['outcome'] == 'UnforcedError')]))

# First serve performance rating metric formula = (First Serve %)x(first serve win %/100)
def first_serve_performance(df, match_id):
    return first_serve_in_percentage(df, match_id) * first_serves_won_percentage(df, match_id)

#add function to differentiate by set???


# match stats data frame

unique_match_ids = list(cleandata['matchId'].unique())

df = cleandata

completed_frame = {'match_id' : unique_match_ids,
                    'player': [get_player_name(df, x) for x in unique_match_ids],
                    'opponent': [get_opp_name(df, x) for x in unique_match_ids],
                    'season' : [get_season(df, x) for x in unique_match_ids],
                    'match_result' : [0]*len(unique_match_ids),
                    'indoors' : [indoors(x) for x in unique_match_ids],
                    'total_points_won' : [total_points_won(df, x) for x in unique_match_ids],
                    'winners' : [winners(df, x) for x in unique_match_ids],
                    'forehand_winner' : [forehand_winners(df, x) for x in unique_match_ids],
                    'backhand_winner' : [backhand_winners(df, x) for x in unique_match_ids],
                    'errors_forced' : [errors_forced(df, x) for x in unique_match_ids],
                    'unforced_errors' : [unforced_errors(df, x) for x in unique_match_ids],
                    #josh added unforced forehand
                    'unforced_errors_forehand' : [unforced_errors_forehand(df, x) for x in unique_match_ids],
                    #josh added unforced backhand
                    'unforced_errors_backhand' : [unforced_errors_backhand(df, x) for x in unique_match_ids],
                    'break_points_total' : [break_points_total(df, x) for x in unique_match_ids],
                    'break_points_won' : [break_points_won(df, x) for x in unique_match_ids],
                    'aces' : [aces(df, x) for x in unique_match_ids],
                    'double_faults' : [double_faults(df, x) for x in unique_match_ids],
                    #add total serves??
                    #add first serves???
                    #josh added first serves in
                    'first_serves_in' : [first_serves_in(df, x) for x in unique_match_ids],
                    'first_serve_in_percentage' : [first_serve_in_percentage(df, x) for x in unique_match_ids],
                    'first_serves_won' : [first_serves_won(df, x) for x in unique_match_ids],
                    #josh added first serves won percentage
                    'first_serves_won_percentage' : [first_serves_won_percentage(df, x) for x in unique_match_ids],
                    #add total second serves???
                    #josh added second serves in 
                    'second_serves_in' : [second_serves_in(df, x) for x in unique_match_ids],
                    'second_serves_in_percentage' : [second_serves_in_percentage(df, x) for x in unique_match_ids],
                    'second_serves_won' : [second_serves_won(df, x) for x in unique_match_ids],
                    #josh added second serves won percentage
                    'second_serves_won_percentage' : [second_serves_won_percentage(df, x) for x in unique_match_ids],
                    'first_serve_returns' : [first_serve_returns(df, x) for x in unique_match_ids],
                    #josh added first serve returns percentage
                    'first_serve_returns_percentage' : [first_serve_returns_percentage(df, x) for x in unique_match_ids],
                    'second_serve_returns' : [second_serve_returns(df, x) for x in unique_match_ids],
                    #josh added second serve returns percentage
                    'second_serve_returns_percentage' : [second_serve_returns_percentage(df, x) for x in unique_match_ids],
                    'short_rallies_won' : [short_rallies_won(df, x) for x in unique_match_ids],
                    #josh added short rallies won percentage
                    'short_rallies_won_percentage' : [short_rallies_won_percentage(df, x) for x in unique_match_ids],
                    'medium_rallies_won' : [medium_rallies_won(df, x) for x in unique_match_ids],
                    #josh added medium rallies won percentage
                    'medium_rallies_won_percentage' : [medium_rallies_won_percentage(df, x) for x in unique_match_ids],
                    'long_rallies_won' : [long_rallies_won(df, x) for x in unique_match_ids],
                    #josh added long rallies won percentage
                    'long_rallies_won_percentage' : [long_rallies_won_percentage(df, x) for x in unique_match_ids],
                    #josh added service games total
                    'service_games_total' : [service_games_total(df, x) for x in unique_match_ids],
                    'service_games_won' : [service_games_won(df, x) for x in unique_match_ids],
                    #josh added service games win percentage
                    'service_games_won_percentage' : [service_games_won_percentage(df, x) for x in unique_match_ids],
                    'aggressive_error_margin' : [aggressive_error_margin(df, x) for x in unique_match_ids],
                    'first_serve_performance' : [first_serve_performance(df, x) for x in unique_match_ids]}


match_stats = pd.DataFrame(data = completed_frame)

match_stats.round(2).to_csv('match_stats.csv')


# player career stats dataframe

player_career = match_stats.loc[:, ~match_stats.columns.isin(['match_id', 'opponent', 'season', 'match_result', 'location'])].groupby('player').mean()

player_career.round(2).to_csv('career_stats.csv')


# player season stats dataframe


# team season stats dataframe


# team career stats dataframe??



# check data types

cleandata.dtypes
cleandata.head()







