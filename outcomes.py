#!/usr/bin/env python3
# -*- coding: utf-8 -*-
"""
Created on Fri Oct 21 14:12:31 2022

@author: noahjohnson
"""

import pandas as pd
import os

os.chdir("/Users/noahjohnson/Downloads")

df = pd.read_csv("/Users/noahjohnson/Downloads/bluefall2hwteam17/NcstateSinglesStatReport.csv")

final_score_list = list(df["finalScore"])

for i in range(len(final_score_list)):
    final_score_list_split = final_score_list[i].split("|")
    outcome_list = []
    for j in range(len(final_score_list_split)):
        final_score_list_split2 = final_score_list_split[j].split("-")
        outcomeWin = ""
        if (int(final_score_list_split2[0]) == 6 and int(final_score_list_split2[1]) <= 4) or \
            (int(final_score_list_split2[0]) == 7):
            outcomeWin = "NCSU"
        elif (int(final_score_list_split2[1]) == 6 and int(final_score_list_split2[0]) <= 4) or \
            (int(final_score_list_split2[1]) == 7):
            outcomeWin = "Visitor"
        else:
            outcomeWin = "Incomplete"
        outcome_list.append(outcomeWin)    
    
    decision = ""
    if "Incomplete" in outcome_list:
        decision = "Incomplete"
    elif outcome_list.count("NCSU") == 2:
        decision = "Win"
    elif outcome_list.count("Visitor") == 2:
        decision = "Loss"
    
    final_score_list[i] = decision
    
print(final_score_list)

col_list = list(df.columns)

col_list1 = col_list[0:29]
col_list2 = col_list[30:len(col_list) - 1]
for k in range(len(col_list2)):
    col_list1.append(col_list2[k])

col_list1.append(col_list[29])
col_list1.append(col_list[len(col_list) - 1])

col_list = col_list1

df = pd.DataFrame(df[col_list], columns = col_list)

df.to_csv("/Users/noahjohnson/Downloads/bluefall2hwteam17/raw_data_plus_outcome.csv")

