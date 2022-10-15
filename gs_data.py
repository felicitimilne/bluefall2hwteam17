#!/usr/bin/env python3
# -*- coding: utf-8 -*-
"""
Created on Fri Oct 14 21:21:27 2022

@author: noahjohnson
"""

import pandas as pd
import numpy as np

gsi = pd.read_csv("/Users/noahjohnson/Downloads/bluefall2hwteam17/dailyvalues.csv")
monthly_date_list = []
year_list = []

for i in range(len(gsi["Date"])):
    date_list = gsi["Date"][i].split("-")
    md_str = date_list[0] + "/" + date_list[1]
    year_str = md_str[0:md_str.index("/")]
    monthly_date_list.append(md_str)
    year_list.append(year_str)
gsi["Monthly Date"] = monthly_date_list
gsi["Year"] = year_list

gsi_grouped = gsi.groupby(by = gsi["Monthly Date"]).mean()
gsi_grouped = gsi_grouped.iloc[46:238]

gsi_yr_grouped = gsi.groupby(by = gsi["Year"]).mean()
gsi_yr_grouped = gsi_yr_grouped.iloc[4:20]

###

df_combine = pd.read_csv("/Users/noahjohnson/Downloads/dataverse_files/Sentiment Data - Country/num_posts_and_sentiment_summary_2012.csv")

for i in range(2013, 2022):
    df_str = "/Users/noahjohnson/Downloads/dataverse_files/Sentiment Data - Country/num_posts_and_sentiment_summary_" + str(i) + ".csv"
    temp_df = pd.read_csv(df_str)
    df_combine = pd.concat([df_combine, temp_df])
    

df_us = df_combine[df_combine["NAME_0"] == "United States"]

monthly_date_list = []
year_list = []
date_store_list = np.array(df_us["DATE"])
for i in range(len(date_store_list)):
    date_list = date_store_list[i].split("-")
    md_str = date_list[0] + "/" + date_list[1]
    year_str = md_str[0:md_str.index("/")]
    monthly_date_list.append(md_str)
    year_list.append(year_str)
    
df_us["Monthly Date"] = monthly_date_list
df_us["Year"] = year_list

mit_grouped = df_us.groupby(by = df_us["Monthly Date"]).mean()

mit_yr_grouped = df_us.groupby(by = df_us["Year"]).mean()

gsi_grouped.to_csv("/Users/noahjohnson/Downloads/bluefall2hwteam17/gsi_monthly.csv")
gsi_yr_grouped.to_csv("/Users/noahjohnson/Downloads/bluefall2hwteam17/gsi_yearly.csv")
mit_grouped.to_csv("/Users/noahjohnson/Downloads/bluefall2hwteam17/mit_monthly.csv")
mit_yr_grouped.to_csv("/Users/noahjohnson/Downloads/bluefall2hwteam17/mit_yearly.csv")
