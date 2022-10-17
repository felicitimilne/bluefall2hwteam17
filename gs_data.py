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

song_df = pd.read_csv("/Users/noahjohnson/Downloads/bluefall2hwteam17/lyrics_with_wc1.csv")
song_df = song_df[["Unnamed: 0", "Song & Artist", "Word Count"]]
lyric_df = pd.read_csv("/Users/noahjohnson/Downloads/bluefall2hwteam17/sent_emo_lyrics.csv")


yearly_wc = []
yearly_fw = []
yearly_comp = []
yearly_ps = []
yearly_ns = []
yearly_ef = []
yearly_anger = []
yearly_antic = []
yearly_disg = []
yearly_fear = []
yearly_joy = []
yearly_neg = []
yearly_pos = []
yearly_sad = []
yearly_surp = []
yearly_trust = []
yearly_ev = []

for i in range(16):
    song_list = list(song_df.iloc[100 * i : 100 * (i + 1)]["Song & Artist"])
    
    temp_wc_list = []
    temp_fw_list = []
    temp_comp_list = []
    temp_ps_list = []
    temp_ns_list = []
    temp_ef_list = []
    temp_anger_list = []
    temp_antic_list = []
    temp_disg_list = []
    temp_fear_list = []
    temp_joy_list = []
    temp_neg_list = []
    temp_pos_list = []
    temp_sad_list = []
    temp_surp_list = []
    temp_trust_list = []
    temp_ev_list = []
    
    for j in range(len(song_list)):
        temp_partition = lyric_df.loc[lyric_df["Song & Artist"] == song_list[j]]
        temp_wc_list.append(float(temp_partition["Word Count"].drop_duplicates()))
        temp_fw_list.append(float(temp_partition["num_fw"].drop_duplicates()))
        temp_comp_list.append(float(temp_partition["Compound Sentiment"].drop_duplicates()))
        temp_ps_list.append(float(temp_partition["Positive Sentiment"].drop_duplicates()))
        temp_ns_list.append(float(temp_partition["Negative Sentiment"].drop_duplicates()))
        temp_ef_list.append(float(temp_partition["Emo Flag Total"].drop_duplicates()))
        temp_anger_list.append(float(temp_partition["Anger"].drop_duplicates()))
        temp_antic_list.append(float(temp_partition["Anticipation"].drop_duplicates()))
        temp_disg_list.append(float(temp_partition["Disgust"].drop_duplicates()))
        temp_fear_list.append(float(temp_partition["Fear"].drop_duplicates()))
        temp_joy_list.append(float(temp_partition["Joy"].drop_duplicates()))
        temp_neg_list.append(float(temp_partition["Positive"].drop_duplicates()))
        temp_pos_list.append(float(temp_partition["Negative"].drop_duplicates()))
        temp_sad_list.append(float(temp_partition["Sadness"].drop_duplicates()))
        temp_surp_list.append(float(temp_partition["Surprise"].drop_duplicates()))
        temp_trust_list.append(float(temp_partition["Trust"].drop_duplicates()))
        temp_ev_list.append(float(temp_partition["Expanded Valence"].drop_duplicates()))
    
    yearly_wc.append(np.array(temp_wc_list).mean())
    yearly_fw.append(np.array(temp_fw_list).mean())
    yearly_comp.append(np.array(temp_comp_list).mean())
    yearly_ps.append(np.array(temp_ps_list).mean())
    yearly_ns.append(np.array(temp_ns_list).mean())
    yearly_ef.append(np.array(temp_ef_list).mean())
    yearly_anger.append(np.array(temp_anger_list).mean())
    yearly_antic.append(np.array(temp_antic_list).mean())
    yearly_disg.append(np.array(temp_disg_list).mean())
    yearly_fear.append(np.array(temp_fear_list).mean())
    yearly_joy.append(np.array(temp_joy_list).mean())
    yearly_neg.append(np.array(temp_neg_list).mean())
    yearly_pos.append(np.array(temp_pos_list).mean())
    yearly_sad.append(np.array(temp_sad_list).mean())
    yearly_surp.append(np.array(temp_surp_list).mean())
    yearly_trust.append(np.array(temp_trust_list).mean())
    yearly_ev.append(np.array(temp_ev_list).mean())
    
gs_df = pd.DataFrame(list(zip(list(gsi_yr_grouped["SGIXSENT"]), list(gsi_yr_grouped["SGXQIN01"]), ([0, 0, 0, 0, 0, 0] + list(mit_yr_grouped["SCORE"])))), columns = ["Avg GS Index", "Avg GS Indicator", "Avg Twitter Sent"])
yearly_emo_stats_df = pd.DataFrame(list(zip(list(range(2006, 2022)), yearly_wc, yearly_fw, yearly_comp, yearly_ps, yearly_ns, yearly_ef, yearly_anger, yearly_antic, yearly_disg, yearly_fear, yearly_joy, yearly_neg, yearly_pos, yearly_sad, yearly_surp, yearly_trust, yearly_ev)), columns = ["Year", "Avg Word Count", "Avg Foreign Words", "Avg Comp Sent", "Avg Pos Sent", "Avg Neg Sent", "Avg Emo Flags", "Avg Anger", "Avg Antic", "Avg Disgust", "Avg Fear", "Avg Joy", "Avg Neg", "Avg Pos", "Avg Sadness", "Avg Surprise", "Avg Trust", "Avg Exp Valence"])
yearly_emo_stats_df = pd.concat([yearly_emo_stats_df, gs_df], axis = 1)
yearly_emo_stats_df.to_csv("/Users/noahjohnson/Downloads/bluefall2hwteam17/yearly_sent_metrics.csv")
