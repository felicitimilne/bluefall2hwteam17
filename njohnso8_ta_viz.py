#!/usr/bin/env python3
# -*- coding: utf-8 -*-
"""
Created on Sun Oct 16 23:35:24 2022

@author: noahjohnson
"""

import matplotlib.pyplot as plt
import seaborn as sns
import pandas as pd
import numpy as np

yes_df = pd.read_csv("/Users/noahjohnson/Downloads/bluefall2hwteam17/yearly_sent_metrics.csv")

plt.plot(yes_df["Year"], yes_df["Avg Anger"], label = "Anger")
plt.plot(yes_df["Year"], yes_df["Avg Disgust"], label = "Disgust")
plt.plot(yes_df["Year"], yes_df["Avg Fear"], label = "Fear")
#plt.plot(yes_df["Year"], yes_df["Avg Neg"], label = "Negativity")
plt.plot(yes_df["Year"], yes_df["Avg Sadness"], label = "Sadness")
plt.legend()
plt.show()

plt.plot(x = yes_df["Year"], y = yes_df["Avg Anticipation"])
plt.plot(x = yes_df["Year"], y = yes_df["Avg Joy"])
plt.plot(x = yes_df["Year"], y = yes_df["Avg Positive"])
plt.plot(x = yes_df["Year"], y = yes_df["Avg Surprise"])
plt.plot(x = yes_df["Year"], y = yes_df["Avg Trust"])
plt.show()

plt.plot(yes_df["Year"], yes_df["Avg Exp Valence"])
plt.plot(yes_df["Year"], [0 for i in range(len(yes_df["Year"]))], linestyle = "--")
plt.plot(2019, yes_df["Avg Exp Valence"][13] - 0.001, marker="o", markersize=8, markeredgecolor = "mediumvioletred", markerfacecolor = "mediumvioletred")
plt.title("Yearly Expanded Valence")
plt.xlabel("Year")
plt.ylabel("Avg Expanded Valence")
plt.show()

sns.set_theme(style = "darkgrid")
ev_plot = sns.relplot(data = yes_df, x = "Year", y = "Avg Exp Valence", kind = "line")

ev_plot
