#!/usr/bin/env python3
# -*- coding: utf-8 -*-
"""
Created on Wed Oct 12 19:26:20 2022

@author: noahjohnson
"""

import pandas as pd
import numpy as np
import nltk
from nrclex import NRCLex
from nltk.sentiment.vader import SentimentIntensityAnalyzer
import string
from polyglot.detect import Detector
from polyglot.detect.base import logger as polyglot_logger
import warnings

polyglot_logger.setLevel("ERROR")
warnings.filterwarnings ('ignore')

#lyric_df = pd.read_csv("~/Downloads/bluefall2hwteam17/lyrics_with_wc1.csv")
lyric_df = (pd.read_csv("~/Downloads/bluefall2hwteam17/sent_emo_lyrics.csv"))[["Song & Artist", "Lyrics", "Word Count"]]
lyric_df_small = lyric_df.head(10)
stop_words = nltk.corpus.stopwords.words("english")

fw_list = []
lyric_str_list = []
com_pol_list = []
pos_pol_list = []
neg_pol_list = []
emo_dict_list = []
emo_flag_list = []
anger_list = []
antic_list = []
disg_list = []
fear_list = []
joy_list = []
neg_list = []
pos_list = []
sad_list = []
surp_list = []
trust_list = []
ev_list = []

for i in range(len(lyric_df["Lyrics"])):
    temp_lyrics = lyric_df["Lyrics"][i]
    temp_lyrics = temp_lyrics.translate(str.maketrans("", "", string.punctuation.replace("\'", "")))
    lyric_list = temp_lyrics.split(" ")
    
    lyric_list_ns = []
    for k in range(len(lyric_list)):
        if lyric_list[k].lower() not in stop_words:
            lyric_list_ns.append(lyric_list[k].lower())
            
    sent_lyric_list = []
    foreign_count = 0
    
    for j in range(len(lyric_list_ns)):
        det = Detector(lyric_list_ns[j], quiet = True)
        if det.languages[0].name == "English":
            #print("Detected to be English by polyglot:", lyric_list_ns[j])
            sent_lyric_list.append(lyric_list_ns[j])
            
        elif lyric_list_ns[j].lower() in nltk.corpus.words.words():
            #print("Ambiguous word but found in English lexicon:", lyric_list_ns[j])
            sent_lyric_list.append(lyric_list_ns[j])
            
        else:
            #print("Word in", det.languages[0].name, "language detected in song:", lyric_list_ns[j])
            foreign_count += 1
    
    lyric_str = ""
    for x in range(len(sent_lyric_list)):
        lyric_str = lyric_str + sent_lyric_list[x] + " "
    
    lyric_str = lyric_str.rstrip()
    #print(lyric_str)
    
    fw_list.append(foreign_count)
    lyric_str_list.append(lyric_str)
            
    #Get polarity scores
    pol = SentimentIntensityAnalyzer()
    lyric_pol = pol.polarity_scores(lyric_str)
    
    com_pol_list.append(lyric_pol["compound"])
    pos_pol_list.append(lyric_pol["pos"])
    neg_pol_list.append(lyric_pol["neg"])
    
    #Get emotion flags
    emo_song = NRCLex(text = lyric_str)
    emo_dict = emo_song.affect_dict
    emo_dict_list.append(emo_dict)
    emo_flag_list.append(np.sum(np.array(list(emo_song.raw_emotion_scores.values()))))
    emo_scores = emo_song.raw_emotion_scores
    emo_scores = {i : emo_scores[i] / np.sum(np.array(list(emo_scores.values()))) for i in emo_scores.keys()}
    emo_list = ['negative','joy','positive','anticipation','fear','sadness','trust','anger','disgust','surprise']
    
    for y in range(len(emo_list)):
        if emo_list[y] not in list(emo_scores.keys()):
            emo_scores[emo_list[y]] = 0
    
    anger_list.append(emo_scores["anger"])
    antic_list.append(emo_scores["anticipation"])
    disg_list.append(emo_scores["disgust"])
    fear_list.append(emo_scores["fear"])
    joy_list.append(emo_scores["joy"])
    neg_list.append(emo_scores["negative"])
    pos_list.append(emo_scores["positive"])
    sad_list.append(emo_scores["sadness"])
    surp_list.append(emo_scores["surprise"])
    trust_list.append(emo_scores["trust"])
    
    expanded_valence = emo_scores["anticipation"] + emo_scores["joy"] + emo_scores["positive"] + emo_scores["surprise"] + emo_scores["trust"] - emo_scores["anger"] - emo_scores["disgust"] - emo_scores["fear"] - emo_scores["negative"] - emo_scores["sadness"]     
    ev_list.append(expanded_valence)
    #print(lyric_df["Song & Artist"][i], "the #", str((i % 100) + 1), "song in", str((range(2006, 2022))[i // 100]), "done.")
    print(lyric_df["Song & Artist"][i], "song #", str(i + 1), "/", str(len(lyric_df["Lyrics"])), "done.")
    
lyric_df["num_fw"] = fw_list
lyric_df["Sent Lyrics"] = lyric_str_list
lyric_df["Compound Sentiment"] = com_pol_list
lyric_df["Positive Sentiment"] = pos_pol_list 
lyric_df["Negative Sentiment"] = neg_pol_list
lyric_df["Emotional Words"] = emo_dict_list
lyric_df["Emo Flag Total"] = emo_flag_list
lyric_df["Anger"] = anger_list 
lyric_df["Anticipation"] = antic_list 
lyric_df["Disgust"] = disg_list 
lyric_df["Fear"] = fear_list 
lyric_df["Joy"] = joy_list 
lyric_df["Negative"] = neg_list 
lyric_df["Positive"] = pos_list 
lyric_df["Sadness"] = sad_list 
lyric_df["Surprise"] = surp_list 
lyric_df["Trust"] = trust_list 
lyric_df["Expanded Valence"] = ev_list 
#lyric_df_nd = lyric_df.drop("Unnamed: 0", axis = 1)
lyric_df = lyric_df.drop_duplicates()



lyric_df.to_csv("/Users/noahjohnson/Downloads/bluefall2hwteam17/sent_emo_lyrics.csv")

