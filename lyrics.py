#!/usr/bin/env python3
# -*- coding: utf-8 -*-
"""
Created on Fri Sep 30 05:50:43 2022

@author: noahjohnson
"""

from lyricsgenius import Genius
import json
import urllib
from bs4 import BeautifulSoup as bs
from nrclex import NRCLex
from nltk.sentiment.vader import SentimentIntensityAnalyzer

#Tests with Genius API
genius = Genius("ELYtGAm_VSwW8BveP9tclUoPtVsnrC_mlH5O9TaRPtpWIi3jzzWFhc6ANA2J5TOb")

# genius.search_album("Night Visions", "Imagine Dragons").save_lyrics()
# json_file_path = "Lyrics_NightVisions.json"

# with open(json_file_path, 'r') as j:
#      contents = json.loads(j.read())
     
# print(contents["tracks"][0])


#Webscraping from Billboard
song_list = []

for j in range(2006, 2022):
    url_text = 'https://www.billboard.com/charts/year-end/' + str(j) + '/hot-100-songs'
    url = urllib.request.urlopen( url_text )
    doc = url.read()
    tree = bs( doc, 'lxml' )
    div_list = tree.find_all("div", "o-chart-results-list-row-container")
    
    for i in range(len(div_list)):
        if j == 2011 and i == 6:
            song_list.append(["F**k You (Forget You)", "CeeLo Green"])
        if j == 2016 and i == 86:
            song_list.append(["All the Way Up", "Fat Joe and Remy Ma featuring French Montana and Infared"])
        
        all_list = div_list[i].find_all("ul")
        song_str = str(all_list[1].find("h3").getText().replace("\n\n\t\n\t\n\t\t\n\t\t\t\t\t", "").replace("\t\t\n\t\n", ""))
        artist_str = str(all_list[1].find("span").getText().replace("\n\t\n\t", "").replace("\n", ""))
        song_list.append([song_str, artist_str])
        
accumulated_sent_dict = {}
        
for i in range(len(song_list)):
    
    lyrics_str = genius.search_song(song_list[0][0], song_list[0][1]).lyrics
    lyric_list = lyrics_str.split("\n")
    done = False
    i = 0
    
    while not done:
        if lyric_list[-1][i] in "1234567890":
            lyric_list[-1] = lyric_list[-1][:i]
            done = True
        i += 1
        
    print(lyric_list)
    lyric_list_final = []
    
    for i in range(len(lyric_list)):
        #Getting rid of one-word lines and verse type that isn't in song
        temp = lyric_list[i]
        if " " in temp and "[" not in temp:
            lyric_list_final.append(temp)
    
    print(lyric_list_final)
    
    sent_dict = {}
    avg_sentiment = 0
    avg_pos_sent = 0
    avg_neg_sent = 0
    pol = SentimentIntensityAnalyzer()
    
    for i in range(len(lyric_list_final)):
        lyric_str = lyric_list_final[i]
        print(lyric_str)
        
        #Get polarity scores
        lyric_pol = pol.polarity_scores(lyric_str)
        
        avg_sentiment += lyric_pol["compound"]
        avg_pos_sent += lyric_pol["pos"]
        avg_neg_sent += lyric_pol["neg"]
        
        #Get emotion flags
        line = NRCLex(text = lyric_str)
        temp_emo_dict = line.affect_dict
        
        if len(temp_emo_dict.keys()) == 0:
            print("No emotional words")
            
        else:
            print("Emotional words found: ", temp_emo_dict)
            sent_dict.update(temp_emo_dict)
        
        
    avg_sentiment /= len(lyric_list_final)
    avg_pos_sent /= len(lyric_list_final)
    avg_neg_sent /= len(lyric_list_final)
    
    print("Emotional Words:", sent_dict)
    print("Overall Sentiment:", avg_sentiment)
    print("Positive:", avg_pos_sent)
    print("Negative:", avg_neg_sent)
    
    accumulated_sent_dict[song_list[i]] = sent_dict
    
        
#Tests with emotion reading text libraries
# radioactive_str = contents["tracks"][0]["song"]["lyrics"]
# ra_list = radioactive_str.split("\n")
# ra_list_final = []
# for i in range(len(ra_list)):
    
#     #Getting rid of one-word lines and verse type that isn't in song
#     if " " in ra_list[i] and "[" not in ra_list[i]:
#         #Some sequence of characters at the end of each call?
#         temp = ra_list[i].replace("Embed", "")
#         ra_list_final.append(temp)
# ra_list_final = [*set(ra_list_final)]

# print(ra_list_final)

# sent_dict = {}

# for i in range(len(ra_list_final)):
#     lyric_str = ra_list_final[i]
#     print(lyric_str)
    
#     #Get polarity scores
#     pol = SentimentIntensityAnalyzer()
#     print(pol.polarity_scores(ra_list_final[i]))
    
#     #Get emotion flags
#     line = NRCLex(text = ra_list_final[i])
#     temp_emo_dict = line.affect_dict
#     if len(temp_emo_dict.keys()) == 0:
#         print("No emotional words")
#     else:
#         print("Emotional words found: ", temp_emo_dict)
#         sent_dict.update(temp_emo_dict)
        
# print(sent_dict)
    
    




