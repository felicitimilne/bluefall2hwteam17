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

#Tests with Genius API
genius = Genius("ELYtGAm_VSwW8BveP9tclUoPtVsnrC_mlH5O9TaRPtpWIi3jzzWFhc6ANA2J5TOb")

genius.search_album("Night Visions", "Imagine Dragons").save_lyrics()
json_file_path = "Lyrics_NightVisions.json"

with open(json_file_path, 'r') as j:
     contents = json.loads(j.read())
     
print(contents["tracks"][0]["song"]["lyrics"])

genius.search_song("Radioactive", "Imagine Dragons").save_lyrics()



#Webscraping from Billboard
song_list = []

for j in range(len(2006, 2022)):
    url_text = 'https://www.billboard.com/charts/year-end/' + str(j) + '/hot-100-songs'
    url = urllib.request.urlopen( url_text )
    doc = url.read()
    tree = bs( doc, 'lxml' )
    div_list = tree.find_all("div", "o-chart-results-list-row-container")
    for i in range(len(100)):
        all_list = div_list[i].find_all("ul")
        song_str = str(all_list[1].find("h3").getText().replace("\n\n\t\n\t\n\t\t\n\t\t\t\t\t", "").replace("\t\t\n\t\n", ""))
        artist_str = str(all_list[1].find("span").getText().replace("\n\t\n\t", "").replace("\n", ""))
        key_str = str(j) + ", song #" + str(i)
        song_list.append([song_str, artist_str])
        
#Tests with emotion reading text libraries
from nltk.sentiment.vader import SentimentIntensityAnalyzer
radioactive_str = contents["tracks"][0]["song"]["lyrics"]
sentence_list = [*set(radioactive_str.split("\n"))]
sentiment = SentimentIntensityAnalyzer()
for i in range(len(sentence_list)):
    if " " in sentence_list[i] and "[" not in sentence_list[i]:
        score = sentiment.polarity_scores(sentence_list[i])
        print(sentence_list[i])
        print(score)

from nrclex import NRCLex

radioactive = NRCLex(text = radioactive_str)
print(radioactive.affect_dict)
print(radioactive.raw_emotion_scores)




