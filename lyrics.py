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
     
print(contents["tracks"][0])


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
from nrclex import NRCLex
radioactive = NRCLex(text = "Lyrics_NightVisions.json")
print(radioactive.words)




