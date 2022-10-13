#!/usr/bin/env python3
# -*- coding: utf-8 -*-
"""
Created on Fri Sep 30 05:50:43 2022

@author: noahjohnson
"""

from lyricsgenius import Genius
#import json
import urllib
from bs4 import BeautifulSoup as bs
import pandas as pd

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
lyric_dict = {}

for i in range(len(song_list)):
    artist_string = song_list[i][1]
    if artist_string == "The Black Eyed Peas": #any(x in artist_string.lower() for x in [" featuring ", " or ", " duet with ", " with ", " feat. ", " x ", " & ", ", ", " / "]):
        artist_string = artist_string[4:]
    elif artist_string == "Lady Antebellum":
        artist_string = artist_string[0:6]
    if " featuring " in artist_string.lower():
        artist_string = artist_string[0:artist_string.lower().index(" featuring ")]
    elif " x " in artist_string.lower() and " x & " not in artist_string.lower():
        artist_string = artist_string[0:artist_string.lower().index(" x ")]
    elif " or " in artist_string.lower():
        artist_string = artist_string[0:artist_string.lower().index(" or ")]
    elif " duet with " in artist_string.lower():
        artist_string = artist_string[0:artist_string.lower().index(" duet with ")]
    elif " with " in artist_string.lower():
        artist_string = artist_string[0:artist_string.lower().index(" with ")]
    elif " feat. " in artist_string.lower():
        artist_string = artist_string[0:artist_string.lower().index(" feat. ")]
    elif ", " in artist_string.lower():
        artist_string = artist_string[0:artist_string.lower().index(", ")]
    elif " & " in artist_string.lower() and " & sons" not in artist_string.lower() and "& the scene" not in artist_string.lower() and "& vinz" not in artist_string.lower():
        artist_string = artist_string[0:artist_string.lower().index(" & ")]
    elif " / " in artist_string.lower():
        artist_string = artist_string[0:artist_string.lower().index(" / ")]
        
    genius = Genius("ELYtGAm_VSwW8BveP9tclUoPtVsnrC_mlH5O9TaRPtpWIi3jzzWFhc6ANA2J5TOb")
    genius_search = genius.search_song(song_list[i][0], artist_string)   
    lyrics_str = genius_search.lyrics
    print(lyrics_str[0:100])
    lyric_dict[song_list[i][0], song_list[i][1]] = lyrics_str
    
    #lyric_temp_list = lyric_dict.values()
    
    length_dict = {}
    wc_list = []
    
    for i in range(len(list(lyric_dict.keys()))):
        lyric_list = lyric_dict[list(lyric_dict.keys())[i]].split("\n")
    #word_list = lyrics_str.replace("\n", " ").split(" ")
    #length_dict[song_list[i][0], song_list[i][1]] = len(word_list)
    
        done = False
        count = 0
        
        if not any(x in lyric_list[-1] for x in ["1", "2", "3", "4", "5", "6", "7", "8", "9", "0"]):
                done = True
    
        while not done:
            if lyric_list[-1][count] in "1234567890":
                lyric_list[-1] = lyric_list[-1][:count]
                done = True
            
            count += 1
        
        #print(lyric_list)
        lyric_list_final = []
    
        for k in range(len(lyric_list)):
            #Getting rid of one-word lines and verse type that isn't in song
            temp = lyric_list[k]
            if " " in temp and "[" not in temp:
                lyric_list_final.append(temp)
        
        temp_str = ""
        for j in range(len(lyric_list_final)):
            temp_str += lyric_list_final[j] + " "
            
        temp_str = temp_str.rstrip()
        lyrics_str = temp_str
        lyric_dict[list(lyric_dict.keys())[i]] = lyrics_str
        word_list = lyrics_str.replace("\n", " ").split(" ")
        #word_list = [i for i in word_list if i != ""]
        word_count = len(word_list)
        wc_list.append(word_count)
        length_dict[list(lyric_dict.keys())[i]] = word_count
        
        #
        print(lyric_list_final)
        
    lyric_df = pd.DataFrame(list(zip(list(lyric_dict.keys()), list(lyric_dict.values()), list(length_dict.values()))), columns =['Song & Artist', 'Lyrics', 'Word Count'])
    lyric_df.to_csv("~/Downloads/lyrics_with_wc2.csv")
    
        
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
    
#genre_dict = {}
# for i in range(1):    
#      genius = Genius("ELYtGAm_VSwW8BveP9tclUoPtVsnrC_mlH5O9TaRPtpWIi3jzzWFhc6ANA2J5TOb")
#      genius_search = genius.search_song(song_list[i][0], list(length_dict.keys())[i][1])
#      url_text_g = genius_search._body['url']
#      url_g = urllib.request.urlopen(url_text_g)
#      doc_g = url_g.read()
#      tree_g = bs(doc_g, 'lxml')
#      genre_list = []
#      a_list = tree.find_all("a", "SongTags_Tag-xixwg3-2 evrydK")
#      for j in range(len(a_list)):
#          genre_list.append(a_list[j].getText())
#      genre_dict[song_list[i][0], song_list[i][1]] = genre_list 
    




