# clean data

import pandas as pd
import csv

# import csv and creating dataframe
rawdata = pd.read_csv("NcstateSinglesStatReport.csv")

# make clean dataset
cleandata = rawdata
cleandata['player'] = cleandata['player'].str.strip()

# check data types
# result = cleandata.dtypes
# print(result)

# create list of player names

players = cleandata['player'].unique()


# # create seasons function

# ### season creating logic
# # if month is 1,2,3,4,5,6 --> current season, take year value 
# # if month is 7,8,9,10,11,12 --> next season, year value + 1

def find_season(date):
    if int(date.split('/')[0]) < 7:
        return int(date.split('/')[2])
    else:
        return int(date.split('/')[2]) + 1

print(find_season('1/05/25'))

cleandata.insert(loc = 3,
          column = 'season',
          value = '')

cleandata['season'] = cleandata['date'].apply(find_season)

# check if season column is correctly added
# print(cleandata.iloc[1000:1300, 2:4])


    #from all rows where player = player, count number of 0's in pointWonby





# append values that function spits out into a row