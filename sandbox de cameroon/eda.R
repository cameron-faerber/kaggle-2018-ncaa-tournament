library(readr)

# import data sets
mord = read_csv('data files/MasseyOrdinals.csv') # data set looks like a ranking by day of all 351 ncaa basketball teams

# data files
seed = read_csv('data files/DataFiles/NCAATourneySeeds.csv') # tournament seed by year
team = read_csv('data files/DataFiles/Teams.csv') # teamID numbers

# results
tourn = read_csv('data files/DataFiles/NCAATourneyCompactResults.csv') # compact ncaa tournament results by year

# sample submission file
subm_sample = read_csv('data files/SampleSubmissionStage1.csv')

# just so I understand what is happenning here:
# phase 1 is using historical data to predict winners of all possible matchups of tournament teams from 2014-2017
# how I see it: "Pred" is the yhat, and we must use a series of data features to create that probaility

