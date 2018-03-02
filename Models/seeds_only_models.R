#Simple model just based on seeds

#Data
library(readr)
seeds = read_csv("Data/NCAATourneySeeds.csv")
seeds$Seed = as.numeric(substr(seeds$Seed,2,3))
sample_submission = read_csv("Data/SampleSubmissionStage1.csv")
tourn_results = read_csv("Data/NCAATourneyCompactResults.csv")

#order by teamID
team1 = numeric()
team2 = numeric()
team1_win = numeric()
for(i in 1:nrow(tourn_results)) {
  if(tourn_results$WTeamID[i]<tourn_results$LTeamID[i]) {
    team1[i] = tourn_results$WTeamID[i]
    team2[i] = tourn_results$LTeamID[i]
    team1_win[i] = 1
  } else {
    team1[i] = tourn_results$LTeamID[i]
    team2[i] = tourn_results$WTeamID[i]
    team1_win[i] = 0
  }
}
tourn_results$team1 = team1
tourn_results$team2 = team2
tourn_results$team1_win = team1_win

#Merge seeds with tournament results
tourn_results = merge(tourn_results,seeds,by.x=c("Season","team1"),by.y=c("Season","TeamID"))
names(tourn_results)[12] = "seed1"
tourn_results = merge(tourn_results,seeds,by.x=c("Season","team2"),by.y=c("Season","TeamID"))
names(tourn_results)[13] = "seed2"

#Logistic Regression Model
mod = glm(team1_win~seed1+seed2,data=tourn_results,family="binomial")

#Random Forest
library(randomForest)
rf = randomForest(as.factor(team1_win)~seed1+seed2,data=tourn_results)

#Predictions
season = as.integer(substr(sample_submission$ID,1,4))
team1 = as.integer(substr(sample_submission$ID,6,9))
team2 = as.integer(substr(sample_submission$ID,11,14))

seed1 = numeric()
seed2 = numeric()
for(i in 1:nrow(sample_submission)) {
  seed1[i] = seeds$Seed[seeds$Season==season[i] & seeds$TeamID==team1[i]]
  seed2[i] = seeds$Seed[seeds$Season==season[i] & seeds$TeamID==team2[i]]
}
sample_submission$Pred = predict(mod,newdata=data.frame(seed1,seed2),type="response")
write_csv(sample_submission,"Submissions/log_reg_seed.csv")
sample_submission$Pred = predict(rf,newdata=data.frame(seed1,seed2),type="prob")[,2]
write_csv(sample_submission,"Submissions/rf_seed.csv")

#Ensemble of two
log_pred = predict(mod,newdata=data.frame(seed1,seed2),type="response")
rf_pred = predict(rf,newdata=data.frame(seed1,seed2),type="prob")[,2]
sample_submission$Pred = apply(cbind(log_pred,rf_pred),1,mean)
write_csv(sample_submission,"Submissions/log_rf_ensemble_seeds.csv")
