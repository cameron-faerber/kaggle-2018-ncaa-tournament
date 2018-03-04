get.log.loss = function(submission) {
  tourn_results = read.csv("Data/NCAATourneyCompactResults.csv",header=T)
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
  val = data.frame(ID = paste(tourn_results$Season,team1,team2,sep="_"), result=team1_win)[tourn_results$Season>2013,]
  val = val[order(val$ID),]
  submission = submission[submission$ID %in% val$ID,]
  log_loss = -1/268 * sum(val$result*log(submission$Pred)+(1-val$result)*log(1-submission$Pred))
  return(log_loss)
}
