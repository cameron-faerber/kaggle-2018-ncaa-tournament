get_tourn_info = function() {
  tourn = read.csv("Data/NCAATourneyCompactResults.csv",header=T)
  tourn = tourn[,c("Season","WTeamID","LTeamID")]
  team1 = numeric()
  team2 = numeric()
  win1 = numeric()
  for(i in 1:nrow(tourn_results)) {
    if(tourn$WTeamID[i]<tourn$LTeamID[i]) {
      team1[i] = tourn$WTeamID[i]
      team2[i] = tourn$LTeamID[i]
      win1[i] = 1
    } else {
      team1[i] = tourn$LTeamID[i]
      team2[i] = tourn$WTeamID[i]
      win1[i] = 0
    }
  }
  tourn_out = data.frame(Season = tourn$Season,team1,team2,win1)
  return(tourn_out)
}