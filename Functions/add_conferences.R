add_conferences = function(df) {
  #requires the teamID to be team1 and team2 where team1 < team2
  conf = read.csv("Data/TeamConferences.csv")
  df = merge(df,conf,by.x=c("Season","team1"),by.y=c("Season","TeamID"))
  names(df)[ncol(df)] = "conf1"
  df = merge(df,conf,by.x=c("Season","team2"),by.y=c("Season","TeamID"))
  names(df)[ncol(df)] = "conf2"
  return(df)
}