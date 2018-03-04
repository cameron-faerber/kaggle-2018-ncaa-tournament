add_seeds = function(df) {
  #requires the teamID to be team1 and team2 where team1 < team2
  seeds = read.csv("Data/NCAATourneySeeds.csv")
  seeds$Seed = as.numeric(substr(seeds$Seed,2,3))
  df = merge(df,seeds,by.x=c("Season","team1"),by.y=c("Season","TeamID"))
  names(df)[ncol(df)] = "seed1"
  df = merge(df,seeds,by.x=c("Season","team2"),by.y=c("Season","TeamID"))
  names(df)[ncol(df)] = "seed2"
  return(df)
}