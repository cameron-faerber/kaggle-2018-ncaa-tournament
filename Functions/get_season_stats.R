source("Functions/get_tourn_info.R")
library(readr)
get_season_stats = function() {
  tourn = get_tourn_info()
  tourn = tourn[tourn$Season>2002,]
  season_stats = read_csv("Data/RegularSeasonDetailedResults.csv")
  win_stats = season_stats[,substr(names(season_stats),1,1)=="W"][,-c(1,3)]
  lose_stats = season_stats[,substr(names(season_stats),1,1)=="L"][,-1]
  diff = win_stats - lose_stats
  names(diff) = paste("Diff",substr(names(diff),2,nchar(names(diff))),sep="")
  season_stats = rbind(data.frame(Season=season_stats$Season,TeamID=season_stats$WTeamID,wins=1,diff),
               data.frame(Season=season_stats$Season,TeamID=season_stats$LTeamID,wins=0,-diff))
  season_stats = aggregate(.~Season+TeamID,data=season_stats,FUN=mean)
  names(season_stats)[3:17]<-paste(names(season_stats)[3:17],"1",sep="")
  tourn = merge(tourn,season_stats,by.x = c("Season","team1"), by.y = c("Season","TeamID"),all.x=T)
  names(season_stats)[3:17]<-paste(substr(names(season_stats)[3:17],1,nchar(names(season_stats)[3:17])-1),"2",sep="")
  tourn = merge(tourn,season_stats,by.x = c("Season","team2"), by.y = c("Season","TeamID"),all.x=T)
  tourn = data.frame(tourn[,1:4],tourn[,5:19]-tourn[,20:34])
  return(tourn)
}
