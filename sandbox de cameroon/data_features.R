# import libraries
library(readr)
library(data.table)
library(reshape2)

### masseyOrdinals 
mord = fread('data files/MasseyOrdinals.csv') # data set looks like a ranking by day of all 351 ncaa basketball teams
## lowest rank by team per year
min = mord[,.(min=min(OrdinalRank)),by=.(Season,SystemName,TeamID)]
## max rank by team per year
max = mord[,.(max=max(OrdinalRank)),by=.(Season,SystemName,TeamID)]
## average rank by team per year
avg = mord[,.(avg=mean(OrdinalRank)),by=.(Season,SystemName,TeamID)]
## ranking last day of season
lastDay = mord[RankingDayNum>=133,.(lastDay=OrdinalRank),by=.(Season,SystemName,TeamID)]

## ranking trend; take last n rankings from each system and calculate the slope
slope = function(y){
  x = cbind(1,1:length(y))
  beta = solve(t(x)%*%x)%*%t(x)%*%y
  return(beta[2,1]) # return the slope
}

# subset to only rankings that include all teams
trendOverall = mord[RankingDayNum>=100,.(trendOverall=slope(OrdinalRank)),by=.(Season,TeamID)]
trendRPI = mord[RankingDayNum>=100&SystemName=="RPI",.(trendRPI=slope(OrdinalRank)),by=.(Season,TeamID)]

# merge features together
mordFeat = merge(avg,lastDay,by.x=c("Season","SystemName","TeamID"),by.y=c("Season","SystemName","TeamID"))
mordFeat = merge(mordFeat,max,by.x=c("Season","SystemName","TeamID"),by.y=c("Season","SystemName","TeamID"))
mordFeat = merge(mordFeat,min,by.x=c("Season","SystemName","TeamID"),by.y=c("Season","SystemName","TeamID"))
mordFeat = merge(mordFeat,trendOverall,by.x=c("Season","TeamID"),by.y=c("Season","TeamID"))
mordFeat = merge(mordFeat,trendRPI,by.x=c("Season","TeamID"),by.y=c("Season","TeamID"))

# to wide (a single row per teamID and season)
mordWide = dcast.data.table(mordFeat,Season+TeamID~SystemName,value.var=c("avg","lastDay","max","min","trendOverall","trendRPI"))
