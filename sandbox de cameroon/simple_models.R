source('sandbox de cameroon/eda.R')
source('Functions/get_log_loss.R')
# load libraries
library(stringr)
library(caret)
library(Metrics)
library(pROC)

## data cleaning
# separate out years and team id's 
subm_sample$idSplit = str_split(subm_sample$ID,"_")
subm_sample$year = unlist(subm_sample$idSplit)[c(T,F,F)]
subm_sample$team1 = unlist(subm_sample$idSplit)[c(F,T,F)]
subm_sample$team2 = unlist(subm_sample$idSplit)[c(F,F,T)]
# create numeric seed variable
seed$seedNumeric = as.integer(unlist(str_extract_all(seed$Seed,pattern="[0-9]+")))

# merge with team seeds
dat = merge(subm_sample,seed,by.x=c("year","team1"),by.y=c("Season","TeamID"))
dat = merge(dat,seed,by.x=c("year","team2"),by.y=c("Season","TeamID"),suffixes=c("1","2"))

# merge tournament w/ seeds
tourn = merge(tourn,seed,by.x=c("Season","WTeamID"),by.y=c("Season","TeamID"))
tourn = merge(tourn,seed,by.x=c("Season","LTeamID"),by.y=c("Season","TeamID"),suffixes=c("W","L"))

# create response variable (probability of team with lower ID winning)
tourn$team1 = ifelse(tourn$LTeamID<tourn$WTeamID,tourn$LTeamID,tourn$WTeamID)
tourn$team1Win = ifelse(tourn$team1==tourn$WTeamID,1,0)
tourn$seed1 = ifelse(tourn$team1==tourn$WTeamID,tourn$seedNumericW,tourn$seedNumericL)
tourn$seed2 = ifelse(tourn$team1==tourn$WTeamID,tourn$seedNumericL,tourn$seedNumericW)
tourn$seedDiff = tourn$seed1 - tourn$seed2

### model
## simple interactive seed model
mod = glm(team1Win~seed1*seed2,family=binomial,data=tourn)

# 10-fold c.v.
set.seed(2018)
k = 10
folds = createFolds(1:nrow(tourn),k,list=F)
auc = numeric(10)
pred = actual = numeric()
for(i in 1:k){
  mod = glm(team1Win~seed1*seed2,family=binomial,data=tourn[folds!=i,])
  pred = c(pred,predict(mod,newdata=tourn[folds==i,],type="response"))
  actual = c(actual,tourn[folds==i,"team1Win"])
}
logLoss(actual,pred)
auc(roc(actual,pred))

