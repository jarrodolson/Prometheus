##Data ingest
library(reshape)
library(dplyr)
options(stringsAsFactors=FALSE)

regSeason <- read.csv("Data\\regular_season_results.csv",header=TRUE)
seasons <- read.csv("Data\\seasons.csv",header=TRUE)
tourneyResults <- read.csv("Data\\tourney_results.csv",header=TRUE)
tourneySeeds <- read.csv("Data\\tourney_seeds.csv",header=TRUE)
tourneySlots <- read.csv("Data\\tourney_slots.csv",header=TRUE)

seasons$dayzero <- as.POSIXct(strptime(seasons$dayzero,format="%m/%d/%Y"))

history <- read.delim("Data\\HistoricTourneyResults.txt",sep="\t",header=TRUE)
history$game <- paste(history$Year,history$Team1,history$Team2,sep="_")
history$RankDiff <- history$Team1Rank-history$Team2Rank
history$ScoreDiff <- history$Team1Score-history$Team2Score
##Need to build out model showing probability of a win given ranking, then given ranking
##  considering opponent rank

teamID <- read.csv("Data\\TeamID.csv",header=TRUE)
teamID$name <- NULL
teamID <- filter(teamID,Team!="")

bringInTeams <- function(){
  teams_other <- read.delim("Data\\teamStats_2014Feb16.txt",sep="\t",header=TRUE,stringsAsFactors=FALSE)
  return(teams_other)
}

getTeamDyads <- function(){
  teamDyads <- read.delim("Data\\teamDyads2014Feb16.txt",sep="\t",header=TRUE,stringsAsFactors=FALSE)
  tempTeamID <- teamID
  tempTeamID$name <- NULL
  tempTeamID1 <- rename(tempTeamID,
                       c(Team="team_play",
                         Conference="conf_play",
                         IDNumber="IDNum_play",
                         id="id_play"))  
  teamDyads <- merge(teamDyads,tempTeamID1,by.x="Team",by.y="team_play",all.x=TRUE)
  tempTeamID <- rename(tempTeamID,
                       c(Team="team_oppo",
                         Conference="conf_oppo",
                         IDNumber="IDNum_oppo",
                         id="id_oppo"))
  teamDyads <- merge(teamDyads,tempTeamID,by.x="OpponentName",by.y="team_oppo",all.x=TRUE)
  rm(tempTeamID)
  ##teamDyads <- unique(teamDyads)
  return(teamDyads)
}

teams <- bringInTeams()
teamDyads <- getTeamDyads()
teamDyads <- select(teamDyads,GameName,conf_play,IDNum_play,id_play,conf_oppo,IDNum_oppo,id_oppo)

teams <- merge(teams,teamID,by="Team",all.x=TRUE)

teams <- merge(teams,teamDyads,by.x=c("GameName","IDNumber"),by.y=c("GameName","IDNum_play"),all.x=TRUE)
##Expect about 10000 failures due to opponents not in database (e.g. "Hawaii-Hilo")
##  currently about 10,000 failures (out of 270k)
table(is.na(teams$IDNum_oppo))
teams_off <- teams[which(teams$StatType=="Offensive"),]
teams_off$home <- 1
teams_off$home[substring(teams_off$Opponent,1,1)=="@"]<-0
teams_off_merge <- teams_off[c("IDNumber","GameName","REB","AST","ST","home")]
names(teams_off_merge) <- c("IDNumber","GameName","Team_REB","Team_AST","Team_ST","home")

teams_def <- teams[which(teams$StatType=="Defensive"),]

##Gets us avg pts allowed
opponentD <- ddply(teams_def,.(IDNumber),summarize,
                   d_avgPtsAllowed = mean(T,na.rm=TRUE),
                   d_avgFoulsAgainst = mean(PF,na.rm=TRUE),
                   d_avgTOForced = mean(TO,na.rm=TRUE),
                   d_avgOppFGPer = mean(FG_Per,na.rm=TRUE),
                   d_avgOppThreePtPer = mean(Three_Per,na.rm=TRUE))
opponentD <- rename(opponentD, c(IDNumber="opponent_IDNumber"))

##Gets us teams data merged with opponent data
##HOWEVER: It needs to be synchronized by time to use for modeling
teams <- merge(teams,opponentD,by.x=c("IDNum_oppo"),by.y=c("opponent_IDNumber"),all.x=TRUE)

teams_off_agg <- aggregate(teams_off,by=list(teams_off$Team),FUN=mean,na.rm=TRUE)
teams_off_agg <- teams_off_agg[c("team_IDNumber","T")]
names(teams_off_agg)<-c("team_IDNumber","avgOffTotalPts")
teams <- merge(teams,teams_off_agg,by=c("team_IDNumber"),all.x=TRUE)
teams$ptsDiff <- teams$T-teams$avgOffTotalPts
teams_off_2 <- teams[which(teams$StatType=="Offensive"),]
library(plyr)
defRating <- ddply(teams_off_2,.(opponent_IDNumber),summarize,meanPtsDiffAllowed=mean(ptsDiff))
names(defRating) <- c("team_IDNumber","defRating")
teams <- merge(teams,defRating,by=c("team_IDNumber"),all.x=TRUE)