# KAGGLE projects
# PUBG


rm(list = ls())

mainPath<-file.path(Sys.getenv("HOME"),"kaggle","PUBG_Rproject", "scripts")
# mainPath<-file.path(Sys.getenv("R_USER"), "PUBG_Rproject", "scripts")
setwd(mainPath)#"~/kaggle/PUBG_R/PUBG_R/"

if(!file.exists("data")){
  dir.create("data")
}

if(!file.exists("working_data")){
   dir.create("working_data")
}
dataDir<-file.path(getwd(), "data");

# install external libraries
library(dplyr);library(ggplot2);library(reshape2)
library(gridExtra)

###########################
#### download data
fileUrl<-"https://www.kaggle.com/c/10335/download-all"
dataPath<-paste0(dataDir,"pubg-finish-placement-prediction.zip")

if (!file.exists(dataPath)){
  download.file(fileUrl,dataPath,method = "auto")
  unzip(dataPath)
  # conTrain<-unz(dataPath,filename = "train_V2.csv",open = "rt")
  # dataTrain<-read.csv(conTrain)
}

#### load data

dataTrainSam<-read.csv(file.path(dataDir,"train_V2.csv"),stringsAsFactors =F,nrows=10)
dataTrain<-read.csv(file.path(dataDir,"train_V2.csv"),stringsAsFactors =F, 
                    col.names = names(dataTrainSam),colClasses = sapply(dataTrainSam,class))

rm(dataTrainSam)
# dataTestSam<-read.csv(paste0(dataDir,"test_V2.csv"),stringsAsFactors =F,nrows=10)
# names(dataTestSam)==names(dataTrain)
# dataTest<-read.csv(paste0(dataDir,"test_V2.csv"),stringsAsFactors =F,
#                     col.names = names(dataTestSam))
# rm(dataTestSam)

dataOut<-read.csv(paste0(dataDir,"sample_submission_V2.csv"),stringsAsFactors =F)

########################### EXPLORATORY ANALYSIS ###############################
# names(dataTrain)

# DBNOs - Number of enemy players knocked.
# assists - Number of enemy players this player damaged that were killed by teammates.
# boosts - Number of boost items used.
# damageDealt - Total damage dealt. Note: Self inflicted damage is subtracted.
# headshotKills - Number of enemy players killed with headshots.
# heals - Number of healing items used.
# Id - Player's Id
# killPlace - Ranking in match of number of enemy players killed.
# killPoints - Kills-based external ranking of player. (Think of this as an Elo ranking where only kills matter.) 
          # If there is a value other than -1 in rankPoints, then any 0 in killPoints should be treated as a "None".
# killStreaks - Max number of enemy players killed in a short amount of time.
# kills - Number of enemy players killed.
# longestKill - Longest distance between player and player killed at time of death. This may be misleading, 
              # as downing a player and driving away may lead to a large longestKill stat.
# matchDuration - Duration of match in seconds.
# matchId - ID to identify match. There are no matches that are in both the training and testing set.
# matchType - String identifying the game mode that the data comes from. The standard modes are 
              # "solo", "duo", "squad", "solo-fpp", "duo-fpp", and "squad-fpp"; other modes are from events or custom matches.
# rankPoints - Elo-like ranking of player. This ranking is inconsistent and is being deprecated in 
              # the API's next version, so use with caution. Value of -1 takes place of "None".
# revives - Number of times this player revived teammates.
# rideDistance - Total distance traveled in vehicles measured in meters.
# roadKills - Number of kills while in a vehicle.
# swimDistance - Total distance traveled by swimming measured in meters.
# teamKills - Number of times this player killed a teammate.
# vehicleDestroys - Number of vehicles destroyed.
# walkDistance - Total distance traveled on foot measured in meters.
# weaponsAcquired - Number of weapons picked up.
# winPoints - Win-based external ranking of player. (Think of this as an Elo ranking where only winning matters.) 
              # If there is a value other than -1 in rankPoints, then any 0 in winPoints should be treated as a "None".
# groupId - ID to identify a group within a match. If the same group of players plays in different matches, they will have 
          # a different groupId each time.
# numGroups - Number of groups we have data for in the match.
# maxPlace - Worst placement we have data for in the match. This may not match with numGroups, 
            # as sometimes the data skips over placements.
# winPlacePerc - The target of prediction. This is a percentile winning placement, where 1 corresponds to 1st place,
                # and 0 corresponds to last place in the match. It is calculated off of maxPlace, not numGroups, 
                # so it is possible to have missing chunks in a match.

str(dataTrain)

# 'data.frame':	4446966 obs. of  29 variables:
# $ Id             : chr  "7f96b2f878858a" "eef90569b9d03c" "1eaf90ac73de72" "4616d365dd2853" ...
# $ groupId        : chr  "4d4b580de459be" "684d5656442f9e" "6a4a42c3245a74" "a930a9c79cd721" ...
# $ matchId        : chr  "a10357fd1a4a91" "aeb375fc57110c" "110163d8bb94ae" "f1f1f4ef412d7e" ...
# $ assists        : int  0 0 1 0 0 0 0 0 0 0 ...
# $ boosts         : int  0 0 0 0 0 0 0 0 0 0 ...
# $ damageDealt    : num  0 91.5 68 32.9 100 ...
# $ DBNOs          : int  0 0 0 0 0 1 0 0 0 0 ...
# $ headshotKills  : int  0 0 0 0 0 1 0 0 0 0 ...
# $ heals          : int  0 0 0 0 0 0 0 0 0 0 ...
# $ killPlace      : int  60 57 47 75 45 44 96 48 64 74 ...
# $ killPoints     : int  1241 0 0 0 0 0 1262 1000 0 0 ...
# $ kills          : int  0 0 0 0 1 1 0 0 0 0 ...
# $ killStreaks    : int  0 0 0 0 1 1 0 0 0 0 ...
# $ longestKill    : num  0 0 0 0 58.5 ...
# $ matchDuration  : int  1306 1777 1318 1436 1424 1395 1316 1967 1375 1930 ...
# $ matchType      : chr  "squad-fpp" "squad-fpp" "duo" "squad-fpp" ...
# $ maxPlace       : int  28 26 50 31 97 28 28 96 28 29 ...
# $ numGroups      : int  26 25 47 30 95 28 28 92 27 27 ...
# $ rankPoints     : int  -1 1484 1491 1408 1560 1418 -1 -1 1493 1349 ...
# $ revives        : int  0 0 0 0 0 0 0 0 0 0 ...
# $ rideDistance   : num  0 0.0045 0 0 0 ...
# $ roadKills      : int  0 0 0 0 0 0 0 0 0 0 ...
# $ swimDistance   : num  0 11 0 0 0 ...
# $ teamKills      : int  0 0 0 0 0 0 0 0 0 0 ...
# $ vehicleDestroys: int  0 0 0 0 0 0 0 0 0 0 ...
# $ walkDistance   : num  244.8 1434 161.8 202.7 49.8 ...
# $ weaponsAcquired: int  1 5 2 3 2 1 1 6 4 1 ...
# $ winPoints      : int  1466 0 0 0 0 0 1497 1500 0 0 ...
# $ winPlacePerc   : num  0.444 0.64 0.775 0.167 0.188 ...

summary(dataTrain)

# Id              groupId            matchId             assists            boosts        damageDealt     
# Length:4446966     Length:4446966     Length:4446966     Min.   : 0.0000   Min.   : 0.000   Min.   :   0.00  
# Class :character   Class :character   Class :character   1st Qu.: 0.0000   1st Qu.: 0.000   1st Qu.:   0.00  
# Mode  :character   Mode  :character   Mode  :character   Median : 0.0000   Median : 0.000   Median :  84.24  
# Mean   : 0.2338   Mean   : 1.107   Mean   : 130.72  
# 3rd Qu.: 0.0000   3rd Qu.: 2.000   3rd Qu.: 186.00  
# Max.   :22.0000   Max.   :33.000   Max.   :6616.00  
# 
# DBNOs         headshotKills         heals         killPlace       killPoints       kills          killStreaks    
# Min.   : 0.0000   Min.   : 0.0000   Min.   : 0.00   Min.   :  1.0   Min.   :   0   Min.   : 0.0000   Min.   : 0.000  
# 1st Qu.: 0.0000   1st Qu.: 0.0000   1st Qu.: 0.00   1st Qu.: 24.0   1st Qu.:   0   1st Qu.: 0.0000   1st Qu.: 0.000  
# Median : 0.0000   Median : 0.0000   Median : 0.00   Median : 47.0   Median :   0   Median : 0.0000   Median : 0.000  
# Mean   : 0.6579   Mean   : 0.2268   Mean   : 1.37   Mean   : 47.6   Mean   : 505   Mean   : 0.9248   Mean   : 0.544  
# 3rd Qu.: 1.0000   3rd Qu.: 0.0000   3rd Qu.: 2.00   3rd Qu.: 71.0   3rd Qu.:1172   3rd Qu.: 1.0000   3rd Qu.: 1.000  
# Max.   :53.0000   Max.   :64.0000   Max.   :80.00   Max.   :101.0   Max.   :2170   Max.   :72.0000   Max.   :20.000  
# 
# longestKill      matchDuration   matchType            maxPlace       numGroups        rankPoints      revives       
# Min.   :   0.00   Min.   :   9   Length:4446966     Min.   :  1.0   Min.   :  1.00   Min.   :  -1   Min.   : 0.0000  
# 1st Qu.:   0.00   1st Qu.:1367   Class :character   1st Qu.: 28.0   1st Qu.: 27.00   1st Qu.:  -1   1st Qu.: 0.0000  
# Median :   0.00   Median :1438   Mode  :character   Median : 30.0   Median : 30.00   Median :1443   Median : 0.0000  
# Mean   :  23.00   Mean   :1580                      Mean   : 44.5   Mean   : 43.01   Mean   : 892   Mean   : 0.1647  
# 3rd Qu.:  21.32   3rd Qu.:1851                      3rd Qu.: 49.0   3rd Qu.: 47.00   3rd Qu.:1500   3rd Qu.: 0.0000  
# Max.   :1094.00   Max.   :2237                      Max.   :100.0   Max.   :100.00   Max.   :5910   Max.   :39.0000  
# 
# rideDistance        roadKills          swimDistance        teamKills        vehicleDestroys     walkDistance    
# Min.   :    0.00   Min.   : 0.000000   Min.   :   0.000   Min.   : 0.00000   Min.   :0.000000   Min.   :    0.0  
# 1st Qu.:    0.00   1st Qu.: 0.000000   1st Qu.:   0.000   1st Qu.: 0.00000   1st Qu.:0.000000   1st Qu.:  155.1  
# Median :    0.00   Median : 0.000000   Median :   0.000   Median : 0.00000   Median :0.000000   Median :  685.6  
# Mean   :  606.12   Mean   : 0.003496   Mean   :   4.509   Mean   : 0.02387   Mean   :0.007918   Mean   : 1154.2  
# 3rd Qu.:    0.19   3rd Qu.: 0.000000   3rd Qu.:   0.000   3rd Qu.: 0.00000   3rd Qu.:0.000000   3rd Qu.: 1976.0  
# Max.   :40710.00   Max.   :18.000000   Max.   :3823.000   Max.   :12.00000   Max.   :5.000000   Max.   :25780.0  
# 
# weaponsAcquired    winPoints       winPlacePerc   
# Min.   :  0.00   Min.   :   0.0   Min.   :0.0000  
# 1st Qu.:  2.00   1st Qu.:   0.0   1st Qu.:0.2000  
# Median :  3.00   Median :   0.0   Median :0.4583  
# Mean   :  3.66   Mean   : 606.5   Mean   :0.4728  
# 3rd Qu.:  5.00   3rd Qu.:1495.0   3rd Qu.:0.7407  
# Max.   :236.00   Max.   :2013.0   Max.   :1.0000  
# NA's   :1       
###########

Nplayers<-length(unique(dataTrain$Id));Nplayers #4446966
Ngroups<-length(unique(dataTrain$groupId));Ngroups # 2026745
Nplayers/Ngroups #2.194142 mean players per group
# Nmatches<-length(unique(dataTrain$matchId));Nmatches #47965

# AveMatchDuration<-mean(data)
NmatchType<-length(unique(dataTrain$matchType));NmatchType #16

sum(is.na(dataTrain$winPlacePerc)) #1
Nwinners<-sum(dataTrain$winPlacePerc==1,na.rm = T);Nwinners #127573
Nlosers<-sum(dataTrain$winPlacePerc==0,na.rm = T);Nlosers #220505
###################

##### match stats

Nmatches<-length(unique(dataTrain$matchId));Nmatches #47965

range(dataTrain$winPlacePerc,na.rm = T)
cutpoints<-c(0,0.2,0.9,0.999999999,1.1);lab=c("tail","heart","head","top") 
scores<-cut(dataTrain$winPlacePerc,breaks=cutpoints,labels =lab,right = F ) #[a,b)
(table(scores)/length(scores))*100
# (table(scores)/sum(table(scores)))*100
# tail      heart      head      top 
# 1107104   2870630    341658    127573 
# 24.895715 64.552551  7.682946  2.868765  %
# sum((table(scores)/length(scores))*100)
# (Nwinners/length(scores))*100 #2.868765


dataMatch<-dataTrain %>% mutate(score=scores=="top") %>% add_count(matchId)  %>% mutate(nPlayers=n) %>% 
   select(-n) %>% group_by(matchId) %>% 
   summarise(type=unique(matchType,matchId),duration=mean(matchDuration),nGroups=mean(numGroups), 
# <<<<<<< HEAD
          # rankPoints=mean(rankPoints),winPoints=mean(winPoints),killPlace=mean(killPlace),longestKill=mean(longestKill),
                                  maxPlace=mean(maxPlace),nPlayers=unique(nPlayers,matchId),nWinners=sum(score))
# =======
                                  # rankPoints=mean(rankPoints),winPoints=mean(winPoints),longestKill=mean(longestKill),
                                  # maxPlace=mean(maxPlace),nPlayers=unique(nPlayers,matchId),nWinners=sum(score))# %>%
# >>>>>>> parent of 5a9c7fd... correlation between variable per score


write.csv(dataMatch,file=file.path("working_data","dataMatch.csv"))

# checks 
winningMatches<-dataTrain$matchId[scores=="top"]
length(winningMatches)/Nmatches
x<-unique(winningMatches);length(x)

##### group stats

# Ngroups<-length(unique(dataTrain$groupId));Ngroups # 2026745
# names(dataTrain)
# with(dataTrain,teamKills[groupId=="289a6836a88d27"])
# with(dataTrain,killPoints[groupId=="289a6836a88d27"]) #?


##### player stats
Nplayers<-length(unique(dataTrain$Id));Nplayers #4446966

range(dataTrain$winPlacePerc,na.rm = T)
cutpoints<-c(0,0.2,0.9,0.999999999,1.1);lab=c("tail","heart","head","top") 
scores<-cut(dataTrain$winPlacePerc,breaks=cutpoints,labels =lab,right = F ) #[a,b)
scores[is.na(scores)]<-"tail"
table(scores)

# tail   heart    head     top 
# 1107105 2870630  341658  127573 

dataSample<-dataTrain#[1:100,]
# dataPlayerS<-dataSample %>% 
#   # select(Id,matchId,headshotKills,killStreaks,roadKills,longestKill,killPlace,killPoints,kills,teamKills) %>%
#   mutate(DBNOs=DBNOs,damageDealt=damageDealt,weaponsAcquired=weaponsAcquired,vehicleDestroys=vehicleDestroys,
#          killPoints=killPoints,killPlace=killPlace,longestKill=longestKill,
#          totKills=headshotKills+killStreaks+roadKills+kills+teamKills,
#          totDistance=swimDistance+walkDistance+rideDistance,
#          heals=heals,revives=revives,assists=assists,boosts=boosts,
#          winPoints=winPoints,score=scores) %>% 
#   select(Id,groupId,DBNOs,damageDealt,weaponsAcquired,killPoints,killPlace,longestKill,
#          totKills,totDistance,
#          vehicleDestroys,
#          heals,revives,assists,boosts,winPoints,score)
# 
# 
# dataPlayerS<-dataPlayerS[complete.cases(dataPlayerS),]
# sum(is.na(dataPlayerS))

write.csv(dataPlayerS,file=file.path("working_data","dataPlayerS.csv"))

(table(dataPlayerS$totKills)/length(dataPlayerS$totKills))*100
#~60% of players has a total of 0 kills

sum(dataPlayerS$check)

# headshotKills,killStreaks,roadKills,kills are indipendent
# longestKill is a personal stat
with(dataTrain,longestKill[matchId=="0000a43bce5eec"])
with(dataTrain,longestKill[groupId=="4d4b580de459be"])
names(dataPlayerS)



##### distributions of kill stats
# plot_kills<-melt(dataPlayerS,id.vars = )
g_kills1<-ggplot(data = dataTrain,aes(y=headshotKills)) +geom_boxplot()
g_kills2<-ggplot(data = dataTrain,aes(y=killStreaks)) +geom_boxplot()
g_kills3<-ggplot(data = dataTrain,aes(y=roadKills)) +geom_boxplot()
g_kills4<-ggplot(data = dataTrain,aes(y=longestKill)) +geom_boxplot()
g_kills5<-ggplot(data = dataTrain,aes(y=killPlace)) +geom_boxplot()
g_kills6<-ggplot(data = dataTrain,aes(y=killPoints)) +geom_boxplot()
g_kills7<-ggplot(data = dataTrain,aes(y=kills)) +geom_boxplot()
g_kills8<-ggplot(data = dataTrain,aes(y=totKills)) +geom_boxplot()
g_kills9<-ggplot(data = dataTrain,aes(y=teamKills)) +geom_boxplot()

grid.arrange(g_kills1,g_kills2,g_kills3,g_kills4,g_kills5,g_kills6,g_kills7,g_kills8,g_kills9,nrow=3)

g_kills1<-ggplot(data = dataTrain,aes(x=headshotKills)) +geom_histogram(binwidth=1)
g_kills2<-ggplot(data = dataTrain,aes(x=killStreaks)) +geom_histogram(binwidth=1)
g_kills3<-ggplot(data = dataTrain,aes(x=roadKills)) + geom_histogram(binwidth=1)
g_kills4<-ggplot(data = dataTrain,aes(x=longestKill)) + geom_histogram(binwidth=1)
g_kills5<-ggplot(data = dataTrain,aes(x=killPlace)) + geom_histogram(binwidth=1)
g_kills6<-ggplot(data = dataTrain,aes(x=killPoints)) + geom_histogram(binwidth=1)
g_kills7<-ggplot(data = dataTrain,aes(x=kills)) + geom_histogram(binwidth=1)
g_kills8<-ggplot(data = dataTrain,aes(x=totKills)) + geom_histogram(binwidth=1)
g_kills9<-ggplot(data = dataTrain,aes(x=teamKills)) + geom_histogram(binwidth=1)


grid.arrange(g_kills1,g_kills2,g_kills3,g_kills4,g_kills5,g_kills6,g_kills7,g_kills8,g_kills9,nrow=3)


######################## distribution of player stats

playerVariables<-c("DBNOs","damageDealt","weaponsAcquired","killPoints","killPlace","longestKill",
                   "totKills","totDistance","vehicleDestroys", "heals","revives","assists","boosts","winPoints")

g_glob<-list()

legend_pos<-c("none","none","none","none","none","none","none","none","none","none","none","none","none","none")
for(i in seq_along(playerVariables)){
  rangeVar<-range(dataPlayerS[,playerVariables[i]])
  g_glob[[i]]<-ggplot(data = dataPlayerS,aes(x=dataPlayerS[,playerVariables[i]],fill=score,stat(ncount))) +
    geom_histogram(binwidth=max(rangeVar)/10,position = "dodge") +coord_cartesian(xlim = rangeVar)+
     xlab(playerVariables[i])+ ylab("% of max")+theme(legend.position = legend_pos[i])
}
g_glob[[length(playerVariables)+1]]<-ggplot(data = dataPlayerS,aes(x=dataPlayerS[,3],fill=score)) +
  theme(legend.position = "right")

# g_glob1<-ggplot(data = dataPlayerS,aes(x="damageDealt",fill=score,stat(ncount))) +geom_histogram(binwidth=100,position = "dodge")+
# coord_cartesian(xlim = c(0, 2500))+ylab("% of max")+theme(legend.position = "none")
# g_glob2<-ggplot(data = dataPlayerS,aes(x=weaponsAcquired,fill=score,stat(ncount))) +geom_histogram(binwidth=1,position = "dodge")+
#   coord_cartesian(xlim = c(0, 20))+ylab("% of max")+theme(legend.position = "none")
# g_glob3<-ggplot(data = dataPlayerS,aes(x=killPoints,fill=score,stat(ncount))) + geom_histogram(binwidth=10,position = "dodge")+
#   coord_cartesian(xlim = c(0, 1700))+ylab("% of max")+theme(legend.position = "none")
# g_glob4<-ggplot(data = dataPlayerS,aes(x=teamKills,fill=score,stat(ncount))) + geom_histogram(binwidth=1,position = "dodge")+
#   ylab("% of max") +theme(legend.position = "none")
# g_glob5<-ggplot(data = dataPlayerS,aes(x=totKills,fill=score,stat(ncount))) + geom_histogram(binwidth=1,position = "dodge")+
#   coord_cartesian(xlim = c(0, 30))+ylab("% of max")+theme(legend.position = "none")
# g_glob6<-ggplot(data = dataPlayerS,aes(x=totDistance,fill=score,stat(ncount))) + geom_histogram(binwidth=10,position = "dodge")+
#   coord_cartesian(xlim = c(0, 1000))+ylab("% of max") +theme(legend.position = "none")
# g_glob7<-ggplot(data = dataPlayerS,aes(x=maxPlace,fill=score,stat(ncount))) + geom_histogram(binwidth=10,position = "dodge") +
#   ylab("% of max") +theme(legend.position = "none")
# g_glob8<-ggplot(data = dataPlayerS,aes(x=heals,fill=score,stat(ncount))) + geom_histogram(binwidth=1,position = "dodge")+
#   ylab("% of max") +theme(legend.position = "none")
# g_glob9<-ggplot(data = dataPlayerS,aes(x=revives,fill=score,stat(ncount))) + geom_histogram(binwidth=10,position = "dodge")+
#   ylab("% of max") +theme(legend.position = "none")


grid.arrange(g_glob[[1]],g_glob[[2]],g_glob[[3]],g_glob[[4]],g_glob[[5]],
             g_glob[[6]],g_glob[[7]],g_glob[[8]],g_glob[[9]],g_glob[[10]],
             g_glob[[11]],g_glob[[12]],g_glob[[13]],g_glob[[14]],nrow=4)

# damageDealt, weaponsAcquired, totKills, totDistance distributions are different for top (i.e. winning) players
# normal, normal, exp, normal?


# Are significantly different?
players_top<-dataPlayerS[dataPlayerS$score=="top",]
players_head<-dataPlayerS[dataPlayerS$score=="head",]
players_heart<-dataPlayerS[dataPlayerS$score=="heart",]
players_tail<-dataPlayerS[dataPlayerS$score=="tail",]
# qqplot(x=players_top$damageDealt,y=players_head$damageDealt)
# x<-qqline(y=players_top$damageDealt,plot.it=F)

players_x<-players_top
title_str<-"top"

qq1<-ggplot(players_x, aes(sample=damageDealt))+
  stat_qq(distribution=qnorm,dparams=list(mean =mean(players_x$damageDealt) ,sd=sd(players_x$damageDealt)))+
  theme(legend.position = "none")+ ylab("damageDealt")
qq2<-ggplot(players_x, aes(sample=weaponsAcquired))+
  stat_qq(distribution=qnorm,dparams=list(mean =mean(players_x$weaponsAcquired) ,sd=sd(players_x$weaponsAcquired)))+
  theme(legend.position = "none")+ ylab("weaponsAcquired")
qq3<-ggplot(players_x, aes(sample=killPoints))+
  stat_qq(distribution=qnorm,dparams=list(mean =mean(players_x$killPoints) ,sd=sd(players_x$killPoints)))+
  theme(legend.position = "none")+ ylab("killPoints")
qq4<-ggplot(players_x, aes(sample=teamKills))+
  stat_qq(distribution=qnorm,dparams=list(mean =mean(players_x$teamKills) ,sd=sd(players_x$teamKills)))+
  theme(legend.position = "none")+ ylab("teamKills")
qq5<-ggplot(players_x, aes(sample=totKills))+
  stat_qq(distribution=qnorm,dparams=list(mean =mean(players_x$totKills) ,sd=sd(players_x$totKills)))+
  theme(legend.position = "none")+ ylab("totKills")
qq6<-ggplot(players_x, aes(sample=totDistance))+
  stat_qq(distribution=qnorm,dparams=list(mean =mean(players_x$totDistance) ,sd=sd(players_x$totDistance)))+
  theme(legend.position = "none")+ ylab("totDistance")
qq7<-ggplot(players_x, aes(sample=rideDistance))+
  stat_qq(distribution=qnorm,dparams=list(mean =mean(players_x$rideDistance) ,sd=sd(players_x$rideDistance)))+
  theme(legend.position = "none")+ ylab("rideDistance")
qq8<-ggplot(players_x, aes(sample=swimDistance))+
  stat_qq(distribution=qnorm,dparams=list(mean =mean(players_x$swimDistance) ,sd=sd(players_x$swimDistance)))+
  theme(legend.position = "none")+ ylab("swimDistance")
qq9<-ggplot(players_x, aes(sample=walkDistance))+
  stat_qq(distribution=qnorm,dparams=list(mean =mean(players_x$walkDistance) ,sd=sd(players_x$walkDistance)))+
  theme(legend.position = "none")+ ylab("walkDistance")
qq10<-ggplot(players_x, aes(sample=maxPlace))+
  stat_qq(distribution=qnorm,dparams=list(mean =mean(players_x$maxPlace) ,sd=sd(players_x$maxPlace)))+
  theme(legend.position = "none")+ ylab("maxPlace")
qq11<-ggplot(players_x, aes(sample=heals))+
  stat_qq(distribution=qnorm,dparams=list(mean =mean(players_x$heals) ,sd=sd(players_x$heals)))+
  theme(legend.position = "none")+ ylab("heals")
qq12<-ggplot(players_x, aes(sample=revives))+
  stat_qq(distribution=qnorm,dparams=list(mean =mean(players_x$revives) ,sd=sd(players_x$revives)))+
  theme(legend.position = "right")+ ylab("revives")


grid.arrange(qq1,qq2,qq3,qq4,qq5,qq6,qq7,qq8,qq9,qq10,qq11,qq12,nrow=4)



randomSel<-runif(length(players_top),min=0,max=length(players_top))
### playersVariables<-c("damageDealt","weaponsAcquired", "killPoints","teamKills","totKills", "totDistance",
###                     "rideDistance","swimDistance","walkDistance","maxPlace","heals","revives")

# playerVariables<-c("DBNOs","damageDealt","weaponsAcquired","killPoints","killPlace","longestKill",
#                    "totKills","totDistance","vehicleDestroys", "heals","revives","assists","boosts","winPoints")

# par(mfcol=c(3,3))
# for (var in playersVariables){
#   qqline(y=players_top[,var],main=var)
# }


ttest_topHead<-sapply(playerVariables,function(var){
  res<-t.test(players_top[,var],players_head[randomSel,var],alternative="greater");
  return(c(res$statistic,res$p.value))
})

#   damageDealt weaponsAcquired killPoints  teamKills    totKills totDistance rideDistance swimDistance walkDistance  
# t 3.853645444      -0.3886146  1.1090707 -0.8706539 2.977544991   0.5786257    0.1871055      81.1031    1.2696461
#   0.000876266       0.6482979  0.1430411  0.8006869 0.004989761   0.2860161    0.4271310       0.0000    0.1124521  
# 
#   maxPlace      heals     revives
# t 1.1860754   -1.7725366 -0.6445508
#   0.1276635    0.9509728  0.7351818

ttest_topHeart<-sapply(playerVariables,function(var){
  res<-t.test(players_top[,var],players_heart[randomSel,var],alternative="greater");
  return(c(res$statistic,res$p.value))
})

#    damageDealt weaponsAcquired killPoints     teamKills     totKills  totDistance rideDistance swimDistance walkDistance   
# t 1.033695e+01     3.426197418 1.78696765  2.951995e+01 8.668574e+00 4.5106661825  2.854621209    0.8485338 4.4298776502 
#   3.034755e-08     0.002046232 0.04779874 3.524043e-191 2.629219e-07 0.0002443533  0.006361026    0.2051986 0.0002854384  
# 
#   maxPlace      heals   revives
# -0.8654241 -1.0094798  3.8388623861
#  0.7993043  0.8350635  0.0009017317

ttest_topTail<-sapply(playerVariables,function(var){
  res<-t.test(players_top[,var],players_tail[randomSel,var],alternative="greater");
  return(c(res$statistic,res$p.value))
})


#    damageDealt weaponsAcquired  killPoints     teamKills     totKills  totDistance rideDistance swimDistance walkDistance  
# t 2.381813e+01    1.485161e+01 -0.03140198  2.951995e+01 2.853327e+01 2.469871e+02     234.1082      81.1031 1.731805e+02 
#   4.382693e-13    2.840774e-10  0.51230394 3.524043e-191 3.392551e-14 2.133977e-35       0.0000       0.0000 3.100929e-26 

#  maxPlace    heals  revives
# 0.2564706 340.8363  226.9797
# 0.4006573   0.0000    0.0000

# <<<<<<< HEAD
# Create table top -other scores pvalues
pvalVar<-data.frame(variable=rep(playerVariables,times=3),score=rep(c("top-head","top-heart","top-tail"),each=length(playerVariables)),
                    tstat=c(ttest_topHead[1,],ttest_topHeart[1,],ttest_topTail[1,]),
                    pvalues=c(ttest_topHead[2,],ttest_topHeart[2,],ttest_topTail[2,]))

pvalVar<-pvalVar %>% mutate(significant=ifelse(pvalues<=0.05,"*","."))
# g_pval<-ggplot(pvalVar,aes(x=variable,y=pvalues,group=score))+geom_line(aes(x=variable,y=pvalues,color=score),size=1.5,linejoin="round")+
#   geom_hline(aes(yintercept=0.05))+ theme(axis.text.x = element_text(angle = 90, hjust = 1))+
#   geom_point(aes(shape=significant),size=2)+scale_shape_manual(values=c(8, 16))
g_pval<-ggplot(pvalVar,aes(x=variable,y=tstat,group=score))+geom_line(aes(x=variable,y=tstat,color=score),size=1,linejoin="round")+
   theme(axis.text.x = element_text(angle = 90, hjust = 1))+ylab("difference in average")+  xlab("")+
  geom_point(aes(shape=significant,color=score),size=2.5)+scale_shape_manual(values=c(8, 16))
g_pval



# significant different variables between top and head players
significantVar<-ttest_topHead[2,]<=0.05;names(significantVar[significantVar==T])
# "DBNOs" "damageDealt" "longestKill" "totKills" "vehicleDestroys" "revives" "assists" 

# Correlation between variables divided per score
playersList<-list(players_top,players_head,players_heart,players_tail)
score_labels<-c("top","head","heart","tail")
legend_pos<-c("none","none","none","right")
g_corrVar<-list()
for(i in seq_along(playersList)){
  CCvariables<-cor(playersList[[i]][,playerVariables])
  CCvar_df<-melt(CCvariables)
  
  
  g_corrVar[[i]] <- ggplot(CCvar_df,aes(x=Var1,y=Var2, fill=value)) + geom_tile() + labs(tag=score_labels[i])+
    scale_fill_gradient(low = "cornsilk2", high = "darkred") +  xlab("")+ylab("") +
    theme(axis.text.x = element_text(angle = 90, hjust = 1),plot.tag.position="top",legend.position=legend_pos[i]) 
   
}
# =======
# >>>>>>> parent of 5a9c7fd... correlation between variable per score




