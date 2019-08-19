# KAGGLE projects
# PUBG
# Machine learning analysis

rm(list = ls())

# mainPath<-file.path(Sys.getenv("HOME"),"kaggle","PUBG_R", "PUBG_R")
mainPath<-file.path(Sys.getenv("R_USER"), "PUBG_R", "PUBG_R", "scripts")
setwd(mainPath)#"~/kaggle/PUBG_R/PUBG_R/"

if(!file.exists("data")){
  dir.create("data")
}

dataDir<-file.path(getwd(), "data");

# install external libraries
library(dplyr);library(ggplot2);library(reshape2)
library(gridExtra);library(caret);library(ModelMetrics)
library(data.table)
# library(earth);library(rattle);library(partykit)# Convert rpart object to BinaryTree
library(pROC)   #for ROC curves
library(rpart);library(rpart.plot)
# library(multiROC)
source("pubg_ml_functions.R")
#######################################################################################
############################### download data
fileUrl<-"https://www.kaggle.com/c/10335/download-all"
dataPath<-paste0(dataDir,"pubg-finish-placement-prediction.zip")

if (!file.exists(dataPath)){
  download.file(fileUrl,dataPath,method = "auto")
  unzip(dataPath)
  # conTrain<-unz(dataPath,filename = "train_V2.csv",open = "rt")
  # dataTrain<-read.csv(conTrain)
}

#### load data
# dataTrainSam<-read.csv(file.path(dataDir,"train_V2.csv"),stringsAsFactors =F,nrows=10)
# dataTrain<-read.csv(file.path(dataDir,"train_V2.csv"),stringsAsFactors =F, col.names = names(dataTrainSam))

dataTrainSam<-read.table(file.path(dataDir,"train_V2.csv"),stringsAsFactors =F,nrows=10,sep = ",",header =T)
dataTrain<-read.table(file.path(dataDir,"train_V2.csv"),sep = ",",header =T,stringsAsFactors =F, 
                      col.names = names(dataTrainSam),colClasses = sapply(dataTrainSam,class))

rm(dataTrainSam)

# dataTestSam<-read.csv(paste0(dataDir,"test_V2.csv"),stringsAsFactors =F,nrows=10)
# names(dataTestSam)==names(dataTrain)
# dataTest<-read.csv(paste0(dataDir,"test_V2.csv"),stringsAsFactors =F,
#                     col.names = names(dataTestSam))
# rm(dataTestSam)

dataOut<-read.csv(file.path(dataDir,"sample_submission_V2.csv"),stringsAsFactors =F)
#only the winners 

################## METHOD SELECTION 1 #######################
# use all variables to chose the best model

dataTrainSam<- dataTrain %>% select(-rankPoints,-groupId,-matchId)
set.seed(1154)
sampleIndices<-as.integer(runif(n=10000,min=0,max=nrow(dataTrain)))
dataTrainSam<-dataTrainSam[sampleIndices,]

target<-dataTrainSam$winPlacePerc
methString<- c("lm","glm","rpart","bagEarth","rf") #,family="poisson"
accMethods<-list()
i<-4
# for(i in methString){
fit<-train(winPlacePerc ~ .-1 -Id -matchType, method=methString[i], data = dataTrainSam)
summary(fit$finalModel)
predictions<-predict(fit,newdata = dataTrainSam[, names(dataTrainSam)!="winPlacePerc"],interval="prediction")
range(predictions)


res<-data.frame(Id=dataTrainSam$Id,actual=dataTrainSam$winPlacePerc,predicted=predictions)
res<-res %>% mutate(err=actual-predicted,err_label=label_error(err)) %>% select(Id,actual,predicted,err,err_label)
# res1<-res[1:10,]
# accuracy<-mse(res$actual,res$predicted);accuracy

# gg_pred<-ggplot(data=res)+geom_point(aes(x=Id,y=actual),color="blue")+geom_point(aes(x=Id,y=predicted),color="red");gg_pred
gg_err<-ggplot(data=res)+geom_point(aes(x=Id,y=err,color=err_label))+
  ggtitle(paste(methString[i],": actual - predicted"))+xlab("Id player")+
  theme(axis.text.x=element_blank(),plot.title = element_text( hjust = 0.5));gg_err

accMethods[[i]]<-list(method=methString[i],accuracy=(table(res$err_label)/length(res$err_label))*100)
# }

accMethods[[i]]$accuracy
    
#on 10000    
# lm:     correct  over-estim under-estim  RANDOM
          # 0.97       48.93       50.10
# glm: poisson  correct  over-estim under-estim RANDOM
               # 0.50       49.89       49.61 
# rpart:    correct  over-estim under-estim  RANDOM
            # 0.48       49.13       50.39 
#bagEarth:     correct  over-estim under-estim 
              # 1.24       49.65       49.11 


# the problem is that the output target is out of range and not precice enough

################## METHOD SELECTION 2.a ###########################
# use labels to the output target:
# opt1:  4 labels ("tail","heart","head","top")
# opt2: 11 labels ("0-10","10-20","20-30","30-40","40-50","50-60","60-70","70-80","80-90","90-99","100")

cutpoints<-c(0,0.2,0.9,0.999999999,1.01);lab=c("tail","heart","head","top") 
scores<-cut(dataTrain$winPlacePerc,breaks=cutpoints,labels =lab,right = F ) #[a,b)
scores[is.na(scores)]<-"tail"
table(scores)
# tail   heart    head     top 
# 1107105 2870630  341658  127573 

dataPlayerS<-dataTrain %>% 
  # select(Id,matchId,headshotKills,killStreaks,roadKills,longestKill,killPlace,killPoints,kills,teamKills) %>%
  mutate(DBNOs=DBNOs,damageDealt=damageDealt,weaponsAcquired=weaponsAcquired,vehicleDestroys=vehicleDestroys,
         killPoints=killPoints,killPlace=killPlace,longestKill=longestKill,
         totKills=headshotKills+killStreaks+roadKills+kills+teamKills,
         totDistance=swimDistance+walkDistance+rideDistance,
         heals=heals,revives=revives,assists=assists,boosts=boosts,
         winPoints=winPoints,score=scores) %>% 
  select(Id,DBNOs,damageDealt,weaponsAcquired,killPoints,killPlace,longestKill,
         totKills,totDistance,
         vehicleDestroys,
         heals,revives,assists,boosts,winPoints,score)

set.seed(1154)
sampleIndices<-as.integer(runif(n=10000,min=0,max=nrow(dataPlayerS)))
dataTrainSam<-dataPlayerS[sampleIndices,]

methString<- c("rpart","rf","treebag","lda") #,family="poisson"
accMethods<-list()
i<-3
# for(i in seq_along(methString)){
  fit<-train(score ~ .-1 -Id, method=methString[i], data = dataTrainSam)
  summary(fit$finalModel)
  predictions<-predict(fit,newdata = dataTrainSam[, names(dataTrainSam)!="score"])
  # plot(fit$finalModel)
  
  res<-data.frame(Id=dataTrainSam$Id,actual=dataTrainSam$score,predicted=predictions)
  # res<-res %>% mutate(err=actual-predicted,err_label=label_error(err)) %>% select(Id,actual,predicted,err,err_label)
  # cmRes<-with(res,confusionMatrix(actual,predicted))
  cmRes<-with(res,table(actual,predicted));cmRes
  table(res$actual)
  plot(cmRes)
  # res1<-res[1:10,]
  # accuracy<-mse(res$actual,res$predicted);accuracy
  
  # gg_err<-ggplot(data=res)+geom_point(aes(x=Id,y=err,color=err_label))+
  #   ggtitle(paste(methString[i],": actual - predicted"))+xlab("Id player")+
  #   theme(axis.text.x=element_blank(),plot.title = element_text( hjust = 0.5));gg_err
  # fancyRpartPlot(fit$finalModel)
  
  accMethods[[i]]<-list(method=methString[i],accuracy=cmRes)
  
  
# }

#rpart
              # predicted
# actual  tail heart head  top
# tail  2148   415    0    0
# heart  380  6026    0    0
# head     2   765    0    0
# top      0   264    0    0

#rf
              # predicted
# actual  tail heart head  top
# tail  2337   226    0    0
# heart  121  6285    0    0
# head     1   101  665    0
# top      2    19    0  243

#treebag
              # predicted
# actual  tail heart head  top
# tail  2551    12    0    0
# heart    9  6397    0    0
# head     0     3  763    1
# top      0     1    0  263
  
#ada 
            # predicted
# actual  tail heart head  top
# tail  2551    12    0    0
# heart    8  6398    0    0
# head     0     3  763    1
# top      0     1    0  263

################## METHOD SELECTION 2.b ################################# 
# opt 2
  
cutpoints<-seq(from=0,to=1.1,by=0.1);
lab=c("r0_10","r10_20","r20_30","r30_40","r40_50","r50_60","r60_70","r70_80","r80_90","r90_99","r100")
scores<-cut(dataTrain$winPlacePerc,breaks=cutpoints,labels =lab,right = F ) #[a,b)
scores[is.na(scores)]<-"r0_10"
table(scores)
# 0-10  10-20  20-30  30-40  40-50  50-60  60-70  70-80  80-90  90-99    100 
# 621847 485258 461998 410339 385639 423227 376146 385841 427440 341658 127573 

dataPlayerS<-dataTrain %>% 
  # select(Id,matchId,headshotKills,killStreaks,roadKills,longestKill,killPlace,killPoints,kills,teamKills) %>%
  mutate(DBNOs=DBNOs,damageDealt=damageDealt,weaponsAcquired=weaponsAcquired,vehicleDestroys=vehicleDestroys,
         killPoints=killPoints,killPlace=killPlace,longestKill=longestKill,
         totKills=headshotKills+killStreaks+roadKills+kills+teamKills,
         totDistance=swimDistance+walkDistance+rideDistance,
         heals=heals,revives=revives,assists=assists,boosts=boosts,
         winPoints=winPoints,score=scores) %>% 
  select(Id,DBNOs,damageDealt,weaponsAcquired,killPoints,killPlace,longestKill,
         totKills,totDistance,
         vehicleDestroys,
         heals,revives,assists,boosts,winPoints,score)

set.seed(1154)
sampleIndices<-as.integer(runif(n=10000,min=0,max=nrow(dataPlayerS)))
dataTrainSam<-dataPlayerS[sampleIndices,]

# target<-dataTrainSam$winPlacePerc
methString<- c("rpart","rf","treebag","lda") #,family="poisson"
accMethods<-list()
i<-4
# for(i in seq_along(methString)){
# Grid <- expand.grid(maxdepth=25,nu=2,iter=100)
cv_opts<-trainControl(method="cv", number=10)
fit<-train(score ~ .-1 -Id, method=methString[i], data = dataTrainSam,trControl=cv_opts)
fit$results
predictions<-predict(fit,newdata = dataTrainSam[, names(dataTrainSam)!="score"],type="raw")
plot.train(fit$finalModel$mtrees)

res<-data.frame(Id=dataTrainSam$Id,actual=dataTrainSam$score,predicted=predictions)
# res<-res %>% mutate(err=actual-predicted,err_label=label_error(err)) %>% select(Id,actual,predicted,err,err_label)
# cmRes<-with(res,confusionMatrix(actual,predicted))
cmRes<-with(res,table(actual,predicted));cmRes
# table(res$actual)
# plot(cmRes)
# res1<-res[1:10,]
# accuracy<-mse(res$actual,res$predicted);accuracy

# gg_err<-ggplot(data=res)+geom_point(aes(x=Id,y=err,color=err_label))+
#   ggtitle(paste(methString[i],": actual - predicted"))+xlab("Id player")+
#   theme(axis.text.x=element_blank(),plot.title = element_text( hjust = 0.5));gg_err
# fancyRpartPlot(fit$finalModel)

accMethods[[i]]<-list(method=methString[i],accuracy=cmRes)


# }

#rpart
                                  # predicted
    # actual  0-10 10-20 20-30 30-40 40-50 50-60 60-70 70-80 80-90 90-99  100
    # 0-10  1109     0   331     0     0     6     0     0     4     0    0
    # 10-20  253     0   838     0     0    19     0     0     3     0    0
    # 20-30    9     0   983     0     0    64     0     0    21     0    0
    # 30-40    0     0   745     0     0   135     0     0    45     0    0
    # 40-50    0     0   547     0     0   182     0     0   126     0    0
    # 50-60    0     0   369     0     0   230     0     0   339     0    0
    # 60-70    0     0   142     0     0   123     0     0   530     0    0
    # 70-80    0     0    71     0     0    37     0     0   753     0    0
    # 80-90    0     0    54     0     0     6     0     0   895     0    0
    # 90-99    0     0    24     0     0     1     0     0   742     0    0
    # 100      0     0     6     0     0     0     0     0   258     0    0
    # 
    
#rf
    # predicted
    # actual  0-10 10-20 20-30 30-40 40-50 50-60 60-70 70-80 80-90 90-99  100
    # 0-10  1449     1     0     0     0     0     0     0     0     0    0
    # 10-20    2  1111     0     0     0     0     0     0     0     0    0
    # 20-30    1     0  1076     0     0     0     0     0     0     0    0
    # 30-40    0     0     0   925     0     0     0     0     0     0    0
    # 40-50    0     0     0     0   855     0     0     0     0     0    0
    # 50-60    0     0     0     0     0   938     0     0     0     0    0
    # 60-70    0     0     0     0     0     0   795     0     0     0    0
    # 70-80    0     0     0     0     0     0     0   861     0     0    0
    # 80-90    0     0     0     0     0     0     0     0   955     0    0
    # 90-99    0     0     0     0     0     0     0     0     0   767    0
    # 100      0     0     0     0     0     0     0     0     0     0  264    
    # 
# treebag <===========================
    # predicted
    # actual  0-10 10-20 20-30 30-40 40-50 50-60 60-70 70-80 80-90 90-99  100
    # 0-10  1449     1     0     0     0     0     0     0     0     0    0
    # 10-20    7  1100     6     0     0     0     0     0     0     0    0
    # 20-30    0     2  1074     1     0     0     0     0     0     0    0
    # 30-40    0     0     0   925     0     0     0     0     0     0    0
    # 40-50    0     0     0     0   855     0     0     0     0     0    0
    # 50-60    0     0     0     0     0   938     0     0     0     0    0
    # 60-70    0     0     0     0     0     0   794     0     0     1    0
    # 70-80    0     0     0     0     0     0     0   860     1     0    0
    # 80-90    0     0     0     0     0     0     0     0   955     0    0
    # 90-99    0     0     0     0     0     0     0     0     1   766    0
    # 100      0     0     0     0     0     0     0     0     0     0  264

    
# ada
    # predicted
    # actual  0-10 10-20 20-30 30-40 40-50 50-60 60-70 70-80 80-90 90-99  100
    # 0-10  1449     1     0     0     0     0     0     0     0     0    0
    # 10-20    7  1102     4     0     0     0     0     0     0     0    0
    # 20-30    0     1  1074     1     1     0     0     0     0     0    0
    # 30-40    0     0     0   925     0     0     0     0     0     0    0
    # 40-50    0     0     0     0   855     0     0     0     0     0    0
    # 50-60    0     0     0     0     0   938     0     0     0     0    0
    # 60-70    0     0     0     0     0     0   794     0     0     1    0
    # 70-80    0     0     0     0     0     0     0   861     0     0    0
    # 80-90    0     0     0     0     0     0     0     0   955     0    0
    # 90-99    0     0     0     0     0     0     0     0     1   766    0
    # 100      0     0     0     0     0     0     0     0     0     0  264   



########################## TREEBAG method
################## control=CV, Y/N? #######################
# summary variables
set.seed(856)
sampleIndices<-as.integer(runif(n=10000,min=0,max=nrow(dataPlayerS)))
dataTrainSam<-dataPlayerS[sampleIndices,]

# system.time({
#   fit<-train(score ~ .-1 -Id, method="treebag", data = dataTrainSam)
#   predictions<-predict(fit,newdata = dataTrainSam[, names(dataTrainSam)!="score"])
# })

# 10000 obs: user 84.15 s
# 100000 obs:  user 4251.11 s <=>  70.85 min
# 1000000 obs:  user  s

# cre
tree_ctrl <- trainControl(method = "cv", p=0.75, classProbs=T,seeds=set.seed(1117),number = 10,returnResamp = "all")
# but treebag is a bagged decision tree without tuning parameters, so using cv seems useless this way
tree_fit <- train(score ~ .-1 -Id, method="treebag", data = dataTrainSam,trControl = tree_ctrl,importance=TRUE ) # trControl = tree_ctrl 
print(tree_fit)
# Bagged CART with ctrl opt
# 10000 samples   # 15 predictor
# 11 classes: 'r0_10', 'r10_20', 'r20_30', 'r30_40', 'r40_50', 'r50_60', 'r60_70', 'r70_80', 'r80_90', 'r90_99', 'r100' 
# No pre-processing
# Resampling: Cross-Validated (10 fold) 
# Summary of sample sizes: 9000, 9000, 9001, 8998, 8999, 9000, ... 
# Resampling results:
#   
# Accuracy   Kappa    
# 0.4061027  0.3403142

# Bagged CART WITHOUT ctrl opt
# 10000 samples   # 15 predictor
# 11 classes: 'r0_10', 'r10_20', 'r20_30', 'r30_40', 'r40_50', 'r50_60', 'r60_70', 'r70_80', 'r80_90', 'r90_99', 'r100' 
# No pre-processing
# Resampling: Bootstrapped (25 reps) 
# Summary of sample sizes: 10000, 10000, 10000, 10000, 10000, 10000, ... 
# Resampling results:
#   
#   Accuracy   Kappa    
# 0.3934143  0.3270301


plot(varImp(tree_fit))
# with or wothout ctrl features has the same importance

# Overall
# totDistance      100.00
# killPlace         85.65
# damageDealt       60.80
# weaponsAcquired   57.20
# longestKill       37.25
# killPoints        36.59
# boosts            34.88
# heals             33.86
# winPoints         29.96
# totKills          19.74
# DBNOs             19.34
# assists           12.20
# revives            8.02
# vehicleDestroys    0.00

# plot(fit,plotType = "scatter", metric = fit$perfNames[1])

predictions<-predict(tree_fit,newdata = dataTrainSam[, names(dataTrainSam)!="score"],type = "raw")

res<-data.frame(Id=dataTrainSam$Id,actual=dataTrainSam$score,predicted=predictions)
cmRes<-with(res,table(actual,predicted));
stasResult<-caret::confusionMatrix(cmRes)
stasResult$table
# predicted
# actual   r0_10 r10_20 r20_30 r30_40 r40_50 r50_60 r60_70 r70_80 r80_90 r90_99 r100
# r0_10   1359      2      1      0      0      0      0      0      0      0    0
# r10_20     3   1174      3      0      0      0      0      0      0      0    0
# r20_30     0      0   1064      0      1      0      0      0      0      0    0
# r30_40     0      0      2    893      3      0      0      0      0      0    0
# r40_50     0      0      0      1    871      0      0      0      0      0    0
# r50_60     0      0      0      1      0    945      0      0      0      0    0
# r60_70     0      0      0      0      0      0    829      0      0      0    0
# r70_80     0      0      0      0      0      1      0    818      0      0    0
# r80_90     0      0      0      0      0      0      0      0    940      1    0
# r90_99     0      0      0      0      0      0      0      0      0    797    0
# r100       0      0      0      0      0      0      0      0      0      0  291
# cmResPerc<-(cmRes/rowSums(cmRes))*100
# round(cmResPerc,digits=2)
#     
predictionsProbs=predict(tree_fit,newdata = dataTrainSam[, names(dataTrainSam)!="score"],type="prob")
# multirocData<-data.frame(r0_10_true=as.integer(dataTrainSam$score=="r0_10"),r10_20_true=as.integer(dataTrainSam$score=="r10_20"),
#                          r20_30_true=as.integer(dataTrainSam$score=="r20_30"),r30_40_true=as.integer(dataTrainSam$score=="r30_40"),
#                          r40_50_true=as.integer(dataTrainSam$score=="r40_50"),r50_60_true=as.integer(dataTrainSam$score=="r50_60"),
#                          r60_70_true=as.integer(dataTrainSam$score=="r60_70"),r70_80_true=as.integer(dataTrainSam$score=="r70_80"),
#                          r80_90_true=as.integer(dataTrainSam$score=="r80_90"),r100_true=as.integer(dataTrainSam$score=="r100"),
#                          r0_10_true=as.integer(dataTrainSam$score=="r0_10"),r10_20_true=as.integer(dataTrainSam$score=="r10_20"),)
# rocCurve <- multiclass.roc(dataTrainSam$score,predictionsProbs[,"r100"])
# auc(rocCurve)
# rs <- rocCurve[['rocs']]
# plot.roc(rs[[1]])
# sapply(2:length(rs),function(i){lines.roc(rs[[i]],col=i)})#plot the ROC curve
# rocLegend<-sapply(2:length(rs), function(i){print(rs[[i]]$levels,sep="-")})
# legend("bottomright", legend=rocLegend, lwd=2)

################## 
# Select the features used in the prediction algorithm

cutpoints<-seq(from=0,to=1.1,by=0.1);
lab=c("r0_10","r10_20","r20_30","r30_40","r40_50","r50_60","r60_70","r70_80","r80_90","r90_99","r100")
scores<-cut(dataTrain$winPlacePerc,breaks=cutpoints,labels =lab,right = F ) #[a,b)
scores[is.na(scores)]<-"r0_10"

dataPlayerS <- dataTrain %>% mutate(score=scores) %>% 
  select(Id, assists, boosts, damageDealt, DBNOs, headshotKills, heals, killPlace, killPoints, kills, killStreaks, 
         longestKill, matchDuration, maxPlace, revives, rideDistance, roadKills, swimDistance, teamKills, 
         vehicleDestroys, walkDistance, weaponsAcquired, winPoints,score)

set.seed(856)
sampleIndices<-as.integer(runif(n=10000,min=0,max=nrow(dataPlayerS)))
dataTrainSam<-dataPlayerS[sampleIndices,]

# system.time({
#   fit<-train(score ~ .-1 -Id, method="treebag", data = dataTrainSam)
#   predictions<-predict(fit,newdata = dataTrainSam[, names(dataTrainSam)!="score"])
# })

# 10000 obs: user 9.68 s
# 100000 obs:  user 95.66 s <=>  1.59 min
# 1000000 obs:  user  s
tree_ctrl <- trainControl(method = "cv", p=0.75, classProbs=T,seeds=set.seed(1117),number = 10,returnResamp = "all")
tree_fit <- train(score ~ .-1 -Id, method="treebag", data = dataTrainSam,trControl =tree_ctrl,importance=TRUE ) # trControl = tree_ctrl 
print(tree_fit)
# Bagged CART 
# 10000 samples  # 23 predictor
# 11 classes: 'r0_10', 'r10_20', 'r20_30', 'r30_40', 'r40_50', 'r50_60', 'r60_70', 'r70_80', 'r80_90', 'r90_99', 'r100' 
# No pre-processing
# Resampling: Bootstrapped (25 reps) 
# Summary of sample sizes: 10000, 10000, 10000, 10000, 10000, 10000, ... 
# Resampling results:
#   
#   Accuracy   Kappa    
# 0.4391208  0.3766504

importantVar<-varImp(tree_fit)
plot(importantVar)

# only 20 most important variables shown (out of 22)
# Overall
# walkDistance    100.000
# killPlace        97.189
# matchDuration    64.843
# damageDealt      57.920
# maxPlace         52.909
# weaponsAcquired  49.080
# boosts           44.553
# heals            36.860
# longestKill      34.372
# killPoints       31.819
# winPoints        26.388
# rideDistance     21.211
# DBNOs            17.522
# kills            14.983
# assists          14.165
# killStreaks       9.618
# headshotKills     8.711
# revives           6.972
# swimDistance      6.498
# teamKills         1.884

# plot(fit,plotType = "scatter", metric = fit$perfNames[1])

predictions<-predict(tree_fit,newdata = dataTrainSam[, names(dataTrainSam)!="score"],type = "raw")

res<-data.frame(Id=dataTrainSam$Id,actual=dataTrainSam$score,predicted=predictions)
cmRes<-with(res,table(actual,predicted));
stasResult<-caret::confusionMatrix(cmRes)
stasResult$table

# predicted
# actual   r0_10 r10_20 r20_30 r30_40 r40_50 r50_60 r60_70 r70_80 r80_90 r90_99 r100
# r0_10   1457      0      0      0      0      0      0      0      0      0    0
# r10_20     1   1125      1      0      0      0      0      0      0      0    0
# r20_30     1      1    984      0      0      0      0      0      0      0    0
# r30_40     0      0      1    950      0      1      0      0      0      0    0
# r40_50     0      0      0      0    856      0      0      0      0      0    0
# r50_60     0      0      0      0      0    917      0      0      0      0    0
# r60_70     0      0      0      0      0      0    827      0      0      0    0
# r70_80     0      0      0      0      0      0      1    869      0      0    0
# r80_90     0      0      0      0      0      0      0      0    996      0    0
# r90_99     0      0      0      0      0      0      0      0      0    750    0
# r100       0      0      0      0      0      0      0      0      0      0  262


fancyRpartPlot(tree_fit[["finalModel"]][["mtrees"]][[1]][["btree"]])
rpart.plot(tree_fit[["finalModel"]][["mtrees"]][[2]][["btree"]])

######################## FEATURE SELECTION #######################################


cutpoints<-seq(from=0,to=1.1,by=0.1);
lab=c("r0_10","r10_20","r20_30","r30_40","r40_50","r50_60","r60_70","r70_80","r80_90","r90_99","r100")
scores<-cut(dataTrain$winPlacePerc,breaks=cutpoints,labels =lab,right = F ) #[a,b)
scores<-scores[!is.na(scores)] #<-"r0_10"

dataPlayerS <- dataTrain[complete.cases(dataTrain), ] %>% mutate(score=scores) %>% 
  select(Id, assists, boosts, damageDealt, DBNOs, headshotKills, heals, killPlace, killPoints, kills, killStreaks, 
         longestKill, matchDuration, maxPlace, revives, rideDistance, roadKills, swimDistance, teamKills, 
         vehicleDestroys, walkDistance, weaponsAcquired, winPoints,score)

set.seed(856)
sampleIndices<-as.integer(runif(n=11000,min=0,max=nrow(dataPlayerS)))
dataTrain1<-dataPlayerS[sampleIndices,]
table(dataTrain1$score)
# r0_10 r10_20 r20_30 r30_40 r40_50 r50_60 r60_70 r70_80 r80_90 r90_99   r100 
# 1614   1222   1090   1061    952    977    913    983   1083    807    298 

#  Imbalance correction in output classes
# Down Sample
dataTrainSam<-data.frame()
set.seed(952)
for (k in seq_along(lab)){
  dataTemp<-dataPlayerS[dataPlayerS$score==lab[k], ] # select all obs belonging to a class
  dataTrainSam<-rbind(dataTrainSam,dataTemp[runif(n=20000,min=0,max=nrow(dataTemp)), ])
  
}
table(dataTrainSam$score)
# r0_10 r10_20 r20_30 r30_40 r40_50 r50_60 r60_70 r70_80 r80_90 r90_99   r100 
# 10000   10000   10000   10000   10000   10000   10000   10000   10000   10000   10000 

tree_ctrl <- trainControl(method = "cv", p=0.75, classProbs=T,seeds=set.seed(1117),number = 10,returnResamp = "all")
tree_fit <- train(score ~ .-1 -Id, method="treebag", data = dataTrainSam,trControl =tree_ctrl,importance=TRUE ) # trControl = tree_ctrl 
# # print(tree_fit)
# variables  importance              balanced importance 
# 1          assists  13.9232412      15.5132471
# 2           boosts  45.6436125      45.6237071
# 3      damageDealt  58.2649034      62.0616588
# 4            DBNOs  17.2065114      19.4325817
# 5    headshotKills   8.3676851       9.6071408
# 6            heals  36.6800133      37.5759452
# 7        killPlace  96.4448574      100.000000
# 8       killPoints  32.1119803      33.3296941
# 9            kills  15.4763327      19.1749173
# 10     killStreaks   9.4627984       8.8960448
# 11     longestKill  34.5761578      36.9411650
# 12   matchDuration  64.2163297      68.3380391
# 13        maxPlace  52.9625665      54.0677887
# 14         revives   6.9203435       7.7674235
# 15    rideDistance  20.8501844      23.8284609
# 16       roadKills   0.0000000       0.0000000
# 17    swimDistance   7.1268419       7.2437493
# 18       teamKills   1.8454533       2.3383597
# 19 vehicleDestroys   0.4772327       0.4606653
# 20    walkDistance 100.0000000      98.3064389
# 21 weaponsAcquired  49.6417530      49.7292494
# 22       winPoints  25.8264505      26.1671661

temp<-varImp(tree_fit)
importanceStats<-data.frame(variables=as.character(rownames(temp$importance)),importance=(temp$importance[,"Overall"]))
rm(temp)

impThresh<-seq(from=0,to=90,by=10)
rocCol<-c("aquamarine","azure4","cornflowerblue","dodgerblue","mediumturquoise" ,"darkblue","blue1","blue4","blueviolet","navyblue" )#blues9 #seq(0,.9,len = length(impThresh))
# rocCurve<-list();
# aucRocCurve<-numeric()
accuracies<-numeric()
# fits<-list()
timeElap<-numeric()
for(i in c(6)){#seq_along(impThresh)
print(i)
  # i<-6
  importantVar<-as.character(importanceStats$variables[importanceStats$importance>=impThresh[i]])
  print(importantVar)
  ctrl<- trainControl(method = "cv", p=0.75, classProbs=T,seeds=set.seed(1013),number = 10,returnResamp = "final")
  tempData<-dataTrainSam[,importantVar]
  tempData<-tempData %>% mutate(score=dataTrainSam$score) 
  timeElap[i]<-system.time({
    fits <- train(score ~ .-1,data =tempData, method="treebag",trControl =ctrl,importance=T ) #data = dataTrainSam
    predProbs<-predict(fits ,newdata = dataTrainSam[, names(dataTrainSam)!="score"],type="prob")
  })[1]
  # allRocCurves <- multiclass.roc(dataTrainSam$score,predProbs)#,direction = "<",auc = T, ci = T
  # rocCurve[[i]]<-allRocCurves[["rocs"]][[55]] #"r90-99/r100"
  # # basicROC<-allRocCurves[["rocs"]][[1]]
  # # plot.roc(basicROC[[1]])
  # plot.roc( rocCurve[[i]][[1]],col=rocCol[i],add=F,print.thres=T,print.auc=T,legend=T) #
  # # title("bhu")
  # # # sapply(2:length(rocCurve[[i]]),function(j){lines.roc( rocCurve[[i]][[j]],col=j)})#plot the ROC curve
  # # plot.roc(rocCurve[[i]][[1]])
  # # plot.roc(rocCurve[[i]][[2]])
  # aucRocCurve[i]<-auc(rocCurve[[i]][[1]])
  accuracies[i]<-fits$results$Accuracy
}



# anova(fits[[1]],fits[[2]],fits[[3]],fits[[4]],fits[[5]],fits[[6]],fits[[7]],fits[[8]],fits[[9]],fits[[10]])
# rpart.plot(fits[["finalModel"]][["mtrees"]][[2]][["btree"]])

# > aucRocCurve
# [1] 0.9999618 0.9999847 0.9999949 0.9999975 0.9999440 0.9999466 0.9999898 0.9981552 0.9981552 0.9981552
# > timeElap
# [1] 39.70 35.31 33.09 31.88 28.86 27.29 27.57 29.33 29.32 29.34

# "damageDealt"   "killPlace"     "matchDuration" "maxPlace"      "walkDistance" same for balanced data train
# "boosts"  "damageDealt" "killPlace" "matchDuration" "maxPlace"  "walkDistance"  "weaponsAcquired"

predictions<- sapply(1:nrow(predProbs),function(i){names(which.max(predProbs[i,]))})
res<-data.frame(Id=dataTrainSam$Id,actual=dataTrainSam$score,predicted=factor(predictions,levels = lab))
cmRes<-with(res,table(actual,predicted));
stasResult<-caret::confusionMatrix(cmRes)
stasResult$table
# 
#  100000 obs, 5 var        predicted
# actual   r0_10 r10_20 r20_30 r30_40 r40_50 r50_60 r60_70 r70_80 r80_90 r90_99  r100
# r0_10  14087     12      0      0      0      0      0      0      0      0     0
# r10_20    34  10783     12      1      0      0      0      0      0      0     0
# r20_30     1     18  10319      6      1      0      0      0      0      0     0
# r30_40     1      0      4   9362      4      1      0      0      0      0     0
# r40_50     0      0      0      5   8697      7      1      0      0      0     0
# r50_60     0      0      0      3      5   9359      2      1      0      0     0
# r60_70     0      0      1      0      2      7   8399      1      0      0     0
# r70_80     0      0      0      0      0      3      1   8717      2      2     0
# r80_90     0      0      0      0      0      0      0      4   9542      7     1
# r90_99     0      0      0      0      0      0      2      0      9   7765     0
# r100       0      0      0      0      0      0      0      0      0      8  2801


# 
# 2500000 obs, 5 var predicted 140 min
# actual   r0_10 r10_20 r20_30 r30_40 r40_50 r50_60 r60_70 r70_80 r80_90 r90_99  r100
# r0_10  34910     49      9      0      0      0      0      0      0      0     0
# r10_20    58  27216     22      2      0      0      0      0      0      0     0
# r20_30    10     44  25711     27      3      2      1      0      0      0     0
# r30_40     6     12     45  23282     15      6      0      0      0      1     0
# r40_50     0      2      9     24  21740     13      1      0      0      0     0
# r50_60     1      1      5     15     19  23581      9      8      0      0     0
# r60_70     0      0      1      5      6     38  20939      2      5      0     0
# r70_80     1      1      1      1      1     12     21  21800     16      4     0
# r80_90     0      1      0      2      4      4      7     18  23818     17     5
# r90_99     1      1      1      1      2      0      4      5     25  19327     2
# r100       2      0      0      1      0      0      1      4      6     11  7017



# 10000 obs 7 var predicted 0.45 accuracy
# actual   r0_10 r10_20 r20_30 r30_40 r40_50 r50_60 r60_70 r70_80 r80_90 r90_99 r100
# r0_10   1466      2      0      0      0      0      0      0      0      0    0
# r10_20     1   1108      2      0      0      0      0      0      0      0    0
# r20_30     0      2    986      0      0      0      0      0      0      0    0
# r30_40     0      0      0    965      0      0      0      0      0      0    0
# r40_50     0      0      2      2    862      1      0      0      0      0    0
# r50_60     0      0      0      0      1    886      0      0      0      0    0
# r60_70     0      0      0      0      0      1    834      1      0      0    0
# r70_80     0      0      0      0      0      0      0    891      0      0    0
# r80_90     0      0      0      0      0      0      0      0    984      0    0
# r90_99     0      0      0      0      0      0      0      0      0    734    0
# r100       0      0      0      0      0      0      0      0      0      1  268

stasResult$byClass
format(object.size(fits),units = "GB") #"947.5 Mb"
save(fits, file = "fits_220000obs_5vars.RData")

#########################################  MODEL TEST #####################
load("fits_250000obs_5vars.RData")
load("fits_100000obs_5vars.RData")
# fits<-

cutpoints<-seq(from=0,to=1.1,by=0.1);
lab=c("r0_10","r10_20","r20_30","r30_40","r40_50","r50_60","r60_70","r70_80","r80_90","r90_99","r100")
scores<-cut(dataTrain$winPlacePerc,breaks=cutpoints,labels =lab,right = F ) #[a,b)
scores[is.na(scores)]<-"r0_10" #scores<- scores[!is.na(scores)]

importantVar<-c("damageDealt","killPlace","matchDuration" ,"maxPlace","walkDistance")
stasResults<-list()
accTests<-numeric()
for (j in seq_len(20)){
  # j<-1
  print(j)
  set.seed(1819+j)
  sampleIndices<-as.integer(runif(n=10000,min=0,max=nrow(dataTrain)))
  # dataTrain[complete.cases(dataTrain), ]
  dataSample<-dataTrain[sampleIndices,importantVar]
  predProbs<-predict(fits ,newdata = dataSample[,names(dataSample)!="score"],type="prob")
  predictions<- sapply(1:nrow(predProbs),function(i){names(which.max(predProbs[i,]))})
  res<-data.frame(Id=dataTrain$Id[sampleIndices],actual=scores[sampleIndices],predicted=factor(predictions,levels = lab))
  cmRes<-with(res,table(actual,predicted));
  stasResults[[j]]<-caret::confusionMatrix(cmRes)
  accTests[j]<- stasResults[[j]]$overall[1]
}

# "fits_250000obs_5vars.RData"
# 0.4810 0.4923 0.4913 0.4887 0.4872 0.4866 0.4875 0.4861 0.4798 0.4854 0.4870 0.4911 0.4796 0.4841 0.4956 0.4898 0.4919 0.4817 0.4955 0.4914
# "fits_100000obs_5vars.RData" mean 0.468245
# 0.4699 0.4730 0.4742 0.4692 0.4615 0.4696 0.4681 0.4698 0.4644 0.4748 0.4663 0.4628 0.4676 0.4603 0.4683 0.4678 0.4710 0.4666 0.4705 0.4692
# "fits_11000obs_5vars.RData" mean 0.43176
# 0.4325 0.4375 0.4339 0.4320 0.4368 0.4282 0.4278 0.4389 0.4153 0.4347
# "fits_110000obs_5vars.RData" mean 0.46224
# 0.4592 0.4603 0.4643 0.4597 0.4635 0.4549 0.4599 0.4651 0.4608 0.4774 0.4582 0.4605 0.4548 0.4558 0.4625 0.4553 0.4766 0.4630 0.4721 0.4609
# predicted
# actual   r0_10 r10_20 r20_30 r30_40 r40_50 r50_60 r60_70 r70_80 r80_90 r90_99 r100
# r0_10   1148    241     45      8      5      1      4      3      0      0    4
# r10_20   166    621    200     56      7      7      1      1      2      0    2
# r20_30    31    208    507    231     46     15      6      2      1      3    3
# r30_40     4     44    209    396    170     56     15      9      2      1    2
# r40_50     4     12     52    197    321    167     43      8      7      3    4
# r50_60     0      5     19     57    222    384    221     60     14     16   15
# r60_70     2      3      2     14     51    191    301    166     50     31   30
# r70_80     2      6      4      6     19     56    190    269    165     79   51
# r80_90     4      8      6      3      9     24     90    192    259    188  152
# r90_99     1      7      0      2      6      9     31    102    169    243  202
# r100       2      3      0      3      0      0     10     12     37     64  160

# "fits_220000obs_5vars.RData" mean 0.47972
# 0.4754 0.4814 0.4806 0.4753 0.4788 0.4785 0.4804 0.4857 0.4733 0.4804 0.4765 0.4828 0.4769 0.4806 0.4849 0.4744 0.4848 0.4818 0.4813 0.4806
# predicted
# actual   r0_10 r10_20 r20_30 r30_40 r40_50 r50_60 r60_70 r70_80 r80_90 r90_99 r100
# r0_10   1147    236     46     10      8      1      3      1      4      1    2
# r10_20   177    623    186     49     16      4      1      2      0      3    2
# r20_30    36    182    531    224     52     13      4      2      2      3    4
# r30_40     7     44    188    426    178     46      5      5      2      3    4
# r40_50     1     16     54    163    343    178     49      6      2      2    4
# r50_60     1      2     22     61    200    409    226     45     21     12   14
# r60_70     1      3      5     13     43    174    325    170     48     26   33
# r70_80     1      6      7      5     17     61    186    270    155     79   60
# r80_90     3      8      4      8      9     28     91    182    277    187  138
# r90_99     1      5      0      1      8      8     27     82    144    275  221
# r100       2      0      2      0      0      3      8     11     33     52  180
