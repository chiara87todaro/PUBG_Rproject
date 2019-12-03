# PUBG machine learning functions

#########################
# create a lebel for the error of prediction
label_error<-function(x,thresh=0.001){
  lab<-case_when(
    abs(x)<=thresh ~ "correct",
    x>thresh ~ "over-estim",
    x< -thresh ~ "under-estim"
  )
  lab
}
###########################
#subsample balanced classes
balanced_subsample<-function(data,lab,unisize){
   dataTrainSam<-data.frame()
   set.seed(952)
   for (k in seq_along(lab)){
      dataTemp<-data[data$score==lab[k], ] # select all obs belonging to a class
      dataTrainSam<-rbind(dataTrainSam,dataTemp[runif(n=unisize,min=0,max=nrow(dataTemp)), ])
   }
   return(dataTrainSam)
}


# table(dataTrainSam$score)
#####
# create model, plot error and calculate accuracy
# pubg_ml_accuracy<-function(){
#   
#   fit<-train(winPlacePerc ~ .-1 -Id, method=methString[i], data = dataTrainSam)
#   summary(fit$finalModel)
#   predictions<-predict(fit,newdata = dataTrainSam[, names(dataTrainSam)!="winPlacePerc"],interval="prediction")
#   range(predictions)
#   
#   
#   res<-data.frame(Id=dataTrainSam$Id,actual=dataTrainSam$winPlacePerc,predicted=predictions)
#   res<-res %>% mutate(err=actual-predicted,err_label=label_error(err)) %>% select(Id,actual,predicted,err,err_label)
#   # res1<-res[1:10,]
#   # accuracy<-mse(res$actual,res$predicted);accuracy
#   
#   # gg_pred<-ggplot(data=res)+geom_point(aes(x=Id,y=actual),color="blue")+geom_point(aes(x=Id,y=predicted),color="red");gg_pred
#   gg_err<-ggplot(data=res)+geom_point(aes(x=Id,y=err,color=err_label))+
#     ggtitle(paste(methString[i],": actual - predicted"))+xlab("Id player")+
#     theme(axis.text.x=element_blank(),plot.title = element_text( hjust = 0.5));gg_err
#   
#   accMethods[[i]]<-list(method=methString[i],accuracy=(table(res$err_label)/length(res$err_label))*100)
#   
#   
# }

########### 
# library(dplyr);library(ggplot2);library(reshape2)
# library(gridExtra);library(caret);library(ModelMetrics)
# 
# # dataTarget<-read.csv(file = file.path("working_data","dataTarget.csv"))
# # dataTarget<-dataTarget %>% select(-X)
# db<-read.csv(file = file.path("working_data","dataTarget.csv"))
# db<-db %>% select(-X)
# lab=c("r0_10","r10_20","r20_30","r30_40","r40_50","r50_60","r60_70","r70_80","r80_90","r90_99","r100")
# # size1sample=1000
# calculate_balanced_importance<-function(db,lab,size1sample=1000,saveModel=F,saveImport=T){
#    dataTrainSam<-data.frame()
#    set.seed(952)
#    for (k in seq_along(lab)){
#       dataTemp<-db[db$score==lab[k], ] # select all obs belonging to a class
#       dataTrainSam<-rbind(dataTrainSam,dataTemp[runif(n=size1sample,min=0,max=nrow(dataTemp)), ])
#    }
#    rm(db,dataTemp)
#    tree_ctrl <- trainControl(method = "cv", p=0.75, classProbs=T,seeds=set.seed(1117),
#                              number = 10,returnResamp = "all")
#    tree_fit <- train(score ~ .-1 -Id, method="treebag", data = dataTrainSam,
#                      trControl =tree_ctrl,importance=TRUE )
#    if(saveModel==T){
#       save(tree_fit,file = file.path("working_data","smallBalanced_treebag.Rdata"))
#    }
#    temp<-varImp(tree_fit)
#    importanceStats<-data.frame(variables=as.character(rownames(temp$importance)),importance=(temp$importance[,"Overall"]))
#    if(saveImport==T){
#       save(importanceStats,file = file.path("working_data","smallBalanced_importance_treebag.Rdata"))
#    }
#    return(importanceStats)
# }

# cutpoints<-seq(from=0,to=1.1,by=0.1);
# lab=c("r0_10","r10_20","r20_30","r30_40","r40_50","r50_60","r60_70","r70_80","r80_90","r90_99","r100")
# scores<-cut(dataTrain$winPlacePerc,breaks=cutpoints,labels =lab,right = F ) #[a,b)
# scores<-scores[!is.na(scores)] #<-"r0_10"

# dataPlayerS <- dataTrain[complete.cases(dataTrain), ] %>% mutate(score=scores) %>% 
#    select(Id, assists, boosts, damageDealt, DBNOs, headshotKills, heals, killPlace, killPoints, kills, killStreaks, 
#           longestKill, matchDuration, maxPlace, revives, rideDistance, roadKills, swimDistance, teamKills, 
#           vehicleDestroys, walkDistance, weaponsAcquired, winPoints,score)
# 
# 
# 
# 
# table(dataTrainSam$score)
