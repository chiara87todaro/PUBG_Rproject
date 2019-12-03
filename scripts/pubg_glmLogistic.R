# KAGGLE projects
# PUBG
# Machine learning analysis: improved

rm(list = ls())

# mainPath<-file.path(Sys.getenv("HOME"),"kaggle","PUBG_R", "PUBG_R")
mainPath<-file.path(Sys.getenv("HOME"),"kaggle","PUBG_Rproject", "scripts")
setwd(mainPath)#"~/kaggle/PUBG_R/PUBG_R/"
dataDir<-file.path(getwd(), "data");

# install external libraries
library(dplyr);library(ggplot2);library(reshape2)
library(gridExtra);library(caret);library(ModelMetrics)
library(ggpubr)
source("pubg_ml_functions.R")
#######################################################################################
############################### #### load data
dataTrainSam<-read.csv(file.path(dataDir,"train_V2.csv"),stringsAsFactors =F,nrows=10)
dataTrain<-read.csv(file.path(dataDir,"train_V2.csv"),stringsAsFactors =F,
                    col.names = names(dataTrainSam))

dataTrain<-dataTrain[complete.cases(dataTrain),]

# remove factor variables to calculate correlations
# dataTrain<-dataTrain %>% select(-groupId,-matchId,-matchType,-rankPoints)
factVar<-c("Id","groupId","matchId","matchType","rankPoints")
factInd<-which(names(dataTrain) %in% factVar)

# rm correlated variables?
CCvariables<-cor(dataTrain[,-factInd]) # discard factor variable 
CCvar_df<-melt(CCvariables)

ind<-CCvar_df$value>0.7 & CCvar_df$value<1
# sum(ind)
temp<-CCvar_df[ind==T,c("Var1","Var2")]
rm(CCvariables,CCvar_df,temp)
uncorrVar<-c("DBNOs","killPoints","kills","killStreaks","numGroups")
#leave in damageDealt, winPoints, maxPlace

cutpoints<-seq(from=0,to=1.1,by=0.1);
lab=c("r0_10","r10_20","r20_30","r30_40","r40_50","r50_60","r60_70",
      "r70_80","r80_90","r90_99","r100")
scores<-cut(dataTrain$winPlacePerc,breaks=cutpoints,labels =lab,right = F ) #[a,b)

uncorrVar<-numeric()
dataTrain<-dataTrain %>% mutate(score=scores) %>% select(-uncorrVar,-factVar,Id)

rm(scores)
# create a balanced subset of data
dataTrainSam<-balanced_subsample(dataTrain,lab=lab,unisize=100000)
table(dataTrainSam$score)
rm(dataTrain)
dataTrainSam<-dataTrainSam %>% select(-score,-Id) #
importantVar<-c("teamKills","vehicleDestroys", "walkDistance","weaponsAcquired","winPoints")
dataTrainSam<-dataTrainSam %>% select(importantVar,winPlacePerc)
row.names(dataTrainSam)<-NULL
# create model
fit<-glm(winPlacePerc ~ .-1,family = binomial("logit"), data = dataTrainSam) #

par(mfcol=c(2,3))
plot(fit,which = 1) #c(1,2,3,4,5,6)

modelFile<-"glmLog_model_1mil_uncorr" # "glmLog_model_55thou" "glmLog_model_1mil_uncorr" glmLog_model_1mil_imp
# save(fit,file = file.path("working_data", paste0(modelFile,".Rdata")))
load(file = file.path("working_data",paste0(modelFile,".Rdata")))
# 
# fit<-fit_uncorr
dataTrainSam<-fit$data
predictions<-predict(fit,newdata = dataTrainSam[, names(dataTrainSam)!="winPlacePerc"],
                    type = "response")

range(predictions[1:length(predictions)],rm.na=T)
sum(is.na(predictions))
# plot(predictions)
res<-data.frame(actual=dataTrainSam$winPlacePerc,predicted=predictions) #Id=dataTrainSam$Id,
res<-res %>% mutate(err=actual-predicted,err_label=label_error(err)) %>% 
   select(actual,predicted,err,err_label) #Id,
# res1<-res[1:10,]
# accuracy<-mse(res$actual,res$predicted);accuracy
plot(fit,wich=c(1))

residFit<-data.frame(fitted.values=fit$fitted.values,residuals=fit$residuals)
residP<-ggplot(data=residFit,aes(x=fitted.values,y=residuals))+
   geom_point(alpha=0.5,color="grey")+ylim(c(-5,5))+geom_hline(yintercept = 0,color="red")
residP
annotate_figure(residP, top = "GLM logistic diagnostic",
                fig.lab = "Figure 6", fig.lab.pos = "bottom")


mean(abs(res$err),na.rm=T) # for 10000 # 0.09591182  for 100000  #0.09587363 for 1000000
# 0.09769545 for 1000000 balanced
# 0.1016122 for 1000000 balanced and uncorrelated
# 0.09780951 for 550000 balanced 
# 0.1731467 for 1000000 balanced and important
# gg_pred<-ggplot(data=res)+geom_point(aes(x=Id,y=actual),color="blue")+geom_point(aes(x=Id,y=predicted),color="red");gg_pred
gg_err<-ggplot(data=res)+ggtitle("In-sample error")+ylab("% of majority")+
   theme(plot.title = element_text( hjust = 0.5))+ xlab("|actual - predicted|")+
   # geom_point(aes(x=1:nrow(res),y=err,color=err_label))
   geom_histogram(aes(x=abs(err),stat(100*ncount)),binwidth = 0.01)+
   geom_vline(xintercept =mean(abs(res$err)),color="blue")
gg_err

mean(res$actual-res$predicted)
mean(abs(res$err))
sd(abs(res$err))
var(res$err)
rm(gg_err)

importanceStats<-data.frame(variable=row.names(varImp(fit)),importance=varImp(fit)$Overall)
importanceStats<-importanceStats %>% mutate(importance=sort(importance,decreasing = F))
impP<- ggplot(data=importanceStats,aes(x=variable,y=importance))+geom_bar(stat = "identity")+
   scale_x_discrete(labels=importanceStats$variable)+ labs(title="GLM importance of all stats")+
   theme(title =element_text(hjust = 0.5))+ coord_flip()
impP

# cmRes<-with(res,table(actual,predicted));cmRes  # there are no classes

##### Calculate accuracy on test data
rm(dataTrainSam)
# importantVar<-c("damageDealt","killPlace","matchDuration" ,"maxPlace","walkDistance")
# importantVar<-as.character(importanceStats$variable[importanceStats$importance>20])

# rm(importanceStats)#,impP
accTests<-list()
meanErr<-numeric()
for (j in seq_len(50)){
   # j<-1
   print(j)
   set.seed(1819+j)
   sampleIndices<-as.integer(runif(n=10000,min=0,max=nrow(dataTrain)))
   # dataTrain[complete.cases(dataTrain), ]
   dataSample<-dataTrain[sampleIndices,]  #importantVar #-factInd[factInd!=1]
   predictions<-predict(fit,newdata = dataSample,type="response")
   # predProbs<- sapply(1:nrow(predProbs),function(i){names(which.max(predProbs[i,]))})
   accTests[[j]]<-data.frame(Id=dataTrain$Id[sampleIndices],actual=dataTrain$winPlacePerc[sampleIndices],
                   predicted=predictions)
   meanErr[j]<- with(accTests[[j]],mean(abs(actual-predicted)))
}

mean(meanErr)
acc10000<-list(single=accTests,mean=meanErr)
# save(acc10000,file=file.path("working_data",paste0("acc10thou_",modelFile,".Rdata")))
load(file=file.path("working_data",paste0("acc10thou_",modelFile,".Rdata")))


resOut<-data.frame()

for (i in seq_len(50)){   
   resOut<-rbind(resOut,acc10000[[1]][[i]])
}
   
resOut<-resOut %>% mutate(err=actual-predicted)

write.csv(resOut,file=file.path("working_data",paste0("resOut_",modelFile,".csv")))

gg_errOut<-ggplot(data=resOut)+ylab("% of majority")+
   theme(plot.title = element_text( hjust = 0.5))+ xlab("|actual - predicted|")+
   # geom_point(aes(x=1:nrow(res),y=err,color=err_label))
   geom_histogram(aes(x=abs(err),stat(100*ncount)),binwidth = 0.01)+
   geom_vline(xintercept =mean(abs(resOut$err)),color="blue")
annotate_figure(gg_errOut, top = "Out-sample error",
                fig.lab = "Figure 9", fig.lab.pos = "bottom")


acc10000
accTests[[1]][1:10,]
sum(accTests[[1]]$actual==1)
sum(accTests[[1]]$predicted==1)

######## 

modelFiles<-c("glmLog_model_1mil_uncorr","glmLog_model_1mil_imp","glmLog_model_55thou")
# save(fit,file = file.path("working_data", paste0(modelFile,".Rdata")))
glmModels<-list()
for (i in seq_along(modelFiles)){
   load(file = file.path("working_data",paste0(modelFiles[i],".Rdata")))
   glmModels[[i]]<-fit
}

comp<-anova(glmModels[[1]],glmModels[[2]]);comp
summary(comp)

############################ TEST #################################
dataTest<-read.csv(file=file.path(dataDir,"test_V2.csv"),header = TRUE,
                   stringsAsFactors =F,nrows = 10) 
varNames<-names(dataTest)


modelFiles<-c("glmLog_model_1mil_uncorr","glmLog_model_1mil_imp","glmLog_model_55thou")
# save(fit,file = file.path("working_data", paste0(modelFile,".Rdata")))
i<-1
load(file = file.path("working_data",paste0(modelFiles[i],".Rdata")))
starts<-seq(from=0,to=1900000,by=100000)
for (j in starts){
   # j<-starts[2]
   dataTest<-read.csv(file=file.path(dataDir,"test_V2.csv"),col.names =varNames,
                      stringsAsFactors =F,skip = j,nrows = 100000) 
   predictions<-predict(fit,newdata = dataTest,type = "response") 
   testPred<-data.frame(Id=dataTest$Id,winPlacePerc=predictions)
   write.csv(testPred,(file = file.path("working_data",paste0("testPred_",j,".csv"))))
   rm(dataTest,predictions)
}

rm(fit)
dataOut<-data.frame()
for (j in starts){
   temp<-read.csv(file = file.path("working_data",paste0("testPred_",j,".csv")))
   dataOut<-rbind(dataOut,temp)
   rm(temp)
}

dataOut<-dataOut %>% select(-X)
# sum(as.character(dataOut$Id[1900001:1934174])==as.character(testPred$Id))
write.csv(dataOut,file.path("working_data","data_output.csv"))

