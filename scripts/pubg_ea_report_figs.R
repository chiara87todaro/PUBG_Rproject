#### report trys
rm(list = ls())

mainPath<-file.path(Sys.getenv("HOME"),"kaggle","PUBG_Rproject", "scripts")
# mainPath<-file.path(Sys.getenv("R_USER"), "PUBG_Rproject", "scripts")
setwd(mainPath)#"~/kaggle/PUBG_R/PUBG_R/"
dataDir<-file.path(getwd(), "data");

# install external libraries
library(dplyr);library(reshape2)
library(ggplot2);library(ggpubr) # graphics
# library(cowplot);library(gridExtra);

########################
# match stats
dataMatch<-read.csv(file=file.path("working_data","dataMatch.csv"))
summMatch<-dataMatch %>% summarise(AVduration=mean(duration),SDduration=sd(duration),
                                   AVnGroups=mean(nGroups), SDnGroups=sd(nGroups),
                                   AVmaxPlace=mean(maxPlace),SDmaxPlace=sd(maxPlace),
                                   AVnPlayers=mean(nPlayers), SDnPlayers=sd(nPlayers),
                                   AVnWinners=mean(nWinners,na.rm = T),SDnWinners=sd(nWinners,na.rm = T))

gm1<-ggplot(data=dataMatch,aes(y=duration,fill=type))+geom_boxplot()+ylab("duration[s]")+
   theme(axis.title.x=element_blank(), axis.text.x=element_blank(),axis.ticks.x=element_blank())
gm2<-ggplot(data=dataMatch,aes(y=nPlayers,fill=type))+geom_boxplot()+ylab("N players")+
   theme(axis.title.x=element_blank(), axis.text.x=element_blank(),axis.ticks.x=element_blank())
gm3<-ggplot(data=dataMatch,aes(y=nGroups,fill=type))+geom_boxplot()+ylab("N groups")+
   theme(axis.title.x=element_blank(), axis.text.x=element_blank(),axis.ticks.x=element_blank())
gm4<-ggplot(data=dataMatch,aes(y=maxPlace,fill=type))+geom_boxplot()+ylab("worst placement")+
   theme(axis.title.x=element_blank(), axis.text.x=element_blank(),axis.ticks.x=element_blank())
gm5<-ggplot(data=dataMatch,aes(y=nWinners,fill=type))+geom_boxplot()+ylab("N winners")+
   theme(axis.title.x=element_blank(), axis.text.x=element_blank(),axis.ticks.x=element_blank())

fig1<-ggarrange(gm1,gm2,gm3,gm4,gm5,nrow=5,common.legend=TRUE,legend="right")
annotate_figure(fig1, top = "Match statistics", fig.lab = "Figure 1", fig.lab.pos = "bottom")



##########
dataTrainSam<-read.csv(file.path(dataDir,"train_V2.csv"),stringsAsFactors =F,nrows=10)
dataTrain<-read.csv(file.path(dataDir,"train_V2.csv"),stringsAsFactors =F,
                    col.names = names(dataTrainSam),colClasses = sapply(dataTrainSam,class))

names(dataTrainSam)
# 
histPlayerS<-dataTrain[complete.cases(dataTrain),] %>%
select(-Id,-groupId,-matchId,-matchDuration,-matchType,-maxPlace,-rankPoints)
playerVariables<-names(histPlayerS)
# 
cutpoints<-c(0,0.2,0.9,0.999999999,1.1);lab=c("tail","heart","head","top")
scores<-cut(histPlayerS$winPlacePerc,breaks=cutpoints,labels =lab,right = F ) #[a,b)
dataPlayerS<-histPlayerS%>% mutate(score=scores)
# rm(dataPlayerS)

write.csv(dataPlayerS,file=file.path("working_data","dataPlayerS_4class.csv"))
dataPlayerS<-read.csv(file=file.path("working_data","dataPlayerS.csv"),stringsAsFactors = F)
# dataPlayerS<-dataPlayerS %>% select(-X)

# playerVariables<-c("assists","boosts","heals","revives",
#                    "headshotKills","killPlace","killPoints","kills",
#                    "teamKills","killStreaks","longestKill", "roadKills",
#                    "rideDistance","swimDistance","walkDistance",
#                    "vehicleDestroys" ,"weaponsAcquired","damageDealt","DBNOs",
#                    "winPoints") #"maxPlace",

playerVariables<-c("walkDistance","weaponsAcquired", #"assists",,"heals"
                   "damageDealt","killPoints",#"killPlace",,"kills"
                   "longestKill","winPoints")
# names(dataPlayerS)

g_glob<-list()

for(i in seq_along(playerVariables)){#
   # i<-2
   rangeVar1<-range(dataPlayerS[,playerVariables[i]])
   g_glob[[i]]<-ggplot(data = dataPlayerS,
                       aes(x=dataPlayerS[,playerVariables[i]],fill=score,stat(ncount))) +
                       # aes(x=playerVariables[i],fill=score,stat="ncount")) +
      geom_histogram(binwidth=(rangeVar1[2]-rangeVar1[1])/15,position = "dodge") +
      xlab(playerVariables[i])+ ylab("% of majority") #coord_cartesian(xlim = rangeVar1)
   # png(filename = file.path(Sys.getenv("HOME"),"kaggle","PUBG_Rproject","images",
   #                          paste0(playerVariables[i],"_hist_perScore.png")))
   # print(g_glob[[i]])
   # dev.off()
}

fig2<-ggarrange(g_glob[[1]],g_glob[[2]],g_glob[[3]],#g_glob[[4]],g_glob[[5]],
                nrow=3,ncol=1,common.legend=TRUE,legend="right")
annotate_figure(fig2, top = "Player statistic histomgrams 1", fig.lab = "Figure 2",
                fig.lab.pos = "bottom")

save(fig2,file = file.path("working_data","group1_hist_perScore.Rdata"))
rm(fig2)

fig3<-ggarrange(g_glob[[4]],g_glob[[5]],g_glob[[6]],
   #g_glob[[6]],g_glob[[7]],g_glob[[8]],g_glob[[9]],g_glob[[10]],
                nrow=3,ncol=1,common.legend=TRUE,legend="right")
annotate_figure(fig3, top = "Player statistic histomgrams 2", fig.lab = "Figure 3",
                fig.lab.pos = "bottom")
save(fig3,file = file.path("working_data","group2_hist_perScore.Rdata"))
rm(fig3)


fig2<-ggarrange(g_glob[[1]],g_glob[[2]],g_glob[[3]],g_glob[[4]],
                nrow=2,ncol=2,common.legend=TRUE,legend="right")
# png(filename = file.path(Sys.getenv("HOME"),"kaggle","PUBG_Rproject","images",
                         # "group1_hist_perScore.png"))
annotate_figure(fig2, top = "Player statistics 1", fig.lab = "Figure 2", fig.lab.pos = "bottom")
# dev.off()
rm(fig2)

fig3<-ggarrange(g_glob[[5]], g_glob[[6]],g_glob[[7]],g_glob[[8]],
                nrow=2,ncol=2,common.legend=TRUE,legend="right")
# png(filename = file.path(Sys.getenv("HOME"),"kaggle","PUBG_Rproject","images",
#                          "group2_hist_perScore.png"))
annotate_figure(fig3, top = "Player statistics 2", fig.lab = "Figure 3", fig.lab.pos = "bottom")
# dev.off()
rm(fig3)
fig4<-ggarrange( g_glob[[9]],g_glob[[10]],g_glob[[11]],g_glob[[12]],
                nrow=2,ncol=2,common.legend=TRUE,legend="right")
png(filename = file.path(Sys.getenv("HOME"),"kaggle","PUBG_Rproject","images",
                         "group3_hist_perScore.png"))
annotate_figure(fig4, top = "Player statistics 3", fig.lab = "Figure 4", fig.lab.pos = "bottom")
dev.off()
rm(fig4)
fig5<-ggarrange( g_glob[[13]],g_glob[[14]],g_glob[[15]],
                 nrow=2,ncol=2,common.legend=TRUE,legend="right")
png(filename = file.path(Sys.getenv("HOME"),"kaggle","PUBG_Rproject","images",
                         "group4_hist_perScore.png"))
annotate_figure(fig5, top = "Player statistics 4", fig.lab = "Figure 5", fig.lab.pos = "bottom")
dev.off()
rm(fig5)

fig6<-ggarrange( g_glob[[16]],g_glob[[17]],g_glob[[18]],g_glob[[19]],
                 nrow=2,ncol=2,common.legend=TRUE,legend="right")
png(filename = file.path(Sys.getenv("HOME"),"kaggle","PUBG_Rproject","images",
                         "group5_hist_perScore.png"))
annotate_figure(fig6, top = "Player statistics 5", fig.lab = "Figure 6", fig.lab.pos = "bottom")
dev.off()
rm(fig6)
fig7<-ggarrange( g_glob[[20]],g_glob[[21]],
                 nrow=1,ncol=2,common.legend=TRUE,legend="right")
png(filename = file.path(Sys.getenv("HOME"),"kaggle","PUBG_Rproject","images",
                         "group6_hist_perScore.png"))
annotate_figure(fig7, top = "Player statistics 6", fig.lab = "Figure 7", fig.lab.pos = "bottom")
dev.off()
rm(fig7)



#### Correlation between variables

# Correlation between variables divided per score
dataPlayerS<-read.csv(file=file.path("working_data","dataPlayerS.csv"),
                      stringsAsFactors = F)

playerVariables<-c("assists","boosts","heals","revives",
                   "headshotKills","killPlace","killPoints","kills",
                   "teamKills","killStreaks","longestKill", "roadKills",
                   "rideDistance","swimDistance","walkDistance",
                   "vehicleDestroys" ,"weaponsAcquired","damageDealt","DBNOs",
                   "winPoints") #"maxPlace",



score_labels<-c("top","head","heart","tail")
tag_pos<-c("top","top","bottom","bottom")
g_corrVar<-list()
for(i in seq_along(score_labels)){
   players_x<-dataPlayerS[dataPlayerS$score==score_labels[i],]
   CCvariables<-cor(players_x[,playerVariables])
   CCvar_df<-melt(CCvariables)
   
   g_corrVar[[i]] <- ggplot(CCvar_df,aes(x=Var1,y=Var2, fill=value)) + geom_tile() + labs(tag=score_labels[i])+
      scale_fill_gradient(low = "cornsilk2", high = "darkred") +  xlab("")+ylab("") +
      theme(axis.text.x =element_text(angle = 90, hjust = 1,size = 10), # element_blank(),#
            axis.text.y = element_text(size = 10), #element_blank(),#
            plot.tag.position="top") #tag_pos[i]
   # print(CCvar_df[CCvar_df$value> (0.5),])
  
}
# g_corrVar[[1]] <-g_corrVar[[1]] + theme(axis.text.y = element_text(hjust = 1,size = 10))
# g_corrVar[[4]] <-g_corrVar[[4]] + theme(axis.text.x = element_text(angle = 90, hjust = 1,size = 10))

fig8<-ggarrange(g_corrVar[[1]],g_corrVar[[2]],g_corrVar[[3]],g_corrVar[[4]],
                nrow=2,ncol=2,common.legend=TRUE,legend="right")
# png(filename = file.path(Sys.getenv("HOME"),"kaggle","PUBG_Rproject","images",
#                          "corrVar_perScore_all.png"))
save(fig8,file = file.path("working_data","corrVar_perScore_all.Rdata"))
# draw_plot(fig8, x = 0, y = 0, width = 1, height = 1)
annotate_figure(fig8, top = "Pair-wise correlation", fig.lab = "Figure 8", fig.lab.pos = "bottom")
                # bottom = text_grob("assists", rot=90,hjust = 1,x = 0.5, size = 10)
# dev.off()

### T-test 

# Are significantly different?
# score_labels<-c("head","heart","tail")
# # tag_pos<-c("top","top","bottom","bottom")
# players_top<-dataPlayerS[dataPlayerS$score=="top",]
# ttest_top<-list()
# 
# for(i in seq_along(score_labels)){
#  
#    players_x<-dataPlayerS[dataPlayerS$score==score_labels[i],]
#    set.seed(1803+i)
#    randomSel<-runif(length(players_top),min=0,max=length(players_top))
#    
#    ttest_top[[i]]<-sapply(playerVariables,function(var){
#       res<-t.test(players_top[,var],players_x[randomSel,var],alternative="greater");
#       return(c(res$statistic,res$p.value))
#    })
# }
#4446966
starts<-seq(from=0,to=4000000,by=500000)
dataPlayerS1<-read.csv(file=file.path("working_data","dataPlayerS_4class.csv"),
                      stringsAsFactors = F,nrows = 10)
varNames<-names(dataPlayerS1)
dataPlayerS<-data.frame()
for (j in starts){
    j<-starts[9]
   dataPlayerS1<-read.csv(file=file.path("working_data","dataPlayerS_4class.csv"),
                          skip=j,stringsAsFactors = F,nrows = 500000,col.names = varNames)
   dataPlayerS<-rbind(dataPlayerS,dataPlayerS1)
   rm(dataPlayerS1)
}
# dataPlayerS1<-read.csv(file=file.path("working_data","dataPlayerS_4class.csv"),
#                       stringsAsFactors = F,nrows = 10)
# dataPlayerS<-read.csv(file=file.path("working_data","dataPlayerS_4class.csv"),
#                       stringsAsFactors = F,col.names = names(dataPlayerS1))
playerVariables<-c("assists","boosts","heals","revives",
                   "headshotKills","killPlace","killPoints","kills",
                   "teamKills","killStreaks","longestKill", "roadKills",
                   "rideDistance","swimDistance","walkDistance",
                   "vehicleDestroys" ,"weaponsAcquired","damageDealt","DBNOs",
                   "winPoints") #"maxPlace", is reltive to the match not the ,"winPlacePerc"
signPval<-0.05
score_labels<-c("head","heart","tail")
# tag_pos<-c("top","top","bottom","bottom")
players_top<-dataPlayerS[dataPlayerS$score=="top",]
ttest_top<-list()

for(i in seq_along(score_labels)){
   i<-3
   players_x<-dataPlayerS[dataPlayerS$score==score_labels[i],]
   set.seed(1032+i)
   randomSel<-runif(nrow(players_top),min=0,max=nrow(players_x))
   res<-data.frame(statistic=numeric(),p.value=numeric())
   for (j in playerVariables){
      # j<-2
      aux<-t.test(players_top[,j],players_x[randomSel,j],alternative="greater");
      res[j,"statistic"]<-aux$statistic
      res[j,"p.value"]<-aux$p.value
   }
   ttest_top[[i]]<-res
   # ttest_top[[i]]<-sapply(playerVariables,function(variab){
   #    # variab<-playerVariables[1]
   #    res<-t.test(players_top[,variab],
   #                players_x[randomSel,variab],
   #                alternative="greater");
   #    # return(c(res$statistic,res$p.value))
   # })
}

rm(dataPlayerS)



# Create table top -other scores pvalues
pvalVar<-data.frame(variable=rep(playerVariables,times=3),
                    score=rep(c("top-head","top-heart","top-tail"),each=length(playerVariables)),
                    tstat=c(ttest_top[[1]][1,1],ttest_top[[2]][1,1],ttest_top[[3]][1,1]),
                    pvalues=c(ttest_top[[1]][2,2],ttest_top[[2]][2,2],ttest_top[[3]][2,2]))

write.csv(pvalVar,file=file.path("working_data","pvalVariables_4classes.csv"))
pvalVar<-pvalVar %>% mutate(significant=ifelse(pvalues<=0.05,"*","."))
# g_pval<-ggplot(pvalVar,aes(x=variable,y=pvalues,group=score))+geom_line(aes(x=variable,y=pvalues,color=score),size=1.5,linejoin="round")+
#   geom_hline(aes(yintercept=0.05))+ theme(axis.text.x = element_text(angle = 90, hjust = 1))+
#   geom_point(aes(shape=significant),size=2)+scale_shape_manual(values=c(8, 16))
g_pval<-ggplot(pvalVar,aes(x=variable,y=tstat,group=score))+
   geom_line(aes(x=variable,y=tstat,color=score),size=1)+ #,linejoin="round"
   theme(axis.text.x = element_text(angle = 90, hjust = 1))+ylab("difference in average")+  xlab("")+
   geom_point(aes(shape=significant,color=score),size=2.5)+scale_shape_manual(values=c(16,8))
g_pval


################### MACHINE LEARNING

dataTarget<-read.csv(file = file.path("working_data","dataTarget.csv"),stringsAsFactors =F)
cutpoints<-seq(from=0,to=1.1,by=0.1);
lab=c("r0_10","r10_20","r20_30","r30_40","r40_50","r50_60","r60_70","r70_80","r80_90","r90_99","r100")

target_labels<-data.frame(score=factor(dataTarget$score,levels = lab))
write.csv(target_labels,file=file.path("working_data","target_labels.csv"))
rm(dataTarget)
targHist<-ggplot(data = xx, aes(x=score,fill=..count..))+ geom_bar(stat="count")
# gg <- ggplot(data, aes(animals)) + 
#    geom_histogram(aes(fill = as.factor(healthy)), stat = "count")
# gg + geom_text(aes(y = y, label = count), data = tabDatY)
#+ ,label=..count.. ,geom="text" stat_bin() #, vjust = -1
rm(targHist)
# annotate_figure(targHist, top = "Target class distributions", 
#                 fig.lab = "Figure 5", fig.lab.pos = "bottom")
# # table(dataTarget$score)
# 



##### importance
load(file = file.path("working_data","smallBalanced_importance_treebag.Rdata"))
#importanceStats$variables
impP<-ggplot(data=importanceStats,aes(x=variables,y=importance))+
   scale_x_discrete(labels=c("assists","boosts","damageDealt","DBNOs","headshotKills",
                             "heals","killPlace", "killPoints","kills","killStreaks",
                             "longestKill","matchDuration","maxPlace","revives" , "rideDistance","roadKills","swimDistance","teamKills" ,"vehicleDestroys", "walkDistance",    "weaponsAcquired" ,"winPoints"))+  #as.character(importanceStats$variables)
   geom_linerange(aes(x=1:22,ymin=0,ymax=importance)) #+coord_flip()

importanceStats<-importanceStats %>% mutate(importance=sort(importance,decreasing = F))
impP<- ggplot(data=importanceStats,aes(x=variables,y=importance))+geom_bar(stat = "identity")+
   scale_x_discrete(labels=importanceStats$variables)+coord_flip()
   # geom_linerange(aes(x=1:22,ymin=0,ymax=importance)) #
# theme(axis.text.y = element_text( angle = 90, hjust = 1))+
annotate_figure(impP, top = "Importance of the statistics", 
                fig.lab = "Figure 6", fig.lab.pos = "bottom")

# load(file = file.path("working_data","smallBalanced_importance_treebag.Rdata"))
# #importanceStats$variables
# impP<-ggplot(data=importanceStats,aes(x=variables,y=importance))+
#    scale_x_discrete(labels=c("assists","boosts","damageDealt","DBNOs","headshotKills",
#                              "heals","killPlace", "killPoints","kills","killStreaks",
#                              "longestKill","matchDuration","maxPlace","revives" , "rideDistance","roadKills","swimDistance","teamKills" ,"vehicleDestroys", "walkDistance",    "weaponsAcquired" ,"winPoints"))+  #as.character(importanceStats$variables)
#    geom_linerange(aes(x=1:22,ymin=0,ymax=importance)) #+coord_flip()
# # theme(axis.text.y = element_text( angle = 90, hjust = 1))+
# annotate_figure(impP, top = "Importance of the statistics", 
#                 fig.lab = "Figure 6", fig.lab.pos = "bottom")

#### GLM

modelFiles<-c("glmLog_model_1mil_uncorr","glmLog_model_1mil_imp","glmLog_model_55thou")
i<-1
load(file = file.path("working_data",paste0(modelFiles[i],".Rdata")))

residFit<-data.frame(fitted.values=fit$fitted.values,residuals=fit$residuals)
residFit<-residFit[complete.cases(residFit),]
rm(fit)
residP<-ggplot(data=residFit,aes(x=fitted.values,y=residuals))+
   geom_point(alpha=0.5,color="grey")+ylim(c(-5,5))+
   geom_hline(yintercept = 0,color="black")+
   geom_hline(yintercept = mean(residFit$residuals),color="red")
annotate_figure(residP, top = "GLM logistic diagnostic",
                fig.lab = "Figure 6", fig.lab.pos = "bottom")

save(residP,file = file.path("working_data","GLM_logistic_diagnostic.Rdata"))


#### plot coeff

modelFiles<-c("glmLog_model_1mil_uncorr","glmLog_model_1mil_imp","glmLog_model_55thou")
i<-1
load(file = file.path("working_data",paste0(modelFiles[i],".Rdata")))

CI<-confint(fit, rownames(summary(fit)$coefficients), level=0.95);
# CIaux<-CI
# CI1<-c(1.820612e-01,2.003895e-01)
# CI<-rbind(CI1,CIaux)
# rownames(CI)<-rownames(summary(fit)$coefficients)

Mcoef<-summary(fit)$coefficients[1:nrow(summary(fit)$coefficients),];
rm(fit)
dfCoef<-data.frame(coeff=Mcoef[,1],covar=rownames(Mcoef),ci2.5=CI[,1],ci97.5=CI[,2]);
dfCoef<-dfCoef  %>% arrange(desc(coeff)) %>% mutate(cil=abs(ci97.5-ci2.5))

pCoef<-ggplot(data=dfCoef,aes(y=covar,x=coeff))+ coord_flip()+ guides(size=FALSE)+
   geom_point(aes(size=cil,fill=covar),shape=21)+geom_vline(xintercept =0,colour="red")+ 
   theme(plot.title = element_text(hjust = 0.5,size=15),axis.text.x =element_text(angle=90,size=10))+ #,legend.position = "none" ,axis.text.x =element_blank()
   theme(legend.position = "none")+
   # theme(axis.text.y=element_text(size=15),legend.key.size = unit(1, "cm"),
   #       legend.text = element_text(size = 15),legend.title = element_blank())+
   # theme(axis.title=element_text(size=15))+
   labs(title = "Average Marginal Effects with confidence intervals")+
   ylab("per unit change")+xlab("linear coefficient")

save(pCoef,file = file.path("working_data","GLM_logistic_coeff.Rdata"))
annotate_figure(pCoef, top = "logistic GLM coefficients",
                fig.lab = "Figure 6", fig.lab.pos = "bottom")
rm(CI,Mcoef,dfCoef,pCoef)
