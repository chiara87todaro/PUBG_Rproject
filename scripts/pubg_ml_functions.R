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
#####
# create model, plot error and calculate accuracy
pubg_ml_accuracy<-function(){
  
  fit<-train(winPlacePerc ~ .-1 -Id, method=methString[i], data = dataTrainSam)
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
  
  
}

