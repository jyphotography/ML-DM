#-----file import functions----------------------------

import.csv <- function(filename) {
  return(read.csv(filename, sep = ",", header = TRUE))
}

file<-"white_wine.csv"
df<-import.csv(file)
#-------------------------------------------------------
#-----------------------Q1------------------------------
#logistic regression classifier
get_pred_logreg<-function(train,test){
  nf <- ncol(train)
  my.model <- glm(train[,nf]~.,data=train[,-nf,drop=FALSE],family="binomial")
  pred <- predict(my.model,test[,-nf,drop=FALSE],type="response")
  return(cbind(pred,test[,nf,drop=FALSE]))
}

#support vector machine classifier
get_pred_svm<-function(train,test){
  nf <- ncol(train)
  my.model<-svm(as.factor(train[,nf])~.,data=train[,-nf,drop=FALSE],probability=TRUE)
  pred<-attr(predict(my.model,test[,-nf],probability = TRUE),'probabilities')[,'1']
  return(cbind(pred,test[,nf,drop=FALSE]))
}

#naive baye's classifier
get_pred_nb <- function(train,test){
  nf <- ncol(train)
  my.model <- naiveBayes(train[,nf]~.,data=train[,-nf,drop=FALSE])
  pred <- predict(my.model,test[,-nf],type='raw')
  pred<-pred[,ncol(pred)]
  return(cbind(pred,test[,nf,drop=FALSE]))
}

#k-nearest neighbor classifier
get_pred_knn<-function(train,test,k){
  nf <- ncol(train)
  my.model <- knn(train[,-nf],test[,-nf],k=k,train[,nf],prob=TRUE)
  prob <- attr(my.model,"prob")
  pred.bi <- my.model
  pred <- ifelse(pred.bi=='1',prob,1-prob)
  return(cbind(pred,test[,nf,drop=FALSE]))
}

#-------------------------------------------------------------
#---------------------------Q4b-------------------------------
#default classifier
get_pred_default <- function(df){
  # Your implementation goes here
  df[,ncol(df)]<- ifelse(df[,ncol(df)]=="high",1,0)
  def_bar <-mean(df[,ncol(df)])
  xmin=0
  xmax=0
  for(i in 1:nrow(df)){
    if(def_bar<0.5){
      if(df[i,ncol(df)]==0)
        xmin=xmin+1
    }else{
      if(df[i,ncol(df)]==1)
        xmax=xmax+1
    }
  }
  if(def_bar<0.5)
    accuracy<-(xmin*100)/nrow(df)
  else
    accuracy<-(xmax*100)/nrow(df)
  return(accuracy)
}
#----------------------------------
#----------------------------------Q2-----------------------------------------
#handler function to perform k-fold validation with datas do_cv and prediction on all 3 models
do_cv_class<-function(df, k, model){
  if(!is.null(k))
  { 
    df<-df[sample(nrow(df)),]
    #convert last column from 'high/low' to '1/0'
    df[,ncol(df)]<- ifelse(df[,ncol(df)]=="high",1,0)
    
    size<-round(floor(nrow(df)/k)) 
    
    Model_output1=NULL
    l=1
    j=size
    for(i in 1:k){
      l=(i-1)*size+1
      j=i*size
      if(i==k)
      {
        j<-nrow(df)
      }
      #test set
      test.s <-df[l:j,]  
      #training set
      training.s<- df[setdiff(rownames(df),rownames(test.s)),]
      #check model type
      if(grepl("nn",model)){
        sub_str<-strsplit(model,"nn")[[1]]
        Model_output<-get_pred_knn(training.s,test.s,as.numeric(sub_str))
        Model_output1<-rbind(Model_output1,Model_output)
      }
      else
        if(model=="logreg"){
          Model_output<-get_pred_logreg(training.s,test.s)
          Model_output1<-rbind(Model_output1,Model_output)
        }
      else
        if(model=="svm"){
          Model_output<-get_pred_svm(training.s,test.s)
          Model_output1<-rbind(Model_output1,Model_output)
        }
      else
        if(model=="nb"){
          Model_output<-get_pred_nb(training.s,test.s)
          Model_output1<-rbind(Model_output1,Model_output)
        }
    }
    return(Model_output1)
  }
}

output_1<-do_cv_class(df,10,"nb")
#-----------------------------------------------------------------------------
#----------------------------------Q3-----------------------------------------
get_metrics<-function(output_1,cutOff){
  predict_bin<-ifelse(output_1[,1]>=cutOff,1,0)
  output_1<-cbind(output_1,predict_bin)
  
  predict_tp<-ifelse((predict_bin==output_1[,2] & output_1[,2]==1),1,0)
  output_1<-cbind(output_1,predict_tp)
  
  predict_tn<-ifelse((predict_bin==output_1[,2] & output_1[,2]==0),1,0)
  output_1<-cbind(output_1,predict_tn)
  
  predict_fn<-ifelse((predict_bin!=output_1[,2] & output_1[,2]==1),1,0)
  output_1<-cbind(output_1,predict_fn)
  
  predict_fp<-ifelse((predict_bin!=output_1[,2] & output_1[,2]==0),1,0)
  output_1<-cbind(output_1,predict_fp)
  
  tp_rate<-(sum(output_1[,4])/(sum(output_1[,4])+sum(output_1[,6])))*100
  
  fp_rate<-(sum(output_1[,7])/(sum(output_1[,5])+sum(output_1[,7])))*100
  total<-sum(output_1[,4])+sum(output_1[,5])+sum(output_1[,6])+sum(output_1[,7])
  accuracy<-((sum(output_1[,4])+sum(output_1[,5]))/total)*100
  
  precision<-(sum(output_1[,4])/(sum(output_1[,4])+sum(output_1[,7])))*100
  
  recall<-tp_rate
  
  metrics<-data.frame(tp_rate,fp_rate,accuracy,precision,recall)
  return(metrics)
}

get_metrics(output_1,0.5)
#-----------------------------------------------------------------------------
#----------------------------------Q4a-----------------------------------------
K_Value<-array()
Accuracy<-array()
for(i in 1:100){
  K_Value[i]<-i
  output_2<- do_cv_class(df,10,paste(i,"nn"))
  metrics<- get_metrics(output_2,0.5)
  Accuracy[i]<-metrics[,3]      
}
plot(K_Value,Accuracy)
lines(K_Value,Accuracy)
#---------------------------------------------------------------------------
#--------------------4b---------------------------------------
get_pred_default(df)