#file import function
import.csv <- function(filename) {
  return(read.csv(filename, sep = ",", header = TRUE))
}

my.data <- import.csv('elementary.csv')

my.data[my.data == "?"] <- NA
my.data[my.data == ""] <- NA
#create a dataframe without missing values in output column
my.data<-my.data[complete.cases(my.data$API),]


#fit a model to tree
fit_rpart1 <- rpart(ACS_CORE ~.,method="anova", data=my.data)
#get the score
score1<-predict(fit_rpart1, my.data)

my.data$ACS_CORE<-as.numeric(my.data$ACS_CORE)

#IMPUTE MISSING ACS_CORE
for(i in 1:nrow(my.data)){
  if(is.na(my.data$ACS_CORE[i])){
  my.data$ACS_CORE[i]<- score1[i]
  }
}


#fit a model to tree
fit_rpart2 <- rpart(YR_RND ~.,method="anova", data=my.data)
#get the score
score2<-predict(fit_rpart2, my.data)

my.data$YR_RND<-as.numeric(my.data$YR_RND)

#IMPUTE MISSING ACS_CORE
for(i in 1:nrow(my.data)){
  if(is.na(my.data$YR_RND[i])){
    my.data$YR_RND[i]<- score2[i]
  }
}

#fit a model to tree
fit_rpart3 <- rpart(AVG_ED ~.,method="anova", data=my.data)
#get the score
score3<-predict(fit_rpart3, my.data)

my.data$AVG_ED<-as.numeric(my.data$AVG_ED)

#IMPUTE MISSING ACS_CORE
for(i in 1:nrow(my.data)){
  if(is.na(my.data$AVG_ED[i])){
    my.data$AVG_ED[i]<- score3[i]
  }
}

#fit a model to tree
fit_rpart4 <- rpart(CHARTER ~., data=my.data)
#get the score
score4<-predict(fit_rpart4, my.data,type="class")

my.data$CHARTER<-as.factor(my.data$CHARTER)

#IMPUTE MISSING ACS_CORE
for(i in 1:nrow(my.data)){
  if(is.na(my.data$CHARTER[i])){
    my.data$CHARTER[i]<- score4[i]
  }
}


get_pred_logreg<-function(train,test){
  nf <- ncol(train)
  my.model <- glm(train[,nf]~.,data=train[,-nf,drop=FALSE],family="binomial")
  pred <- predict(my.model,test[,-nf,drop=FALSE],type="response")
  return(cbind(pred,test[,nf,drop=FALSE]))
}


get_pred_svm<-function(train,test){
  nf <- ncol(train)
  my.model<-svm(as.factor(train[,nf])~.,data=train[,-nf,drop=FALSE],probability=TRUE)
  pred<-attr(predict(my.model,test[,-nf],probability = TRUE),'probabilities')[,'1']
  return(cbind(pred,test[,nf,drop=FALSE]))
}


do_cv_class<-function(df, k, model){
  if(!is.null(k))
  { 
    df<-df[sample(nrow(df)),]
    #convert last column from 'high/low' to '1/0'
    df[,ncol(df)]<- ifelse(df[,ncol(df)]=="High",1,0)
    df[,ncol(df)]<-  as.numeric(   df[,ncol(df)])
    df$VALID<-as.factor(df$VALID)
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
      Model_output<-get_pred_svm(training.s,test.s)
      Model_output1<-rbind(Model_output1,Model_output)
    }
    return(Model_output1)
  }
}

my.data$VALID<-as.numeric(my.data$VALID)
output_1<-do_cv_class(my.data,10,"logreg")
