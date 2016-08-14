#sahibdeep Singh
#andrew ID:sahibdes


options(scipen=999)
# You need to install package 'FNN'
library(FNN)

# utility function for import from csv file
import.csv <- function(filename) {
  return(read.csv(filename, sep = ",", header = TRUE))
}

# utility function for export to csv file
write.csv <- function(ob, filename) {
  write.table(ob, filename, quote = FALSE, sep = ",", row.names = FALSE)
}

# Connect-the-dots model that learns from train set and is being tested using test set
# Assumes the last column of data is the output dimension
get_pred_dots <- function(train,test){
  nf <- ncol(train)
  input <- train[,-nf]
  query <- test[,-nf]
  my.knn <- get.knnx(input,query,k=2) # Get two nearest neighbors
  nn.index <- my.knn$nn.index
  pred <- rep(NA,nrow(test))
  for (ii in 1:nrow(test)){
    y1 <- train[nn.index[ii,1],nf]
    y2 <- train[nn.index[ii,2],nf]
    pred[ii] = (y1+y2)/2
  }
  return(pred)  
}

# Linear model
# Assumes the last column of data is the output dimension
get_pred_lr <- function(train,test){
  nf <- ncol(train)
  input <- train[,-nf,drop=FALSE]
  query <- test[,-nf,drop=FALSE]
  pred<-lm(train[,nf]~.,data=input)
  result<-predict(pred,query)
  return(result)
}

# Default predictor model
# Assumes the last column of data is the output dimension
get_pred_default <- function(train,test){
  # Your implementation goes here
  nf <- ncol(train)
  input <- train[,-nf]
  query <- test[,-nf]
  ybar<-array()
  for(i in 1:nrow(test)){
    ybar[i] <-mean(train[,nf])
  }
  return(ybar)
}

#handler function to perform k-fold validation with dataset and prediction on all 3 models
do_cv<-function(df, output, k, model){
  if(!is.null(k))
  { 
    #move the output variable to last
    nf <- df[c(setdiff(names(df), output), output)]
    size<-round(floor(nrow(nf)/k)) 
    size1<-nrow(nf)-(size*k)
    for(i in 1:ncol(nf))
    {
      #remove factor variables
      if(class(nf[,i])=="factor"){
        nf1<- nf[,-i]
      }
    }
    arr<-array()
    l=1
    j=size
    for(i in 1:k){
      l=(i-1)*size+1
      j=i*size
      if(i==k)
      {
        j<-nrow(nf1)
      }
      #test set
      test.s <-nf1[l:j,]
      #training set
      training.s<- nf1[setdiff(rownames(nf1),rownames(test.s)),]
      #output vector from model
      Model_output<-model(training.s,test.s)
      #MSE
      MSE <-((Model_output-test.s[,ncol(nf1)])^2)
      arr[i]<-mean(MSE)
    }
    return(arr)
  }
}

file<-"house_no_missing.csv"
df<-import.csv(file)
do_cv(df,"house_value",10,get_pred_dots)

#------------------------------------
#2b
nf1<-cbind(log(df["Crime_Rate"]),df["house_value"])
v1<-do_cv(nf1,"house_value",nrow(nf1),get_pred_default)
m1<-mean(v1)
sd1<-sd(v1)
cf1low<-m1-(1.96*(sd1))/sqrt(nrow(nf1))
cf1high<-m1+(1.96*(sd1))/sqrt(nrow(nf1))
v2<-do_cv(nf1,"house_value",nrow(nf1),get_pred_dots)
m2<-mean(v2)
sd2<-sd(v2)
cf2low<-m2-(1.96*(sd2))/sqrt(nrow(nf1))
cf2high<-m2+(1.96*(sd2))/sqrt(nrow(nf1))
v3<-do_cv(nf1,"house_value",nrow(nf1),get_pred_lr)
m3<-mean(v3)
sd3<-sd(v3)
cf3low<-m3-(1.96*(sd3))/sqrt(nrow(nf1))
cf3high<-m3+(1.96*(sd3))/sqrt(nrow(nf1))
ci.low<-c(cf1low,cf2low,cf3low)
ci.upper<-c(cf1high,cf2high,cf3high)
h1<-c(m1,m2,m3)

LinerRegression<-get_pred_lr
DefaultModel<-get_pred_default
ConnectTheDots<-get_pred_dots

barplot2(height = h1, main = "Mean values of error along with 95% CI's",names.arg =  c("LinerRegression","DefaultModel","ConnectTheDots") , xlab = "Models",ylab = "Mean Values of Error",
         col = c("lavender", "lightcyan", "lightblue"),plot.ci = TRUE , ci.l = ci.low,ci.u = ci.upper)