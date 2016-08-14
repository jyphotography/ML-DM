#Q1
#import function for file
import.csv <- function(filename){
  return(read.csv(filename,sep="," ,header=TRUE))
}

mydata <-import.csv('SP500_close_price.csv')
date <- as.Date(as.character(mydata[, 1]), format="%m/%d/%Y")
myzoo <- zoo(mydata[,-1], date)
myzoo <- na.locf(myzoo)
prices2returns <- function(x) 100*diff(log(x)) 
log.return.zoo <- prices2returns(myzoo)
log.return.data <- coredata(log.return.zoo)
log.return.date <- time(log.return.zoo) 
distribution_of_variance_in_PCA<-prcomp(log.return.data, retx = TRUE, center = TRUE, scale = FALSE)
distribution_of_variance_in_PCA$x
distribution_of_variance_in_PCA$sdev
distribution_of_variance_in_PCA$rotation
#plot for PCAs
screeplot(distribution_of_variance_in_PCA,type = 'lines')
#---------------------------------------------------
#Q2
#plot for & variance explained
plot(cumsum((distribution_of_variance_in_PCA$sdev^2/sum(distribution_of_variance_in_PCA$sdev^2))*100),type="l",xlab="nth component",ylab="%Variance Explained")
#--------------------
#Q3
eiganVal<-data.frame(cumsum(distribution_of_variance_in_PCA$sdev^2/sum(distribution_of_variance_in_PCA$sdev^2)))

#this will return minimum PCA to capture at least 80% of variance 
which(eiganVal[,1]*100>80)[1]

#---------------------------
#Q4
min<-min(distribution_of_variance_in_PCA$x[,1])
#reconstruction error
reconstructionError<-(1-eiganVal[2,1])*100
print(reconstructionError)
#----------------
#Qb
#Q1
plot(log.return.date,distribution_of_variance_in_PCA$x[,1],'lines',xlab="Year",ylab="")

#date for the minimum PCA1 variance
date<-log.return.date[which(distribution_of_variance_in_PCA$x[,1]==min(distribution_of_variance_in_PCA$x[,1]),distribution_of_variance_in_PCA$x)]
print(date)
#---------------
#Q2
Weights<-data.frame(distribution_of_variance_in_PCA$rotation[,1],distribution_of_variance_in_PCA$rotation[,2])
names(Weights)<-c("Weight_PCA1","Weight_PCA2")
print(Weights)
#-------------------
#Q3 & Q4
mydata_ticker <-import.csv('SP500_ticker.csv')

#reference from stackoverflow.com
trim.leading <- function (x)  sub("^\\s+", "", x)
trim.trailing <- function (x) sub("\\s+$", "", x)

mydata_ticker$sector<-trim.leading(mydata_ticker$sector)
mydata_ticker$sector<-trim.trailing(mydata_ticker$sector)

mydataframe_PC1<-data.frame(distribution_of_variance_in_PCA$rotation[,1])
mydataframe_PC2<-data.frame(distribution_of_variance_in_PCA$rotation[,2])

mydataframe_PC1<-cbind(mydataframe_PC1,row.names(mydataframe_PC1))
mydataframe_PC2<-cbind(mydataframe_PC2,row.names(mydataframe_PC2))

colnames(mydataframe_PC1)[2]<-"ticker" 
colnames(mydataframe_PC2)[2]<-"ticker" 

#join the data by tickers
mydata_sectorwise<-join(mydataframe_PC1, mydata_ticker, type ="inner",by="ticker")
mydata_sectorwise2<-join(mydataframe_PC2, mydata_ticker, type ="inner",by="ticker")

colnames(mydata_sectorwise)[1]<-"val" 
colnames(mydata_sectorwise2)[1]<-"val" 

#apply grouping by sector
grp<-ddply(mydata_sectorwise,~sector,summarise,mean=round(mean(val),4))
grp2<-ddply(mydata_sectorwise2,~sector,summarise,mean=round(mean(val),4))

#plotting the mean weights by sector
ggplot(data = grp, aes(x = sector, y = mean))+geom_bar(stat="identity")+xlab("Sector")+ylab("Mean")+ggtitle("Weights of 1st PC by industry sector")
ggplot(data = grp2, aes(x = sector, y = mean))+geom_bar(stat="identity")+xlab("Sector")+ylab("Mean")+ggtitle("Weights of 2nd PC by industry sector")
#------------
#Problem 2
#Qa
bmi.file<-import.csv('BMI.csv')

filter.features.by.cor<-function(df){
  nf<-df[,-ncol(df),drop=FALSE]
  Attribute<-colnames(nf)
  lr<-array()
  for(i in 1:ncol(nf))
  {
    lr[i]<-abs(cor(nf[,i],df[,ncol(df)]))
  }
  newVal<-data.frame()
  newVal<-cbind.data.frame(Attribute,lr)
  newVal<-newVal[order(-lr),]
  return(newVal)
}

filter.features.by.cor(bmi.file)
#----------------------
#Qb
leaps<-regsubsets(fatpctg~.,data=bmi.file,nbest=1,nvmax = 3)
summary(leaps)
#---------------
#Qc
fit <- lm(fatpctg~., data=bmi.file)
stepAIC(fit,direction =  "backward")
