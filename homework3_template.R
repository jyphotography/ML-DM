#######################################################
#Homework 3
#Name:  
#andrew ID:
#email:
#######################################################

#Problem 1

#####################################
#code to download yahoo finance data#
####################################
# source('yahoo_finance_data_loading.R')
# ticker.list <- read.csv('SP500_ticker.csv',header=TRUE)[,1]
# first.day <- "2010-01-01"
# last.day <-"2014-02-14"
# 
# 
# 
# download.data <- NULL
# for (ticker in ticker.list){
#     print(ticker)
#     dataset <- NULL
#     try(dataset<- data.loading(ticker, first.day, last.day)$close)
#     download.data <-cbind(download.data, dataset)
# }
# 
# date <- row.names(download.data)
# download.data <- data.frame(date=date,download.data)
# write.table(download.data,'SP500_close_price_raw.csv',quote=FALSE,sep=",",row.names=FALSE)
# 
# #remove stocks with more than na.thresh missing values
# na.thresh <- 60
# stay.col <- colnames(download.data)[which(colSums(1*(is.na(download.data)))<na.thresh)]
# download.data2 <- download.data[,stay.col]
# write.csv(download.data2, 'SP500_close_price.csv')

#reference for zoo package
#http://cran.r-project.org/web/packages/zoo/vignettes/zoo-quickref.pdf
library(zoo)
library(plyr)


import.csv <- function(filename){
  return(read.csv(filename,sep="," ,header=TRUE))
}

write.csv <- function(ob,filename){
  write.table(ob, filename, quote=FALSE, sep=",", row.names=FALSE)
}

##############################
#prepare data for PCA analysis
##############################
mydata <-import.csv('SP500_close_price.csv')
date <- as.Date(as.character(mydata[, 1]), format="%m/%d/%Y")
myzoo <- zoo(mydata[,-1], date )
myzoo <- na.locf(myzoo) #impute missing values
prices2returns <- function(x) 100*diff(log(x)) #function to covert from price to return
log.return.zoo <- prices2returns(myzoo)
log.return.data <- coredata(log.return.zoo) #data
log.return.date <- time(log.return.zoo) #date
