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
 # Your implementation goes here
 # You may leverage lm function available in R
}

# Default predictor model
# Assumes the last column of data is the output dimension
get_pred_default <- function(train,test){
 # Your implementation goes here
}