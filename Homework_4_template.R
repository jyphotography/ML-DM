library(arules)
library(rpart)
library(ROCR)
library(party)

import.csv <- function(filename) {
  return(read.csv(filename, sep = ",", header = TRUE))
}

write.csv <- function(ob, filename) {
  write.table(ob, filename, quote = FALSE, sep = ",", row.names = FALSE)
}

my.data <- import.csv('cars.csv')

#######################################################
# Problem 1
# Entropy, Information gain, Feature selection
#######################################################

# Compute entropy for a vector of symbolic values
# Inputs: Vector of symbolic values
# Output: Entropy (in bits)
entropy <- function(x) {
 #@@@@@@@@ Your function goes here @@@@@@@@@@@@@@@@
}

# Unit test for function entropy
x <- c(rep('A', 3),rep('B', 2),rep('C', 5))
print(ifelse(abs(entropy(x) - 1.485475 ) < 1.0e-05, 'entropy function has passed this test!', 'entropy function has failed this test'))

# Compute information gain IG(x,y)
# Inputs: x, y: vectors of symbolic values of equal length
# Output: information gain IG(x,y)
info.gain <- function(x,y){
	#@@@@@@@@@@@@@@@Your function goes here@@@@@@@@
  }

# Unit test for function info.gain
x <- c(rep('A',3),rep('B',2),rep('C',5))
y <- c(rep('X',4),rep('Y',6))
print(ifelse(abs(info.gain(x,y) - 0.7709506 ) < 1.0e-05, 'Info.gain function has passed this test!', 'info.gain function has failed this test'))

# Information-gain-based feature selection: exhaustive search
# Input: df is a data frame with last column being the output attribute
#        m: size of feature set, default is 1
# Output: data frame with name(s) of selected feature(s), information gain, relative information gain, sorted by the value of information gain
features <- function(df, m = 1){
  nf <- ncol(df) -1 # number of input features
  idx <- 1: nf  # column indices of input features
  output <- df[, ncol(df)]  # output column
  outputH <- entropy(output) # entropy for output
  idx.list <- combn(idx, m) # matrix storing all combinations of size m from idx
  IG.res <-NULL # output data frame
  # iterate through all combinations of index 
  for (ii in 1:ncol(idx.list)){
    this.idx <- idx.list[, ii]  
    input.df <- data.frame(df[,this.idx]) 
    # create a vector where each element is a concatenation of all values of a row of a data frame
    this.input <- apply(input.df, 1, paste, collapse='') 
    # create a new feature name which is a concatenation of feature names in a feature set
    this.input.names <- paste(names(df)[this.idx], collapse=' ')    
    this.IG <-info.gain(this.input,output) # information gain
    this.RIG <- this.IG / outputH # relative information gain
    this.res <- data.frame(feature = this.input.names, IG = this.IG, RIG = this.RIG) #assemble a df
    IG.res <- rbind(IG.res, this.res) # concatenate the results    
  }
  sorted <- IG.res[order(-IG.res$IG), ] # sort the result by information gain in a descending order
  return (sorted)
}

##################
# Problem 2
# Association Rules
#################

# Inputs: rules is the object returned by the apriori function
#         df is a data frame from which the rules are learned
# Output: a rule object with extra metrics (95% CI of score)
expand_rule_metrics <- function(rules,df){
  rule.df <- interestMeasure(rules, c('support', 'confidence'), df) # extract metrics into a data frame
  nn <- nrow(df)
  ci.low <-  -1  #@@@@@@@@@@@@@@ your code for computing ci.low goes here @@@@@@@@@@@@@@
  ci.high <- -1  ##@@@@@@@@@@@@@ your code for computing ci.high goes here @@@@@@@@@@@@@@
  quality(rules) <-cbind(quality(rules), ci.low, ci.high) # update the quality slot of the rules object
  return(rules)
}
