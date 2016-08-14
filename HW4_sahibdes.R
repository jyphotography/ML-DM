
#file import function
import.csv <- function(filename) {
  return(read.csv(filename, sep = ",", header = TRUE))
}

#read file
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
  unique_values<-unique(x)
  entropy_val=0
  for(i in 1:length(unique_values)){
  prob<-length(x[x==unique_values[i]])/length(x)
  log<- (log2(prob))
  entropy_val<-entropy_val+(-prob*log)
}
 print(entropy_val)
}

# Unit test for function entropy
x <- c(rep('A', 3),rep('B', 2),rep('C', 5))
print(ifelse(abs(entropy(x) - 1.485475 ) < 1.0e-05, 'entropy function has passed this test!', 'entropy function has failed this test'))
entropy(my.data[,6])

#1b
# Compute information gain IG(x,y)
# Inputs: x, y: vectors of symbolic values of equal length
# Output: information gain IG(x,y)
info.gain <- function(x,y){
#@@@@@@@@@@@@@@@Your function goes here@@@@@@@@
  subset_xy<-data.frame(x,y)
  unique_y<-unique(y)
  total=0
  entropy_x<-entropy(x)
  for(i in 1:length(unique_y)){
    subset_xy1<- subset_xy[subset_xy[,2]==unique_y[i],]
    prob_y<-length(y[y==unique_y[i]])/length(y)
    entropy_y<-entropy(subset_xy1[,1])
    total=total+(prob_y*entropy_y)
  }
  IG=entropy_x-total
  return(IG)
}


# Unit test for function info.gain
x <- c(rep('A',3),rep('B',2),rep('C',5))
y <- c(rep('X',4),rep('Y',6))
print(ifelse(abs(info.gain(x,y) - 0.7709506 ) < 1.0e-05, 'Info.gain function has passed this test!', 'info.gain function has failed this test'))
#calculate information gain with price and class
info.gain(my.data[,5],my.data[,6])

#calculate the relative information gain
RIG<-(info.gain(my.data[,5],my.data[,6])/entropy(my.data[,6]))*100
print(RIG)

#1c
# Information-gain-based feature selection: exhaustive search
# Input: df is a data frame with last column being the output attribute
#        m: size of feature set, default is 1
# Output: data frame with name(s) of selected feature(s), information gain, relative information gain, sorted by the value of information gain
features <- function(df, m){
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

#set the output class to the last column
my.data<-my.data[c(setdiff(names(my.data), "class"),"class")]

#read features 
features(my.data,1)
features(my.data,2)
features(my.data,3)

df_1<-data.frame(features(my.data,1))
dframe_IG<-data.frame(features(my.data,1)[2])
dframe_RIG<-data.frame(features(my.data,1)[3])

#plots for information gain with feature set 1
barplot(dframe_IG$IG,names.arg=df_1$feature, col=c("light blue"),xlab="Features",main="Feature Set 1 With Information Gain")

#plots for relative information gain with feature set 1
barplot(dframe_RIG$RIG,names.arg=df_1$feature, col=c("light blue"),xlab="Features",main="Feature Set 1 With Relative Information Gain")
##################  
# Problem 2
# Association Rules
#################
rules.all <- apriori(my.data,parameter=list(minlen=2,support=0.1,confidence=0.5),
                     appearance=list(rhs=c("price=high"),default="lhs"))


#rules sorted by lift
rules.sorted <- sort(rules.all, by="lift")
inspect(rules.sorted)

#pruning redundant rules
subset.matrix <- is.subset(rules.sorted, rules.sorted)
subset.matrix <- is.subset(rules.sorted, rules.sorted)
subset.matrix[lower.tri(subset.matrix, diag=T)] <- NA

redundant <- colSums(subset.matrix, na.rm=T) >= 1
which(redundant)
rules.pruned <- rules.sorted[!redundant]
inspect(rules.pruned)

#sort rules by support
    rules.sorted_support <- sort(rules.all, by="support")
    inspect(rules.sorted_support[1:5])

#sort rules by confidence
rules.sorted_confidence <- sort(rules.all, by="confidence")
inspect(rules.sorted_confidence[1:5])

#2b
# Inputs: rules is the object returned by the apriori function
#         df is a data frame from which the rules are learned
# Output: a rule object with extra metrics (95% CI of score)
expand_rule_metrics <- function(rules,df){
  rule.df <- interestMeasure(rules, c('support', 'confidence'), df)# extract metrics into a data frame
  nn <- nrow(df)
  ci.low <- rule.df[,ncol(rule.df)]-1.96*(sqrt(rule.df[,ncol(rule.df)]*(1-rule.df[,ncol(rule.df)])))/sqrt(rule.df[,1]*nn)  #@@@@@@@@@@@@@@ your code for computing ci.low goes here @@@@@@@@@@@@@@
  ci.high <-rule.df[,ncol(rule.df)]+1.96*(sqrt(rule.df[,ncol(rule.df)]*(1-rule.df[,ncol(rule.df)])))/sqrt(rule.df[,1]*nn)  ##@@@@@@@@@@@@@ your code for computing ci.high goes here @@@@@@@@@@@@@@
  quality(rules) <-cbind(quality(rules), ci.low, ci.high) # update the quality slot of the rules object
  return(rules)
}

inspect(expand_rule_metrics(rules.all,my.data))

#2c
#get the top 5 
inspect(sort(expand_rule_metrics(rules.all,my.data),by="ci.low")[1])                          

#3a 
#fit a model to tree
fit_rpart <- rpart(price ~.,method="class", data=my.data)

#get the score
score<-predict(fit_rpart, my.data)
pred<-prediction(score[,1],ifelse(my.data[,5]=="high",1,0))
perform<-performance(pred,'auc')
#auc score
auc_score<-perform@y.values[[1]]
print(auc_score)

#plot auc 
perf_rtree<-performance(pred,"tpr","fpr")
plot(perf_rtree)

#plot the decision tree
plot(fit_rpart, uniform=TRUE,main="Classification Tree for Cars")
text(fit_rpart, use.n=TRUE, all=TRUE, cex=.8,pretty=TRUE)

#3b
#fit a model to tree
fit_ctree <- ctree(price ~., data=my.data)
pred_ctree<-predict(fit_ctree, my.data, type='prob', simply=FALSE)
tree_frame<-data.frame()
for(i in 1:length(pred_ctree)){
  tree_frame[i,1]<- pred_ctree[[i]][1]
}

tree_frame<-as.matrix(tree_frame)
prediction_ctree<-prediction(tree_frame[,1],ifelse(my.data[,5]=="high",1,0))
perform_ctree<-performance(prediction_ctree,'auc')
#auc score
perform_ctree@y.values[[1]]

#plot the auc
plot(performance(prediction_ctree,"tpr","fpr"))

#plot the decision tree
plot(fit_ctree, uniform=TRUE,main="Classification Tree for Cars")
