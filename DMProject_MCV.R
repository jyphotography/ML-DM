options(scipen=999)
# utility function for import from csv file
import.csv <- function(filename) {
  return(read.csv(filename, sep = ",", header = TRUE))
} 

brief <- function(dframe){  
  # Print out how many rows and attributes the data set has 
  cat("This dataset has ", nrow(dframe), " Rows ", ncol(dframe), " Attributes\n\n")
  
  # Set up the data set for real and symbolic values to be later added with appropriate values and printed
  dfOfRealVal <- data.frame(Attribute_ID=numeric(0),Attribute_Name=character(0),Missing=numeric(0),Mean=numeric(0),Median=numeric(0),Sdev=numeric(0),Min=numeric(0),Max=numeric(0))
  dfOfSymbVal <- data.frame(Attribute_ID=numeric(0),Attribute_Name=character(0),Missing=numeric(0),arity=numeric(0),MCVs_Counts=character(0))
  
  # Go through each of the columns of the data
  for (i in 1:ncol(dframe) ) {
    # Get the column to be evaluated
    colEval <- dframe[,i]
    
    # See if column is a symbolic or real attribute and add appropriate values to the specific dfOfRealVal or dfOfSybVal dataframe
    if(!is.factor(colEval)) {
      colID <- i
      nameOfCol <- colnames(dframe)[i]
      missingNoOfCol <- length(colEval[colEval==""|is.na(colEval)])
      meanOfCol <- format(round(mean(colEval,na.rm=TRUE),digits=2),nsmall=2)
      medianOfCol <- format(round(median(colEval,na.rm=TRUE),digits=2),nsmall=2)
      stdDevOfCol <- format(round(sd(colEval,na.rm=TRUE),digits=2),nsmall=2)
      maxOfCol <- format(round(max(colEval,na.rm=TRUE),digits=2),nsmall=2)
      minOfCol <- format(round(min(colEval,na.rm=TRUE),digits=2),nsmall=2)
      
      # Add the new row of real attribute to the dfOfRealVal dataframe
      dfOfRealVal <-   rbind(dfOfRealVal,data.frame(Attribute_ID=colID,Attribute_Name=nameOfCol,Missing=missingNoOfCol,Mean=meanOfCol,Median=medianOfCol,Sdev=stdDevOfCol,Min=minOfCol,Max=maxOfCol))
    }
    else {
      colID <- i
      nameOfCol <- colnames(dframe)[i]
      missingNoOfCol <- length(colEval[colEval=="?"])
      colWithNoBlank <- colEval[!(colEval=="?")]
      arityOfCol <- length(unique(colWithNoBlank))
      mcvCountOfColDF <- as.data.frame(table(colWithNoBlank))
      
      # Order the frequency which is in column 2 of mcvCountOfColDF
      mcvCountOfColDF <- mcvCountOfColDF[order(-mcvCountOfColDF[,2]),]
      
      # Output to add to the MCVs_Counts column of dfOfSymbVal dataframe
      mcvCountOfCol <- ""
      
      # Show only the top three of MCVs_Count unique values
      itrOfMCVCountOfColDF = nrow(mcvCountOfColDF)
      
      # Make a string of the values that are in the mcv count and its frequency
      for(j in 1:itrOfMCVCountOfColDF) {
        if(mcvCountOfColDF[j,1]!="?") {
          mcvCountOfCol <- paste(mcvCountOfCol," ",mcvCountOfColDF[j,1],"(",mcvCountOfColDF[j,2],")",sep="")
        }
      }
      
      # Add the new row of symbolic attribute to the dfOfSymbVal dataframe
      dfOfSymbVal <- rbind(dfOfSymbVal,data.frame(Attribute_ID=colID,Attribute_Name=nameOfCol,Missing=missingNoOfCol,arity=arityOfCol,MCVs_Counts=mcvCountOfCol))
    }
  }
  
  # Print out the real attributes data frame values
  cat("real valued attributes\n")
  cat("----------------------\n")
  print(dfOfRealVal)
  
  cat("\n")
  
  # Print out the symbolic attributes data frame values
  cat("symbolic attributes\n")
  cat("-------------------\n")
  print(dfOfSymbVal)
}


elementaryschool <- import.csv("elementary.csv")

elementaryschool$VALID<-as.numeric(elementaryschool$VALID)
elementaryschool$ACS_CORE<-as.numeric(elementaryschool$ACS_CORE)
elementaryschool$AVG_ED<-as.numeric(elementaryschool$AVG_ED)
elementaryschool$NUMTEACH<-as.numeric(elementaryschool$NUMTEACH)
elementaryschool$FULL_PCT<-as.numeric(elementaryschool$FULL_PCT)
elementaryschool$EMER_PCT<-as.numeric(elementaryschool$EMER_PCT)
elementaryschool$WVR_PCT<-as.numeric(elementaryschool$WVR_PCT)
elementaryschool$YRS_TEACH<-as.numeric(elementaryschool$YRS_TEACH)
elementaryschool$YRONE_TCH<-as.numeric(elementaryschool$YRONE_TCH)
elementaryschool$YRTWO_TCH<-as.numeric(elementaryschool$YRTWO_TCH)

elementaryschool$CHARTER<-factor(elementaryschool$CHARTER)
elementaryschool$AA_SIG<-factor(elementaryschool$AA_SIG)
elementaryschool$AS_SIG<-factor(elementaryschool$AS_SIG)
elementaryschool$HI_SIG<-factor(elementaryschool$HI_SIG)
elementaryschool$WH_SIG<-factor(elementaryschool$WH_SIG)
elementaryschool$SD_SIG<-factor(elementaryschool$SD_SIG)
elementaryschool$YR_RND<-factor(elementaryschool$YR_RND)
elementaryschool$API<-factor(elementaryschool$API)

levels(elementaryschool$API)[levels(elementaryschool$API)=="High"] <- 1
levels(elementaryschool$API)[levels(elementaryschool$API)=="Low"] <- 0
brief(elementaryschool)
elementaryschool[elementaryschool=="?"] <- NA
mean(elementaryschool$ACS_CORE,na.rm=TRUE)

middleschool <- import.csv("middle.csv")
middleschool$VALID<-as.numeric(middleschool$VALID)
middleschool$ACS_CORE<-as.numeric(middleschool$ACS_CORE)
middleschool$AVG_ED<-as.numeric(middleschool$AVG_ED)
middleschool$NUMTEACH<-as.numeric(middleschool$NUMTEACH)
middleschool$FULL_PCT<-as.numeric(middleschool$FULL_PCT)
middleschool$EMER_PCT<-as.numeric(middleschool$EMER_PCT)
middleschool$WVR_PCT<-as.numeric(middleschool$WVR_PCT)
middleschool$YRS_TEACH<-as.numeric(middleschool$YRS_TEACH)
middleschool$YRONE_TCH<-as.numeric(middleschool$YRONE_TCH)
middleschool$YRTWO_TCH<-as.numeric(middleschool$YRTWO_TCH)

middleschool$CHARTER<-factor(middleschool$CHARTER)
middleschool$AA_SIG<-factor(middleschool$AA_SIG)
middleschool$AS_SIG<-factor(middleschool$AS_SIG)
middleschool$HI_SIG<-factor(middleschool$HI_SIG)
middleschool$WH_SIG<-factor(middleschool$WH_SIG)
middleschool$SD_SIG<-factor(middleschool$SD_SIG)
middleschool$YR_RND<-factor(middleschool$YR_RND)
middleschool$API<-factor(middleschool$API)

levels(middleschool$API)[levels(middleschool$API)=="High"] <- 1
levels(middleschool$API)[levels(middleschool$API)=="Low"] <- 0
middleschool[middleschool=="?"] <- NA

highschool <- import.csv("high.csv")
highschool$VALID<-as.numeric(highschool$VALID)
highschool$ACS_CORE<-as.numeric(highschool$ACS_CORE)
highschool$AVG_ED<-as.numeric(highschool$AVG_ED)
highschool$NUMTEACH<-as.numeric(highschool$NUMTEACH)
highschool$FULL_PCT<-as.numeric(highschool$FULL_PCT)
highschool$EMER_PCT<-as.numeric(highschool$EMER_PCT)
highschool$WVR_PCT<-as.numeric(highschool$WVR_PCT)
highschool$YRS_TEACH<-as.numeric(highschool$YRS_TEACH)
highschool$YRONE_TCH<-as.numeric(highschool$YRONE_TCH)
highschool$YRTWO_TCH<-as.numeric(highschool$YRTWO_TCH)

highschool$CHARTER<-factor(highschool$CHARTER)
highschool$AA_SIG<-factor(highschool$AA_SIG)
highschool$AS_SIG<-factor(highschool$AS_SIG)
highschool$HI_SIG<-factor(highschool$HI_SIG)
highschool$WH_SIG<-factor(highschool$WH_SIG)
highschool$SD_SIG<-factor(highschool$SD_SIG)
highschool$YR_RND<-factor(highschool$YR_RND)
highschool$API<-factor(highschool$API)

levels(highschool$API)[levels(highschool$API)=="High"] <- 1
levels(highschool$API)[levels(highschool$API)=="Low"] <- 0
highschool[highschool=="?"] <- NA


printMCVs <- function(column) {
  factor <- factor(column)
  levels <- levels(factor)
  noLevels <- length(levels)  
  table <- table(factor)
  mcvsCounts <- ""
  greatestCount <- table[[1]]
  levelImpute <- levels[1]
  for(i in 1:noLevels) {
    if(levels[i]!="NA" || levels[i]!="<NA>"){
    if(greatestCount < table[[i]] ) {
      levelImpute <- levels[i]
    }
    mcvsCounts <- paste(mcvsCounts,"(",levels[i],")",table[[i]],sep="")
    print(mcvsCounts)
    }
  }
  column[which(is.na(column))] <- levelImpute
  column <- factor(column)
  return(column)
}

impute_numeric<-function(df){
  for(i in 1:(ncol(df)-1)) {
    colEval <- df[,i]
    if(!is.factor(colEval)){
      meanOfCol <- mean(df[,i],na.rm="TRUE")
      colEval[which(is.na(colEval))] <- meanOfCol
      df[,i] <- colEval
    }
  }
  return(df)
}


elementaryschool$CHARTER <- printMCVs(elementaryschool$CHARTER)
elementaryschool$AA_SIG <- printMCVs(elementaryschool$AA_SIG)
elementaryschool$AS_SIG <- printMCVs(elementaryschool$AS_SIG)
elementaryschool$HI_SIG <- printMCVs(elementaryschool$HI_SIG)
elementaryschool$WH_SIG <- printMCVs(elementaryschool$WH_SIG)
elementaryschool$SD_SIG <- printMCVs(elementaryschool$SD_SIG)
elementaryschool$YR_RND <- printMCVs(elementaryschool$YR_RND)
elementaryschool<-impute_numeric(elementaryschool)
elementaryschool$YRONE_TCH<-elementaryschool$YRONE_TCH/elementaryschool$NUMTEACH
elementaryschool<-elementaryschool[complete.cases(elementaryschool$API),]
brief(elementaryschool)

highschool$CHARTER <- printMCVs(highschool$CHARTER)
highschool$AA_SIG <- printMCVs(highschool$AA_SIG)
highschool$AS_SIG <- printMCVs(highschool$AS_SIG)
highschool$HI_SIG <- printMCVs(highschool$HI_SIG)
highschool$WH_SIG <- printMCVs(highschool$WH_SIG)
highschool$SD_SIG <- printMCVs(highschool$SD_SIG)
highschool$YR_RND <- printMCVs(highschool$YR_RND)
highschool<-impute_numeric(highschool)
highschool$YRONE_TCH<-highschool$YRONE_TCH/highschool$NUMTEACH
highschool<-highschool[complete.cases(highschool$API),]

middleschool$CHARTER <- printMCVs(middleschool$CHARTER)
middleschool$AA_SIG <- printMCVs(middleschool$AA_SIG)
middleschool$AS_SIG <- printMCVs(middleschool$AS_SIG)
middleschool$HI_SIG <- printMCVs(middleschool$HI_SIG)
middleschool$WH_SIG <- printMCVs(middleschool$WH_SIG)
middleschool$SD_SIG <- printMCVs(middleschool$SD_SIG)
middleschool$YR_RND <- printMCVs(middleschool$YR_RND)
middleschool<-impute_numeric(middleschool)
middleschool$YRONE_TCH<-middleschool$YRONE_TCH/middleschool$NUMTEACH
middleschool<-middleschool[complete.cases(middleschool$API),]


get_pred_svm <- function(train,test) {
  # Get the indices of output column of training set and testing set
  numColTrain <- ncol(train)
  numColTest <- ncol(test)
  
  # Get the input and output data frame of training set
  inputTrain <- train[,-numColTrain,drop=FALSE]
  outputTrain <- train[,numColTrain]
  
  # Get the input data frame of testing set
  inputTest <- test[,-numColTest,drop=FALSE] 
  outputTest <- test[,numColTest]
  
  # Calculate the linear model for the output and input of training set
  svmModel <- svm(outputTrain ~ ., data = inputTrain, probability=TRUE)
  
  # Predict the value of input of testing set based on lm calculated before
  predModel <- predict(svmModel, inputTest, probability=TRUE)
  
  predModelDF <- as.data.frame(attr(predModel,"probabilities"))
  
  # Get the index where the column name is 1
  indexOfHigh <- grep("1", colnames(predModelDF))
  
  # Add the prediction and real output to a data frame to be returned
  model <- cbind(predModelDF[,indexOfHigh],as.data.frame(test$API))
  
  return(model)
}

# Get the prediction value of the output column using Naive Bayes model
get_pred_nb <- function(train,test)  {
  # Get the indices of output column of training set and testing set
  numColTrain <- ncol(train)
  numColTest <- ncol(test)
  
  # Get the input and output data frame of training set
  inputTrain <- train[,-numColTrain,drop=FALSE]
  outputTrain <- train[,numColTrain]
  
  # Get the input data frame of testing set
  inputTest <- test[,-numColTest,drop=FALSE] 
  outputTest <- test[,numColTest]
  
  # outputTrain <- as.numeric(outputTrain)
  
  # Calculate the naive model for the output and input of training set
  bayesModel <- naiveBayes(as.formula(train$API ~ .), data = inputTrain)
  
  # Predict the value of input of testing set based on naive bayes model calculated before
  predModel <- as.data.frame(predict(bayesModel, inputTest, type="raw"))
  
  # Get the index where the column name is 1
  indexOfHigh <- grep("1", colnames(predModel))
  
  # Add the prediction and real output to a data frame to be returned
  model <- cbind(predModel[,indexOfHigh],as.data.frame(test[,numColTest]))
  
  # Return the model data frame
  return(model)
}

get_pred_tree <- function(train,test) {
  # Get the indices of output column of training set and testing set
  numColTrain <- ncol(train)
  numColTest <- ncol(test)
  
  # Get the decision tree for the "price" as the output for all the input variables in cars.csv
  treePrice <- rpart(API ~ ., method="class", data=train)
  
  # Predict the values for the decision tree treePrice 
  predTree <- predict(treePrice,test)
  
  # Convert the prediction to data frame
  predTreeDF <- as.data.frame(predTree)
  
  # Get the index where the column name is 1
  indexOfHigh <- grep("1", colnames(predTreeDF))
  
  # Add the prediction and real output to a data frame to be returned
  model <- cbind(predTreeDF[,indexOfHigh],as.data.frame(test$API))
  
  # Return the model data frame
  return(model)
  
}

get_pred_logreg <- function(train,test) {
  # Get the indices of output column of training set and testing set
  numColTrain <- ncol(train)
  numColTest <- ncol(test)
  
  # Get the input and output data frame of training set
  inputTrain <- train[,-numColTrain,drop=FALSE]
  outputTrain <- train[,numColTrain]
  
  # Get the input data frame of testing set
  inputTest <- test[,-numColTest,drop=FALSE] 
  outputTest <- test[,numColTest]
  
  # Calculate the linear model for the output and input of training set
  logregModel <- glm(train$API ~ ., family = binomial(logit), data = inputTrain, na.action="na.exclude")
  
  # Predict the value of input of testing set based on lm calculated before
  predModel <- predict(logregModel, inputTest, type="response")
  
  model <- cbind(1-predModel,as.data.frame(test$API))
  View(exp(coef(logregModel)))
  return(model)
}


# Run k-fold cross validation on particular model
do_cv_class <- function(df,num_folds,model_name) {
  # Randomize the row samples to get it ready for k-fold cross validation
  df <- df[sample(nrow(df)),]
  
  # Number of rows to split in k-fold cross validation
  countOfKSplit = round(nrow(df)/num_folds,0)    
  
  # Get the integer value that is in the model for the kNN model
  intKnn <- as.numeric(gsub("\\D", "", model_name))
  
  dfModel <- data.frame()
  
  # Create a k-fold cross validation for the df passed to the function
  for (i in 1:num_folds){
    predictedValue <- data.frame()
    
    # Initialize the predicted, actual, training, and testing set to be later used
    trainingSet <- data.frame()
    testingSet <- data.frame()
    
    # Set the proper indices for where the testing set starts. If i == k, then get all the rows from 
    # countOfKSplit*(i-1) to the end of the data frame
    if(i==num_folds) {
      testingStartIndex <- (i-1)*countOfKSplit+1;
      testingEndIndex <- nrow(df)
    } 
    else {
      testingStartIndex <- (i-1)*countOfKSplit+1;  
      testingEndIndex <- i*countOfKSplit
    }
    
    # Get the training set and testing set for the k-fold cross validation in the i'th iteration
    testingSet <- df[testingStartIndex:testingEndIndex,]
    trainingSet <- df[-(testingStartIndex:testingEndIndex),]
    
    # Call the appropriate model based on the model_name passed for the testing and training set
    if(grepl("svm",model_name)) {
      predictedValue <- get_pred_svm(trainingSet,testingSet)
    } 
    else if(grepl("nb",model_name)) {
      predictedValue <- get_pred_nb(trainingSet,testingSet)
    } 
    else if(grepl("logreg",model_name)) {
      predictedValue <- get_pred_logreg(trainingSet,testingSet)
    }
    else {
      predictedValue <- get_pred_tree(trainingSet,testingSet)
    }
    
    # Combine all the k-folds 
    dfModel <- rbind(dfModel,predictedValue)
  }
  
  # Return the prediction value and real output for all the k-fold cross validation
  return(dfModel) 
}

backwardStepwiseRegressionSearch <- function(DFrame) {
  # Get the indices of output column of DFrame passed to the function
  numColDFrame <- ncol(DFrame)  
  
  # Get the input columns of DFrame
  inputFeatures <- DFrame[,-numColDFrame]
  
  # Calculate the linear regression model for the output and all the input columns
  logregModel <- glm(DFrame$API ~ ., family = binomial(logit), data = inputFeatures, na.action="na.exclude")
  
  # Run the stepAIC (stepwise backward regression) to find the features corresponding to calculating
  # Fat percentage
  step <- stepAIC(logregModel, direction="backward")
  
  # Print the results of the variables needed for modeling for Fat Percentages based on the input 
  # features
  cat("\nBackward Stepwise Regression:\n")
  print(step$anova) # display results
  cat("\n\n")
}

# Get the TPR, FPR, Accuracy, Precision, and Recall values for all 
get_metrics <- function(pDFrame,cutOffScore=.5) {
  # Get the prediction value from the data frame
  predValue <- pDFrame[,1]
  
  # Get the actual value from the data frame
  actualValue <- as.numeric(pDFrame[,2])-1
  
  # Assign proper 0 or 1 based on the cut off score
  predValue[predValue>cutOffScore] <- 1
  predValue[predValue<=cutOffScore] <- 0
  
  pDFrame[,1] <- predValue
  
  # Calculate TP, FP, FN, TN
  TP <- sum(pDFrame[,2][predValue==1]==1)
  FP <- sum(pDFrame[,2][predValue==1]==0)
  FN <- sum(pDFrame[,2][predValue==0]==1)
  TN <- sum(pDFrame[,2][predValue==0]==0)
  
  # Calculate TPR, FPR, ACC, PRECISION, RECALL
  TPR <- TP/(TP+FN)
  FPR <- FP/(TN+FP)
  ACC <- (TP+TN)/nrow(pDFrame)
  PREC <- TP/(TP+FP)
  REC <- TP/(TP+FN)
  
  # Add all the metrics value 
  metrics <- as.data.frame(cbind(TPR,FPR,ACC,PREC,REC))
  
  # Return the metrics
  return(metrics)
}

tree_ES <- do_cv_class(elementaryschool,10,"tree")
metricsTreeES <- get_metrics(tree_ES)
print(metricsTreeES)

logreg_ES <- do_cv_class(elementaryschool,10,"logreg")
metriclogES <- get_metrics(logreg_ES)
print(metriclogES)

svm_ES <- do_cv_class(elementaryschool,10,"svm")
metricsvmES <- get_metrics(svm_ES)
print(metricsvmES)

nb_ES <- do_cv_class(elementaryschool,10,"nb")
metricsnbES <- get_metrics(nb_ES)
print(metricsnbES)


backwardStepwiseRegressionSearch(elementaryschool)
backwardStepwiseRegressionSearch(middleschool)
backwardStepwiseRegressionSearch(highschool)
