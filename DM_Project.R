summary(df)
brief<-function(df){
  
  trim.leading <- function (x)  sub("^\\s+", "", x)
  trim.trailing <- function (x) sub("\\s+$", "", x)
  
  cat("-------------------------------------------------\n")
  cat("brief function output for ",file,"\n")
  cat("-------------------------------------------------\n")
  cat("\n")
  cat("This dataset has ",nrow(df)," rows ",ncol(df)," Attributes\n")
  cat("\n")
  cat("real valued attributes\n")
  cat("-------------------------")
  cat("\n")
  #empty frame for real valued attr
  b1=data.frame(Attribute_ID=NA,Attribute_Name=NA,Missing=NA,Mean=NA,Median=NA,Sdev=NA,Min=NA,Max=NA)  
  j=1
  df[df == "?"] <- NA
  df[df == ""] <- NA
  #loop across the dataset
  for(i in 1:ncol(df)){  
    if(!is.factor(df[,i])){
      b1[j,1]=j
      b1[j,2]=colnames(df)[i]
      b1[j,3]= sum(is.na(df[i]))
      b1[j,4]=round(mean(df[,i],na.rm = TRUE),2)
      b1[j,5]=round(median(df[,i],na.rm = TRUE),2)
      b1[j,6]=round(sd(df[,i],na.rm = TRUE),2)
      b1[j,7]=round(max(df[,i],na.rm = TRUE),2)
      b1[j,8]=round(min(df[,i],na.rm = TRUE),2)
      j=j+1
    }
  }
  print(b1)
  
  cat("\n")
  cat("symbolic attributes\n")
  cat("-------------------------")
  cat("\n")
  #empty frame for symbolic valued attr
  b2=data.frame(Attribute_ID=NA,Attribute_Name=NA,Missing=NA,arity=NA,MCVs_counts=NA)
  j=1

  for(i in 1:ncol(df)){
    count1=0
    if(class(df[,i])=="factor"){
      a5=count(df,colnames(df[i]))
      e=array(df[i])
      b2[j,1]=i
      b2[j,2]=colnames(df)[i]
      #missing count
      for(z in 1:nrow(df[1]))
      {
        if(is.na(df[z,i]))
          count1=count1+1
      }
      
      b2[j,3]= count1
      #arity
      b2[j,4]=apply(e, 2, function(x)length(unique(x[x!=""])))
      b2[j,5]=""
      #mcvs_count    
      j=j+1
    }
  }
  print(b2)
}

#utilityfunction to read file
my.data <- function(df) {
  return(read.csv(df, sep = ",", header = TRUE))
}

# utility function for import from csv file
file<-"high.csv"
brief(my.data)
#finding correlation between input variables and output variable

