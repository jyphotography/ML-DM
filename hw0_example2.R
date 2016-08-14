# Homework0 - Example 2

# Must do: You need to set work directory to the source file location.
# You may do this by going to menu Tools->Set Working Directory->To source file location

# Utility function for importing data from a csv (comma-separated values, flat table) file
import.csv <- function(filename) {
  return(read.csv(filename, sep = ",", header = TRUE))
}
# Utility function for exporting data to csv file
# Note that csv files are very portable, practically all tools understand them,
# including Microsoft Excel
write.csv <- function(ob, filename) {
  write.table(ob, filename, quote = FALSE, sep = ",", row.names = FALSE)
}

# Let us load some data set: mpg.csv
# This data collects information about various car models and ther fuel efficiency
# It is publicly available at University of California at Irvine benchmark
# data repository, a popular resource for data miners who want to showcase their
# algorithms on data that is well understood and appropriate for public showing
# To find more about this data set go to http://archive.ics.uci.edu/ml/datasets/Auto+MPG
my.data <- import.csv("mpg.csv")

# To examine the contents of the just loaded data set
# feel free to type any/all of the following commands in the R Studio console window
# (of course skip the hash marks which R considers as comment tags)

# head(my.data) # print top 10 rows of data
# head(my.data,20) # print top 20 rows of data
# tail(my.data) # print the last few rows of data
# summary(my.data) 
# str(my.data) # which variable is of type 'factor'?

# Below we will perform various modifications to the data, feel free
# to inspect the resultant using trhe functions shown above
# by e.g. typing head(x) in your console

##############################
# Q1: Slicing data frames
##############################
x <- my.data[1:10, ] # first 10 rows of the data frame
x <- my.data[,c(1,3,5)] # select columns number 1,3,5
x <- my.data[, -1] # select all but first column
x <- my.data$mpg # select column named mpg
x <- my.data[, 'mpg'] # select column named mpg

##############################
# Q1 Exercise:
# goal: create a data frame with all but last column of my.data, print dimensionality of the resulting data frame
# hint: use ncol to get the index of the last column, dim() to get dimension information
##############################
cat('Q1\n')
nf <- ncol(my.data)
x <- my.data[, -nf]
print(dim(x))

##############################
# Q2: Subsetting data frames
##############################
x <- my.data$mpg # extract attribute named 'mpg'
high.mpg <- subset(my.data,mpg > median(x)) # extract records for which mpg is above median

# as above, but include only three selected attributes in the resulting data frame
high.mpg2 <- subset(my.data,mpg > median(x), select = c(cylinders, displacement, horsepower)) 

# as above, retain all features but 'cylinders'
high.mpg3 <- subset(my.data,mpg > median(x), select = -cylinders) 

##############################
# Q2 Exercise:
# goal: create a data frame called x with records of non-US cars with all but 'maker' attribute
# and report dimensionality of the resulting data frame
# hint: since 'maker' dimension is imported as a factor data type, we need to convert it
# to a string type for comparision; we may use as.character() function to accomplish that
##############################
cat('Q2\n')
x <- subset(my.data,as.character(maker)!='usa',select = -maker)
print(dim(x))

##############################
# Q3: function 'which'
##############################
# extract the index of records for which maker == 'usa'
# function 'with' saves us from having to type 'my.data$maker'
x.index <- with(my.data,which(maker=='usa')) 
x.count <- length(x.index) # count how many elements are in the index 
x <- length(which(my.data$maker=='usa')) # put together
mpg_cat <- with(my.data,ifelse(mpg>mean(mpg),'high','low')) # ifelse is another useful function

##############################
# Q3 Exercise:
# for variable 'mpg_cat' output the percentage of records with high mpg 
##############################
cat('Q3\n')
x <- length(which(mpg_cat=='high'))/length(mpg_cat)
print(x)

##############################
# Q4: function 'table'
##############################
x <- with(my.data, table(maker)) # frequency count of makers, note that 'maker' is a factor 
x <- with(my.data, table(modelyear))# fequency count of modelyear, which is an integer
x <- with(my.data, table(acceleration)) # frequency count of acceleration which is a float (numeric variable), but does it make practical sense to do this?
x <- with(my.data, table(modelyear, maker)) # cross-tabluation of modelyear and maker

##############################
# Q4 Exercise:
# goals: (a) create a table showing the count of records by maker and by mpg_cat 
# (b) from the table, output the number of cars made in usa and in low mpg category
# hint: first create a data frame: data.frame(mpg_cat,maker), error? remember '$'
# hint: table object is like a data frame which can be referenced by row/column names, cf. Q1 above
##############################
cat('Q4\n')
df <- data.frame(mpg_cat, maker=my.data$maker)
tbl <- table(df)
print(tbl['low', 'usa'])

##############################
# Q5: sorting
##############################
x <- my.data[order(my.data$mpg), ] # sort the data frame by mpg from low to high
x <- my.data[order(-my.data$mpg), ] # sort the data frame by mpg from high to low

# sort the data frame by mpg then by horsepower, in decreasing order
x <- with(my.data,my.data[order(-mpg,-horsepower),]) 

##############################
# Q5: Exercise
# goal: tabulate modelyear attribute, sort the resulting table,
# output the number of cars from the most populous model year 
# hints: we will need to convert table into a data frame: data.frame(our_table)
# then we need to take a look at the table to decide which attribute to sort on
##############################
cat('Q5\n')
tbl <- data.frame(table(modelyear=my.data$modelyear))
sorted.tbl <- tbl[order(-tbl$Freq), ]
print(sorted.tbl[1, 2])

##############################
# Q6: summarization of data using plyr package and declaring functions
##############################
library(plyr) # we need to install package plyr if we have not done so yet

# here we demonstrate a group aggregation function implemented in plyr package
# it is very similar to the SQL "group by" function

# in this example, we compute the mean and standard deviation of mpg for each unique combination 
# of maker and cylinders
x <- ddply(my.data, c('maker','cylinders'), function(df) c(mean=mean(df$mpg),sd=sd(df$mpg)))

# it often makes sense to declare a sequence of frequently executed commands as a function
# it saves typing time, and neatly organizes structure of our scripts 
# especially if we end up standarizing non-trivial functionality

# in this example, we define a function 'get_cor' that computes linear correlation between mpg and weight
# and then we use it to compute correlations for each combination of maker and cylinders 
get_cor <- function(df){  
  return(c(cor=cor(df$mpg,df$weight)))
}
x <- ddply(my.data, c('maker','cylinders'), get_cor)

##############################
# Q6 Exercise:
# goal: for each combination of maker and modelyear, 
# compute the ratio of mean(displacement) to mean(horsepower)
# output the the max ratio
##############################
cat('Q6\n')
get_ratio <- function(df){
  return(c(ratio=mean(df$displacement)/mean(df$horsepower)))
}
x <- ddply(my.data, c('maker','modelyear'), get_ratio)
print(max(x$ratio))






