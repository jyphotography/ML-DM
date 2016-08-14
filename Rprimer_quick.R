############## Objects ##################
# Objects are named entities created using an assignment operation ('<-','=',etc.)
# Common object types include
# 1. numeric
a <- 1
b <- c(2:10) # a numeric array

# 2. character
a <- "hello world!"
b <- c("A","B","C")

# 3. logical
a <- TRUE;
b <- c(TRUE,FALSE,T,F)

# 4. factor
a <- factor(c("Alan","Alan","Alan","Bob","Bob"))

# 5. array/matrix
a <- array(1:12,dim=c(2,3,2)) # a three dimensional array
b <- matrix(1:6,nrow=3)

# 6. list
a <- list(
  1,
  "a string",
  1:10,
  c(T,F,T)
  )

# 7. data.frame
library(datasets)
df <- mtcars

# 8. functions
a <- function() return(5)
a()

############## Scope ##################
# a quick word on scope
# variables come into existence when first assigned a value
# R uses lexical scoping

f <- function(b){ # b is a 'formal parameter'
   a <- b*10      # a is a 'local variable'
   return(a*z)      # z is a 'free variable'
}
# z is resolved when f is called by first looking in the 
# environment that called the function, then the environment that contains
# that environment, and so on until the global environment is reached.

# '<<-' is the 'super assign' operator, assigns values to 'free variables'
# in their native scope

# thanks to Thomas Lumley, Univ. Auckland
make.accumulator <- function(){
   a <- 0
   function(x){
     a<<-a+x
     a
   }
}
f <- make.accumulator()
f(1)
f(1)
f(11)
f(11)

############## Working with arrays ##################
a <- 1:4
a*a   # standard mathematical operations operate element-wise
a+2   # R repeates objects to make sense of commands
a+1:2 
a+1:3 # this can present problems

rep(2,5)
seq(1,6,length.out=4)
1:5 > 3
1:5 > 3 & 1:5 < 5
1:5 > 3 && 1:5 < 5 # & (and |) are vectorized, && (and ||) are not.

a <- factor(c("10","2","5","5","10")) # watch out for factors when converting
as.numeric(a)
as.character(a)
as.numeric( as.character(a) )

a <- array(1:12,dim=c(2,3,2))
apply(a,2,sum) # use apply, lapply, sapply to iterate over objects

############## Working with lists ##################
a <- list(1, "a string", 1:10, c(T,F,T))
a[[2]]
a[2]
a[1:3]
a[[1:3]]

lapply(a,length)
unlist( lapply(a,length) )

a[[2]] <- NULL
a
a[[length(a)+1]]<- "new object"
a

############## Working with data.frame ##################

# See HW 0.

df <- mtcars
df[1:5,]
rbind(df[1:5,],df[30:32,])
cbind(df[1:5,],test=1:5)
sapply(df,class)

############## Working with functions ##################
# looping and logical branching work as in other languages
for(i in 1:10){
  if(i>1 && i<3){
    print("A")
  }else if(i<5){
    print("B")
  }else{
    print("C")
  }
}

