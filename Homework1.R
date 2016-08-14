#function to get breif summary statistics about the data
brief<-function(df){
cat("brief function output for \n")
cat("\n")
cat("This dataset has ",nrow(df)," rows ",ncol(df)," Attributes\n")
cat("\n")
cat("real valued attributes\n")
cat("-------------------------")
cat("\n")
#empty frame for real valued attr
b1=data.frame(Attribute_ID=NA,Attribute_Name=NA,Missing=NA,Mean=NA,Median=NA,Sdev=NA,Min=NA,Max=NA)  
j=1
#loop across the dataset
for(i in 1:ncol(df)){
if(class(df[,i])=="integer"||class(df[,i])=="numeric"){
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
#empty frame for syymbolic valued attr
b2=data.frame(Attribute_ID=NA,Attribute_Name=NA,Missing=NA,arity=NA,MCVs_counts=NA)
j=1
count1=0
for(i in 1:ncol(df)){
 
  if(class(df[,i])=="factor"){
    a5=count(df,colnames(df[i]))
    e=array(df[i])
    b2[j,1]=i
    b2[j,2]=colnames(df)[i]
   #missing count
   for(z in 1:nrow(df[i]))
   {
     if(df[z,i]=="")
      {
       count1=count1+1
      }
   }
   
    b2[j,3]= count1
   #arity
    b2[j,4]=apply(e, 2, function(x)length(unique(x[x!=""])))
   b2[j,5]=""
   #mcvs_count
   for(m in 1:nrow(a5[j]))
   {
     if(a5[m,j]!="")
    {     
      l=1
      b2[j,5]=paste(b2[j,5],a5[m,l],"(",a5[m,l+1],")")
    } 
   }   
    j=j+1
  }
}
print(b2)
}

my.data <- function(df) {
  return(read.csv(df, sep = ",", header = TRUE))
}


V1<-brief(my.data("house_no_missing.csv"))

#Q1b
#plot for house_value and Dist_to_employment_center
ax = df$dist_to_employment_center  
bx = df$house_value   
plot(ax, bx, xlab="Dist_to_employment_center", ylab="House Value")   
abline(lm(bx ~ ax))

ax = df$accessiblity_to_highway   
bx = df$house_value     
plot(ax, bx, xlab="accessiblity_to_highway", ylab="House Value")   
abline(lm(bx ~ ax))

ax = df$Nitric_Oxides 
bx = df$house_value 
plot(ax, bx, xlab="Nitric_Oxides", ylab="House Value")   
abline(lm(bx ~ ax))

ax = df$house_value

hist_House<- hist( ax,col = "lightblue",breaks = 28,
           main = "Distribution of Prices of House",
           xlab = "Price of houses", ylab = "Number of Houses")


h = hist( house_val,col = "lightblue",breaks = 28,
          main = "Price of House v/s Normal Curve",
          xlab = "Price of houses", ylab = "Number of Houses")
xfit<-seq(min(house_val),max(house_val),length=40) 
yfit<-dnorm(xfit,mean=mean(house_val),sd=sd(house_val)) 
yfit <- yfit*diff(h$mids[1:2])*length(house_val) 
lines(xfit, yfit, col="blue", lwd=2)

qplot(factor(Charles_river_bound),house_value, data=df, geom= "boxplot",,xlab="Charles_river_bound",
ylab="Price of Houses", main="Variation in Price of Houses conditioned on River_bound or Not")


qplot(factor(num_of_rooms),house_value, data= df, geom= "boxplot",,xlab="Num_Of_Rooms",
      ylab="Price of Houses", main="Variation in Price of Houses conditioned on num of rooms")


