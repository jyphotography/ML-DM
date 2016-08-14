# Homework0 - Example 1

# I am generating 1000 random samples from N(0,1) - normal distribution with mean
# 0 and sd 1, assign it to x. 

# In R '<-' is used for assigning values, '=' is also acceptable, but '<-' is less 
# likely to be confused with '==' which means 'equal'.

# In R, the capabilities are rendered through functions. Here rnorm is a function, and
# it takes serveral parameters. This is a function from the base package, so we don't have to
# make a reference to a library. Type help(rnorm) to learn more about this function.

# You may execute the command by highlighting the line/chunk of code and clicking 'run' button in the top right
# corner or by hitting CTRL+ENTER. Now please do it.
x <- rnorm(1000,0,1)

# After executing the command, the properties of value x show up in the workspace window. It says 
# x is numeric[1000] - it tells you that x is a numeric vector of length 1000, as it should be.

# Let us plot x as it is, this will simply be a plot of x values ordered by theor vector index sequence 
# (X-axis is the sequential number, and Y-axis coordinate reflects the x's value)
# As expected, the values of x are hovering around 0 and ~68% of data should stay between -2 and 2 
plot(x)

# Let's look at some statistics of x
summary(x)

# And its histogram
hist(x)

# Multiple plots on the same screen
par(mfrow=c(1,2))  # This tells R to divide the plot space into 1 row and 2 columns, do help(par) to find more
plot(x)
hist(x)

# Output the plot to a pdf file
cat('plotting to pdf\n') 
pdf('my_plot.pdf')  # Output to a pdf file in the current working directory. You may set your working directory 
                    # Tools->set working directory->to source file loction
par(mfrow=c(1,2))  
plot(x)
hist(x)
dev.off() # Remember to close the pdf device to switch back to screen plotting

# Finally you may batch run all the code together by sourcing the R file. Click "source" button at top right
# or type 'source("R filename") in the console

# ** You will notice that the summary statistics do not print. You need to change line #26 summary(x) to
# print(summary(x)) in order for it to print in batch script mode

# That's it!



