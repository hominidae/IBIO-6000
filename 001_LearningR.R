# Test Script for R for Data science
# Load tidyverse library
library(tidyverse)
# Generate a plot of MPG data
ggplot(data = mpg) +
  geom_point(mapping = aes(x = displ, y = hwy))
# displ is displacement or engine size
# hwy is fuel economy
# geom_point plots displacement on the x axis and fuel economy on the y axis
# The first argument that ggplot is the dataset, in this case mpg.
# You can complete your graph by adding more layers to ggplot.
# The function geom_point() adds layers of points.
#

# Here is a template for ggplot
#ggplot(data = <DATA>) + 
#  <GEOM_FUNCTION>(mapping = aes(<MAPPINGS>))
# Where <DATA> is your dataset
# where <GEOM_FUNCTION> is your type of graph
# where aes(<MAPPINGS>) is an aesthethic assignment for the chart

# Exercise 1: Run ggplot(data = mpg)
ggplot(data = mpg)
# Does nothing since it does not have instructions

# Exercise 2: How many rows are in mpg? How many columns?
# nrow() is a function to count the number of rows in a dataset
nrow(mpg)
#  ncol() is a function to count the number of columns in a dataset
ncol(mpg)

# Exercise 3: What does the drv variable describe?
# Use the console and ?mpg to find out
# drv is the type of drive train fwd = f, rwd = r, 4wd = 4

# Exercise 4: Make a scatterplot of hwy vs cyl
# geom_point() is a simple scatterplot
# It may take three arguments for size, color, and shape.
# geom_point(size, color, shape) 
ggplot(data = mpg) +
  geom_point(mapping = aes(x=hwy, y=cyl))

# This is another example for using ggplot
# I've taken these examples from here:
# http://www.sthda.com/english/wiki/ggplot2-scatter-plots-quick-start-guide-r-software-and-data-visualization
ggplot(mtcars, aes(x=wt, y=mpg)) + geom_point()

# Here is an example where the size and shape of points within the graph
# are defined.
ggplot(mtcars, aes(x=wt, y=mpg)) +
  geom_point(size=2, shape=23)

# Here is another example, qsec as a size puts a legend with approximate sizing
# within the chart
ggplot(mtcars, aes(x=wt, y=mpg)) +
  geom_point(aes(size=qsec))

# You can also label points within the scatterplot
ggplot(mtcars, aes(x=wt, y=mpg)) +
  geom_point() +
  geom_text(label=rownames(mtcars))
# As you can see, the names are derived from rows  within the mtcars dataset
# But since mtcars is a built-in dataset within tidyverse, where exactly are
# those labels derived from?
# Let's have a look at the dataset.
cardataset <- mtcars
# Opening cardataset within theR studio data explorer reveals that the car names
# do not have a description. They simply start off that way.
# If we cound the number of rows, there's 32
nrow(cardataset)
# If we count the number of columns, there's 11
ncol(cardataset)
# However, if you look at the data...
# Strangely enough, the car names do not have their own column.
# Most data processing won't be this straight forward where a unique ID
# exists and works so well in differentiating that data.
# In practice, there are 12 columns. But the plot earlier derived the data
# from the first column it encountered which just happens to be the car name.

# Next up, let's add regression lines:
# The following functions can be used to add regression lines to scatterplots.
# geom_smooth()
# stat_smooth()
# geom_abline()
# Let's look at geom_smooth()
ggplot(mtcars, aes(x=wt, y=,mpg)) +
  geom_point() +
  geom_smooth(method=lm)
# Next, with confidence interval removed
# That's the shaded area in the graph
ggplot(mtcars, aes(x=wt, y=mpg)) + 
  geom_point() +
  geom_smooth(method=lm, se=FALSE)
# Loess regression, or loess curves are used on multiple points
# to produce a single line that is the average of those points.
ggplot(mtcars, aes(x=wt, y=mpg)) + 
  geom_point() +
  geom_smooth()
# As you can see, geom_smooth() defaults to creating a Loess regression curve
# You can also change the appearance of points and lines
ggplot(mtcars, aes(x=wt, y=mpg)) + 
  geom_point(shape=18, color="blue")+
  geom_smooth(method=lm, se=FALSE, linetype="dashed", color="darkred")
# Changes the confidence interval fill color
ggplot(mtcars, aes(x=wt, y=mpg)) + 
  geom_point(shape=18, color="blue") +
  geom_smooth(method=lm,  linetype="dashed",
              color="darkred", fill="blue")
# But what about changing a loess curve line?
ggplot(mtcars, aes(x=wt, y=mpg)) +
  geom_point() +
  geom_smooth(color="blue")

# You can also set the color and size of points automatically using the
# following examples:

# Change point shapes by the levels of cyl
# The examples on the STHDA website fail due to the shape=<VAR> not being
# in quotation marks.
ggplot(mtcars, aes(x=wt, y=mpg, shape="cyl")) +
  geom_point()
# Change point shapes and colors
ggplot(mtcars, aes(x=wt, y=mpg, shape="cyl", color="cyl")) +
  geom_point()
# Change point shapes, colors and sizes
ggplot(mtcars, aes(x=wt, y=mpg, shape="cyl", color="cyl", size="cyl")) +
  geom_point()
# There are even more examples available from the STHDA website but let's leave
# that alone for now.

# %>% is a compound assignment. It is used to update a value by first piping it
# into one or more expressions and then assigning the result.

# Iteration, for loops
x <- c(2,5,3,9,8,11,6) # Create a dataframe called x
count <- 0 # Create a variable called count and set it to zero
for (val in x) { # This is a for loop.
  if(val %% 2 == 0)  count = count+1 # Iterate count by 1 for each element that is an even number
}
print(count) # Print the result
count <- 0 # Set count back to zero
for (val in x) { # What if we just want a count?
  count = count+1 # Iterate count by +1 for each element in x
}
print(count)
# 1 - Compute the mean of every column in mtcars
# 2 - Determine the type of each column in nycflights13::flights
# 3 - Compute the number of unique values in each column of iris
# 4 - Generate 10 random normals from distributions with means of -10, 0, 10, and 100

# For 1 - mtcars, there are 11 columns, and 32 rows.
# Write a for loop that will go through each column, add the values within
#  that column together then divide by the number of rows.

# For 2 - Determine the type of each column of nycflights13::flights
# Let's take
map(flights, class)
for (val in mappedflights) {
  print(val)
}
# Interesting, but maybe we want the names
