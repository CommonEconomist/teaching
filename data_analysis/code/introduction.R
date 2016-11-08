#------------------------------------------------------------------------------
				# Tutorial 1: Introduction to R
#------------------------------------------------------------------------------
# In this first R tutorial of the course we will be focusing on some of the 
# basic functions of R. Which includes creating objects, manipulating data, 
# carrying out calculations, and getting descriptive statistics.
# We will also have a look at writing functions which can help us in 
# executing certain data processing steps more efficiently.
# Note that you can execute the line in your code simply by using 
# ctrl + enter. 

setwd("~/Dropbox/ucd/data_analysis/") # Set appropriate directory
par(las=1,bty="n")                    # Settings for plots

#------------------------------------------------------------------------------
#### 1) Creating variables ####
# Creating variables in R is rather straightforward using either 
# the '='' or '<-' operator. R will save the variable as an object in the workspace. 
# This way you can create an variable as for instance a vector and use it for 
# later processing. Next to vectors R also accommodates matrices, data frames,
# lists, and other more complicated data structures. 

# Just one important note concerning the creation of objects; never use one of 
# the following letters as a name for you variable or object:
# 'c', 'F', 'T'. 
# These letters are used in R for specific functions. For instance c is actually
# used to create a vector. 
# You also want to avoid giving generic names to your objects that correspond 
# with functions such as 'mean' for instance. 

# In terms of housekeeping, objects can be remover using 'rm(OBJECT NAME)'.
# Enough background for now, let's begin with creating some data:
a=3
a+a       # Can add
a*a       # Or multiply 
a^2       # Square
b=a+6     # Or use to create a new variable
d<-c(a,b) # Concatonating vectors is also easy
e<-t(d)   # Transposing our newly created vector 
m<-matrix(c(3,9,6,2),nrow=2) # Use 'matrix to create a 2x2 matrix
m2<-e%*%m  # Matrix multiplication is done this way

# As you can see, R uses the same mathematical expressions as other programming 
# languages such as Python and Matlab, or indeed Excel 
# (NB - we use '==' instead '='). 
# Let's use this to check if 'a' is larger than 'b':
a<b

# We get a logical indicator as output here, stating simply whether the 
# statement is true or false. 
# In order to turn this into something useful we can use in our data 
# processing and calculations, we use 'as.numeric' to create a binary indicator.
as.numeric(a<b)

# Note that R is pretty versatile and there is almost never just one way of 
# accomplishing something. 
# Often there are multiple options that achieve the same objective. 
# For isntance, let's create a vector containing all integeres from 1 to 10. 
x1<-c(1,2,3,4,5,6,7,8,9,10)
x2<-1:10 # What's the diffence with 'x1'?
x3<-seq(1,10,1)

# Another useful function is 'rep'
s<-rep(1.5,10)
forest<-rep("tree",20) # Can do the same for characters. 

# If you want to know a bit more about a particular function you can use '?'
# to access the documentation. 
# Have a look at '?matrix' to see what the matrix function does. 
## Q1: Create matrix M according to the structure below:
# 16 2 3 13
# 5 11 10 8
# 9 7  6 12
# 4 14 15 1

# A matrix, as other data objects, has a set of dimensions that we can 
# access using its index.
# If I want to know the average of the first row, or the sum of the first 
# column, I do:
mean(M[1,])
sum(M[,1])

## Q2: Find out the mean and sum of the other rows and columns of the matrix. 

#------------------------------------------------------------------------------
#### 2) Working with data frames ####
# One of the most versatile data structures in R is the data frame. 
# This structure is similar to what you work with in excel (not always though), 
# which we call a long structure. 
# You also have a wide structure, we'll come back to this in a later tutorial.
# In the long structure each column represents a variable and the observations
# are placed on the rows. 
# A data frame is very similar to a matrix and easy to create in R using 
# 'data.frame', e.g.:
df<-data.frame(constant=s,sequence=x1)

# You now have a data frame with 2 variables and 10 observations. 
# The data frame is compatible with most R functions such as 'summary', 
# which will summarise the data for us: 
summary(df)

# In cases where we want the output for just a particular variable we can 
# use '$' operator to access this variable in the data frame:
sd(df$sequence) # Standard deviation 

## Q3: You are given trade data for two countries, Australia and South Africa, 
# on their exports in iron and wheat between 2005-2009.
# You're also given international market prices for these two commodities. 
# Unfortunately, the data is provided as a raw data dump. 
# This means that you have to input the data into R yourself in order to work
# with it. 
# Combine the data below into a data frame and calculate an annual price index,
# as a new variable in the data frame, for each country. 
# Trade data (exports in 1000 metric tonnes)
# Australia, iron: 85, 98, 86, 86, 119
# Australia, wheat: 46, 64, 66, 58, 39
# South Africa, iron: 68, 62, 59, 70, 94
# South Africa, wheat: 19, 24, 21, 28, 9
## International price data (per metric tonn)
# Iron: 28,33,37,61,72
# Wheat: 520,640,732,793,516

#------------------------------------------------------------------------------
#### 3) Whale beachings in the Netherlands ####
# The Netherlands have a coastline of about 523 Kilometers. 
# Given the length of the coastline and its position on the North Sea, 
# it is not surprising that now and then a whale might beach. 
# We have the following data on whale beachings in the Netherlands, 
# covering the period 2000-2015. 
w<-c(88,126,122,163,223,320,561,347,375,520,450,883,755,882,585,314)

## Q4: In how many years was the number of beachings above average?
# And in how many years was the number of beachings at least one standard 
# deviation above average?

# When working with data it is almost always a good idea to visualise the data.
# So let's plot a histogram of our whale beachings
hist(w) 
hist(w,prob=TRUE) # Rather than frequencies can get probability

# For the beachings we have repeated observations over time, so we can also
# plot the data as a time-series to check if there are patterns over time:
yr<-2000:2015 # Create a vector for the years 2000 to 2015. 
plot(yr,w)
plot(yr,w,type="l") # Can change it into a line
plot(yr,w,type="b") # Or better a combination
plot(yr,w,type="b",
     xlab="Year",ylab="Number of beachings") # With labeled axis

# Can use 'abline' to add statistics
abline(h=mean(w),col="red")
abline(h=mean(w)+sd(w),col="blue",lty=2)

#------------------------------------------------------------------------------
#### 4) Average temperature in San Francisco ####
# For this class you downloaded a csv-file which contains data for
# the average annual temperature in San Francisco covering the years 1921-2015.
# We can load this data into a data frame using 'read.csv':
sf<-read.csv("sanfrancisco.csv",header=TRUE)

# Check the data
str(sf)     # Gives information on the type of data
summary(sf) # Summary statistics

# The data includes 5 variables. 
# One is the station at which the temperature was measured, which in this case 
# is just one station. 
# There is a variable for date which indicates the year, and than there are 
# three variables for temperature: the average, the maximum, and the minimum. 
# Now onfortunately because this data was taken from the website of the 
# National Oceanic and Atmospheric Administration in the USA, the temperature
# is measured in degrees Fahrenheit, whereas most people tend to measure it
# in a more appropriate scale like Celsius (or Kelvin). 
# It is straightforward to convert Fahrenheit to Celsius by simply 
# subtracting 32 and multiplying by 5/9. 
# So let's do that for the average temperature. 
(mean(sf$TAVG)-32)*5/9

# If we want to do this for the whole data series it might be easier to write
# a specific function. 
# Functions in R are created as follows:
# F<-function(x){"STUFF THE FUNCTION DOES"}, where 'x' indicates the 
# input data. 
# So if we create a function called 'Fh2C', we can use this function to 
# convert the data doing 'Fh2C(sf$TAVG)'.

## Q5: Write a function to convert Fahrenheit to Celsius and convert the 
# average temperature. 
# Plot the average temperature over time and add a line to indicate the 
# average. 
# Analyse the data; what would you say about the average temperature in 
# San Francisco?

## Q6: Concerning climate change, one of the effects that the warming planet
# has on weather patterns is that there tend to be more extremes. 
# In other words, weather becomes more volatile. 
# We could see this in the data by looking at the difference between the 
# minimum and maximum temperature measured in a year. 
# Do this for the data and plot the results. 
# What are your conclusions?