#------------------------------------------------------------------------------
				# Tutorial 3: Data transformation
#------------------------------------------------------------------------------

setwd("~/Dropbox/ucd/data_analysis") # Set working directory
par(las=1,bty="n")                   # Plot settings

# Unless you're a fancy professor, it is unlikely that the data you work with
# comes in a ready to use form. 
# Often we have to clean the data and apply some transformations to get it into
# a desired format and variables we can work with. 
# In this tutorial we will be focusing on the data transformation process, which
# can be tedious but is a necessary step in data analysis. 
# Indeed, choices we make in this stage influece the further analysis. 

#------------------------------------------------------------------------------
#### 1) Rainfall in South Africa ####
# In the file that you've downloaded you're given monthly precipitation data for # South Africa between 1979-2014. 
# The data comes from NASA's Global Precipiation Climatology Project and is 
# provided on a grid with 2.5x2.5 degree cells.
# I have been so nice to already aggregate the data to the province level. 
# The data is what we call time-series cross-section data, i.e. for a number
# of cross-sections (provinces) we have repeated observations over time. 
rain<-read.csv("precipitation.csv",header=TRUE,sep=",",stringsAsFactors=FALSE)

# As you can see, the data is in an unhelpful format:
# We have 9 observations and 433 variables. 
# The data is in what we call wide format; the repeated observations are
# on the columns whereas the cross-sections are on the rows. 
# For data preparation and analysis it would be more useful to have the data 
# in long format. 
# That is one variable on the column (precipitation in this case) and the 
# repeated observations per province on the rows. 
# (See for background: http://vita.had.co.nz/papers/tidy-data.pdf)
# Let's make that happen. 
# First we with given the columns some meaningful names.
# The first column is the province whereas all the other columns are months. 
colnames(rain)[1]<-"province"  # First column is province
colnames(rain)[-1]<-as.character(seq(as.Date("1979/1/1"),
                               as.Date("2014/12/1"),"months")) # Others months
# Note the negative indexing here. 
# We're now going to reshape the data for which we need the 'reshape' package.
library(reshape) # Loads package
rain_long<-melt(rain,"province") # Check '?melt' to see function details. 

# Let's see what the data looks like now:
str(rain_long)
table(rain_long$province)
head(rain_long)

# The reshape function has created two variables: 'variable' and 'value'. 
# 'variable' in this case is the date and 'value' the rainfall data. 
# Annoyingly the variable for data is a factor, which we need to change:
rain_long$variable<-as.Date(rain_long$variable,format="%Y-%m-%d")
class(rain_long$variable)

# For future reference, let's add a numeric indicator for the month
rain_long$month<-as.numeric(format(rain_long$variable,format="%m"))

## Q1: Give the columns meaningful names like "date" and "precipitation". 
## Q2: Examine the rainfall distribution, both at the aggregate level and at
# the province level.  
# Which kind of distribution do you think would best fit the rainfall data?
# Give the boxplot of precipitation per province.
# Which province has the largest rainfall variability?
# Which is the driest province?

# As you probably found out, there is quite some variation in precipitation
# across the provinces. 
# Let's zoom in on a single province, Western Cape, and create a time-series
# object.
westcap<-rain_long[rain_long$province=="Western Cape",]$precipitation
westcap.ts<-ts(westcap,start=c(1979,1),frequency=12)
plot(westcap.ts)

## Q3: Analyse the data focusing on patterns you might see exhibited in the 
# time-series. 

## Q4: Now as you can see having done the previous question, there is quite 
# some variation in rainfall over time even within provinces. 
# In the climate literature one measure they use for rainfall shocks are 
# anomalies. 
# An anomaly is calculated by subtracting the long-term mean from the 
# observation and dividing by the standard deviation. 
# This creates a normalised measure. 
# Apply this measure for the Western Cape data and plot your results. 
# Does your previous analysis change?

# To calculate anomalies for each province we can use the 'plyr' package to 
# calculate summary statistics for each province. 
# We could also use R's built in 'aggregate' function but 'ddply' is a bit more 
# versatile. 
library(plyr)
rain.ds<-ddply(rain_long,.(province),summarise,
               rain.m=mean(precipitation),
               rain.sd=sd(precipitation))

## Q4: Referring back to question 2: which is the driest province and which
# province has the largest variability? 
# Is there a relation between average rainfall and its standard deviation?

# Add summary statistics to the main data frame
rain_long<-merge(rain_long,rain.ds,all.x=TRUE) 

## Q5: For each province calculate the anomalies from the long term mean 
# and plot the results in a boxplot. 
# Analyse precipitation patterns in South Africa. 

## Q6: Repeat question 6 but instead of looking at the long-term mean, now take
# into account the monthly variation. I.e. anomalies from the monthly mean. 