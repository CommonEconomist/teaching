#------------------------------------------------------------------------------
				# Tutorial 4: Time-series data
#------------------------------------------------------------------------------

setwd("~/Dropbox/ucd/data_analysis/") # Set appropriate directory
par(las=1,bty="n")                    # Settings for plots

# Time series are a very common type of data in science, and also in economics. 
# Although the data type itself is pretty straightforward, using the data
# can bring some complications. 
# In this tutorial we will be analysing time series data and process it to account # for seasonality and trends

#------------------------------------------------------------------------------
#### 1) International coffee prices ####
# In the csv-file you will find monthly international prices for two types of 
# coffee: Arabica and Robusta. 
# Arabica is a high quality bean and the bulk of international trade is in 
# this coffee variety. 
# Robusta is a coffee variety that is popular for processin into instant coffee. 
# The data is taken from the IMF and covers the period from January 1980 up 
# untill September 2016. 
# Data source: http://www.imf.org/external/np/res/commod/index.aspx
coffee<-read.csv("data/coffee.csv",header=TRUE,sep=",")
summary(coffee)

# There are different libraries for working with time-series data in R, but
# here we will just use the standard 'ts' function. 
# Using 'ts' we can write the data from the data frame to a time series object,
# indicating the start of the period and the frequency of the data. 
# Let's start with having a look at the data for Arabica, which is the coffee
# of choice for the espresso cognoscenti. 
arabica<-ts(coffee$arabica,start=c(1980,1),frequency=12)
plot(arabica,ylim=c(20,320),axes=FALSE,xlab="",ylab="Price (US$/tonnes)")
axis(1,tick=FALSE,at=seq(1980,2020,5),label=seq(1980,2020,5))
axis(2,tick=FALSE)

## Q1: 
# Explain whether the fluctuations in the coffee price are random.
# Add the mean of the data to the plot and explain if you think whether
# the data is stationary. 

# Besides price level we can also have a look at the growth rate. 
# To calculate the lag of the time series simply use 'lag'. 

# Q2: 
# Calculate the growth rate and plot the data. 
# Add the mean growth rate to the plot and analyse the data. 
# Is this data stationary?

# Before we continue with further processing and analysing the data,
# we remove all observations in 2016 to make things a little bit easier. 
# We use the 'window' function to do this. 
arabica<-window(arabica,start=c(1980,1),end=c(2015,12))

# Time series data often exhibit trends and seasonality. 
# These are things we have to account for in the analysis. 
# We can decompose time series by using a moving average to smooth the data, 
# and separate it into a trend and irregular component. 
# Let's try this with the coffee data.
# But first we have to create a function to construct the moving average:
ma<-function(x,n)filter(x,rep(1/n,n),sides=2)

# This functions takes two arguments: 'x' is the data and 'n' the order of the 
# moving average. 
# Which in the case of our data is the number of months on either side. 

## Q3: 
# Calculate moving averages with order 3, 6, and 12, and plot them 
# along with the original data. 
# Which moving average best captures the trend?

# We are now going to decompose the data accounting for the seasonality and 
# the trend such that we are left with the random component. 
# First we detrend the data accounting for seasonality. 
# Since coffee is an agricultural products, prices are likley influenced by
# the harvest cycle as well as stock, etc.
# Here we will be using the moving average of order 12 to detrend the data.
arabica.trend<-ma(arabica,12)
arabica.dt<-arabica-arabica.trend

plot(arabica.dt,ylim=c(-50,110),axes=FALSE,xlab="",ylab="")
abline(h=mean(arabica.dt,na.rm=TRUE),lty=2,lwd=2)
axis(1,tick=FALSE,at=seq(1980,2020,5),label=seq(1980,2020,5))
axis(2,tick=FALSE)

# We use the detrended data as input to calculate seasonality. 
# This is done by feeding the time series into a matrix where each column
# contains the elements of the same period. 
# The month in this case.
# Then the average for each column is calculated and used to create another
# time series object. 
m_arabica=t(matrix(data=arabica.dt,nrow=12))
s_arabica=colMeans(m_arabica,na.rm=TRUE)
arabica.season<-ts(rep(s_arabica,36),start=c(1980,1),freq=12)

## Q4: 
# Before we continue, let's analyse the seasonality in the data.
# Which conclusions do you draw from the data?
plot(s_arabica,type="l")

# We have already extracted most data from the original time series meaning 
# that there is only the random component left. 
# Coffee prices are additive so the time series can be determined by:
# coffee_price= seasonal + trend + random.

## Q5: Get the random component (call it 'arabica.random')

# Let's plot the different components:
par(mfrow=c(4,1),mar=c(2,5,.5,.5),bty="n",las=1)
plot(arabica,ylab="observed",axes=FALSE)
axis(2,tick=FALSE)
plot(arabica.dt,ylab="trend",axes=FALSE)
axis(2,tick=FALSE)
plot(arabica.season,ylab="seasonal",axes=FALSE)
axis(2,tick=FALSE)
plot(arabica.random,ylab="random",axes=FALSE)
axis(2,tick=FALSE)

## Q6: Reconstruct the time series and plot it against the observed data

# Instead of decomposing the time series by hand we can also use
# the 'decompose' function. 
## Q7: Does this function produce the same results as yours? Why or why not?
arabica_decomposed<-decompose(arabica)
plot(arabica_decomposed)

#------------------------------------------------------------------------------
#### 2) Arabica vs. Robusta ####
robusta<-ts(coffee$robusta,start=c(1980,1),frequency=12)
arabica.p<-window(arabica,start=c(2010,1),end=c(2015,12))
robusta.p<-window(robusta,start=c(2010,1),end=c(2015,12))

## Q8: Analyse the data in the following plot:
plot(arabica.p,ylim=c(80,320),axes=FALSE,xlab="",ylab="Price (US$/tonnes)")
lines(robusta.p,col="blue")
axis(1,tick=FALSE)
axis(2,tick=FALSE)

