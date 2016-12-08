#------------------------------------------------------------------------------
				# Tutorial 6: Regression analysis
#------------------------------------------------------------------------------

setwd("~/Dropbox/ucd/data_analysis") # Set working directory
par(las=1,bty="n")                   # Plot settings

# In this tutorial we are going to focus on linear regression to model the 
# relationship between an outcome variable and one or more explanatory
# variables. 
# In this case we will be using Ordinary Least Squares (OLS) which is probably
# one of the most commonly used regression methods. 
# For an overview on the history of regression see:
# https://priceonomics.com/the-discovery-of-statistical-regression/

#------------------------------------------------------------------------------
#### 1) Example using simulated data ####

# OLS is a linear regression method in which we try to model the 
# relationship between outcome variable Y and explanatory variable(s) X using
# a linear predictor function like Y=alpha+beta*X.
# We fit model to the data using a straight line to describe a relation, trying
# to minimise the error between the observed data and fitted values. 
# Let's check how this works by first examining this with randomly generated
# data where we actually know the exact data generating process. 
# In this case we ill generate Y as Y=2*X. 
set.seed(42);x=runif(1000) # Exogenous variable x
y=2*x                      # Outcome variable y

# For good measure we create some noise to add an extra degree of randomness 
# to both variables.
X=x+.1*rnorm(1000,0,1)
Y=y+.1*rnorm(1000,0,1)

# Time to fit the regression model which we do using the 'lm' function. 
# We save the output as an object and use 'summary' to see the output:
m1<-lm(Y~X)
summary(m1)

# Of interest here are the estimate and the standard error. 
# We see that in the model the coefficient for X has an estimate of 1.77, 
# which is not that far off its true value. 
# Below the the estimates are some model diagnostics such as the R^2, 
# a measure for the fit of the model. 
# We won't be dealing with these at the moment. 

# Let's examine the the model by looking at the estimated linear fit:
plot(X,Y)            # Plot the data
abline(m1,col="red") # Add the model

# To see what is actually stored in the linear model we can use 
# 'names':
names(m1) 

# We will focus on the fitted values which we can extract via 'm1$fitted.values'
# or we can simply use 'fitted'
yhat<-fitted(m1) 
u<-m1$residuals  # Extract residuals

# Now plot the observed outcome versus the fitted values, and the fitted values
# versus the errors. 
par(mfrow=c(1,2),pty="s",las=1) # 'pty="s"' makes a square plot
plot(Y,yhat) 
plot(u,yhat) 

# If a model provides a good fit, then we would expect to see almost a straight
# diagonal relation if we plot the actual outcome versus the fitted values. 
# In contrast, for the errors we want to see a very random pattern. 
# If the standard errors are not randomly distributed, this means that there is
# some sort of bias in the data which will affect the estimates. 
# You'll learn more about the technicalities this in the near future. 
# For now we just use it to check the model fit. 

#------------------------------------------------------------------------------
#### 2) Galton's idea (No, not that one) ####
# Francis Galton (1822-1911) was a Victorian polymath who made important 
# contributions to a number of scientific fields such as meteorology, biology, 
# crimonology, and statistics. 
# In statistics he created the concept of correlation and is probably best 
# known for his explanation on regression to the mean which he explained using
# data on children and parents' height (given in the csv-file).
# Let's have a look at the data that he collected:
dat<-read.csv("galton.csv",stringsAsFactors=FALSE)
str(dat)

hist(dat$childHeight,xlim=c(50,80),ylim=c(0,250),breaks=15)
hist(dat$midparentHeight,xlim=c(50,80),ylim=c(0,250),breaks=15)

# To examine hereditary effects in height we will examine the relation between
# the midparent height, and child height, where the latter naturally will be
# the outcome variable. 
# The variable midparent height is in this case constructed to account for 
# sex differences (see https://en.wikipedia.org/wiki/Midparent).
y<-dat$childHeight
x<-dat$midparentHeight

# Scatterplot of the data
dev.off();par(pty="s",bty="n")
plot(x,y,xlim=c(55,80),ylim=c(55,80),
     xlab="Midparent height",ylab="Child height")
abline(a=0,b=1,col="red")

## Q1: Describe the relation between child and midparent height. 
# If you were to fit a linear model to this data, what do you think would be
# the estimated parameter on the midparent height?

# Following Galton we have a look at the correlation, although he had to 
# calculate this all by hand; count yourself lucky. 
cor(y,x)

## Q2: Fit a linear regression to the model and check if your answer to 
# question 1 was correct. 
 

## Q3: As we discussed before, when we run a regression we fit a linear model
# to the data of the form Y=a+b*X. 
# In this model the estimated coefficient b is basically the covariance ('cov')
# of X and Y divided by the variance ('var') of X. 
# Calculate the coefficient for midparent hight using the variance and 
# covariance and also calculate the intercept. 


## Q4: Re-estimate the model normalising the variables by subtracting the mean.
# What changes in the estimates? Plot the data along with the regression line.

#------------------------------------------------------------------------------
#### 3) Diamonds are forever ####
# Diamonds are basicaly nothing more than carbon atoms nicely arranged in 
# a lattice as a result of pressure. 
# Nonetheless, thanks to smart marketing and stockpiling most of the diamond
# supply by a company called De Beers, prices for diamonds are rather high. 
# Also note that in contrast to Rihanna's assertion, diamonds don't shine. 
# It is just the light reflected in the diamond. 
# Anyway, let's get on with the analysis. 
# In the csv-file you find a large number of observations of diamonds that were
# sold along with their characteristics that are important determinants for a 
# diamonds rarity and thus price. 
# Such as the weight, the clarity, and the cut. 
# Examining the data we can see that some of the variables are 
# ordinal variables, such as color, clarity, and cut. 
diamonds<-read.csv("diamonds.csv",header=TRUE,sep=",")
str(diamonds)

# Let's first have a look at the pricing problem.
# In general we would expect that the price (given in US$) will increase when 
# the caratage, or weight increases. 
# We can plot this relation. Note that due to the size of the dataset this can
# take slightly longer than normal.
par(las=1,pty="m")
plot(diamonds$carat,diamonds$price)

## Q5: What do you notice about the relation between caratage and price?
# Fit a regression model to the data and summarise your results. 


## Q6: Continue the analysis by fitting a model to the data including variables
# for caratage, the cut, color, and clarity. 
# Analyse the results. 

# Because the variables for cut, color, and clarity are ordinal they are
# included as factors in the model meaning that the regression line is fit
# for each subcategory of the variable. 
# We can tansform the variables into a single measure based on their order
# using 'as.numeric'. 
# Before we do this it is very important to check the ordering of the levels.
# The following information is given about the variables:
# - cut. quality of the cut (Fair, Good, Very Good, Premium, Ideal) 
# - color. diamond colour, from J (worst) to D (best) 
# - clarity. a measurement of how clear the diamond is (I1 (worst), SI1, SI2, 
# VS1, VS2, VVS1, VVS2, IF (best)) 
levels(diamonds$cut)
levels(diamonds$color)
levels(diamonds$clarity)

# Only 'cut' is not ordered properly, so we create a re-order variable to
# deal with this:
diamonds$cut2<-factor(diamonds$cut,levels=c("Fair","Good","Very Good",
                                            "Premium","Ideal"))

# We can now fit the model
d3<-lm(price~carat+as.numeric(cut2)+as.numeric(clarity)+
         as.numeric(color),data=diamonds)

## Q7: Analyse the results. 

# One of the reasons why we use linear regression is to estimate the relation 
# between the outcome variable and one or more explanatory variables, with 
# the aim to find relations that can be generalised. 
# What ideally we want from a model is that it has a good fit, and what better
# fit is there when a model is accurate in prediction?
# We can test how well the model predicts the price. 
# First we have to split the data to have some out-of-sample data:
diamonds.is<-diamonds[1:35960,]    # In-sample data
diamonds.ous<-diamonds[-1:-35960,] # Out-of-sample data

# We fit the model to the in-sample data and use the estimated coefficients
# to predcit the outcome for the out-of-sample data. 
# To check the goodness-of-fit of the model we will use the 
# Root Mean Squared Error, i.e. for each predicted value we check how far it 
# is off the observed value. 
m1<-lm(price~carat,data=diamonds.is) # Fit the model
ous<-predict(m1,diamonds.ous)        # Predict the outcome
RMSE=function(m,o){sqrt(mean((m-o)^2,
                             na.rm=TRUE))} # Function for error
RMSE(ous,diamonds.ous$price)         # Calculate error

# Can see that the error is pretty large if we compare it to the 
# standard deviation of the outcome variable. 
sd(diamonds.ous$price)

## Q8: Redo the predictions but now account for other factors that might
# influence price. 
# Use both ordinal and continuous measures. 
# Which model is more accurate?