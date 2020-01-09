#------------------------------------------------------------------------------
				# Problem set 2: Natural resource abundance and economic growth
#------------------------------------------------------------------------------
#----------------------------------------------------------
#### IMPORTANT ####
# Write down your code and answers in the script. 
# When finished send it to: stijn.vanweezel@ucd.ie
#----------------------------------------------------------

# For the second problem set you are going to replicate one of the seminal
# works in economics, examining the link between natural resource abundance
# and economic growth. 
# The paper "Natural resource abundance and economic growth" 
# (Sachs and Warner, 1997) has been cited close to 4500 times, and it wasn't
# even published in a journal. 
# You might actually heard of one of the authors as Sachs recently received 
# the  Ulysses medal from UCD.
# The data contains information for 91 countries on the following variables:

# DTT7090: Average annual growth of the natural logarithm of the external 
# terms of trade between 1970 and 1990
# GEA7090: Average annual growth of purchasing power adjusted GDP per person 
# aged 15–64 (economically active population) between the years 1970 and 1990.
# LGDPEA70: Natural logarithm of real GDP per person aged 15–64 in 1970.
# LINV7089: Natural logarithm of the ratio of real gross domestic investment 
# (public plus private) to real GDP, averaged over the period 1970–1989.
# RL:Index for rule of law ranging from 0 (low) to 6 (high) measured as of 1982.
# SOPEN: The fraction of years during 1970–1990 in which SW rate an 
# economy as open.
# SXP: Share of primary products exports in GNP in 1970 (SW, 1997a).     

# Use the data to answer the question below. 
# You can write your answers, and the code you used to get the answer in the 
# script itself. 
load("sachs_warner.RData")

#### Question 1 (1) ####
# Find the the average value of the share of primary products exports (SXP) and 
# compare the GDP growth rate (GEA7090) for countries with above average
# values for the share of primary products exports with the other countries. 
# Which conclusion would you draw here?
#--------------------------------------
# ANSWER: 


#--------------------------------------
#### Question 2 (2) #### 
# As the paper's title suggests, Sachs & Warner examined the link between
# abudance in natural resources and economic growth. 
# Use the data to find the correlation between economic growth and natural 
# resources and analyse it. 
# Let's assume that the relation between the two can be described as:
# GEA7090=a+b*SXP. 
# What would be the expected average GDP growth rate when the
# share of primary products exports is 0, and what if it was 1? 
#--------------------------------------
# ANSWER: 


#--------------------------------------
#### Question 3 (3) ####
# Plot the share of primary products versus the average GDP growth rate 
# and add a regression line to the plot. 
# (NB - You can do this using 'abline(MODEL)')
# Do you suspect there are outliers in the data?
# If so which, if not explain why.
#--------------------------------------
# ANSWER: 


#--------------------------------------
#### Question 4 (3) ####
# Fit a regression model to the data estimating the effect of 
# primary products exports on the GDP growth rate, accounting for initial
# GDP level (LGDPEA70).
# What is the estimated effect of resource abundance on GDP growth?
# Inspect the fit of the model by plotting the observed versus the fitted
# values and the residuals versus the fitted values. 
# Analyse your results.
#--------------------------------------
# ANSWER: 


#--------------------------------------
#### Question 5 (1) ####
# Repeat question 4 but now also accounting for the level of openess of the 
# economy (SOPEN). 
# How does this model compare to the previous model?
#--------------------------------------
# ANSWER: 


