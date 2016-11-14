#------------------------------------------------------------------------------
				# Tutorial 5: Exploratory data analysis
#------------------------------------------------------------------------------

setwd("~/Dropbox/ucd/data_analysis") # Set working directory
par(las=1,bty="n")                   # Plot settings
options(scipen=4)

# An important, but sometimes overlooked, part of statistical analysis is the
# exploratory data analysis. 
# Before we use formal statistical models it can help to visualise the data
# in order to get descriptive evidence for the relation we're interested in or
# summarise the data. 
# Exploratory data analysis is an iterative cycle where we 
# (i) generate questions about the data, (ii) search for answers by visualising
# and transforming the data, and (iii) and refine questions based on what we've
# see. 
# In this tutorial we are going to work with some data from the World Bank,
# the World Development Indicators. 
# When you load the dataset, most variable names will be pretty 
# self-explanatory.
# Just for clarification 'pc' means per capita and '.l' indicates the lag
# of a variable. 
# Sometimes we don't have data for a particular observation, these cases are
# indicated with 'NA'. 
load("WDI.RData")

#------------------------------------------------------------------------------
#### 1) Socio-economic well-being ####
# Public discussion often center on the Gross Domestic Product (GDP). 
# Now the GDP can be a useful yardstick, but as Robert Kennedy said, 
# it measures everything except that is worthwile. 
# The philiposphical ramifactions of this statement being that there is more 
# to life than being productive. 
# GDP as you might know is simply a monetary measure of of all the final goods 
# and services produced in a country. 
# Simplified it measures productivity. 
# Now, if you have a larger population, that might entail that you will have 
# a higher productivty as you have more people to produce stuff. 
# Let's see if this is true using a scatterplot.
# For now, we will focus on a single year: 2015
wdi15<-wdi[wdi$year==2015,]
plot(wdi15$population,wdi15$gdp_constant,xlab="Population",ylab="GDP")

# Well, that's not very helpful is it?
# Due to the large scale differences in the data we get this monstrosity of 
# a plot. 
# To improve we take the log of both GDP and population
plot(wdi15$population,wdi15$gdp_constant,log="xy",xlab="Population",ylab="GDP")

# That's better. 
# The figure illustrates that there is some sort of linear relation between
# population and GDP, or productivity. 
# But does this also mean that a country with a large population such as 
# India is richer compared to for instance Belgium?
# Let's first check this with this data:
points(wdi15[wdi15$country=="India",]$population,
       wdi15[wdi15$country=="India",]$gdp_constant,pc=19,col="blue")
points(wdi15[wdi15$country=="Belgium",]$population,
       wdi15[wdi15$country=="Belgium",]$gdp_constant,pc=19,col="red")

# In absolute terms this seems to be the case. 
# But you can observe by the position of the dots that Belgium has a rather high
# GDP for it's population. 
# Let's see how things changes when we account for population size by using
# GDP per capita. 
plot(wdi15$population,wdi15$gdp_pc_constant,log="xy",
     xlab="Population",ylab="GDP per capita")
points(wdi15[wdi15$country=="India",]$population,
       wdi15[wdi15$country=="India",]$gdp_pc_constant,pc=19,col="blue")
points(wdi15[wdi15$country=="Belgium",]$population,
       wdi15[wdi15$country=="Belgium",]$gdp_pc_constant,pc=19,col="red")

## Q1: What would be your conclusions concerning the relation between GDP
# and population?
# Which country has the highest GDP per capita and which the lowest?

## Q2: In an important area of research in economics is the relation with
# urbanisation. 
# Urbanisation often coincides with a shift from an agricultural to 
# a more industrial society meaning increases in productivity and higher
# income levels. 
# Does the data show a relation between urbanisation, income, and income growth?

# We continue by examing the relation between income and other development
# indicators such as the infant mortality rate. 
# We plot the data, again using the natural log to account for scale
# differences. 
## Q3: Which conclusions do you draw from the data?
plot(wdi15$gdp_pc_constant,wdi15$under5_mortality_per1000,log="xy",
     xlab="GDP per capita", ylab="Infant mortality rate (per 1000)")

# We can check if there are any systematic differences between continents. 
# We start with looking at the European countries:
points(wdi15[wdi15$continent=="Europe",]$gdp_pc_constant,
      wdi15[wdi15$continent=="Europe",]$under5_mortality_per1000,
      col="blue",pch=19)

# And add the African countries:
points(wdi15[wdi15$continent=="Africa",]$gdp_pc_constant,
      wdi15[wdi15$continent=="Africa",]$under5_mortality_per1000,
      col="black",pch=19)

## Q4: What would you conclude from comparing the data for Europe with that
# of Africa?
# How do the other continents compare? 

#------------------------------------------------------------------------------
#### 2) Public health ####
# Over the past decades great progress has been made in improving 
# global public health. 
# There have been the succes stories of eradicating certain diseases in large 
# parts of the world, as well as improving access to health facilities. 
# As a result we have seen that globally life expectancy has increase. 
# We can check this with the data using the familiar boxplot, to 
# visualise the trend over time:
boxplot(wdi$life_expectancy~wdi$year)

# The figure shows that the first quartile during the 2010s is about where
# the average was in the mid-1960s. 
# Despite the strong upward trend, there are some notable outliers during
# the 1970s and the 1990s. 
# We start by examining the outliers in the 1990s:
table(wdi[wdi$year>=1988 & wdi$life_expectancy<40,][,c("country","year")])

# The data shows that two African countries reported very low life expectancies:
# Rwanda and Sierra Leone. 
# We can add a country-specific trend line to the boxplot to observe the 
# individual progress for each country. 
lines(wdi[wdi$country=="Rwanda",]$life_expectancy,col="red")
lines(wdi[wdi$country=="Sierra Leone",]$life_expectancy,col="blue")

## Q 5:
# Analyse the trend in life expectancy of Sierra Leone relative to that of 
# Rwanda.
# What is the main conclusion you draw from the data?

# We examine the development for Rwanda in a bit more detail. 
# It would be interesting to know what the particular cause is for the decline
# in life expectancy. 
# We can do this by examining other countries with known problems and see
# whether they have a similar trend line. 
# One explanation could be that the decrease is the result of a severe economic
# crisis as higher incomes are often associated with better health outcomes. 
# One country that has experienced severe economic difficulties in the past 
# decade is Zimbabwe:
lines(wdi[wdi$country=="Zimbabwe",]$life_expectancy,col="black")

# Another possible cause of the drop in life expectancy could be the 
# AIDS epidemic which hit Sub Sahara Africa particularly hard. 
# As an example we can look at Bostwana, which is a country with a relativley 
# high income level but also a very high AIDS prevalence rate among adults of
# 25%:
lines(wdi[wdi$country=="Botswana",]$life_expectancy,col="green")

# Finally, the decline could also be due to civil war. 
# Rwanda has experienced one of, if not the, largest outbreaks of violence
# since the end of the Second World War, with the Rwandan Genocide decimating
# the population. 
# Although the Rwandad Genocide was particular severe, other countries have also
# experienced civil war with comparable levels of violence, such as Cambodia:
lines(wdi[wdi$country=="Cambodia",]$life_expectancy,col="purple")

## Q6:
# What do you think explains the drop in life expectancy in Rwanda?
# Feel free to add additional lines. 

# Let's focus on another health outcome: the under-5 mortality rate.
boxplot(wdi$under5_mortality_per1000~wdi$year)

# We observe a similar trend to life expectancy, the health situation in many
# countries have improved over time leading to lower infant mortality rates. 
# There are still some outliers here and there.
# Can check if Rwanda is one of them:
lines(wdi[wdi$country=="Rwanda",]$under5_mortality_per1000,col="red")

## Q7: 
# Find out which countries are reponsible for the main outliers. 
# Which causes do you think are there for these outliers?
