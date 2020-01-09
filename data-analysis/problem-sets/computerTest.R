#------------------------------------------------------------------------------
				# Computer test: Food supply, regime type, and income
#------------------------------------------------------------------------------

#----------------------------------------------------------
#### IMPORTANT ####
# Write down your code and answers in the script. 
# When finished send it to: stijn.vanweezel@ucd.ie
#----------------------------------------------------------

# The dataset for this test contains information for 99 countries
# in Africa, Asia, and Central and South America on food supply levels,
# regime type, and income levels.
# Food supply levels ('food_sup_total' and temporal lag 'food_sup_total.l') are
# measured in kcal/day/capita. 
# For income levels we use GDP per capita ('gdpcap') measured in 
# constant U.S. dollars 
# Regime type ('regime') is given by an ordinal variable on a 1-5 scale, where 
# 1 is a full autocracy, 2 partial autocracy, 3 partial democracy, 
# 4 partial democracy with factionalism, and 5 is a  democracy. 
# You don't need to understand all these categories, only that moving 
# from 1 to 5 regimes become more democratic. 
# The variable 'time' is a year indicator whereas 'year' is the actual year. 
# Use the data to answer the questions below (points given in parenthese).
load("food_supply.RData")

#### Question 1 (1) ####
# Which type of distribution do you think would best fit the food
# supply data? 

#### Question 2 (1) ####
# Plot the time-series for food supply levels for Indonesia and briefly
# discuss the data. 

#### Question 3 (2) ####
# To examine the trend in food supply levels over time, use the boxplot to
# visualise the distribution of food supply levels for each year and discuss
# whether you notice any trends or patterns in the data. 

#### Question 4 (2) ####
# How would you describe the relation between i) income levels and food supply
# and ii) regime type and food supply?

#### Question 5 (3) #### 
# You're asked to estimate the effect of regime type and income on food 
# supply levels. 
# Unfortunately, due to a change in methodology related to measuring the food
# supply levels you can only use data from 1991 onwards. 
# Additionally, you have to rescale the GDP data to be measured in 1000s. 
# Estimate the regression models and discuss the estimated coefficients as well
# as the fit of the model. 

#### Question 6 (1) ####
# Re-estimate your model from question 5 but now include the lagged outcome
# variable. 
# How does this alter the results?




