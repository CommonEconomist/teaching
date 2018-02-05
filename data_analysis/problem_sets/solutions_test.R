## Suggested solutions test

#### Question 1 (1) ####
# Which type of probability distribution do you think would best fit the food
# supply data? 
hist(x$food_sup_total)

#### Question 2 (1) ####
# Plot the time-series for food supply levels for Indonesia and briefly
# discuss the data. 
idn<-ts(x[x$country=="Indonesia",]$food_sup_total,start=c(1962,1),freq=1)
plot(idn)

#### Question 3 (2) ####
# To examine the trend in food supply levels over time, use the boxplot to
# visualise the distribution of food supply levels for each year and discuss
# whether you notice any trends or patterns in the data. 
boxplot(food_sup_total~year,x)

#### Question 4 (3) ####
# How would you describe the relation between income levels and food supply and 
# regime type and food supply?
plot(x$gdpcap,x$food_sup_total,log="x")
boxplot(food_sup_total~regime,x,horizontal=TRUE)

#### Question 5 (2) #### 
# You're asked to estimate the effect of regime type and income on food 
# supply levels. 
# Unfortunately, due to a change in methodology related to measuring the food
# supply levels you can only use data from 1991 onwards. 
# Additionally, you have to rescale the GDP data to be measured in 1000s. 
# Estimate the regression models and discuss the estimated coefficients as well
# as the fit of the model. 
x<-x[x$year>1990,]
x$income=x$gdpcap/1000

m1<-lm(food_sup_total~regime+income,x)
summary(m1)

par(mfrow=c(1,2),pty="s",las=1)
plot(x$food_sup_total,fitted(m1))
abline(a=0,b=1,col="red")
plot(m1$residuals,fitted(m1)) 

#### Questions 6 (1) ####
# Re-estimate your model from question 5 but now include the lagged outcome
# variable. 
# How does this alter the results?
m2<-lm(food_sup_total~regime+income+food_sup_total.l,x)
summary(m2)

plot(x$food_sup_total,fitted(m2))
abline(a=0,b=1,col="red")
plot(m2$residuals,fitted(m2)) 
