# Suggested solutions tutorial 6

## Q1:
# Rather hard to tell. There does seem to be some sort of positive relation
# between the effect is not very profound. 

## Q2:
m2<-lm(y~x)
summary(m2)

# There is indeed a positive relation. 
# Here for each inch of increase in midparent height, the height of the child
# increases by 0.64 inches. 

## Q3:
# We know that the regression model has the form Y=a+b*X.
# Therefore a can be calculated as a=Y-b*X. 
# First we have to find out b:
b=cov(x,y)/var(x)

# To calculate a we use the mean for Y and X;
a=mean(y)-b*mean(x)

# The intercept is 22.6, which means that with a midparent height of 0 the child
# would have a height of 22.6. Which is a bit absurd of course. 

## Q4:
y.z<-y-mean(y)
x.z<-x-mean(x)
m2a<-lm(y.z~x.z)
summary(m2a)

# Can see that the estimate for the effect of midparent height stays the same,
# only the intercept is now -roughly- zero. 
plot(x.z,y.z,xlim=c(-11,13),ylim=c(-11,13))
abline(lm(y.z~x.z),col="red")

## Q5:
# There seems to be a positive relation between caratage and price, i.e. 
# heavier diamonds are worth more. 
d1<-lm(price~carat,data=diamonds)
summary(d1)

## Q6:
d2<-lm(price~carat+cut+clarity+color,data=diamonds)
summary(d2)

# Note that some of the variable we included in the model are not continious
# or even integers but ordinal. 
# As a result R will give an estimate of the effect per category. 
# So we see for instance that a diamond with color J will be less worth compared
# to a diamond with color E, all else equal. 
# Similarly a Ideal cut is better than a Good cut. 

## Q7:
summary(d3)
# We are now able to analyse the results on a continuous scale. 
# The results show that price will increase when we have a higher carat, level
# of cut, and clarity, and a lower color. 

## Q8:
# So here we use the two models we already estimate.
# Let's start with the model where we didn't adjust the levels:
ous2<-predict(d2,diamonds.ous)
RMSE(ous2,diamonds.ous$price)
# Still a relatively high error but smalle compared to the baseline model. 

ous3<-predict(d3,diamonds.ous)
RMSE(ous3,diamonds.ous$price)
# Here we also see a drop in the error, but not as large as in the other model. 
# So for prediction it seems to be better to keep the variables as they are and
# not use continuous data. 

