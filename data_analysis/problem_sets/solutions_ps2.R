## Suggested solutions problem set 2

#### Question 1 (1) ####
# Find the the average value of the share of primary products exports (SXP) and 
# compare the GDP growth rate (GEA7090) for countries with above average
# values for the share of primary products exports with the average GDP growth 
# rate. 
# Which conclusion would you make here?
mean(sw$SXP)
mean(sw$GEA7090)
mean(sw[sw$SXP>mean(sw$SXP),]$GEA7090)

#### Question 2 (2) #### 
# As the paper's title suggests, Sachs & Warner examined the link between
# abudance in natural resources and economic growth. 
# Use the data to find the correlation between economic growth and natural 
# resources and analyse it. 
# Let's assume that the relation between the two can be described as:
# GEA7090=a+b*SXP. What would be the expected average GDP growth rate when the
# share of primary products exports is 0, and what if it was 1? 
cor(sw$GEA7090,sw$SXP)
b=cov(sw$GEA7090,sw$SXP)/var(sw$SXP)
a=mean(sw$GEA7090)-b*mean(sw$SXP)
a+b*0
a+b*1

#### Question 3 (3) ####
# Plot the share of primary products (SXP) versus the average GDP growth rate 
# and add a regression line to the plot. 
# (NB - You can do this using 'abline(MODEL)')
# Do you suspect there are outliers in the data?
# If so which, if not explain why.
plot(sw$SXP,sw$GEA7090,
     xlab="Primary products exports/GNP",ylab="Average annual GDP growth rate")

m1<-lm(GEA7090~SXP,sw)
abline(m1)

abline(v=.29)
sw_out<-sw[sw$SXP>=.29,]

#### Question 4 (3) ####
# Fit a regression model to the data estimating the effect of 
# primary products exports on the GDP growth rate, accounting for initial
# GDP level.
# What is the estimated effect of resource abundance on GDP growth?
# Inspect the fit of the model by plotting the observed versus the fitted
# values and the residuals versus the fitted values. 
# Analyse your results.
m2<-lm(GEA7090~SXP+LGDPEA70,sw)
summary(m2)

yhat<-fitted(m2) 
u<-m2$residuals  

par(mfrow=c(1,2),pty="s",las=1) # 'pty="s"' makes a square plot
plot(sw$GEA7090,yhat) 
plot(u,yhat) 

#### Question 5 (1) ####
# Repeat question 4 but not also accounting for the level of openess of the 
# economy. How does this model compare to the previous model?
m3<-lm(GEA7090~SXP+LGDPEA70+SOPEN,sw)
summary(m3)

yhat<-fitted(m3) 
u<-m3$residuals  

plot(sw$GEA7090,yhat) 
plot(u,yhat) 