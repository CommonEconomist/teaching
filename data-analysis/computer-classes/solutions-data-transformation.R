## Q1:
colnames(rain_long)[2:3]<-c("date","precipitation")

## Q2:
par(mar=c(4.5,9.5,1,1),cex.axis=1.5,cex.lab=1.5)
hist(rain_long$precipitation)
boxplot(rain_long$precipitation~rain_long$province,
     xlab="Precipitation (mm)",ylab="",horizontal=TRUE)

# From a previous tutorial we might know that a gamma distribution would 
# probably best fit the data. 
# Based on the boxplot Limpopo seems to have the largest variability, although
# KwaZulu Natal and Mpumalanga also have wide distributions.
# The driest province is likely Northern Cape.

## Q3: Not really a pattern in the data except for the large swings from
# month to month. It also seem that over time there has been a drying trend. 
westcap.ts.a<-(westcap.ts-mean(westcap.ts))/sd(westcap.ts)
plot(westcap.ts.a)

# Not really a change with regard to the previous conclusions. 

## Q4: 
# Driest province is indeed Northern Cape.
rain.ds$var<-rain.ds$rain.m/rain.ds$rain.sd

# In absolute terms KwaZulu-Natal has the largest variability, but relative to
# the mean the Western Cape displays more variability.

## Q5: 
rain.n<-merge(rain_long,rain.ds,all.x=TRUE)
rain.n$rain.a<-(rain.n$precipitation-rain.n$rain.m)/rain.n$rain.sd
boxplot(rain.n$rain.a~rain.n$province,
     xlab="Anomaly",ylab="",horizontal=TRUE)

# Normalizing the distribution means that the precipitation patterns across
# provinces are much more comparable. 