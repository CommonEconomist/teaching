## Q1:
Normal<-rnorm(1000,2,1)  
Uniform<-runif(1000,0,10) 
Poisson<-rpois(1000,2)                
Gamma<-rgamma(1000,2)         
par(mfrow=c(2,2)) # Plot with 2 rows and 2 columns. 
hist(Normal);hist(Uniform);hist(Poisson);hist(Gamma)

## Q2: 
# Normal: length of population
# Uniform: Chance of rolling a number on a dice
# Poisson: Goals scored during football match
# Gamma: Precipiation levels

## Q3: 16%
set.seed(42);x<-rnorm(150000,182.5,6)
hist(x)
abline(v=mean(x),col="red")

## Q4:
m<-182.5-169
dev<-sqrt(6^2+2^2)
pnorm(0,m,dev)*8500000

## Q5: 
# If we look at the boxplot, the number of outliers in each will be an 
# indication of the size of the sample.
# When we generate more data, the probability of sampling extreme values is 
# slightly higher. 

## Q6: No because there is a lot of variation in the data with relatively
# small whales and very larges ones. As such the average will be relatively 
# large compared to the median for instance.

## Q7: The Physeteridae family consists of three species, two relatively small
# and one rather large. 

## Q8: The fatality data follows a Poisson distribution. 

## Q9: 
pois<-rpois(nrow(prussia),mean(prussia$deaths))
P<-as.vector(table(pois))
perc2<-P/sum(P)*100
lines(0:4,perc2,lty=2,type="b") 

## Q10:
kicks.c<-aggregate(deaths~corps,prussia,sum)
# Avoid corps XI as average is 11 higher than average.
# Corps below average are: 
kicks.c[kicks.c$deaths<mean(kicks.c$deaths),]$corps
