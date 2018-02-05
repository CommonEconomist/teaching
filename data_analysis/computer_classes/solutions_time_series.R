## Solutions: time-series

## Q1: 
# Coffee prices are probably not random as they are subject to market mechanisms.
# The price will be influenced by various demand and supply side factors, such
# as the popularity of coffee or the effect of a drought on the harvest etc. 
# Adding the average to plot one might almost conclude that there is some 
# stationarity in the data as the prices fluctuate around this mean, but there 
# are some sharp trends in the data. For instance the large price increase in
# 2011, followed by an equally large decrease in the years after, or the large
# tumble in coffee prices in the late 1980s and beginning of the 1990s. 
abline(h=mean(arabica),lty=2,lwd=2)

## Q2: 
arabica.g=(arabica-lag(arabica,k=-1))/lag(arabica,k=-1)*100
plot(arabica.g,xlab="",ylab="Growth rate",axes=FALSE)
axis(1,tick=FALSE,at=seq(1980,2020,5),label=seq(1980,2020,5))
axis(2,tick=FALSE)
abline(h=mean(arabica.g),lty=2,lwd=2)

# The growth rate seems much more stationary.

## Q3:
arabica3<-ma(arabica,3)
arabica6<-ma(arabica,6)
arabica12<-ma(arabica,12)

plot(arabica,ylim=c(20,320),axes=FALSE,xlab="",ylab="Price (US$/tonnes)",
     lty=2,col="grey70",lwd=3)
axis(1,tick=FALSE,at=seq(1980,2020,5),label=seq(1980,2020,5))
axis(2,tick=FALSE)

lines(arabica3,lty=1,col="steelblue4")
lines(arabica6,lty=1,col="firebrick3")
lines(arabica12,lty=1,col="black")

# If we use short moving averages the data will mimick the short term 
# fluctuations in the original time-series. 
# Therefore, to get a better idea of the underlying trend, a longer moving 
# average would be better. 

## Q4:
# As you can see from the figure prices are relativley high in the beginning
# of the year compared to the later months. 
# This is the result of the harvest season. 

## Q5:
# The random component is simply the observed data minus the seasonality and 
# the trend. 
arabica.random=arabica-arabica.season-arabica.trend

## Q6:
# Here we just add everything together. 
arabica_recomposed=arabica.season+arabica.trend+arabica.random

plot(arabica,ylim=c(20,320),axes=FALSE,xlab="",ylab="Price (US$/tonnes)")
lines(arabica_recomposed,lty=2,col="red")
axis(1,tick=FALSE,at=seq(1980,2020,5),label=seq(1980,2020,5))
axis(2,tick=FALSE)

## Q7:
# The result of the 'decompose' function is not entirely similar to our own 
# attempt. 
# This is largely the result of how the trend is estimated. 
# You can see that for the R-function this trend is much smoother than our
# cruder moving average approach. 

## Q8: 
# So here we plot the coffee prices for arabica coffee versus that of robusta. 
# You can see that arabica coffee is more expensive, this is because there is
# a higher demand for this coffee. 
# The two time-series do roughly follow the same pattern. 
# We do observe that for 2014 there was a break where the coffee prices for
# arabica exhibit much more volatility compared to the robusta prices. 
# Amongst other this is the result of an increase in demand for robusta coffee.
