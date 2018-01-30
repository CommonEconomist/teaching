## Growth European countries
par(mar=c(5,5,2,2),bty='n',las=1,pty='m',cex.lab=2,cex.axis=2,cex.main=2)

# Libraries
library(pwt9)
library(countrycode)
library(plyr)

ma<-function(x,n=5){stats::filter(x,rep(1/n,n),sides=1)} # Function moving average
data("pwt9.0");pwt9.0 # Get data

# Aggregate data
pwt9.0$region<-countrycode(pwt9.0$isocode,'iso3c','continent',warn=TRUE)
eur<-ddply(pwt9.0[pwt9.0$region=='Europe',],.(year),summarise,
           Y=sum(rgdpna,na.rm=TRUE),emp=sum(emp,na.rm=TRUE))

# Calculate growth
dY=ts(diff(log(eur$Y/eur$emp))*100,start=1950)
dY.ma<-ma(dY)

# Plot data
plot(dY,lwd=.5,type='b',col='grey50',axes=FALSE,
     xlab='',ylab='dY',main='Growth in European countries')
lines(dY.ma,lwd=2)
axis(1,tick=FALSE)
axis(2,tick=FALSE,line=-1)
