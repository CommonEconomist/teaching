par(mar=c(5,5,3,2),bty='n',las=1,cex.lab=2,cex.axis=2,cex.main=2)
library(quantmod)
library(dplyr)


# Calculate inflation rate
getSymbols('GDPCTPI',src='FRED')
GDPCTPI$L<-lag(GDPCTPI$GDPCTPI,lag=1)
pi=400*(log(GDPCTPI$GDPCTPI/GDPCTPI$L))

pi<-ts(pi,start=c(1947,1),frequency=4)
plot(pi)

# Unemployment rate
getSymbols('UNRATE',src='FRED')
u<-ts(UNRATE$UNRATE,start=c(1947,1),frequency=12)
u<-aggregate(u,nfrequency=4,FUN=mean)

# Plot data
plot(pi,lwd=2,axes=F,xlab='',ylab='')
lines(u,lty=2,lwd=2,col='blue')
axis(1,tick=F)
axis(2,tick=F)

text(2010,-1,'Inflation',cex=2)
text(2010,11,'Unemployment',cex=2)


x<-as.vector(u[2:280])
y<-as.vector(pi[2:280])

lw<-loess(y~x)
j<-order(x)

par(pty='s')
plot(y~x,cex=1,axes=F,xlab='Unemployment (%)',ylab='Inflation (%)')
lines(x[j],lw$fitted[j],lwd=2)
axis(1,tick=F)
axis(2,tick=F,line=-2)
