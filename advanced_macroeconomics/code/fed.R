## Macroeconomics identification
setwd('~/Dropbox/github/teaching/advanced_macroeconomics')
x<-read.csv('data/fed.csv')
ir<-ts(x$rate,start=c(1999,1),frequency=12)

par(mar=c(5,5,2,2),bty='n',las=1,cex.axis=2,cex.lab=2)
plot(ir,lwd=2,axes=FALSE,xlab='',ylab='')
axis(1,tick=FALSE);axis(2,tick=FALSE)
text(2015,6,'Interest rate',cex=2)

# Example regression
gdp<-read.csv('data/GDPCTPI.csv')
gdp<-ts(gdp[,2],start=c(1947,1),frequency=4)
gdp<-window(gdp,start=c(1999,1))
ir<-aggregate(ir,nfrequency=4,FUN=mean)

x<-as.vector(diff(ir,lag=1))
y<-as.vector(diff(gdp,lag=1))

plot(x,y,pch=19,cex=1.5,axes=FALSE,xlab='D.i',ylab='D.gdp')
axis(1,tick=FALSE);axis(2,tick=FALSE,line=-1)
abline(lm(y~x),lwd=1.5,lty=2)
