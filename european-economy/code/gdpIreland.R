# GDP ireland 
library(zoo)
library(mFilter)
par(mar=c(5,5,4,2),las=1,bty="n",cex.lab=2,cex.axis=2,cex.main=2) 

d<-read.csv('data/gdp_ireland_q.csv')
gdp<-ts(d[,2],start=c(1997,1),frequency=4)

# Plot
plot(gdp,type='l',lwd=2,xlab='',ylab='',main='Ireland GDP',axes=FALSE,log='y') 
axis(1,tick=FALSE)
axis(2,tick=FALSE,line=-1)

lines(hpfilter(gdp,freq=1600)$trend,lwd=2,lty=2)


# Detrend
gdp.hp<-hpfilter(log(gdp),freq=1600)$cycle
plot(gdp.hp,lwd=2,xlab='',ylab='',axes=FALSE,main='Ireland GDP cycles')
abline(h=0,lty=3)
axis(1,tick=FALSE);axis(2,tick=FALSE,line=-1)

