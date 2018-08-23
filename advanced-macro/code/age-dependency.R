## Age dependency
setwd('~/github/teaching/advanced_macroeconomics')
par(mar=c(5,5,2,2),bty='n',las=1,cex.axis=2,cex.lab=2,cex.main=2)

require(WDI)
dependency<-WDIsearch("dependency ratio",field="name",short=FALSE)
wdi2<-WDI(c("US","DE"),dependency[,1],start=1961,end=2015)

# Data to time series
US.o<-ts(wdi2[wdi2$iso2c=="US",][,5],start=c(1961,1),freq=1)
US.y<-ts(wdi2[wdi2$iso2c=="US",][,6],start=c(1961,1),freq=1)
DE.o<-ts(wdi2[wdi2$iso2c=="DE",][,5],start=c(1961,1),freq=1)
DE.y<-ts(wdi2[wdi2$iso2c=="DE",][,6],start=c(1961,1),freq=1)

# Age dependency USA
plot(US.o,ylim=c(0,55),,xlab="",ylab="",axes=FALSE,lwd=2,
     main="Age dependency USA")
lines(US.y,lty=2,lwd=2)
axis(1,tick=FALSE);axis(2,tick=FALSE,line=-1)

text(1962,48,"Young",cex=1.5)
text(1962,12,"Old",cex=1.5)

# Age dependency Germany
plot(DE.o,ylim=c(0,55),,xlab="",ylab="",axes=FALSE,lwd=2,
     main="Age dependency Germany")
lines(DE.y,lty=2,lwd=2)
axis(1,tick=FALSE);axis(2,tick=FALSE,line=-1)

text(1962,16,"Old",cex=1.5)
text(1962,30,"Young",cex=1.5)
