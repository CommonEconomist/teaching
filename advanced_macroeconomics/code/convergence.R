## Convergence
setwd('~/Dropbox/github/teaching/advanced_macroeconomics')
par(mar=c(5,6,2,2),bty='n',las=1,cex.axis=2,cex.lab=2,cex.main=2)
library(WDI)

# Example with Denmark and South Korea
gdp_m<-WDIsearch("gdp per capita",field="name",short=FALSE)
wdi<-WDI(c("DK","KR"),gdp_m[5,1],start=1961,end=2015)

# Data to time-series objects
wdi<-wdi[order(wdi[,4]),]
DK<-ts(wdi[wdi$iso2c=="DK",][,3],start=c(1961,1),freq=1)
KR<-ts(wdi[wdi$iso2c=="KR",][,3],start=c(1961,1),freq=1)

# Figure GDP over time Denmark and South Korea
plot(DK,ylim=c(1000,65000),log="y",xlab="",ylab="",axes=FALSE,lwd=2,
     main="GDP per capita \n Denmark and South Korea 1961-2015")
lines(KR,lty=2,lwd=2)
axis(1,tick=FALSE);axis(2,tick=FALSE,line=-2)

text(1964,20000,"Denmark",cex=1.5)
text(1964,1000,"South Korea",cex=1.5)

