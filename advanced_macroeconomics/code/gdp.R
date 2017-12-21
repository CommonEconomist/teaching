## GDP examples
setwd('~/Dropbox/github/teaching/advanced_macroeconomics')
par(mar=c(5,5,2,2),bty='n',las=1,cex.axis=2,cex.lab=2)
library(WDI)

# Denmark, South Korea, Hong Kong, Singapore
gdp_m<-WDIsearch("gdp per capita",field="name",short=FALSE)
wdi<-WDI(c("DK","KR","HK","SG"),gdp_m[5,1],start=1961,end=2015)

# Data to time-series objects
wdi<-wdi[order(wdi[,4]),]
DK<-ts(wdi[wdi$iso2c=="DK",][,3],start=c(1961,1),freq=1)
KR<-ts(wdi[wdi$iso2c=="KR",][,3],start=c(1961,1),freq=1)
HK<-ts(wdi[wdi$iso2c=="HK",][,3],start=c(1961,1),freq=1)
SG<-ts(wdi[wdi$iso2c=="SG",][,3],start=c(1961,1),freq=1)

# Figure GDP over time Denmark and South Korea
par(mar=c(4,4,3.5,1),mfrow=c(1,1))
plot(DK,ylim=c(1000,65000),log="y",xlab="",ylab="",axes=FALSE,lwd=2,
     main="GDP per capita \n Denmark and South Korea 1961-2015")
lines(KR,lty=2,lwd=2)
axis(1,tick=FALSE);axis(2,tick=FALSE,line=-2)

text(1964,20000,"Denmark",cex=1.5)
text(1964,1000,"South Korea",cex=1.5)

# Figure GDP over time Hong Kong and Singapore
plot(HK,ylim=c(1000,65000),log="y",xlab="",ylab="",axes=FALSE,lwd=2,
     main="GDP per capita Hong Kong and Singapore 1961-2015")
lines(SG,lty=2,lwd=2)
axis(1,tick=FALSE);axis(2,tick=FALSE,line=-1)

text(1964,3000,"Singapore",cex=1.5)
text(1964,6000,"Hong Kong",cex=1.5)


