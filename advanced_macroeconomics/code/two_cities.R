## Tale of two cities: Hong Kong and Singapore
setwd('~/Dropbox/github/teaching/advanced_macroeconomics')
par(mar=c(5,6,2,2),bty='n',las=1,cex.axis=2,cex.lab=2,cex.main=2)
library(WDI)

# Download data 
gdp_m<-WDIsearch("gdp per capita",field="name",short=FALSE)
wdi<-WDI(c("HK","SG"),gdp_m[5,1],start=1961,end=2015)

# Data to time-series objects
wdi<-wdi[order(wdi[,4]),]
HK<-ts(wdi[wdi$iso2c=="HK",][,3],start=c(1961,1),freq=1)
SG<-ts(wdi[wdi$iso2c=="SG",][,3],start=c(1961,1),freq=1)

# Plot data
plot(HK,ylim=c(1000,65000),log="y",xlab="",ylab="",axes=FALSE,lwd=2,
     main="GDP per capita Hong Kong and Singapore 1961-2015")
lines(SG,lty=2,lwd=2)
axis(1,tick=FALSE);axis(2,tick=FALSE,line=-1)

text(1964,3000,"Singapore",cex=1.5)
text(1964,6000,"Hong Kong",cex=1.5)


