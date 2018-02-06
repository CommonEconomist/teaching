setwd('~/Dropbox/github/teaching/advanced_macroeconomics')
par(mar=c(5,5,3,2),bty="n",las=1,cex.axis=2,cex.lab=2,cex.main=2,pty='s')
require(zoo)
uk<-read.csv("data/uk_economic_data.csv",header=TRUE)
year=1700:2015

gdp<-ts(uk$gdp.cap,start=1700)
gdp.l<-lag(gdp,-1,na.pad=TRUE)
gdp.g<-(gdp-gdp.l)/gdp.l
gdp.g2<-log(gdp)-log(gdp.l)

plot(gdp.g,gdp.g2,axes=FALSE,xlab='GDP growth rate',ylab='GDP growth (log)')
abline(a=0,b=1,lty=2)
axis(1,tick=FALSE,line=-1);axis(2,tick=FALSE,line=-2)
