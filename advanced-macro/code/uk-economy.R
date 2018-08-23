## Time-series data: Example UK economy
setwd('~/Dropbox/github/teaching/advanced_macroeconomics')
par(mar=c(5,5,3,2),bty="n",las=1,cex.axis=2,cex.lab=2,cex.main=2)
ma<-function(x,n)filter(x,rep(1/n,n),sides=2) # Function moving average

# Libraries
require(mFilter)
require(zoo)

uk<-read.csv("data/uk_economic_data.csv",header=TRUE)

#### Prepare data ####
year=1700:2015

## Create time-series objects
gdp.cap.uk<-ts(uk$gdp.cap,start=1700)
gdp.cap.uk_1946<-window(gdp.cap.uk,start=1946)

C<-ts(uk$consumption.real,start=1700)
I<-ts(uk$investments.real,start=1700)

infl.uk<-ts(uk$inflation,start=1700)
infl.uk<-window(infl.uk,start=1946)

#### Detrend data ####

# Log-linear
gdp.uk.dt<-log(gdp.cap.uk_1946)-fitted(lm(log(gdp.cap.uk_1946)~year[247:316]))

# HP-filter
gdp.cap.uk.hp<-hpfilter(log(gdp.cap.uk_1946),freq=6.25)
gdp.uk.hp.dt<-log(gdp.cap.uk_1946)-gdp.cap.uk.hp$trend

# 5-year moving average 
gdp.uk.ma<-ma(log(gdp.cap.uk_1946),5) 
gdp.uk.ma.dt<-log(gdp.cap.uk_1946)-gdp.uk.ma

Cdt<-log(C)-ma(log(C),5)
Idt<-log(I)-ma(log(I),5)

#### Growth rates and volatility ####

# UK GDP
uk.gdp<-ts(uk$gdp.cap,start=1700)
gdp.cap.uk.l<-lag(uk.gdp,-1,na.pad=TRUE)
gdp.cap.uk.g<-(uk.gdp-gdp.cap.uk.l)/gdp.cap.uk.l*100
gdp.cap.uk.g<-window(gdp.cap.uk.g,start=1946)

# Rolling average 5-year standard deviation
gdp.cap.uk.sd5<-rollapply(gdp.cap.uk.g,width=5,FUN=sd)
infl.uk.sd5<-rollapply(infl.uk,width=5,FUN=sd)

#### Figure: GDP per capita UK 1700-2015 ####
plot(gdp.cap.uk,type="l",axes=FALSE,xlab="",ylab="",lwd=2,log='y',
     main='UK GDP per capita')
axis(1,tick=FALSE)
axis(2,tick=FALSE,line=-2)

#### Figure: UK 1946-2015 #### 
plot(gdp.cap.uk_1946,type="l",axes=FALSE,xlab="",ylab="",lwd=2,
     main='UK GDP per capita')
abline(lm(gdp.cap.uk_1946~year[247:316]),col="steelblue4",untf=TRUE,lwd=1.5)
axis(1,tick=FALSE)
axis(2,tick=FALSE,line=-2)

#### Figure: Cycles from log-linear trend model
plot(gdp.uk.dt,type="l",xlab="",ylab="",axes=FALSE,ylim=c(-.15,.1),lwd=2,
     main='Cycles from log-linear trend model')
abline(h=0,lty=2,lwd=1.5)
axis(1,tick=FALSE)
axis(2,tick=FALSE,line=-2)

#### Figure: HP-filtered data ####
plot(gdp.uk.dt,type="n",xlab="",ylab="",axes=FALSE,ylim=c(-.15,.1),
     main='HP-filtered data')

# Recessions in UK (approximately)
rect(1961.5,-.15,1962,.1,col="grey90",lwd=0)  
rect(1973.5,-.15,1974.25,.1,col="grey90",lwd=0)  
rect(1975.25,-.15,1977.75,.1,col="grey90",lwd=0)  
rect(1980,-.15,1981.25,.1,col="grey90",lwd=0)  
rect(1990.5,-.15,1991.5,.1,col="grey90",lwd=0)  
rect(2008.25,-.15,2009.25,.1,col="grey90",lwd=0)  

lines(gdp.uk.dt,lwd=2,col="grey50")
lines(gdp.uk.hp.dt,col="black",lwd=2)

axis(1,tick=FALSE)
axis(2,tick=FALSE,line=-1)
text(1947,.03,"HP-filter",cex=1.5)

#### Figure: HP-filer consumption and investment ####
par(mfrow=c(2,1))
plot(window(Cdt,start=1946),xlab="",ylab="",axes=FALSE,ylim=c(-.15,.35),
     lwd=2)
axis(2,tick=FALSE,line=-1)
text(1950,.075,"Consumption",cex=1.7)

plot(window(Idt,start=1946),xlab="",ylab="",axes=FALSE,ylim=c(-.15,.35),
     lwd=2)
axis(1,tick=FALSE)
axis(2,tick=FALSE)
text(1953,.15,"Investment",cex=1.7)

#### Figure: GDP growth and inflation ####
par(mfrow=c(1,1))
plot(gdp.cap.uk.g,axes=FALSE,xlab="",ylab="",ylim=c(-6,25),lwd=2)
lines(infl.uk,col="steelblue4",lwd=2)
abline(h=0,lty=2,lwd=2)

axis(1,tick=FALSE)
axis(2,tick=FALSE)

text(1950,-3,"GDP growth",cex=1.5)
text(1947,9,"Inflation",cex=1.5)

#### Figure: GDP growth and standard deviations ####
plot(gdp.cap.uk.sd5,axes=FALSE,xlab="",ylab="",ylim=c(0,6.5),lwd=2)
lines(infl.uk.sd5,col="steelblue4",lwd=2)

axis(1,tick=FALSE)
axis(2,tick=FALSE)

text(1950,.5,"GDP growth",cex=1.5)
text(1950,4,"Inflation",cex=1.5)

