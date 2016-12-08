# Code to create figures for lecture:
# "Time series data and macroeconomics"
par(bty="n",las=1,cex.axis=1.5,cex.lab=1.5,cex.main=1.7)

# Libraries
require(mFilter)
require(zoo)

# Data
us<-read.csv("data/us.csv",header=TRUE)
uk<-read.csv("data/uk.csv",header=TRUE)
year=1700:2015

# Functions: moving average
ma<-function(x,n)filter(x,rep(1/n,n),sides=2)

#-------------------------------------------------------------------------------
#### 1) Data to time-series objects ####
gdp.cap.uk<-ts(log(uk$gdp.cap),start=c(1700,1),frequency=1)
gdp.cap.uk_1946<-window(gdp.cap.uk,start=c(1946,1))
gdp.cap.us<-ts(log(us$gdp),start=c(1929,1),frequency=1)

C<-ts(log(uk$consumption.real),start=c(1700,1),frequency=1)
I<-ts(log(uk$investments.real),start=c(1700,1),frequency=1)

infl.uk<-ts(uk$inflation,start=c(1700,1),frequency=1)
infl.uk<-window(infl.uk,start=c(1946,1))
infl.us<-ts(us$inflation,start=c(1929,1),frequency=1)

#-------------------------------------------------------------------------------
#### 2) Trend and cycles in data ####

# Log-linear
gdp.uk.dt<-gdp.cap.uk_1946-fitted(lm(gdp.cap.uk_1946~year[247:316]))
gdp.us.dt<-gdp.cap.us-fitted(lm(gdp.cap.us~year[230:316]))

# HP-filter
gdp.cap.uk.hp<-hpfilter(gdp.cap.uk_1946,freq=6.25)
gdp.uk.hp.dt<-gdp.cap.uk_1946-gdp.cap.uk.hp$trend

gdp.cap.us.hp<-hpfilter(gdp.cap.us,freq=6.25)
gdp.us.hp.dt<-gdp.cap.us-gdp.cap.us.hp$trend

# 5-year moving average 
gdp.uk.ma<-ma(gdp.cap.uk_1946,5) 
gdp.uk.ma.dt<-gdp.cap.uk_1946-gdp.uk.ma

Cdt<-C-ma(C,5)
Idt<-I-ma(I,5)

#-------------------------------------------------------------------------------
#### 3) Growth rates and volatility ####

# UK GDP
uk.gdp<-ts(uk$gdp.cap,start=c(1700,1),frequency=1)
gdp.cap.uk.l<-lag(uk.gdp,-1,na.pad=TRUE)
gdp.cap.uk.g<-(uk.gdp-gdp.cap.uk.l)/gdp.cap.uk.l*100
gdp.cap.uk.g<-window(gdp.cap.uk.g,start=c(1946,1))

# US GDP
us.gdp<-ts(us$gdp,start=c(1929,1),frequency=1)
gdp.cap.us.l<-lag(us.gdp,-1,na.pad=TRUE)
gdp.cap.us.g<-(us.gdp-gdp.cap.us.l)/gdp.cap.us.l*100

# Rolling average 5-year standard deviation
gdp.cap.uk.sd5<-rollapply(gdp.cap.uk.g,width=5,FUN=sd)
infl.uk.sd5<-rollapply(infl.uk,width=5,FUN=sd)

gdp.cap.us.sd5<-rollapply(gdp.cap.us.g,width=5,FUN=sd)
infl.us.sd5<-rollapply(infl.us,width=5,FUN=sd)

#------------------------------------------------------------------------------
#### 4) GDP per capita UK 1700-2015 ####
par(mar=c(4,5,2,1))

plot(gdp.cap.uk,type="l",axes=FALSE,xlab="",ylab="",lwd=2)
axis(1,tick=FALSE)
axis(2,tick=FALSE,at=c(log(2000),log(5000),log(10000),log(20000),log(25000)),
     label=c(2000,5000,10000,20000,25000))

abline(lm(gdp.cap.uk~year),col="steelblue4",untf=TRUE,lwd=1.5) # Trend line

#------------------------------------------------------------------------------
#### 5) GDP per capita UK 1946-2015 ####
plot(gdp.cap.uk_1946,type="l",axes=FALSE,xlab="",ylab="",lwd=2)
     
abline(lm(gdp.cap.uk_1946~year[247:316]),col="steelblue4",untf=TRUE,lwd=1.5)

axis(1,tick=FALSE)
axis(2,tick=FALSE,at=c(log(7500),log(10000),log(15000),log(20000),log(25000)),
     label=c(7500,10000,15000,20000,25000))

#------------------------------------------------------------------------------
#### 6) Cycles from log-linear trend model ####
plot(gdp.uk.dt,type="l",xlab="",ylab="",axes=FALSE,ylim=c(-.15,.1),lwd=2)
abline(h=0,lty=2,lwd=1.5)
axis(1,tick=FALSE)
axis(2,tick=FALSE)

#------------------------------------------------------------------------------
#### 7) HP-filtered data UK ####
plot(gdp.uk.dt,type="n",xlab="",ylab="",axes=FALSE,ylim=c(-.15,.1))

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
axis(2,tick=FALSE)
text(1946.5,.03,"HP-filter",cex=1.5)

#lines(gdp.uk.ma.dt,col="blue",lty=2,lwd=4) # 5-year moving average

#------------------------------------------------------------------------------
#### 8) HP-filtered data US ####
plot(gdp.us.dt,type="n",xlab="",ylab="",axes=FALSE,ylim=c(-.15,.15))

# Recessions in US (approximately)
rect(1929+8/12,-.15,1933+3/12,.15,col="grey90",lwd=0)
rect(1937+5/12,-.15,1938+6/12,.15,col="grey90",lwd=0)
rect(1945+2/12,-.15,1945+10/12,.15,col="grey90",lwd=0)
rect(1948+11/12,-.15,1949+10/12,.15,col="grey90",lwd=0)
rect(1953+7/12,-.15,1954+5/12,.15,col="grey90",lwd=0)
rect(1957+8/12,-.15,1958+4/12,.15,col="grey90",lwd=0)  
rect(1960+4/12,-.15,1961+2/12,.15,col="grey90",lwd=0)  
rect(1969+12/12,-.15,1970+11/12,.15,col="grey90",lwd=0)
rect(1973+11/12,-.15,1975+3/12,.15,col="grey90",lwd=0)
rect(1980+1/12,-.15,1980+7/12,.15,col="grey90",lwd=0)
rect(1981+7/12,-.15,1982+11/12,.15,col="grey90",lwd=0)
rect(1990+7/12,-.15,1991+3/12,.15,col="grey90",lwd=0)
rect(2001+3/12,-.15,2001+11/12,.15,col="grey90",lwd=0)
rect(2007+12/12,-.15,2009+6/12,.15,col="grey90",lwd=0)

lines(gdp.us.hp.dt,col="black",lwd=2)

axis(1,tick=FALSE)
axis(2,tick=FALSE)

#------------------------------------------------------------------------------
#### 9) HP-filter consumption and investment ####
par(mfrow=c(2,1),mar=c(5,5,1,1))
plot(window(Cdt,start=c(1946,1)),xlab="",ylab="",axes=FALSE,ylim=c(-.45,.35),
     lwd=2)
axis(2,tick=FALSE)
text(1950,.075,"Consumption",cex=1.7)

plot(window(Idt,start=c(1946,1)),xlab="",ylab="",axes=FALSE,ylim=c(-.45,.35),
     lwd=2)
axis(1,tick=FALSE)
axis(2,tick=FALSE)
text(1953,.15,"Investment",cex=1.7)

#------------------------------------------------------------------------------
#### 10) GDP growth and inflation ####
par(mfrow=c(1,1),mar=c(5,5,1,1))
plot(gdp.cap.uk.g,axes=FALSE,xlab="",ylab="",ylim=c(-6,25),lwd=2)
lines(infl.uk,col="steelblue4",lwd=2)
abline(h=0,lty=2,lwd=2)

axis(1,tick=FALSE)
axis(2,tick=FALSE)

text(1950,-3,"GDP growth",cex=1.5)
text(1947,9,"Inflation",cex=1.5)

#------------------------------------------------------------------------------
#### 11) GDP growth and inflation standard deviations ####
plot(gdp.cap.uk.sd5,axes=FALSE,xlab="",ylab="",ylim=c(0,6.5),lwd=2)
lines(infl.uk.sd5,col="steelblue4",lwd=2)

axis(1,tick=FALSE)
axis(2,tick=FALSE)

text(1950,.5,"GDP growth",cex=1.5)
text(1950,4,"Inflation",cex=1.5)

#------------------------------------------------------------------------------
#### 12) US GDP growth and inflation standard deviations ####
plot(window(gdp.cap.us.sd5,start=c(1962,1)),
     axes=FALSE,xlab="",ylab="",ylim=c(0,6),lwd=2)
lines(window(infl.us.sd5,start=c(1962,1)),col="steelblue4",lwd=2)

axis(1,tick=FALSE)
axis(2,tick=FALSE)

text(1964,2.2,"GDP growth",cex=1.5)
text(1966,0,"Inflation",cex=1.5)

