# Figures for lecture on Growth Accounting
par(bty="n",las=1,cex.axis=1.5,cex.lab=1.5,cex.main=1.7)
require(WDI)

#------------------------------------------------------------------------------
#### 1) GDP data ####
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
par(mar=c(4,4,3.5,1))
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

#------------------------------------------------------------------------------
#### 2) Age dependency ####
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

text(1962,48,"Old",cex=1.5)
text(1962,12,"Young",cex=1.5)

# Age dependency Germany
plot(DE.o,ylim=c(0,55),,xlab="",ylab="",axes=FALSE,lwd=2,
     main="Age dependency Germany")
lines(DE.y,lty=2,lwd=2)
axis(1,tick=FALSE);axis(2,tick=FALSE,line=-1)

text(1962,16,"Old",cex=1.5)
text(1962,30,"Young",cex=1.5)
