#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#
# Versus US growth
# Last update: 2018 02 20
#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#
library(WDI)
library(plyr)
library(reshape2)

source("code/lines.R")

#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#
# 1) Growth vs. US
# Download World Bank data
gdp_m<-WDIsearch("gdp",field="name",short=FALSE)
wdi<-WDI(c("US","BE","LU","NL","DE","FR","IT","AT","FI","SE",
           "DK","GB","IE","ES","PT","GR"),
         c(gdp_m[87,1],gdp_m[94,1],gdp_m[95,1]),start=1961,end=2015) 
wdi<-wdi[order(wdi$iso2c,wdi$year),]

# Create time-series objects
us<-ts(wdi[,4][wdi$iso2c=="US"],start=c(1961,1),frequency=1)
eu15<-ddply(wdi[wdi$iso2c!="US",],.(year),summarise,
            gdp.g=mean(NY.GDP.MKTP.KD.ZG,na.rm=TRUE),
            gdp.cap=mean(NY.GDP.PCAP.KD,na.rm=TRUE))
eu15.g<-ts(eu15$gdp.g,start=c(1961,1),frequency=1)

gdp.g.d<-eu15.g-us

# Growth of EU15 compared to US
par(mar=c(5,5,1,1),bty="n",las=1,cex.axis=2,cex.lab=2)
plot(gdp.g.d,ylim=c(-5,5),xlab="",ylab="GDP growth relative to US",axes=FALSE,
     type="h",lwd=5)
axis(1,tick=FALSE)
axis(2,tick=FALSE,line=-1)

#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#
# 2) Growth over time

# Reshape data from long to wide
dat.l<-wdi[,c("country","year","NY.GDP.PCAP.KD")]
dat.l<-dat.l[order(dat.l$country,dat.l$year),]
dat.w<-reshape(dat.l,timevar="year",idvar=c("country"),direction="wide")
rownames(dat.w)<-dat.w$country; m<-round(dat.w[,-1],2)
startYear<-min(dat.l$year);endYear<-max(dat.l$year)

# GDP per capita over time
par(las=1,tck=0.02,mar=c(4,5,3,5),mgp=c(2.8,0.3,2.8),
    cex.lab=1.2,cex.axis=1.2,cex.main=0.9)
plot(0,xlim=c(startYear,endYear),ylim=c(4000,110000),type="n",bty="n",
     main="",xlab="",ylab="GDP per capita",axes=FALSE,log="y")
lifeLines(m,col="grey60")
lines(startYear:endYear,m[16,],col="black",lwd=2.5)
lines(startYear:endYear,colMeans(m[-16,],na.rm=TRUE),
      col="steelblue4",lwd=2.5,lty=2)

axis(1,tick=FALSE); axis(2,tick=FALSE,line=-1.75,
                         at=c(5000,10000,20000,50000,100000),
                         label=c(5000,10000,20000,50000,100000))

#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#
# 3) Compared to 1980s

# 1980 vs 2007 & 2015
df<-wdi[wdi$year==1980 | wdi$year==2007 | wdi$year==2015,]
x1<-df[,5][df$year==1980 & df$iso2c!="LU"];x1<-x1/x1[15]
x2<-df[,5][df$year==2007 & df$iso2c!="LU"];x2<-x2/x2[15]
x3<-df[,5][df$year==2015 & df$iso2c!="LU"];x3<-x3/x3[15]
lbl<-df$iso2c[df$year==1980 & df$iso2c!="LU"]

# Plot data
par(mar=c(5,5,1,1),mfrow=c(1,2),pty="s",bty="n",cex.lab=1.5,cex.axis=1.5)
plot(x1,x2,axes=FALSE,xlim=c(.4,1.3),ylim=c(.4,1.3),type="n",
     xlab="GDP per capita 1980 (US=1)",ylab="GDP per capita 2007 (US=1)")
abline(a=0,b=1,lty=2);text(x1,x2,lbl)
axis(1,tick=FALSE);axis(2,tick=FALSE,line=-.5)

plot(x1,x3,axes=FALSE,xlim=c(.4,1.3),ylim=c(.4,1.3),type="n",
     xlab="GDP per capita 1980 (US=1)",ylab="GDP per capita 2015 (US=1)")
abline(a=0,b=1,lty=2);text(x1,x3,lbl)
axis(1,tick=FALSE);axis(2,tick=FALSE,line=-.5)


rm(list=ls()) 
