#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#
# GDP since EU membership
# Last update: 2018 02 20
#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#
library(pwt9)
library(countrycode)
library(plyr)

#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#
# 1) Prepare data
# Use OECD as baseline
oecd<-read.csv("data/oecd.csv",header=TRUE)
data('pwt9.0');
pwt<-pwt9.0

pwt$gdpcap<-pwt$rgdpe/pwt$pop # GDP per capita

# Fix countrycodes
oecd$iso3c<-countrycode(oecd$country,"country.name","iso3c",warn=TRUE)
oecd.iso<-oecd[oecd$membership_year<=1990,]$iso3c

# Indicator for OECD members as of 1990
pwt$oecd<-ifelse(pwt$isocode %in% oecd.iso,1,0)

# Calculate OECD average
oecd.av<-ddply(pwt[pwt$isocode %in% oecd.iso,],.(year),summarise,
               gdpcap=mean(gdpcap,na.rm=TRUE))
oecd.av<-ts(oecd.av$gdpcap,start=min(oecd.av$year))

# Some settings for the plot
Y<-c(.25,1.35)
par(mar=c(4,4,1,1),las=1,bty="n",cex.axis=2,cex.lab=2,mfrow=c(3,1))

#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#
# 2) 1973 enlargement
dnk<-ts(pwt[pwt$isocode=="DNK",]$gdpcap,start=1950)/oecd.av
irl<-ts(pwt[pwt$isocode=="IRL",]$gdpcap,start=1950)/oecd.av
gbr<-ts(pwt[pwt$isocode=="GBR",]$gdpcap,start=1950)/oecd.av

# Denmark
plot(dnk,ylim=Y,xlab="",ylab="",axes=FALSE,lwd=2)
abline(v=1973,lty=2)
abline(h=mean(window(dnk,end=c(1973)),na.rm=TRUE),lty=2)
axis(2,tick=FALSE)
text(1960,.4,"Denmark",cex=1.5)

# Ireland
plot(irl,ylim=Y,xlab="",ylab="",axes=FALSE,lwd=2)
abline(v=1973,lty=2)
abline(h=mean(window(irl,end=c(1973)),na.rm=TRUE),lty=2)
axis(2,tick=FALSE)
text(1960,.4,"Ireland",cex=1.5)

# Great Britain
plot(gbr,ylim=Y,xlab="",ylab="",axes=FALSE,lwd=2)
abline(v=1973,lty=2)
abline(h=mean(window(gbr,end=c(1973)),na.rm=TRUE),lty=2)
axis(2,tick=FALSE)
axis(1,tick=FALSE)
text(1960,.4,"Great Britain",cex=1.5)

#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#
# 3) Former Southern European juntas
grc<-ts(pwt[pwt$isocode=="GRC",]$gdpcap,start=1950)/oecd.av
prt<-ts(pwt[pwt$isocode=="PRT",]$gdpcap,start=1950)/oecd.av
esp<-ts(pwt[pwt$isocode=="ESP",]$gdpcap,start=1950)/oecd.av

# Greece
plot(grc,ylim=Y,xlab="",ylab="",axes=FALSE,lwd=2)
abline(v=1981,lty=2)
abline(h=mean(window(grc,end=c(1981)),na.rm=TRUE),lty=2)
axis(2,tick=FALSE)
text(1960,.4,"Greece",cex=1.5)

# Spain
plot(esp,ylim=Y,xlab="",ylab="",axes=FALSE,lwd=2)
abline(v=1986,lty=2)
abline(h=mean(window(esp,end=c(1986)),na.rm=TRUE),lty=2)
axis(2,tick=FALSE)
text(1960,.4,"Spain",cex=1.5)

# Portugal
plot(prt,ylim=Y,xlab="",ylab="",axes=FALSE,lwd=2)
abline(v=1986,lty=2)
abline(h=mean(window(prt,end=c(1986)),na.rm=TRUE),lty=2)
axis(2,tick=FALSE)
axis(1,tick=FALSE)
text(1960,.5,"Portugal",cex=1.5)

#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#
# 4) Neutral countries from Cold War
aut<-ts(pwt[pwt$isocode=="AUT",]$gdpcap,start=1950)/oecd.av
fin<-ts(pwt[pwt$isocode=="FIN",]$gdpcap,start=1950)/oecd.av
swe<-ts(pwt[pwt$isocode=="SWE",]$gdpcap,start=1950)/oecd.av

# Austria
plot(aut,ylim=Y,xlab="",ylab="",axes=FALSE,lwd=2)
abline(v=1995,lty=2)
abline(h=mean(window(aut,end=c(1995)),na.rm=TRUE),lty=2)
axis(2,tick=FALSE)
text(1960,.4,"Austria",cex=1.5)

# Finland
plot(fin,ylim=Y,xlab="",ylab="",axes=FALSE,lwd=2)
abline(v=1995,lty=2)
abline(h=mean(window(fin,end=c(1995)),na.rm=TRUE),lty=2)
axis(2,tick=FALSE)
text(1960,.4,"Finland",cex=1.5)

# Sweden
plot(swe,ylim=Y,xlab="",ylab="",axes=FALSE,lwd=2)
abline(v=1995,lty=2)
abline(h=mean(window(swe,end=c(1995)),na.rm=TRUE),lty=2)
axis(2,tick=FALSE)
axis(1,tick=FALSE)
text(1960,.4,"Sweden",cex=1.5)

#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#
# 5) Eastern Europe
cze<-ts(pwt[pwt$isocode=="CZE",]$gdpcap,1950)/oecd.av
hun<-ts(pwt[pwt$isocode=="HUN",]$gdpcap,1950)/oecd.av
pol<-ts(pwt[pwt$isocode=="POL",]$gdpcap,1950)/oecd.av

# Czechia
plot(cze,ylim=Y,xlab="",ylab="",axes=FALSE,lwd=2)
abline(v=2004,lty=2)
abline(h=mean(window(cze,end=c(2004)),na.rm=TRUE),lty=2)
axis(2,tick=FALSE)
text(1960,.4,"Czechia",cex=1.5)

# Hungary
plot(hun,ylim=Y,xlab="",ylab="",axes=FALSE,lwd=2)
abline(v=2004,lty=2)
abline(h=mean(window(hun,end=c(2004)),na.rm=TRUE),lty=2)
axis(2,tick=FALSE)
text(1960,.4,"Hungary",cex=1.5)

# Poland
plot(pol,ylim=Y,xlab="",ylab="",axes=FALSE,lwd=2)
abline(v=2004,lty=2)
abline(h=mean(window(pol,end=c(2004)),na.rm=TRUE),lty=2)
axis(2,tick=FALSE)
axis(1,tick=FALSE)
text(1960,.4,"Poland",cex=1.5)

## FIN

