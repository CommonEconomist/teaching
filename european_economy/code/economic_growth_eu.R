# Figures for lecture on economic growth in the EU

#------------------------------------------------------------------------------
#### Characteristics new EU member states ####
require(WDI)

# Focus on GDP and trade relative to GDP
gdp_m<-WDIsearch("gdp",field="name",short=FALSE)
trade_m<-WDIsearch("trade",field="name",short=FALSE)
wdi<-WDI(c("CZ","BG","EE","HU","LV","LT","MT","PL","RO","SK","SI","DE"),
         c(gdp_m[89,1],trade_m[108,1]),start=2004,end=2004) 

wdi$GDP<-wdi[,4]/wdi[wdi$country=="Germany",][,4] # GDP relative to Germany

# Plot data
par(mar=c(5,5,1,1),pty="s",bty="n",las=1,cex.axis=1.5,cex.lab=1.5)
plot(wdi[,5],wdi$GDP,ylim=c(0,.25),axes=FALSE,pch=19,
     xlab="Trade % of GDP", ylab="GDP relative to Germany")
text(wdi[,5],wdi$GDP+.01,wdi$iso2c,cex=1.3)
axis(1,tick=FALSE)
axis(2,tick=FALSE,line=-1)

#------------------------------------------------------------------------------
# Tracking economic development since EU membership
# Use OECD as baseline
oecd<-read.csv("data_raw/oecd.csv",header=TRUE)
pwt<-read.csv("data_raw/pwt90.csv",header=TRUE)

# GDP per capita
pwt$gdpcap<-pwt$rgdpo/pwt$pop

# Fix countrycodes
require(countrycode)
oecd$iso3c<-countrycode(oecd$country,"country.name","iso3c",warn=TRUE)
oecd.iso<-oecd[oecd$membership_year<=1990,]$iso3c

# Indicator for OECD members as of 1990
pwt$oecd<-ifelse(pwt$countrycode %in% oecd.iso,1,0)
pwt2<-pwt[pwt$oecd==1,]

# Calculate OECD average
require(plyr)
oecd.av<-ddply(pwt2,.(year),summarise,gdpcap=mean(gdpcap,na.rm=TRUE))
oecd.av<-ts(oecd.av$gdpcap,start=c(1950,1),frequency=1)

# Some settings for the plot
Y<-c(.25,1.35)
par(mar=c(4,4,1,1),las=1,bty="n",cex.axis=1.5,cex.lab=1.5,mfrow=c(3,1))

#------------------------------------------------------------------------------
#### First enlargement in 1973 ####
dnk<-ts(pwt[pwt$countrycode=="DNK",]$gdpcap,start=c(1950,1),frequency=1)/oecd.av
irl<-ts(pwt[pwt$countrycode=="IRL",]$gdpcap,start=c(1950,1),frequency=1)/oecd.av
gbr<-ts(pwt[pwt$countrycode=="GBR",]$gdpcap,start=c(1950,1),frequency=1)/oecd.av

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

#------------------------------------------------------------------------------
#### Second enlargement following end of dictatorship in Southern Europe ####
grc<-ts(pwt[pwt$countrycode=="GRC",]$gdpcap,start=c(1950,1),frequency=1)/oecd.av
prt<-ts(pwt[pwt$countrycode=="PRT",]$gdpcap,start=c(1950,1),frequency=1)/oecd.av
esp<-ts(pwt[pwt$countrycode=="ESP",]$gdpcap,start=c(1950,1),frequency=1)/oecd.av

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
abline(h=mean(window(esp,end=c(1986)),na.rm=TRUE),lty=2)
axis(2,tick=FALSE)
axis(1,tick=FALSE)
text(1960,.5,"Portugal",cex=1.5)

#------------------------------------------------------------------------------
#### Third enlargement of former neutral countries ####
aut<-ts(pwt[pwt$countrycode=="AUT",]$gdpcap,start=c(1950,1),frequency=1)/oecd.av
fin<-ts(pwt[pwt$countrycode=="FIN",]$gdpcap,start=c(1950,1),frequency=1)/oecd.av
swe<-ts(pwt[pwt$countrycode=="SWE",]$gdpcap,start=c(1950,1),frequency=1)/oecd.av

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

#------------------------------------------------------------------------------
#### Fourth wave including former East bloc countries ####
cze<-ts(pwt[pwt$countrycode=="CZE",]$gdpcap,start=c(1950,1),frequency=1)/oecd.av
hun<-ts(pwt[pwt$countrycode=="HUN",]$gdpcap,start=c(1950,1),frequency=1)/oecd.av
pol<-ts(pwt[pwt$countrycode=="POL",]$gdpcap,start=c(1950,1),frequency=1)/oecd.av

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
