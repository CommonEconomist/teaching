## Figures gravity model
setwd("~/Dropbox/github/teaching/international_trade/ucd")

#### US trade with Europe ####
# Load data
d<-read.csv("data_raw/trade_partners_us.csv")
d<-d[d$year==2015,]

# Import/export for Europe as a whole
eu.trade<-d$IYR[d$CTYNAME=="Europe"][1]+d$EYR[d$CTYNAME=="Europe"][1]

# Subset to European countries
require(countrycode)
d$region<-countrycode(d$CTYNAME,"country.name","continent",warn=TRUE)
d<-na.omit(d[d$region=="Europe" | d$CTYNAME=="Canada" | d$CTYNAME=="Mexico",])

# Download GDP data
require(WDI)
d$iso2c<-countrycode(d$CTYNAME,"country.name","iso2c",warn=TRUE)
gdp_m<-WDIsearch("gdp",field="name",short=FALSE) # 82
wdi<-na.omit(WDI(d$iso2c,gdp_m[82,1],start=2015,end=2015)) 

d<-d[d$iso2c %in% wdi$iso2c,]
wdi<-wdi[order(wdi$iso2c),]
d<-d[order(d$iso2c),]

# Prepare data for plot
gdp<-wdi[,3]/sum(wdi[,3])*100
trade<-(d$IYR+d$EYR)/eu.trade*100
iso2c<-d$iso2c

# Plot data
par(mar=c(5,6,2,2),las=1,bty="n",cex.lab=2,cex.axis=2)
plot(gdp[!(iso2c %in% c("CA","MX"))],
     trade[!(iso2c %in% c("CA","MX"))],type="p",pch=19,cex=2,axes=FALSE,
     xlab="Percent of European GDP",ylab="Percent of US-European trade")
abline(a=0,b=1,lwd=2,lty=2)
axis(1,tick=FALSE);axis(2,tick=FALSE,line=-1)

# Add some markers
text(gdp[iso2c=="RU"]+.7,trade[iso2c=="RU"],"Russia",cex=1.2)
text(gdp[iso2c=="GB"]+.4,trade[iso2c=="GB"],"UK",cex=1.2)
text(gdp[iso2c=="DE"]-1.2,trade[iso2c=="DE"],"Germany",cex=1.2)
text(gdp[iso2c=="FR"]+.7,trade[iso2c=="FR"],"France",cex=1.2)
text(gdp[iso2c=="IT"]+.7,trade[iso2c=="IT"],"Italy",cex=1.2)


## Plot data including NAFTA
plot(gdp,trade,type="p",axes=FALSE,pch=19,cex=2,
     xlab="Percent of European GDP",ylab="Percent of US-European trade")
abline(a=0,b=1,lwd=2,lty=2)
axis(1,tick=FALSE);axis(2,tick=FALSE,line=-1)

# Markers
text(gdp[iso2c=="MX"]+.7,trade[iso2c=="MX"],"Mexico",cex=1.2)
text(gdp[iso2c=="CA"]+.7,trade[iso2c=="CA"],"Canada",cex=1.2)


#### Gravity model for UK ####
df<-read.csv("data_raw/Distance-exports.csv")

# Data to vectors
d<-df[,2]
x<-df[,3]
options(scipen=4)

plot(d,x,log="xy",axes=FALSE,pch=19,
     xlab="Distance (in Km)",ylab="Exports (in millions)")
axis(1,tick=F)
axis(2,tick=FALSE,line=-3,at=c(10,100,1000,10000,100000),
     lab=c(10,100,1000,10000,100000))

#### Trade relative to GDP ####
require(plyr)
require(WDI)
require(countrycode)

# Need to get countrycodes
data(countrycode_data)
iso2c<-countrycode_data$iso2c

# Trade data
trade_m<-WDIsearch("trade",field="name",short=FALSE) # 108
wdi<-WDI(iso2c,trade_m[108,1],start=1960,end=2015) 
d<-ddply(wdi,.(year),summarise,trade_gdp=mean(NE.TRD.GNFS.ZS,na.rm=TRUE))

trade<-ts(d$trade_gdp,start=c(1960,1)) # To time series

# Plot data
plot(trade,xlab="",ylab="",axes=FALSE,type="b",cex=2,lwd=2,ylim=c(40,100))
axis(1,tick=FALSE);axis(2,tick=FALSE,line=-1)
text(1970,90,"Trade relative to GDP",cex=2)
