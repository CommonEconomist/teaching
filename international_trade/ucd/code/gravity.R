## Figures lecture Gravity Model (in progress)
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
     trade[!(iso2c %in% c("CA","MX"))],type="n",axes=FALSE,
     xlab="Percent of European GDP",ylab="Percent of US-European trade")
abline(a=0,b=1,lwd=2,lty=2)
text(gdp[!(iso2c %in% c("CA","MX"))],trade[!(iso2c %in% c("CA","MX"))],
     iso2c[!(iso2c %in% c("CA","MX"))])
axis(1,tick=FALSE);axis(2,tick=FALSE,line=-1)

# Plot data including NAFTA
plot(gdp,trade,type="n",axes=FALSE,
     xlab="Percent of European GDP",ylab="Percent of US-European trade")
abline(a=0,b=1,lwd=2,lty=2)
text(gdp,trade,iso2c)
axis(1,tick=FALSE);axis(2,tick=FALSE,line=-1)

#### Gravity model for Ireland ####
require(haven)
d<-read_dta("data_raw/col_regfile09.dta") # Takes some time to load
d<-d[d$iso_o=="IRL" & d$year==2006,]

plot(d$distw,d$flow,log="xy",axes=FALSE,
     xlab="Distance (in Km)",ylab="Trade flow (in millions)")
axis(1,tick=F)
axis(2,tick=FALSE,line=-3,at=c(.001,.1,10,1000),label=c(.001,.1,10,1000))

#### Trade relative to GDP ####
# Download data
require(plyr)
trade_m<-WDIsearch("trade",field="name",short=FALSE) # 108
wdi<-WDI("all",trade_m[108,1],start=1960,end=2015) 
d<-ddply(wdi,.(year),summarise,trade_gdp=mean(NE.TRD.GNFS.ZS,na.rm=TRUE))

trade<-ts(d$trade_gdp,start=c(1960,1))

# Plot data
plot(trade,xlab="",ylab="Trade flow relative to GDP",axes=FALSE,lwd=2)
axis(1,tick=FALSE);axis(2,tick=FALSE,line=-1)

#### Trade UK with distant partners ####
# Prepare data





x<-d$distw
y<-d$flow/d$gdp_o
ccode<-d$iso_d

# Plot data
plot(x,y,log="xy",axes=FALSE,
     xlab="Distance (km)",ylab="Trade flow (in millions)",type="n")
text(x,y,ccode,cex=2)
axis(1,tick=FALSE)
axis(2,tick=FALSE,line=-2)
