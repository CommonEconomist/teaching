## Figures Ricardian model (in progress)
# https://www.unido.org/data1/wpd/Index.cfm
# http://ec.europa.eu/eurostat/web/products-datasets/-/tsdec310
#https://www.bls.gov/fls/#compensation
setwd("~/Dropbox/github/Teaching/international_trade/ucd")

#### Trade for South Korea ####

# Download data
require(WDI)
gdp_m<-WDIsearch("gdp",field="name",short=FALSE) # 82
trade_m<-WDIsearch("trade",field="name",short=FALSE) # 108

wdi<-WDI("KR",trade_m[108,1],start=1960,end=2015)
wdi<-wdi[order(wdi$year),]
kr<-ts(wdi$NE.TRD.GNFS.ZS,start=c(1960,1))

# Plot data
plot(kr,lwd=2,xlab="",ylab="Trade as percentage of GDP",axes=FALSE)
axis(1,tick=FALSE);axis(2,tick=FALSE,line=-1)

#### Productivity and wages Korea ####
# Load data
d<-read.csv("data_raw/WPD_UNIDO.csv")
x<-ts(d$TFP_K06_US[d$Country=="Korea, Republic of"],start=c(1961,1))
y<-ts(d$Inc_pw_us[d$Country=="Korea, Republic of"],start=c(1961,1))

# Plot data
par(mar=c(5,5,1,1),las=1,bty="n",cex.lab=2,cex.axis=2,pty="s")
plot(x,y,xlab="Productivity relative to US",
     ylab="Income per worker relative to US",axes=FALSE,cex=.7)
axis(1,tick=FALSE);axis(2,tick=FALSE,line=-1)

#### Global productivity and wages ####
d<-d[d$Year==2000,]

plot(d$TFP_K06_US,d$Inc_pw_us,xlab="Productivity relative to US",
     ylab="Income per worker relative to US",axes=FALSE,pch=19,cex=2)
abline(a=0,b=1,lty=2,lwd=2)
axis(1,tick=FALSE);axis(2,tick=FALSE,line=-1)


#### Productivity and labour costs EU ####
d<-read.csv("data_raw/nama_10_lp_ulc_1_Data.csv")
d<-na.omit(d[d$TIME==2015,])


