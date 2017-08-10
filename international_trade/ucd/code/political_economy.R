## Figures Political Economy of Trade
setwd("~/Dropbox/github/teaching/international_trade/ucd")

#### US income from tariffs ###
d<-read.csv("data_raw/usgs_1792_2016.csv",stringsAsFactors=FALSE)
income<-ts(d[,2],start=c(1792,1))
tariff<-ts(d[,3],start=c(1792,1))


par(mar=c(5,5,2,1),las=1,bty="n",cex.axis=2,cex.lab=2)
plot(tariff,ylim=c(.5,16),axes=FALSE,xlab="",ylab="",lwd=2)
lines(income,col="steelblue4",lty=2)
axis(1,tick=FALSE)
axis(2,tick=FALSE)

text(1820,6,"Tariffs",cex=2)
text(1960,8,"Income \n tax",cex=2)

#### Consumer price index ####
d<-read.csv("data_raw/oecd_cpi.csv",stringsAsFactors=FALSE)
oecde<-ts(d$Value[d$LOCATION=="OECDE"],start=c(2013,1),frequency=12)
rus<-ts(d$Value[d$LOCATION=="RUS"],start=c(2013,1),frequency=12)

o<-oecde/oecde[1]*100 # OECD/EU
r<-rus/rus[1]*100 # Russia

# Plot data
par(mar=c(5,5,2,1),las=1,bty="n",cex.axis=2,cex.lab=2)
plot(o,ylim=c(100,150),xlim=c(2013,2017.8),lwd=2,lty=2,axes=FALSE,xlab="",ylab="")
lines(r,lty=2,lwd=2,col="firebrick3")

axis(1,tick=FALSE)
axis(2,tick=FALSE)

text(2017.6,o[54],"OECD \n Europe",cex=1.5)
text(2017.6,r[54],"Russia",cex=1.5)
text(2013.6,150,"Consumer Price Index",cex=2)
