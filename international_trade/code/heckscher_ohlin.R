## Figures Heckscher-Ohlin model(in progress)
setwd("~/Dropbox/github/teaching/international_trade/ucd")

#### Wage equalisation ####
d<-read.csv("data_raw/us_bls.csv") # Data already orderd

par(mar=c(12,5,2,0),las=1,bty="n",cex.lab=2)
plot(d$total,type="h",lwd=10,axes=FALSE,xlab="",
     ylab="Hourly compensation cost (US$)")
axis(1,at=1:34,label=d$country,las=2,tick=FALSE,cex=1.5)
axis(2,tick=FALSE,cex=2,line=-2)

#### Trade compensation ####
tf<-read.csv("data_raw/oecd_trade.csv",stringsAsFactors=F)
pe<-read.csv("data_raw/oecd_social_spending.csv",stringsAsFactors=F)

tf<-data.frame(iso=tf$LOCATION,trade=tf$Value)
pe<-data.frame(iso=pe$COUNTRY,expenditures=pe$Value)
x<-merge(tf,pe,all.x=TRUE)

# Plot figure
par(mar=c(5,5,0,0),las=1,bty="n",cex.lab=2,cex.axis=2,pty="m")
plot(x$trade,x$expenditures,pch=19,cex=2,axes=FALSE,
     xlab="Trade relative to GDP",
     ylab="Public spending relative to GDP")
axis(1,tick=FALSE)
axis(2,tick=FALSE,line=-1.5)
abline(lm(expenditures~trade,x),lwd=2,lty=2)

