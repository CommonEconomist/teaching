## GDP examples
setwd('~/Dropbox/github/teaching/advanced_macroeconomics')
par(mar=c(5,5,2,2),bty='n',las=1,cex.axis=2,cex.lab=2)

# Data
x<-read.csv('data/RNDEBIS.csv')
md<-ts(x[,2],start=c(1964,1),frequency=12)
md.l<-lag(md,k=-1)

mc<-md-md.l
mc<-window(mc,start=c(1964,1),end=c(1985,12))



plot(mc,xlab="",ylab="",axes=FALSE,lwd=2,
     main="Monthly change in US-German exchange rate")
abline(v=1973,lty=2)
axis(1,tick=FALSE);axis(2,tick=FALSE,line=-2)

