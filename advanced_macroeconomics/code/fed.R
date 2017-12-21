## Macroeconomics identification
setwd('~/Dropbox/github/teaching/advanced_macroeconomics')
x<-read.csv('data/fed.csv')
ir<-ts(x$rate,start=c(1999,1),frequency=12)

par(mar=c(5,5,2,2),bty='n',las=1,cex.axis=2,cex.lab=2)
plot(ir,lwd=2,axes=FALSE,xlab='',ylab='')
axis(1,tick=FALSE);axis(2,tick=FALSE)
text(2015,6,'Interest rate',cex=2)
