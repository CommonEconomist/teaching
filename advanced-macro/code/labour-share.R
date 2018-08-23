setwd('~/github/teaching/advanced_macroeconomics')
d<-read.csv('data/fed_labour_share.csv')
d<-ts(d[,2],start=c(1947,1),frequency=4)

par(mar=c(5,5,4,2),bty='n',las=1,cex.lab=2,cex.axis=2,cex.main=2)
plot(d,lwd=2,xlab='',ylab='',main='Nonfarm business sector \n Labour share',
     axes=F)
axis(1,tick=F)
axis(2,tick=F,line=-2)
