## Industrial production
setwd('~/Dropbox/github/teaching/advanced_macroeconomics')
par(mar=c(5,5,2,2),bty='n',las=1,cex.axis=2,cex.lab=2,cex.main=2)

# Data
x<-read.csv('data/INDPRO.csv')
ip<-ts(x[,2],start=c(1919,1),frequency=12)
ip=ip/ip[1]*100

# plot
plot(ip,xlab="",ylab="",axes=FALSE,main="Industrial production",type='n')
rect(1920,75,1920.5,195,lwd=0,col='grey70',border=NA)
rect(1931.83,75,1931.92,195,lwd=0,col='grey70',border=NA)
rect(1936.583,75,1937.167,195,lwd=0,col='grey70',border=NA)

lines(ip)
axis(1,tick=FALSE);axis(2,tick=FALSE,line=-2)
