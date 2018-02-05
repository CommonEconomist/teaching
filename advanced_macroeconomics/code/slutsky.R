## Time-series data: Slutsky effect
par(mar=c(5,5,4,2),bty='n',las=1,cex.lab=2,cex.axis=2)
ma<-function(x,n)filter(x,rep(1/n,n),sides=1) 

set.seed(2018);x<-rnorm(500)
t<-1:500

# Plot part of data
plot(x[5:17],type='l',lwd=2,xlab='t',ylab='Y',axes=FALSE)
axis(1,tick=FALSE);axis(2,tick=FALSE,line=-1)

# Smooth data
ma10<-ma(x,10)
ma25<-ma(x,25)
lo<-loess(x~t,span=1/3)

plot(ma10,type='l',xlim=c(0,500),xlab='',ylab='',axes=FALSE,col='grey50')
axis(1,tick=FALSE);axis(2,tick=FALSE,line=-1)
lines(ma25,lty=2,col='blue')
lines(lo$fit,col="darkred", lwd=2)
abline(h=0,lty=5)


plot(ma10,type='l',xlim=c(0,500),ylim=c(-3,3),
     xlab='',ylab='',axes=FALSE,col='grey50')
axis(1,tick=FALSE);axis(2,tick=FALSE,line=-1)
lines(ma25,lty=2,col='blue')
lines(lo$fit,col="darkred", lwd=2)
abline(h=0,lty=5)
points(x,cex=.1)




