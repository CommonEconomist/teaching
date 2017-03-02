## Choosing priors

# Normal distribution
x<-seq(-5,5,length=1000)
y1<-dnorm(x,0,.5)
y2<-dnorm(x,0,1)
y3<-dnorm(x,0,5)

par(mar=c(4,4,1,1),bty="n",cex.axis=1.5,cex.lab=1.5,las=1)
plot(x,y1,type="l",lwd=2,col="black",axes=FALSE,xlab="",ylab="",
     xlim=c(-5,5),ylim=c(0,1))
lines(x,y2,lwd=2,col="steelblue4")
lines(x,y3,lwd=2,col="firebrick3")
axis(1,tick=FALSE)
axis(2,tick=FALSE)

# Gamma distribution
x<-seq(0,20,length=1000)
y1<-dgamma(x,shape=1,scale=2)
y2<-dgamma(x,shape=3,scale=2)
y3<-dgamma(x,shape=7,scale=1)

plot(x,y1,type="l",lwd=2,col="black",axes=FALSE,xlab="",ylab="",
     xlim=c(0,20),ylim=c(0,.5))
lines(x,y2,lwd=2,col="steelblue4")
lines(x,y3,lwd=2,col="firebrick3")
axis(1,tick=FALSE)
axis(2,tick=FALSE)

# Beta distribution
x<-seq(0,1,length=1000)
y1<-dbeta(x,.5,.5)
y2<-dbeta(x,2,5)
y3<-dbeta(x,2,2)

plot(x,y1,type="l",lwd=2,col="black",axes=FALSE,xlab="",ylab="",
     xlim=c(0,1),ylim=c(0,2.5))
lines(x,y2,lwd=2,col="steelblue4")
lines(x,y3,lwd=2,col="firebrick3")
axis(1,tick=FALSE)
axis(2,tick=FALSE)

