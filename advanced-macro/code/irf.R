# Simulate Impulse Response Function for AR(2) model
par(mar=c(4,5,2,1),las=1,cex.axis=2,cex.lab=2)

# Response function AR(1) model
y<-c(1,.5*1,rep(NA,18))
for(i in 3:length(y)){y[i]=.5*y[i-1] }

plot(y,type="l",lwd=2,ylim=c(0,1),xlab="",ylab="",axes=FALSE,main='IRF AR(1)')
axis(1,tick=FALSE,at=1:20);axis(2,tick=FALSE)

# Response functions AR(2) model
y1<-c(1,.6*1,rep(NA,18))
for(i in 3:20){
  y1[i]=.6*y1[i-1]+.3*y1[i-2]
}

y2<-c(1,1.5*1,rep(NA,18))
for(i in 3:20){
  y2[i]=1.5*y2[i-1]+-.6*y2[i-2]
}

# Plot results
plot(y1,type="l",lwd=2,ylim=c(-.1,1.7),xlab="",ylab="",axes=FALSE,
     main='IRF AR(2)')
lines(y2,lwd=2,col="steelblue4",lty=2)
axis(1,tick=FALSE,at=1:20);axis(2,tick=FALSE)

text(2.4,y1[2]-.15,cex=1.5,expression(y==paste(0.6*y[t-1]+0.3*y[t-2])))
text(3.3,y2[3]+.05,cex=1.5,expression(y==paste(1.5*y[t-1]-0.6*y[t-2])))
