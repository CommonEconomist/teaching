par(mar=c(5,5,2,2),las=1,bty="n",cex.lab=2)
plot(0:10,0:10,type="n",axes=FALSE,xlab="Qd",ylab="Qm")

segments(0,4,8,0,lwd=5)
segments(0,7,5.6,0,lwd=5,col="steelblue4")

segments(0,2,4,2,lty=2,lwd=2)
segments(4,0,4,2,lty=2,lwd=2)

axis(2,tick=FALSE,at=c(2,4,7),label=c("5V-G","3V","3/2G"),
     line=-2,cex.axis=2)
axis(1,tick=FALSE,at=c(4,5.6,8),label=c("2G-4V","6/5G","6V"),
     line=-1.5,cex.axis=2)

