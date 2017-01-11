# Average global temperature
temp<-read.csv("data/temperature.csv",sep="")
par(mar=c(4,4,3.5,1),las=1,cex.axis=1.7,cex.main=1.7)


temp.av<-ts(temp[,2],start=c(1880,1),freq=1) # Average
temp.ra<-ts(temp[,3],start=c(1880,1),freq=1) # 5-year moving average

# Plot data
plot(temp.av,axes=FALSE,xlab="",ylab="",lwd=2.5,
     main="Temperature anomaly (base=1951-1980)")
lines(temp.ra,col="steelblue4",lty=2,lwd=2.5)
abline(h=0,lty=2)
axis(1,tick=FALSE,line=-1);axis(2,tick=FALSE,line=-1)
