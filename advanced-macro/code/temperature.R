## Example temperature: Average global temperature
# Source: https://climate.nasa.gov/vital-signs/global-temperature/
par(mar=c(4,4,3.5,1),las=1,cex.axis=2,cex.main=2)
temp<-read.csv("data/global_temperature.csv",sep="",header=F)

temp.av<-ts(temp[,2],start=c(1880,1),freq=1) # Average
temp.ra<-ts(temp[,3],start=c(1880,1),freq=1) # 5-year moving average

# Plot data
plot(temp.av,axes=FALSE,xlab="",ylab="",type='b',
     main="Temperature anomaly \n (base=1951-1980)")
lines(temp.ra,col="steelblue4",lwd=2.5)
abline(h=0,lty=2)
axis(1,tick=FALSE,line=-1);axis(2,tick=FALSE,line=-1)
