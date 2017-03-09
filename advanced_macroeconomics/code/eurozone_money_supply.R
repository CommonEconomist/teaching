# Eurozone M3 money supply: month-on-month change
ms<-read.csv("~/Downloads/ecb_m3.csv",header=TRUE)
m3<-ts(ms[,3],start=c(1999,1),frequency=12)

par(las=1,bty="n",cex.axis=1.5,cex.lab=1.5)
plot(m3,lwd=2,xlab="",ylab="",axes=FALSE)
axis(1,tick=FALSE);axis(2,tick=FALSE)


