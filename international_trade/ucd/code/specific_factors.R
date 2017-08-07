## Figures specific factors model

require(WDI)


trade_m<-WDIsearch("trade",field="name",short=FALSE) # 108
un_m<-WDIsearch("unemployment",field="name",short=FALSE) # 18

wdi<-WDI("ES",c(trade_m[108,1],un_m[18,1]),start=1991,end=2015)

trd<-ts(wdi[,4],start=c(1991,1))
uem<-ts(wdi[,5],start=c(1991,1))

par(mar=c(5,6,2,2),las=1,bty="n",cex.lab=2,cex.axis=2)
plot(trd,lwd=2,axes=FALSE,xlab="",ylab="",ylim=c(5,65))
lines(uem,lwd=2,lty=2)
axis(1,tick=FALSE)
axis(2,tick=FALSE)

text(2009,44,"Imports (% GDP)",cex=1.5)
text(2009,30,"Unemployment (% workforce)",cex=1.5)
