## Eurozone population projection
setwd('~/Dropbox/github/teaching/advanced_macroeconomics')
par(mar=c(5,5,2,2),bty='n',las=1,cex.axis=2,cex.lab=2)

require(jsonlite)
pop<-fromJSON("data/eurozone.json",flatten=TRUE)

Value<-pop$data$Value
pop_total<-ts(Value[1:91],start=c(1960,1),frequency=1)/1000000
pop_aged<-ts(Value[92:182],start=c(1960,1),frequency=1)/1000000

# Plot
par(bty="n",las=1,cex.axis=1.5,cex.lab=1.5,cex.main=1.7,mfrow=c(2,1))
plot(pop_total,axes=FALSE,xlab="",ylab="",main="Total population (millions)",
     lwd=2)
abline(v=2017,lty=2)
axis(2,tick=FALSE)

plot(pop_aged,axes=FALSE,xlab="",ylab="",
     main="Population aged 15-64 (millions)",lwd=2)
abline(v=2017,lty=2)
axis(2,tick=FALSE);axis(1,tick=FALSE)
