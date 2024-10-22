## VaR example
par(mar=c(5,5,1,1),bty='n',las=1)
x <- seq(-5,5,length=1000)
y <- dnorm(x, mean=0, sd=1.5)
plot(x,y,type='l',lwd=2,ylim=c(-.1,.4),xlim=c(-5,5),axes=F,ylab='',xlab='')
abline(h=0,lwd=2)
text(4,-.05,'W',cex=2)
text(median(x),.05,'W0',cex=2)
segments(median(x),-.02,median(x),.02,lwd=2)
segments(x[100],-.02,x[100],.02,lwd=2)
text(x[100],.05,'0.01',cex=2)
arrows(x[100],-.05,median(x),-.05,code=3,lwd=2)
text(x[300],-.07,'VaR',cex=2)


plot(x,y,type='l',lwd=2,ylim=c(-.1,.4),xlim=c(-5,5),axes=F,ylab='',xlab='')
abline(h=0,lwd=2)
text(4,-.05,'W',cex=2)
text(median(x),.05,'W0',cex=2)
segments(median(x),-.02,median(x),.02,lwd=2)
segments(x[100],-.02,x[100],.02,lwd=2)

text(x[85],.05,'p',cex=2)
arrows(x[100],-.05,median(x),-.05,code=3,lwd=2)
text(x[300],-.07,'Capital',cex=2)

## FIN