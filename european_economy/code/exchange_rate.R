## Exchange rate
d<-read.csv('data/exchange_rate.csv')
f<-1/ts(rev(d[,2]),start=c(1999,1),frequency=12)
p<-1/ts(rev(d[,3]),start=c(1999,1),frequency=12)

f=f/f[1]
p=p/p[1]

par(mar=c(5,5,2,2),las=1,bty='n',cex.lab=2,cex.axis=2)
plot(f,ylim=c(.7,1.6),axes=FALSE,ylab='',xlab='')
lines(p)
axis(1,tick=F);axis(2,tick=F)
