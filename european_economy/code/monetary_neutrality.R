## Exchange rate
d<-read.csv('data/exchange_rate.csv')
hcip<-ts(d[,2],start=c(1996,1),frequency=12)
m3<-ts(d[,3],start=c(1996,1),frequency=12)

hcip=hcip/hcip[1]
m3=m3/m3[1]

par(mar=c(5,5,2,2),las=1,bty='n',cex.lab=2,cex.axis=2)
plot(hcip,ylim=c(.5,1.5),axes=FALSE,ylab='',xlab='')
lines(m3)
axis(1,tick=F);axis(2,tick=F)
