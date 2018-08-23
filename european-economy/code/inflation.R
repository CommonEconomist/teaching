
par(mar=c(5,5,2,2),las=1,bty='n',cex.lab=2,cex.axis=2,cex.main=2)

x<-read.csv('data/oecd_inflation.csv')
ger<-ts(x$Value[x$LOCATION=='DEU'],start=c(1950,1),frequency=4)
fra<-ts(x$Value[x$LOCATION=='FRA'],start=c(1950,1),frequency=4)
ita<-ts(x$Value[x$LOCATION=='ITA'],start=c(1950,1),frequency=4)

ger<-window(ger,start=c(1979,1))
fra<-window(fra,start=c(1979,1))
ita<-window(ita,start=c(1979,1))

plot(ger,ylim=c(-1,10),lty=2,lwd=2,axes=F,xlab='',ylab='',main='CPI')
lines(fra,col='steelblue4',lwd=2)
lines(ita,col='firebrick3',lwd=2)
axis(1,tick=F);axis(2,tick=F,line=-1)
text(1984,1,'GER',cex=2)
text(1984,4,'FRA',cex=2)
text(1984,7,'ITA',cex=2)
