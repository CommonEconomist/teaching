# Public spending
d<-read.csv('data/eurostat_public_spending.csv')
require(plyr)
x<-ddply(d,.(TIME),summarise,ps=mean(Value,na.rm=TRUE))
y<-ts(x$ps,start=1999)
plot(y,ylim=c(30,60),lwd=2,axes=F,xlab='',ylab='',
     main='EU average government spending')
axis(1,tick=F);axis(2,tick=F,line=-1)
