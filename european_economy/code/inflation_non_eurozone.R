# Inflation eurozone
par(mar=c(5,5,2,2),las=1,bty='n',cex.lab=2,cex.axis=2,cex.main=2)
library(plyr)
x<-read.csv('data/oecd_inflation.csv',stringsAsFactors=F)

EUR<-c('AUT','BEL','DEU','ESP','FIN','FRA','GRC','IRL','ITA','LUX','NLD','PRT')
eur<-ddply(x[x$LOCATION %in% EUR,],.(TIME),summarise,cpi=mean(Value))

x<-x[x$LOCATION %in% c('CZE','DNK','HUN','POL','SWE'),]
t<-ddply(x,.(LOCATION),summarise,q=min(TIME))

cze<-ts(x$Value[x$LOCATION=='CZE'],start=c(1992,1),frequency=4)
dnk<-ts(x$Value[x$LOCATION=='DNK'],start=c(1967,1),frequency=4)
hun<-ts(x$Value[x$LOCATION=='HUN'],start=c(1981,1),frequency=4)
pol<-ts(x$Value[x$LOCATION=='POL'],start=c(1990,1),frequency=4)
swe<-ts(x$Value[x$LOCATION=='SWE'],start=c(1956,1),frequency=4)
eur<-ts(eur[,2],start=c(1955,2),frequency=4)

cze<-window(cze,start=c(1992,1))
dnk<-window(dnk,start=c(1992,1))
hun<-window(hun,start=c(1992,1))
pol<-window(pol,start=c(1992,1))
swe<-window(swe,start=c(1992,1))
eur<-window(eur,start=c(1992,1))


plot(cze,ylim=c(-2,50),axes=F,xlab='',ylab='',main='CPI',lwd=2,col='firebrick3')
lines(dnk,lwd=2,lty=2)
lines(hun,lwd=2,col='firebrick3')
lines(pol,lwd=2,col='firebrick3')
lines(swe,lwd=2,lty=2)
lines(eur,col='steelblue4',lwd=2)



axis(1,tick=F)
axis(2,tick=F,line=-2)
