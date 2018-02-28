# Inflation eurozone
par(mar=c(5,5,2,2),las=1,bty='n',cex.lab=2,cex.axis=2,cex.main=2)
library(plyr)
x<-read.csv('data/oecd_inflation.csv',stringsAsFactors=F)

EUR<-c('AUT','BEL','DEU','ESP','FIN','FRA','GRC','IRL','ITA','LUX','NLD','PRT')
x<-x[x$LOCATION %in% EUR,]
t<-ddply(x,.(LOCATION),summarise,q=min(TIME))


aut<-ts(x$Value[x$LOCATION=='AUT'],start=c(1959,1),frequency=4)
bel<-ts(x$Value[x$LOCATION=='BEL'],start=c(1956,1),frequency=4)
deu<-ts(x$Value[x$LOCATION=='DEU'],start=c(1956,1),frequency=4)
esp<-ts(x$Value[x$LOCATION=='ESP'],start=c(1955,2),frequency=4)
fin<-ts(x$Value[x$LOCATION=='FIN'],start=c(1956,1),frequency=4)
fra<-ts(x$Value[x$LOCATION=='FRA'],start=c(1956,1),frequency=4)
grc<-ts(x$Value[x$LOCATION=='GRC'],start=c(1955,1),frequency=4)
irl<-ts(x$Value[x$LOCATION=='IRL'],start=c(1976,1),frequency=4)
ita<-ts(x$Value[x$LOCATION=='ITA'],start=c(1956,1),frequency=4)
lux<-ts(x$Value[x$LOCATION=='LUX'],start=c(1956,1),frequency=4)
nld<-ts(x$Value[x$LOCATION=='NLD'],start=c(1960,2),frequency=4)
prt<-ts(x$Value[x$LOCATION=='PRT'],start=c(1956,1),frequency=4)

plot(aut,ylim=c(-10,50),axes=F,xlab='',ylab='',main='CPI')
lines(bel)
lines(deu)
lines(esp)
lines(fin)
lines(fra)
lines(grc)
lines(irl)
lines(ita)
lines(lux)
lines(nld)
lines(prt)

axis(1,tick=F)
axis(2,tick=F)
