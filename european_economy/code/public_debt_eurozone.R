# Public debt over time
# Last update: 2018 04
par(mar=c(5,5,2,2),las=1,bty='n',cex.lab=2,cex.axis=2,cex.main=2)
library(plyr)
x<-read.csv('data/eurostat_public_debt.csv',stringsAsFactors=F)


geo<-sort(unique(x$GEO))
eur<-geo[c(1,2,11,12,13,14,16,17,20,22,24,28)]
x<-x[x$GEO %in% eur,]

# probably quiker way to do this
aut<-ts(x$Value[x$GEO==eur[1]],start=1995)
bel<-ts(x$Value[x$GEO==eur[2]],start=1995)
deu<-ts(x$Value[x$GEO==eur[5]],start=1995)
esp<-ts(x$Value[x$GEO==eur[12]],start=1995)
fin<-ts(x$Value[x$GEO==eur[3]],start=1995)
fra<-ts(x$Value[x$GEO==eur[4]],start=1995)
grc<-ts(x$Value[x$GEO==eur[6]],start=1995)
irl<-ts(x$Value[x$GEO==eur[7]],start=1995)
ita<-ts(x$Value[x$GEO==eur[8]],start=1995)
lux<-ts(x$Value[x$GEO==eur[9]],start=1995)
nld<-ts(x$Value[x$GEO==eur[10]],start=1995)
prt<-ts(x$Value[x$GEO==eur[11]],start=1995)

plot(aut,ylim=c(5,180),axes=F,xlab='',ylab='',main='Public debt (% of GDP)')
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

abline(h=60,lty=2)
axis(1,tick=F)
axis(2,tick=F)
