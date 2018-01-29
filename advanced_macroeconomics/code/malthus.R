## Agricultural production and population in England
setwd('~/Dropbox/github/teaching/advanced_macroeconomics')
par(mar=c(5,5,2,2),bty='n',las=1,cex.lab=2,cex.axis=2,cex.main=2)

#### Example England ####
x<-read.csv('data/uk_agri.csv')

# Data to time-series
a<-ts(x$agri_output,start=min(x$year))
p<-ts(x$population,start=min(x$year))/x$population[x$year==1700]*100

a<-window(a,start=1300)
p<-window(p,start=1300)

# Plot data
plot(a,ylim=c(30,410),lwd=2,lty=2,col='steelblue4',axes=FALSE,
     xlab='',ylab='',main='England (1700=100)')
lines(p,lwd=2)
axis(1,tick=FALSE)
axis(2,tick=FALSE,line=-1)

text(1750,200,'Food',cex=2)
text(1780,90,'Population',cex=2)

#### Example Zimbabwe ####
fao<-read.csv('data/food_production.csv')
pop<-read.csv('data/population.csv')

# Data to time series
zwe.f<-ts(fao$Value[fao$Area=='Zimbabwe'],start=1961)
zwe.f=zwe.f/zwe.f[40]*100

zwe.p<-ts(pop$Value[pop$Area=='Zimbabwe'],start=1950)
zwe.p<-window(zwe.p,start=1961)
zwe.p=zwe.p/zwe.p[40]*100

# Plot
plot(zwe.f,ylim=c(30,150),lwd=2,lty=2,col='steelblue4',
     axes=FALSE,xlab='',ylab='',main='Zimbabwe (2000=100)')
lines(zwe.p,lwd=2)
axis(1,tick=FALSE)
axis(2,tick=FALSE,line=-1)

text(2000,110,'Population',cex=2)
text(2000,67,'Food',cex=2)
