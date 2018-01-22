## Misc. figures lecture on identification
setwd('~/Dropbox/github/teaching/advanced_macroeconomics')
par(mar=c(5,5,2,2),bty='n',las=1,cex.axis=2,cex.lab=2,cex.main=2)

## Military expenditures
x<-read.csv('data/military-expenditure-by-country-in-thousands-of-2000-us-dollars.csv')
x<-x[x$Code=='USA',]
us<-ts(x[,4],start=min(x$Year))/1000000000

plot(us,lwd=2,axes=FALSE,xlab='',ylab='')
axis(1,tick=FALSE);axis(2,tick=FALSE)
text(1990,700,'US military expenditures \n in billion USD',cex=2)

## Inflation
x<-read.csv('data/GDPCTPI.csv')

d$gdp.l<-lag(d[,2],lag=1)
d$pi=400*(log(d[,2]/d[,3]))

pi<-ts(d$pi,start=c(1947,1),frequency=4)

plot(pi,lwd=2,axes=FALSE,xlab='',ylab='',main='US inflation rate')
axis(1,tick=FALSE);axis(2,tick=FALSE)
