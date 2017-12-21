## UK GDP 
setwd('~/Dropbox/github/teaching/advanced_macroeconomics')
par(mar=c(5,5,2,2),bty='n',las=1,cex.axis=2,cex.lab=2)

x<-read.csv('data/uk_gdp.csv',header=FALSE)
gdp<-ts(x[,2],start=min(x[,1]))

plot(gdp,lwd=2,axes=FALSE,xlab='',ylab='',log='y')
axis(1,tick=FALSE)
axis(2,tick=FALSE,line=-1,
     at=c(10000,100000,1000000),lab=c('10^4','10^5','10^6'))
text(1500,1000000,'Real GDP (in millions)',cex=2)
