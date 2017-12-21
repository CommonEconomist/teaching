## Agricultural production and population in England
setwd('~/Dropbox/github/teaching/advanced_macroeconomics')
x<-read.csv('data/uk_agri.csv')

# Data to time-series
a<-ts(x$agri_output,start=min(x$year))
p<-ts(x$population,start=min(x$year))/x$population[x$year==1700]*100

a<-window(a,start=1300)
p<-window(p,start=1300)

# Plot data
par(mar=c(5,5,2,2),bty='n',las=1,cex.lab=2,cex.axis=2)
plot(a,ylim=c(30,410),lwd=2,lty=2,col='steelblue4',axes=FALSE,xlab='',ylab='')
lines(p,lwd=2)
axis(1,tick=FALSE)
axis(2,tick=FALSE,line=-1)
