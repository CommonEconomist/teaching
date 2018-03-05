#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#
# Business cycles
# Data source:
# https://fred.stlouisfed.org/series/GDPC1/
# https://fred.stlouisfed.org/series/PCECC96
# https://fred.stlouisfed.org/series/GPDIC1
#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#
library(mFilter)

# Load data
d<-read.csv('data/FRED.csv')
d<-d[,-1]

# Move to time-series object
dev<-list()
for(i in 1:4){
  x<-ts(d[,i],start=c(1947,1),frequency=4)
  trend<-hpfilter(x,freq=1600)$trend
  cycle<-hpfilter(x,freq=1600)$cycle
  dev[[i]]<-cycle/trend*100
}


# Plot figures
par(mar=c(5,5,4,2),bty='n',las=1,cex.lab=2,cex.axis=2,cex.main=2)
plot(dev[[1]],lty=2,axes=F,xlab='',ylab='',ylim=c(-5,5),
     main='GDP v. Consumption \n % deviations from trend')
lines(dev[[2]],lwd=2,col='blue')
axis(1,tick=F);axis(2,tick=F)

plot(dev[[1]],lty=2,axes=F,xlab='',ylab='',ylim=c(-25,30),
     main='GDP v. Investments \n % deviations from trend')
lines(dev[[3]],lwd=2,col='blue')
axis(1,tick=F);axis(2,tick=F)

plot(dev[[1]],lty=2,axes=F,xlab='',ylab='',ylim=c(-5,5),
     main='GDP v. Price index \n % deviations from trend')
lines(dev[[4]],lwd=2,col='blue')
axis(1,tick=F);axis(2,tick=F)
