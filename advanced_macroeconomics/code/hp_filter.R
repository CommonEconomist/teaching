## Example; Different lamba values in HP-filter
par(mar=c(5,5,4,2),las=1,bty="n",cex.lab=2,cex.axis=2,cex.main=2) 

library(mFilter)
library(Quandl)

# Download data
gdp<-Quandl("FRED/GDPC1",order="asc") # Download data via Quandl
gdp<-ts(log(gdp$Value),start=c(1947,1),frequency=4)

# Apply HP filter
hp1<-hpfilter(gdp,type='lambda',freq=100)$trend
hp2<-hpfilter(gdp,type='lambda',freq=1600)$trend
hp3<-hpfilter(gdp,type='lambda',freq=25600)$trend

# plot results
plot(window(gdp,start=c(2003,1)),col='grey50',xlab='',ylab='',axes=FALSE)
lines(hp1,col='steelblue4',lty=2,lwd=2)
lines(hp2,lty=3,lwd=2)
lines(hp3,col='firebrick3',lty=4,lwd=2)

axis(1,tick=FALSE)
axis(2,tick=FALSE,line=-1)

