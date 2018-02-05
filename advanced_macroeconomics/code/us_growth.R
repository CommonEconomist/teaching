## Example: Different growth rate US economy
par(mar=c(5,5,4,2),las=1,bty="n",cex.lab=2,cex.axis=2,cex.main=2) 
us<-read.csv("data/us_gdp.csv",header=TRUE)
ma<-function(x,n)filter(x,rep(1/n,n),sides=1) # Function moving average

# Calculate GDP growth
gdp<-ts(us$gdp,start=c(1929,1),frequency=1)
gdp.l<-lag(gdp,-1,na.pad=TRUE)
gdp.g<-(gdp-gdp.l)/gdp*100

gdp.m<-ma(gdp.g,10)


# Plot data
plot(window(gdp.g,start=c(1950,1)),main='',
     axes=FALSE,xlab="",ylab="",ylim=c(-5,15),lwd=1,type='b',col='grey50')
lines(window(gdp.m,start=c(1950,1)),lwd=2)
abline(h=mean(window(gdp.g,start=c(1950,1))),lty=2)

axis(1,tick=FALSE)
axis(2,tick=FALSE,line=-1)

