## Example time series data commodity prices.
# Focus on international prices of rice
# Commodity price data taken from:
# https://www.imf.org/external/np/res/commod/index.aspx
# Producer Price Index data (WPU012) taken from:
# http://data.bls.gov/cgi-bin/surveymost?wp
setwd('~/Dropbox/github/teaching/advanced_macroeconomics')
par(mar=c(5,5,4,2),bty='n',las=1,cex.axis=2,cex.lab=2,cex.main=2)

# Load the commodity price data and create time-series object
imf<-read.csv("data/commodities.csv",header=TRUE)
rice<-ts(imf$Rice,start=c(1980,1),frequency=12)
#cocoa<-ts(imf$Cocoa,start=c(1980,1),frequency=12)

# Plot raw data
par(mfrow=c(1,1))
plot(rice,xlab="",ylab="",main="International rice prices \n (nominal)",
     axes=FALSE,lwd=2,ylim=c(100,1200))
axis(1,tick=FALSE)
axis(2,tick=FALSE)

# The prices are nominal, need to deflate to het real prices. 
# Use the US PPI (for grains) to deflate the prices. 
# Set 2010 as base year. 
ppi<-read.csv("data/ppi.csv",header=TRUE)
ppi$def<-(ppi$Value/mean(ppi[ppi$Year==2010,]$Value))
deflator<-ts(ppi$def,start=c(1980,1),frequency=12)
rice.r<-rice/deflator

# Plot data along with nominal prices
plot(rice.r,xlab="",ylab="",main="International rice prices \n (real)",
     axes=FALSE,lwd=2,col="steelblue4",ylim=c(100,1200))
lines(rice,lwd=1,lty=2,col='grey50')
axis(1,tick=FALSE)
axis(2,tick=FALSE)

# For illustration, can decompose the data:
rice.dt<-decompose(rice.r)
par(mar=c(4,5,3,1),las=0)
plot(rice.dt)

# Next calculate volatility. 
# Follow standard practice by using the change in logarithmic prices
rice.pc=log(rice.r)-log(lag(rice.r,k=-1))

plot(rice.pc,xlab="",ylab="",main="",axes=FALSE,lwd=2,ylim=c(-.3,.45),
     type="h")
abline(h=0,lty=2)
axis(1,tick=FALSE);axis(2,tick=FALSE)

mean(rice.pc[325:441])    # Volatility since 2007
mean(rice.pc[-325:-441])  # Volatility before 2007

# Fit AR model to data
rice.ar<-ar(rice.pc);rice.ar


###
est<-rice.ar$ar
rice.sim<-rep(NA,24)
rice.sim[1]<-1
rice.sim[2]<-rice.sim[1]*est[1]
rice.sim[3]<-rice.sim[1]*est[2]+rice.sim[2]*est[1]
rice.sim[4]<-rice.sim[1]*est[3]+rice.sim[2]*est[2]+rice.sim[3]*est[1]
rice.sim[5]<-rice.sim[1]*est[4]+rice.sim[2]*est[3]+rice.sim[3]*est[2]+
  rice.sim[4]*est[1]

for(i in 6:length(rice.sim)){
  rice.sim[i]=est[1]*rice.sim[i-1]+est[2]*rice.sim[i-2]+
    est[3]*rice.sim[i-3]+est[4]*rice.sim[i-4]
}

plot(rice.sim,type="l",lwd=2,ylim=c(-.15,1.15),main='AR(4) model for rice \n',
     xlab="",ylab="",axes=FALSE)
abline(h=0,lty=2)
axis(1,tick=FALSE,at=1:24);axis(2,tick=FALSE)

