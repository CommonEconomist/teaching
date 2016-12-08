# Example time series data commodity prices.
# Focus on international prices of cocoa and rice
# Commodity price data taken from:
# https://www.imf.org/external/np/res/commod/index.aspx
# Producer Price Index data (WPU012) taken from:
# http://data.bls.gov/cgi-bin/surveymost?wp
par(bty="n",las=1,cex.axis=1.5,cex.lab=1.5,cex.main=1.7,mar=c(5,5,3,1))

# Load the commodit price data and create time-series object
imf<-read.csv("data/commodities.csv",header=TRUE)
rice<-ts(imf$Rice,start=c(1980,1),frequency=12)
cocoa<-ts(imf$Cocoa,start=c(1980,1),frequency=12)

# Plot raw data
par(mfrow=c(2,1))
plot(cocoa,xlab="",ylab="",main="Cocoa",axes=FALSE,lwd=2,ylim=c(500,6000))
axis(2,tick=FALSE)
plot(rice,xlab="",ylab="",main="Rice",axes=FALSE,lwd=2,ylim=c(100,1200))
axis(1,tick=FALSE)
axis(2,tick=FALSE)

# The prices are nominal, need to deflate to het real prices. 
# Use the US PPI (for grains) to deflate the prices. 
# Set 2010 as base year. 
ppi<-read.csv("data/ppi.csv",header=TRUE)
ppi$def<-(ppi$Value/mean(ppi[ppi$Year==2010,]$Value))
deflator<-ts(ppi$def,start=c(1980,1),frequency=12)

rice.r<-rice/deflator
cocoa.r<-cocoa/deflator

# Plot data along with nominal prices
par(mfrow=c(2,1))
plot(cocoa.r,xlab="",ylab="",main="Cocoa",axes=FALSE,lwd=2,col="steelblue4",
     ylim=c(500,6000))
lines(cocoa,lwd=2,lty=2)
axis(2,tick=FALSE)
plot(rice.r,xlab="",ylab="",main="Rice",axes=FALSE,lwd=2,col="steelblue4",
     ylim=c(100,1200))
lines(rice,lwd=2,lty=2)
axis(1,tick=FALSE)
axis(2,tick=FALSE)

# Next calculate volatility. 
# Follow standard practice by using the change in logarithmic prices
rice.pc=log(rice.r)-log(lag(rice.r,k=-1))
cocoa.pc=log(cocoa.r)-log(lag(cocoa.r,k=-1))

# Let's look at price change for cocoa:
par(mfrow=c(1,1))
plot(cocoa.pc,xlab="",ylab="",main="Cocoa",axes=FALSE,lwd=2,ylim=c(-.3,.45),
     type="h")
abline(h=0,lty=2)
axis(1,tick=FALSE);axis(2,tick=FALSE)

mean(cocoa.pc[325:441])   # Volatility since 2007
mean(cocoa.pc[-325:-441]) # Volatility before 2007

# For rice:
plot(rice.pc,xlab="",ylab="",main="Rice",axes=FALSE,lwd=2,ylim=c(-.3,.45),
     type="h")
abline(h=0,lty=2)
axis(1,tick=FALSE);axis(2,tick=FALSE)

mean(rice.pc[325:441])    # Volatility since 2007
mean(rice.pc[-325:-441])  # Volatility before 2007

# Fit AR model to data
rice.ar<-ar(rice.pc);rice.ar
cocoa.ar<-ar(cocoa.pc);cocoa.ar
