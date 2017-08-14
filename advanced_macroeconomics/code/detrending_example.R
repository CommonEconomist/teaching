## Examples of detrending time-series data
# Taken from: http://tinyurl.com/y8elvhbp
par(mar=c(5,5,2,2),las=1,bty="n",cex.lab=2,cex.axis=2) # Plot settings

# Libraries
require(Quandl)  # For data
require(mFilter) # For HP and BK filter
require(waveslim)# For wavelet

# Download data
gdp<-Quandl("FRED/GDPC1",order="asc") # Download data via Quandl

# Log transform data and write to time-series object
gdp<-ts(log(gdp$Value),start=c(1947,1),frequency=4)

# Plot raw data
plot(gdp,type="l",lwd=2,xlab="",ylab="",axes=FALSE) 
axis(1,tick=FALSE)
axis(2,tick=FALSE)

#### Linear detrending ####
# Fit linear regression and use the residuals
gdp.dt<-ts(residuals(lm(gdp~index(gdp))),
           start=c(1947,1),frequency=4)

plot(gdp.dt,lty=2,lwd=2,xlab="",ylab="",axes=FALSE)

#### First differencing ####
gdp.fd<-diff(gdp)
lines(gdp.fd,lwd=2) # Add to plot

#### Hodrick-Prescott filter ####
gdp.hp<-hpfilter(gdp,freq=1600)$cycle

# Plot against linear detrended data
plot(gdp.dt,lty=2,lwd=2,xlab="",ylab="",axes=FALSE)
lines(gdp.hp,lwd=2)
axis(1,tick=FALSE);axis(2,tick=FALSE)

#### Baxter King filter ####
gdp.bk<-bkfilter(gdp,pl=6,pu=32,nfix=12)$cycle[,1]

# Compare with HP filter
plot(gdp.hp,lty=2,lwd=2,xlab="",ylab="",axes=FALSE)
lines(gdp.bk,lwd=2)
axis(1,tick=FALSE);axis(2,tick=FALSE)

#### Wavelet filter ####
wavelet<-as.data.frame(mra(diff(gdp),J=5))
gdp.wv=ts(c(NA,cumsum(rowSums(wavelet[,3:4]))),
          start=c(1947,1),frequency=4)

# Compare with BK filter
plot(gdp.bk,lty=2,lwd=2,xlab="",ylab="",axes=FALSE)
lines(gdp.wv,lwd=2)
axis(1,tick=FALSE);axis(2,tick=FALSE)
