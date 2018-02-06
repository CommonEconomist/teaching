## Detrending time-series data
# Taken from: http://tinyurl.com/y8elvhbp
par(mar=c(5,5,4,2),las=1,bty="n",cex.lab=2,cex.axis=2,cex.main=2) 

# Libraries
require(Quandl)  # For data
require(mFilter) # For HP and BK filter
require(waveslim)# For wavelet

# Download data
gdp<-Quandl("FRED/GDPC1",order="asc") # Download data via Quandl
gdp<-ts(gdp$Value,start=c(1947,1),frequency=4)

# Plot raw data
plot(gdp,type='l',lwd=2,xlab='',ylab='',main='US real gdp \n (log)',
     axes=FALSE,log='y') 
axis(1,tick=FALSE)
axis(2,tick=FALSE,line=-1)

# Add linear trend line
abline(lm(gdp~index(gdp)),col="steelblue4",untf=TRUE,lwd=1.5)

#### Linear detrending ####
gdp<-log(gdp) # Log transform data

# Fit linear regression and use the residuals
gdp.dt<-ts(residuals(lm(gdp~index(gdp))),
           start=c(1947,1),frequency=4)
plot(gdp.dt,lty=2,lwd=1,xlab='',ylab='',axes=FALSE,col='grey50',
     main='Linearly detrended')
axis(1,tick=FALSE);axis(2,tick=FALSE,line=-1)

#### First differencing ####
gdp.fd<-diff(gdp)
plot(gdp.dt,lty=2,lwd=1,xlab='',ylab='',axes=FALSE,col='grey50',
     main='Linearly detrended vs. First-differences')
axis(1,tick=FALSE);axis(2,tick=FALSE,line=-1)
lines(gdp.fd,lwd=2) # Add to plot

#### Hodrick-Prescott filter ####
gdp.hp<-hpfilter(gdp,freq=1600)$cycle

# Plot against linear detrended data
plot(gdp.dt,lty=2,lwd=1,xlab="",ylab="",axes=FALSE,col='grey50',
     main='Linearly detrended vs. Hodrick-Prescott filter')
lines(gdp.hp,lwd=2)
axis(1,tick=FALSE);axis(2,tick=FALSE,line=-1)

#### Figure: Recessions ####
plot(gdp.hp,type="n",xlab="",ylab="",axes=FALSE,ylim=c(-.1,.1),
     main='HP filter and recessions')

# Recessions
rect(1948+11/12,-.15,1949+10/12,.15,col="grey90",lwd=0,border=NA)
rect(1953+7/12,-.15,1954+5/12,.15,col="grey90",lwd=0,border=NA)
rect(1957+8/12,-.15,1958+4/12,.15,col="grey90",lwd=0,border=NA)  
rect(1960+4/12,-.15,1961+2/12,.15,col="grey90",lwd=0,border=NA)  
rect(1969+12/12,-.15,1970+11/12,.15,col="grey90",lwd=0,border=NA)
rect(1973+11/12,-.15,1975+3/12,.15,col="grey90",lwd=0,border=NA)
rect(1980+1/12,-.15,1980+7/12,.15,col="grey90",lwd=0,border=NA)
rect(1981+7/12,-.15,1982+11/12,.15,col="grey90",lwd=0,border=NA)
rect(1990+7/12,-.15,1991+3/12,.15,col="grey90",lwd=0,border=NA)
rect(2001+3/12,-.15,2001+11/12,.15,col="grey90",lwd=0,border=NA)
rect(2007+12/12,-.15,2009+6/12,.15,col="grey90",lwd=0,border=NA)

lines(gdp.hp,lwd=2)
axis(1,tick=FALSE);axis(2,tick=FALSE,line=-1)

#### Baxter King filter ####
gdp.bk<-bkfilter(gdp,pl=6,pu=32,nfix=12)$cycle[,1]

# Compare with HP filter
plot(gdp.hp,lty=2,xlab="",ylab="",axes=FALSE,
     main='HP filter vs, Baxter-King filter')
lines(gdp.bk,lwd=2)
axis(1,tick=FALSE);axis(2,tick=FALSE,line=-1)

#### Wavelet filter ####
wavelet<-as.data.frame(mra(diff(gdp),J=5))
gdp.wv=ts(c(NA,cumsum(rowSums(wavelet[,3:4]))),
          start=c(1947,1),frequency=4)

# Compare with HP filter
plot(gdp.hp,lty=2,lwd=2,xlab="",ylab="",axes=FALSE,
     main='HP filter vs. Wavelet filter')
lines(gdp.wv,lwd=2)
axis(1,tick=FALSE);axis(2,tick=FALSE,line=-1)
