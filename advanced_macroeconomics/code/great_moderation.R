## Example: the Great Moderation
par(mar=c(5,5,4,2),las=1,bty="n",cex.lab=2,cex.axis=2,cex.main=2) 
us<-read.csv("data/us_gdp.csv",header=TRUE)

# Libraries
require(mFilter)
require(zoo)

# Calculate GDP growth
gdp<-ts(us$gdp,start=c(1929,1),frequency=1)
gdp.l<-lag(gdp,-1,na.pad=TRUE)
gdp.g<-(gdp-gdp.l)/gdp*100

# Rolling average 5-year standard deviation
gdp.sd<-rollapply(gdp.g,width=5,FUN=sd)
infl<-ts(us$inflation,start=c(1929,1),frequency=1)
infl.sd<-rollapply(infl,width=5,FUN=sd)

# Plot data
plot(window(gdp.sd,start=c(1962,1)),main='Rolling 5-year average',
     axes=FALSE,xlab="",ylab="",ylim=c(0,6),lwd=2)
lines(window(infl.sd,start=c(1962,1)),col="steelblue4",lwd=2)

axis(1,tick=FALSE)
axis(2,tick=FALSE,line=-1)

text(1964,2.2,"GDP growth",cex=1.5)
text(1966,0,"Inflation",cex=1.5)

