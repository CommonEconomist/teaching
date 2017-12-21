## Bond yields for PIGS economies
# (Include Germany as reference)
setwd('~/Dropbox/github/teaching/advanced_macroeconomics')
par(mar=c(5,5,2,2),bty='n',las=1,cex.axis=2,cex.lab=2)

# Load data
bonds<-read.csv("data/bonds.csv",header=TRUE)

# Data to time-series
portugal<-ts(bonds[bonds$GEO=="Portugal",]$Value,start=c(1980,1),frequency=12)
ireland<-ts(bonds[bonds$GEO=="Ireland",]$Value,start=c(1980,1),frequency=12)
greece<-ts(bonds[bonds$GEO=="Greece",]$Value,start=c(1980,1),frequency=12)
spain<-ts(bonds[bonds$GEO=="Spain",]$Value,start=c(1980,1),frequency=12)
germany<-ts(bonds[bonds$GEO=="Germany (until 1990 former territory of the FRG)",]$Value,start=c(1980,1),frequency=12)

# Subset to last 10 years
portugal<-window(portugal,start=c(2006,1),frequency=12)
ireland<-window(ireland,start=c(2006,1),frequency=12)
greece<-window(greece,start=c(2006,1),frequency=12)
spain<-window(spain,start=c(2006,1),frequency=12)
germany<-window(germany,start=c(2006,1),frequency=12)

# Plot data
plot(portugal,ylim=c(0,30),col="chartreuse4",axes=FALSE,xlab="",ylab="%",
     lwd=2)
lines(ireland,col="steelblue4",lwd=2,lty=2)
lines(greece,col="dodgerblue2",lwd=2)
lines(spain,col="firebrick3",lwd=2)
lines(germany,lty=2,lwd=2)
axis(1,tick=FALSE)
axis(2,tick=FALSE)

text(2012,.5,"Germany",cex=1.2)
text(2012,4,"Spain",cex=1.2)
text(2012,10,"Ireland",cex=1.2)
text(2012,14.2,"Portugal",cex=1.2)
text(2012,17,"Greece",cex=1.2)
