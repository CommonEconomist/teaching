## Productivity
setwd('~/Dropbox/github/teaching/advanced_macroeconomics')
par(mar=c(5,5,2,2),bty='n',las=1,cex.axis=2,cex.lab=2)

# Function for drawing lines 
lifeLines<-function(series,col="black",hcol="black",lwd=1,hlwd=2){
  for (i in 1:length(series[,1])){
    lines(startYear:endYear,series[i,],col=col,lwd=lwd)
  }
}

# Data
require(reshape2)
oecd<-read.csv('data/oecd.csv')
df<-data.frame(country=oecd$Country,year=oecd$Time,gdp.h=oecd$Value)
df.w<-reshape(df,timevar="year",idvar=c("country"),direction="wide")
rownames(df.w)<-df.w$country; m<-df.w[,-1]
startYear<-min(df$year);endYear<-max(df$year)

# Plot
par(las=1,tck=0.02,mar=c(4,5,3,5),mgp=c(2.8,0.3,2.8),
    cex.lab=1.2,cex.axis=1.2,cex.main=0.9)
plot(0,xlim=c(startYear,endYear),ylim=c(0,100),type="n",bty="n",
     main="",xlab="",ylab="GDP per hour worked",axes=FALSE)

lifeLines(m,col="grey60")
lines(startYear:endYear,m[16,],col="black",lwd=2.5)
lines(startYear:endYear,m[13,],col="steelblue4",lwd=2.5)
axis(1,tick=FALSE); axis(2,tick=FALSE)

