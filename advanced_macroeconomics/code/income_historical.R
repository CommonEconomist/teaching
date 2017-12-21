## Example income over time
# Data taken from:
# http://www.ggdc.net/maddison/maddison-project/home.htm
setwd('~/Dropbox/github/teaching/advanced_macroeconomics')
par(mar=c(5,5,2,2),bty="n",las=1,cex.axis=2,cex.lab=2)

# Load data and write to vectors
maddison<-read.csv("data/maddison.csv",header=TRUE)
maddison<-maddison[maddison$Year>=1700,] # Subset due to missing values

year<-maddison$Year
nld<-maddison[,2] # Netherlands
zaf<-maddison[,3] # South Africa (Cape Colony)
idn<-maddison[,4] # Indonesia (Java)

# Plot data
plot(year,nld,ylim=c(450,26000),xlim=c(1700,2020),type="b",log="y",
     xlab="",ylab="",axes=FALSE,pch=0)
text(1720,nld[1]+250,"the Netherlands",cex=1.2)
par(new=TRUE)
plot(year,zaf,ylim=c(450,26000),xlim=c(1700,2020),type="b",log="y",
     xlab="",ylab="",axes=FALSE,pch=19)
text(1720,zaf[1]-400,"South Africa \n (Cape Colony)",cex=1.2)
par(new=TRUE)
plot(year,idn,ylim=c(450,26000),xlim=c(1700,2020),type="b",log="y",
     xlab="",ylab="",axes=FALSE,pch=17,col="steelblue4")
text(1790,idn[20],"Indonesia \n (Java)",cex=1.2)
axis(1,tick=FALSE);axis(2,tick=FALSE,line=-1)


