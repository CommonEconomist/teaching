#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#
# GDP growth
# Last update: 2018 02 28
#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#
source('code/lines.R')
library(reshape2)
d<-read.csv("data/eurostat_gdp_growth.csv",header=TRUE)
d<-d[d$TIME>=1999,]

# Process data
require(reshape2)
dat.l<-d[,c("GEO","TIME","Value")]
dat.l<-dat.l[order(dat.l$GEO,dat.l$TIME),]
dat.w<-reshape(dat.l,timevar="TIME",idvar=c("GEO"),direction="wide")

rownames(dat.w)<-dat.w$GEO;m<-dat.w[,-1]
startYear<-min(dat.l$TIME);endYear<-max(dat.l$TIME)

# Plot data
par(las=1,tck=0.02,mar=c(4,5,3,5),mgp=c(2.8,0.3,2.8),
    cex.lab=1.2,cex.axis=1.2,cex.main=1.7)

plot(0,xlim=c(startYear,endYear),ylim=c(-15,30),type="n",bty="n",
     main="GDP growth",xlab="",ylab="",axes=FALSE)

lifeLines(m,col="grey60")
lines(startYear:endYear,m[9,],col="black",lwd=2)       # Euro area
lines(startYear:endYear,m[13,],col="steelblue4",lwd=2) # Greece

axis(1,tick=FALSE); axis(2,tick=FALSE)

