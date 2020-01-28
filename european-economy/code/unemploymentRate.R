#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#
# Unemployment
# Last update: 2018 04 18
#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#
source('code/lines.R')
library(reshape2)
d<-read.csv("data/eurostat_unemployment.csv",header=TRUE)
d<-d[d$TIME>=1999,]

# Process data
dat.l<-d[,c("GEO","TIME","Value")]
dat.l<-dat.l[order(dat.l$GEO,dat.l$TIME),]
dat.w<-reshape(dat.l,timevar="TIME",idvar=c("GEO"),direction="wide")

rownames(dat.w)<-dat.w$GEO;m<-dat.w[,-1]
startYear<-min(dat.l$TIME);endYear<-max(dat.l$TIME)

# Plot data
plot(0,xlim=c(startYear,endYear),ylim=c(0,30),type="n",bty="n",
     main="Unemployment rate",xlab="",ylab="",axes=FALSE)

lifeLines(m,col="grey60")
lines(startYear:endYear,m[9,],col="black",lwd=2)       # Euro area
lines(startYear:endYear,m[27,],col="firebrick3",lwd=2) # Spain
axis(1,tick=FALSE); axis(2,tick=FALSE)
