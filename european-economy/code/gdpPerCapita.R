#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#
# GDP EC8 over time
# Last update: 2018 02 20
#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#
par(las=1,tck=0.02,mar=c(4,5,3,5),mgp=c(2.8,0.3,2.8),
    cex.lab=2,cex.axis=2,cex.main=2)
source("code/lines.R")
library(reshape2)

# Prepare data
d<-read.csv("data/eurostat_gdp_per_capita.csv")
dat.l<-d[,c("GEO","TIME","Value")]
dat.l<-dat.l[order(dat.l$GEO,dat.l$TIME),]
dat.w<-reshape(dat.l,timevar="TIME",idvar=c("GEO"),direction="wide")
country<-c("Poland","Czech Republic","Hungary",
           "Lithuania","Romania","Estonia","Latvia","Slovak Republic",
           "Slovenia","Bulgaria","Romania")

rownames(dat.w)<-dat.w$GEO;m<-dat.w[,-1]
startYear<-min(dat.l$TIME);endYear<-max(dat.l$TIME)

# Plot data
plot(0,xlim=c(startYear,endYear),ylim=c(30,140),type="n",bty="n",
     main="GDP per capita (EU-28=100)",xlab="",ylab="",axes=FALSE)
lifeLines(m,col="grey60")
lifeLines(m[rownames(m) %in% country,],col="black",lwd=2)
lines(startYear:endYear,m[22,],col="firebrick3",lwd=2.5) # Portugal
axis(1,tick=FALSE); axis(2,tick=FALSE)
