#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#
# Budget deficit
# Last update: 2018 02 28
#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#
source('code/lines.R')
d<-read.csv("data/eurostat_budget_deficit.csv",header=TRUE)
d<-d[d$TIME>=1999,]

# Process data
dat.l<-d[,c("GEO","TIME","Value")]
dat.l<-dat.l[order(dat.l$GEO,dat.l$TIME),]
dat.w<-reshape(dat.l,timevar="TIME",idvar=c("GEO"),direction="wide")

rownames(dat.w)<-dat.w$GEO;m<-dat.w[,-1]
startYear<-min(dat.l$TIME);endYear<-max(dat.l$TIME)

# Plot data
plot(0,xlim=c(startYear,endYear),ylim=c(-35,10),type="n",bty="n",
     main="Budget surplus (% of GDP)",xlab="",ylab="",axes=FALSE)
abline(h=-3,lwd=2.5)

lifeLines(m,col="grey60")
lines(startYear:endYear,m[9,],col="black",lwd=2,lty=2)   # Euro area
lines(startYear:endYear,m[14,],col="steelblue4",lwd=2) # Greece
axis(1,tick=FALSE); axis(2,tick=FALSE)
