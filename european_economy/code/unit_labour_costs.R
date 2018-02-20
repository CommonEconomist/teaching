#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#
# Relative unit labour costs
# Last update: 2018 02 20
#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#
library(reshape2)
source("code/lines.R")

# Load and subset data
d<-read.csv("data/eurostat_labour_productivity.csv")
euro<-c("Austria","Belgium","Finland","France",
        "Germany (until 1990 former territory of the FRG)","Greece","Ireland",
        "Italy","Luxembourg","Netherlands","Portugal","Spain")

d<-d[d$NA_ITEM=="Nominal unit labour cost based on persons",]
d<-d[d$TIME>=1999 & d$GEO %in% euro,]
d<-d[order(d$GEO,d$TIME),]

# Reshape data from long to wide
d.l<-d[,c("GEO","TIME","Value")]
d.w<-reshape(d.l,timevar="TIME",idvar=c("GEO"),direction="wide")
rownames(d.w)<-d.w$GEO; m<-d.w[,-1]
startYear<-1999;endYear<-2016
m=m/m[,1]*100

# Trend line
l<-c(100)
for(i in 1:length(startYear:endYear)){
  l[i+1]=l[i]*1.02
}

# Plot figure
par(las=1,tck=0.02,mar=c(4,5,3,5),mgp=c(2.8,0.3,2.8),
    cex.lab=1.2,cex.axis=1.2,cex.main=0.9)

plot(0,xlim=c(startYear,endYear),ylim=c(95,160),type="n",bty="n",
     main="",xlab="Nominal Unit Labour Cost Index",ylab="",axes=FALSE)
lines(startYear:endYear,l[1:18],lwd=2,col="black")

lifeLines(m,col="grey60") # Inflation target
lines(startYear:endYear,colMeans(m,na.rm=TRUE),col="black",lwd=2,lty=2) # Mean
lines(startYear:endYear,m[5,],col="gold",lwd=2.5) # Germany
lines(startYear:endYear,m[12,],col="firebrick3",lwd=2.5) # Spain
abline(v=2008)
axis(1,tick=FALSE); axis(2,tick=FALSE)
