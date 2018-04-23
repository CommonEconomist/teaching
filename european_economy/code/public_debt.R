#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#
# Public debt
# Last update: 2018 02 28
#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#
d<-read.csv("data/eurostat_public_debt.csv",header=TRUE)

# Plot figure
plot(d$TIME,d$Value,axes=FALSE,xlab="",ylab="",ylim=c(0,200),
     main="Public debt (% of GDP)",pch=1,col="grey50",cex=.7)
lines(d$TIME[d$GEO=="Euro area (17 countries)"],
      d$Value[d$GEO=="Euro area (17 countries)"],lwd=2,col="black")
axis(1,tick=FALSE);axis(2,tick=FALSE)
abline(h=60,lwd=2,lty=2)
