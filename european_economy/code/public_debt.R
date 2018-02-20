#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#
# Public debt
# Last update: 2018 02 20
#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#
d<-read.csv("data/eurostat_public_debt.csv",header=TRUE)

# Plot figure
plot(d$TIME,d$Value,axes=FALSE,xlab="",ylab="",ylim=c(0,200),
     main="Public debt (% of GDP)",pch=19,col="grey50")
lines(d$TIME[d$GEO=="Euro area (17 countries)"],
      d$Value[d$GEO=="Euro area (17 countries)"],lwd=2,col="steelblue4")
axis(1,tick=FALSE);axis(2,tick=FALSE)
abline(h=60,lwd=2,lty=2)
