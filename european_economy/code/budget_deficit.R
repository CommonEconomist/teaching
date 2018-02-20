#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#
# Budget deficit
# Last update: 2018 02 20
#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#
d<-read.csv("data/eurostat_budget_deficit.csv",header=TRUE)
d$Deficit<-d$Value*-1

# Plot data
plot(d$TIME,d$Deficit,axes=FALSE,xlab="",ylab="",ylim=c(-10,40),
     main="Budget deficit (% of GDP)",pch=19,col="grey50")
lines(d$TIME[d$GEO=="Euro area (17 countries)"],
      d$Deficit[d$GEO=="Euro area (17 countries)"],lwd=2,col="steelblue4")
axis(1,tick=FALSE);axis(2,tick=FALSE)
abline(h=3,lwd=2,lty=2)
