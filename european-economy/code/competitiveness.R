#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#
# Comeptitiveness
# Last update: 2018 02 20
#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#
d<-read.csv("data/oecd_unit_labour_costs.csv")

# Time-series objects
jpn<-ts(d$Value[d$LOCATION=="JPN"],start=c(1990,1),frequency=4)
usa<-ts(d$Value[d$LOCATION=="USA"],start=c(1990,1),frequency=4)
eur<-ts(d$Value[d$LOCATION=="EA19"],start=c(1990,1),frequency=4)

# Plot data
par(mar=c(5,4,1,1),bty="n",las=1,cex.axis=1.5,cex.lab=1.5)
plot(eur,ylim=c(65,135),axes=FALSE,xlab="",ylab="",lwd=2)
lines(jpn,col="firebrick3",lwd=2)
lines(usa,col="steelblue4",lwd=2)
axis(1,tick=FALSE);axis(2,tick=FALSE)

text(1990,125,"Japan",cex=1.5)
text(1990.5,85,"Eurozone",cex=1.5)
text(1990,75,"USA",cex=1.5)
text(2010,130,"Unit labour costs",cex=1.7)
