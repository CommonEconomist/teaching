#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#
# Government bond yields 
# Germany and Greece
# Last update: 2018 03 07
#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#
bonds<-read.csv("data/eurostat_bond_yields.csv",header=TRUE)

# Data to time-series
greece<-ts(bonds[bonds$GEO=="Greece",]$Value,start=c(1980,1),frequency=12)
germany<-ts(bonds[bonds$GEO=="Germany (until 1990 former territory of the FRG)",]$Value,start=c(1980,1),frequency=12)

greece<-window(greece,start=c(1992,1))
germany<-window(germany,start=c(1992,1))

# Plot data
par(las=1,bty="n",cex.axis=1.5,cex.lab=1.5,mar=c(4,5,3,1))
plot(greece,ylim=c(0,30),col="steelblue4",axes=FALSE,xlab="",
     ylab="",lwd=2,main='Government bond interest rates')
lines(germany,lwd=2)

axis(1,tick=FALSE);axis(2,tick=FALSE)

text(1993,9,"Germany",cex=1.2)
text(1992,23,"Greece",cex=1.2)
