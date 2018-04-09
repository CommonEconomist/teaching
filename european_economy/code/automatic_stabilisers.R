#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#
# Automatic stabilisers
# Last update; 2018 02 20
#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#
par(mar=c(5,5,2,2),bty="n",las=1,cex.axis=2,cex.lab=2,cex.main=2)

# Prepare data
ps<-read.csv("data/eurostat_public_spending.csv")
tr<-read.csv("data/eurostat_tax_revenue.csv")
x<-ts(ps$Value[ps$GEO=="Netherlands"],start=c(1999,1),frequency=1)
y<-ts(tr$Value[tr$GEO=="Netherlands"],start=c(1999,1),frequency=1)

# Plot
plot(x,ylim=c(35,50),type="b",axes=FALSE,xlab="",ylab="Percentage of GDP",pch=0)
text(2015,x[17]+1.2,"Public \n spending")
lines(y,lty=2,type="b")
text(2015,y[17]+1.2,"Tax \n revenue")

axis(1,tick=FALSE,at=seq(1999,2015,2),label=seq(1999,2015,2));
axis(2,at=seq(35,50,5),label=seq(35,50,5),tick=FALSE,line=-2)
