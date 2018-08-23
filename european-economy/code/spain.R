#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#
# Spain spiral
# Last update: 2018 04 18
#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#
deficit<-read.csv("data/eurostat_budget_deficit.csv",header=TRUE)
debt<-read.csv("data/eurostat_public_debt.csv",header=TRUE)

x<-deficit[deficit$GEO=="Spain",]$Value
y<-debt[debt$GEO=="Spain",]$Value
years<-1995:2015

# Plot
par(mar=c(5,5,3,1),bty="n",las=1,cex.axis=1.5,cex.lab=1.5,cex.main=1.7)
plot(x,y,type="b",cex=0,lty=3,lwd=.75,main='Spain',
     xlab="Budget surplus (% of GDP)",ylab="Public debt (% of GDP)",axes=FALSE)
abline(h=60,lty=2);abline(v=-3,lty=2)
text(x,y,label=years,cex=1.2)
axis(1,tick=FALSE);axis(2,tick=FALSE)
