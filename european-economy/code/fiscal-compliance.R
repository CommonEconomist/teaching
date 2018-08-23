#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#
# Fiscal compliance
# Last update: 2018 02 20
#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#
library(plyr)
library(countrycode)

# Prepare data
d<-read.csv("data/eurostat_budget_deficit.csv",header=TRUE)
d$Deficit<-d$Value*-1
def<-ddply(d[d$TIME>2010,],.(GEO),summarise,deficit=mean(Deficit,na.rm=TRUE))

d<-read.csv("data/eurostat_public_debt.csv",header=TRUE)
debt<-ddply(d[d$TIME>2010,],.(GEO),summarise,debt=mean(Value,na.rm=TRUE))

x<-merge(debt,def)
x$iso3c<-countrycode(x$GEO,"country.name","iso3c",warn=TRUE)

# Plot data
par(pty="s")
plot(x$deficit,x$debt,xlim=c(-1,9),ylim=c(5,175),type="n",axes=FALSE,
     xlab="Budget deficit (% of GDP)",ylab="Public debt (% of GDP)")
abline(h=60,lty=2,lwd=2);abline(v=3,lty=2,lwd=2)
text(x$deficit,x$debt,label=x$iso3c,cex=1.2)
axis(1,tick=FALSE,line=-1);axis(2,tick=FALSE,line=-1)
