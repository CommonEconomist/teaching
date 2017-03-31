# Figures lecture on Stability and Growth Pact
setwd("~/Dropbox/github/Teaching/european_economy")
par(mar=c(5,5,2,2),bty="n",las=1,cex.axis=1.5,cex.lab=1.5,cex.main=1.7)

#------------------------------------------------------------------------------
#### Budget deficit over time ####
d<-read.csv("data_raw/eurostat_budget_deficit.csv",header=TRUE)
d$Deficit<-d$Value*-1

# Save some data for other figure
require(plyr)
def<-ddply(d[d$TIME>2010,],.(GEO),summarise,deficit=mean(Deficit,na.rm=TRUE))

# Plot data
plot(d$TIME,d$Deficit,axes=FALSE,xlab="",ylab="",ylim=c(-10,40),
     main="Budget deficit (% of GDP)",pch=19,col="grey50")
lines(d$TIME[d$GEO=="Euro area (17 countries)"],
      d$Deficit[d$GEO=="Euro area (17 countries)"],lwd=2,col="steelblue4")
axis(1,tick=FALSE);axis(2,tick=FALSE)
abline(h=3,lwd=2,lty=2)

#------------------------------------------------------------------------------
#### Public debt over time ####
d<-read.csv("data_raw/eurostat_public_debt.csv",header=TRUE)

# Save some data for other figure
debt<-ddply(d[d$TIME>2010,],.(GEO),summarise,debt=mean(Value,na.rm=TRUE))

# Plot figure
plot(d$TIME,d$Value,axes=FALSE,xlab="",ylab="",ylim=c(0,200),
     main="Public debt (% of GDP)",pch=19,col="grey50")
lines(d$TIME[d$GEO=="Euro area (17 countries)"],
      d$Value[d$GEO=="Euro area (17 countries)"],lwd=2,col="steelblue4")
axis(1,tick=FALSE);axis(2,tick=FALSE)
abline(h=60,lwd=2,lty=2)

#------------------------------------------------------------------------------
#### Fiscal compliance ####
df<-merge(debt,def)
require(countrycode)
df$iso3c<-countrycode(df$GEO,"country.name","iso3c",warn=TRUE)

# Plot data
par(pty="s")
plot(df$deficit,df$debt,xlim=c(-1,9),ylim=c(5,175),type="n",axes=FALSE,
     xlab="Budget deficit (% of GDP)",ylab="Public debt (% of GDP)")
abline(h=60,lty=2,lwd=2);abline(v=3,lty=2,lwd=2)
text(df$deficit,df$debt,label=df$iso3c,cex=1.2)
axis(1,tick=FALSE,line=-1);axis(2,tick=FALSE,line=-1)
