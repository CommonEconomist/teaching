#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#
# Figures for lecture on OCA conditions in the EU
# Last update: 2018 02 20
#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#
library(WDI)
library(psych)
library(plyr)
library(countrycode)

#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#
# 1) Opennes to trade 
# Parameters for data
EU<-c("AT","BE","BG","HR","CY","CZ","DK","EE","FI","FR",
      "DE","GR","HU","IE","IT","LV","LT","LU","MT","NL",
      "PL","RO","SK","SI","ES","SE","GB")

gdp_m<-WDIsearch("gdp",field="name",short=FALSE)
import_m<-WDIsearch("import",field="name",short=FALSE)
export_m<-WDIsearch("export",field="name",short=FALSE)

# Get data
wdi<-WDI(EU,c(gdp_m[89,1],import_m[7,1],export_m[16,1]),start=2015,end=2015) 
wdi$flow<-(wdi[,5]+wdi[,6])/wdi[,4]*100
wdi<-wdi[order(wdi$flow),]

# Plot data
par(las=1)
barplot(wdi$flow,xaxt="n",yaxt="n",ylab="",border=F,width=c(.35),space=1.8,
        horiz=TRUE,col="black")
axis(2,at=(1:27)-.26,labels=wdi$iso2c, tick=F)
axis(1,tick=F)
abline(v=seq(0,350,10),col="white",lwd=3)
abline(v=0,col="gray",lwd=2)

#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#
# 2) Inflation
u<-read.csv("data/eurostat_inflation.csv")

# Data to time-series
deu<-ts(u[u$GEO=="Germany (until 1990 former territory of the FRG)",]$Value,
        start=c(1996,1),frequency=1)
ita<-ts(u[u$GEO=="Italy",]$Value,start=c(1996,1),frequency=1)
gre<-ts(u[u$GEO=="Greece",]$Value,start=c(1996,1),frequency=1)

# Plot data
par(las=1,bty="n",cex.axis=1.5,cex.lab=1.5,mar=c(4,5,1,1))
plot(deu,xlab="",ylab="",axes=FALSE,ylim=c(-1.5,8),lwd=2)
lines(ita,col="steelblue4",lwd=2)
lines(gre,col="steelblue4",lty=2,lwd=2)

axis(1,tick=FALSE)
axis(2,tick=FALSE)

#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#
# 3) Public debt 
d<-read.csv("data/eurostat_public_debt_q.csv",stringsAsFactors=FALSE)

# Aggregate data to create average
d<-ddply(d,.(GEO),summarise,debt=mean(Value,na.rm=TRUE))
d<-d[-9:-15,]

# Change name for Germany
d$GEO[d$GEO=="Germany (until 1990 former territory of the FRG)"]<-"Germany"
d$iso2c<-countrycode(d$GEO,"country.name","iso2c",warn=TRUE)
d<-na.omit(d[order(d$debt),])

# Plot data
barplot(d$debt,xaxt="n",yaxt="n",ylab="",border=F,width=c(.35),space=1.8,
        horiz=TRUE,col="black")
axis(2,at=(1:29)-.26,labels=d$iso2c, tick=F)
axis(1,tick=F)
abline(v=seq(0,130,10),col="white",lwd=3)
abline(v=0,col="gray",lwd=2)
abline(v=60,lty=2)

#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#
# 4) Public opinion on Europe
p<-read.csv("data/eurobarometer.csv")
p$d<-as.numeric(p$Date)
p<-p[p$d==2,]
p$value<-p$Often*100
p<-p[order(p$value),]

# Plot data
par(mar=c(4,10,1,1))
barplot(p$value,xaxt="n",yaxt="n",ylab="",border=F,width=c(.35),space=1.8,
        horiz=TRUE,col="black")
axis(2,at=(1:12)-.26,labels=p$Country, tick=F)
axis(1,tick=F)
abline(v=seq(0,35,5),col="white",lwd=3)
abline(v=0,col="gray",lwd=2)
