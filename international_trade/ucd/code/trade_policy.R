## Figures trade policy
setwd("~/Dropbox/github/teaching/international_trade/ucd")


#### Tariff rates and revenues ####

# Countrycodes
require(countrycode)
data(countrycode_data)
iso2c<-countrycode_data$iso2c

# Download world bank data
require(WDI)
t_m<-WDIsearch("tariff",field="name",short=FALSE) # 11
t2_m<-WDIsearch("trade",field="name",short=FALSE) # 89
wdi<-WDI(country=iso2c,c(t_m[11,1],t2_m[89,1]),start=2005,end=2015) 
d<-ddply(wdi,.(country),summarise,
         tariff=mean(TM.TAX.MANF.SM.AR.ZS,na.rm=TRUE),
         revenue=mean(GC.TAX.INTT.RV.ZS,na.rm=TRUE))
d<-na.omit(d)

# Different points for continent
d$continent<-countrycode(d$country,"country.name","continent")
d$p<-as.numeric(factor(d$continent))-1

# Plot data
par(mar=c(5,5,2,0),bty="n",las=1,cex.axis=2,cex.lab=2)
plot(d$tariff,d$revenue,pch=d$p,cex=2,axes=FALSE,
     xlab="Tariff rate",ylab="Tariff revenue")
abline(a=0,b=1,lty=2,lwd=2)
axis(1,tick=FALSE,line=-1.5);axis(2,tick=FALSE,line=-1.5)
text(25,60,"Average for 2005-2015",cex=2)

#### Sugar prices ####

# US domestic prices
m<-read.csv("data_raw/us_sugar.csv")
m<-as.matrix(m[,-1])
v<-as.vector(t(m))
us<-ts(v,start=c(1960,1),frequency=12)

# World prices
m<-read.csv("data_raw/world_sugar.csv")
m<-as.matrix(m[,-1])
v<-as.vector(t(m))
world<-ts(v,start=c(1960,1),frequency=12)

# Price difference
d<-us-world

# Plot data
plot(us,lwd=2,axes=FALSE,xlab="",ylab="",ylim=c(1,60),col="steelblue4")
lines(world,lwd=2,lty=2)
axis(1,tick=FALSE)
axis(2,tick=FALSE)

text(1970,60,"Sugar price, cents per pound",cex=2)
text(2019,us[690],"US",cex=1.5)
text(2010,10,"World",cex=1.5)

# Price difference
#plot(test,type="h")
