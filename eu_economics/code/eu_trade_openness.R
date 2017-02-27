# Openness to trade EU countries
require(WDI)

EU<-c("AT","BE","BG","HR","CY","CZ","DK","EE","FI","FR",
      "DE","GR","HU","IE","IT","LV","LT","LU","MT","NL",
      "PL","RO","SK","SI","ES","SE","GB")

gdp_m<-WDIsearch("gdp",field="name",short=FALSE)
import_m<-WDIsearch("import",field="name",short=FALSE)
export_m<-WDIsearch("export",field="name",short=FALSE)


wdi<-WDI(EU,c(gdp_m[89,1],import_m[7,1],export_m[16,1]),start=2015,end=2015) 
wdi$flow<-(wdi[,5]+wdi[,6])/wdi[,4]*100
wdi<-wdi[order(wdi$flow),]


# Plot
require(psych)
par(las=1)
barplot(wdi$flow,xaxt="n",yaxt="n",ylab="",border=F,width=c(.35),space=1.8,
        horiz=TRUE)
axis(2,at=(1:27)-.26,labels=wdi$iso2c, tick=F)
axis(1,tick=F)
abline(v=seq(0,350,10),col="white",lwd=3)
abline(v=0,col="gray",lwd=2)




