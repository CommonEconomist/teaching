#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#
# Trade new member states
# Last update: 2018 02 20
#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#
library(WDI)

# Focus on GDP and trade relative to GDP
gdp_m<-WDIsearch("gdp",field="name",short=FALSE)
trade_m<-WDIsearch("trade",field="name",short=FALSE)
wdi<-WDI(c("CZ","BG","EE","HU","LV","LT","MT","PL","RO","SK","SI","DE"),
         c(gdp_m[89,1],trade_m[108,1]),start=2004,end=2004) 

wdi$GDP<-wdi[,4]/wdi[wdi$country=="Germany",][,4] # GDP relative to Germany

# Plot data
par(mar=c(5,5,1,1),pty="s",bty="n",las=1,cex.axis=1.5,cex.lab=1.5)
plot(wdi[,5],wdi$GDP,ylim=c(0,.25),axes=FALSE,pch=19,
     xlab="Trade % of GDP", ylab="GDP relative to Germany")
text(wdi[,5],wdi$GDP+.01,wdi$iso2c,cex=1.3)
axis(1,tick=FALSE);axis(2,tick=FALSE,line=-1)


