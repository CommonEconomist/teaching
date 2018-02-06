## Figures specific factors model
setwd("~/Dropbox/github/teaching/international_trade/ucd")

#### Imports and unemployment in Spain ####
require(WDI)

trade_m<-WDIsearch("trade",field="name",short=FALSE) # 108
un_m<-WDIsearch("unemployment",field="name",short=FALSE) # 18

wdi<-WDI("ES",c(trade_m[108,1],un_m[18,1]),start=1991,end=2015)

trd<-ts(wdi[,4],start=c(1991,1))
uem<-ts(wdi[,5],start=c(1991,1))

# Plot figure
par(mar=c(5,6,2,2),las=1,bty="n",cex.lab=2,cex.axis=2)
plot(trd,lwd=2,axes=FALSE,xlab="",ylab="",ylim=c(5,65))
lines(uem,lwd=2,lty=2)
axis(1,tick=FALSE)
axis(2,tick=FALSE)

text(2009,44,"Imports (% GDP)",cex=1.5)
text(2009,30,"Unemployment (% workforce)",cex=1.5)

#### Exports the Netherlands ####
d<-read.csv("data_raw/cbs_nl_export.csv")
m<-ts((d$machines+d$manufactures)/d$total,start=1950)
f<-ts(d$mineral_fuels/d$total,start=1950)

m<-window(m,start=1950,end=2000)
f<-window(f,start=1950,end=2000)

# Plot
par(mar=c(5,5,2,2),las=1,bty="n",cex.lab=2,cex.axis=2)
plot(m,ylim=c(0,.6),xlim=c(1950,2007),
     xlab="",ylab="",axes=FALSE,lwd=2,col="steelblue4")
lines(f,lwd=2)

axis(1,tick=FALSE)
axis(2,tick=FALSE)
text(1960,.6,"Proportion of total Dutch exports",cex=1.5)
text(2005,m[51],"Manufactures + \n machinery",cex=1.2)
text(2005,f[51],"Mineral fuels",cex=1.2)
