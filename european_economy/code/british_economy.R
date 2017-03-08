# Figures for lecture on Brexit
# Examining UK trade
# Data taken from Eurostat and HR Customs and Revenue
par(mar=c(5,5,1,1.5),bty="n",las=1,cex.axis=1.5,cex.lab=1.5)
options(scipen=4)

#------------------------------------------------------------------------------
#### Exchange rate ####
sterling<-read.csv("data_raw/sterling_euro_exchange_daily.csv")

par(las=1,cex.lab=1.5,cex.axis=1.5,mar=c(4,5,1,1))
plot(sterling$Value,type="b",axes=FALSE,xlab="",ylab="",lwd=2,pch=19,
     ylim=c(0.7,1))
axis(2,tick=FALSE,line=-1)
text(40,.97,"Euro/Pound exchange rate \n (trading days only)",cex=1.7)

text(130,0.74,"Brexit referendum",cex=1.5)
segments(125,.743,125,0.7595,lwd=1.5,col="black")

segments(139,.81,139,0.83,lwd=1.5,col="black")
text(145,0.80,"Cameron \n resignation",cex=1.5)

segments(197,.87,197,.83,lwd=1.5,col="black")
text(197,.82,"Conservatives \n conference",cex=1.5)

segments(126,mean(sterling[-1:-125,]$Value,na.rm=TRUE),261,lty=2)
segments(1,mean(sterling[1:125,]$Value,na.rm=TRUE),125,lty=2)

text(126,.87,"0.87 (0.02)",cex=1.5)
text(4.5,.785,"0.78 (0.02)",cex=1.5)
axis(1,tick=FALSE,at=c(22,43,67,87,109,131,152,175,200,218,240),
     label=c("Feb","Mar","Apr","May","Jun","Jul","Aug","Sep","Oct","Nov","Dec"))



#------------------------------------------------------------------------------
##### Quarterly trade data ####
tradeq<-read.csv("data_raw/uk_trade_quarterly.csv",stringsAsFactors=FALSE)

# Calculate trade flows
x<-tradeq[tradeq$NA_ITEM=="Exports of goods and services",]$Value
m<-tradeq[tradeq$NA_ITEM=="Imports of goods and services",]$Value
x.eu<-tradeq[tradeq$NA_ITEM==
               "Exports of goods and services to the European Union",]$Value
m.eu<-tradeq[tradeq$NA_ITEM==
           "Imports of goods and services from the European Union",]$Value

x.eu.p<-x.eu/x*100
m.eu.p<-m.eu/m*100

trade.eu<-(x.eu+m.eu)/(x+m)*100
trade.other=100-trade.eu

# Data to time series object
trade.eu<-ts(trade.eu,start=c(1995,1),frequency=4)
trade.other<-ts(trade.other,start=c(1995,1),frequency=4)
export.eu<-ts(x.eu.p,start=c(1995,1),frequency=4)
import.eu<-ts(m.eu.p,start=c(1995,1),frequency=4)

# Plot trade data relative to total flow
plot(trade.eu,ylim=c(40,60),xlim=c(1995,2018),axes=FALSE,lwd=2,
     col="steelblue4",xlab="",ylab="")
lines(trade.other,lwd=2)
abline(h=50,lty=2)
axis(1,tick=FALSE)
axis(2,tick=FALSE)

text(2017.5,trade.eu[87],"EU",cex=1.7)
text(2017.5,trade.other[87],"Other",cex=1.7)

# Plot imports and exports for EU
plot(export.eu,ylim=c(40,60),xlim=c(1995,2018),axes=FALSE,lwd=2,xlab="",ylab="")
lines(import.eu,lwd=2,col="steelblue4")
abline(h=50,lty=2)
axis(1,tick=FALSE)
axis(2,tick=FALSE)
text(2017.7,export.eu[87],"Exports",cex=1.7)
text(2017.7,import.eu[87],"Imports",cex=1.7)

#------------------------------------------------------------------------------
#### Break down per country for 2015
trade<-read.csv("data_raw/uk_trade_2015.csv")
pts<-ifelse(trade$EU==1,15,3)
par(pty="s")

plot(trade$Import,trade$Export,log="xy",pch=pts,axes=FALSE,
     xlim=c(1000,100000000000),ylim=c(1000,100000000000),
     xlab="Imports",ylab="Exports")
axis(1,tick=FALSE)
axis(2,tick=FALSE,line=-2)
