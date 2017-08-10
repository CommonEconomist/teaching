## Figures trade patterns
setwd("~/Dropbox/github/teaching/international_trade/ucd")

#### Trade balance USA and China ####

# Prepare data
oecd<-read.csv("data_raw/trade_flow_usa_chn.csv")
usa<-ts(oecd$Value[oecd$LOCATION=="USA"],start=c(1990,1),frequency=12)
chn<-ts(oecd$Value[oecd$LOCATION=="CHN"],start=c(1990,1),frequency=12)

# Plot figure
par(mar=c(5,5,2,2),las=1,bty="n",cex.lab=2,cex.axis=2)

plot(usa,axes=FALSE,xlab="",ylab="",xlim=c(1990,2018.5),ylim=c(-80,120),lwd=2)
lines(chn,lty=2,col="firebrick3",lwd=2)

text(2017,chn[305],"China",cex=1.5)
text(2018.5,usa[329],"USA",cex=1.5)
text(1995,100,"Trade balance \n (in billion USD)",cex=2)

axis(1,tick=FALSE);axis(2,tick=FALSE,line=-1)

#### Bilateral trade balance USA-China ####

# Load data
d<-read.csv("data_raw/bilateral_trade_usa_chn.csv")
im<-ts(d$Imports,start=c(1985,1))
ex<-ts(d$Exports,start=c(1985,1))
tb<-(ex-im)/1000

# Plot figure
plot(tb,axes=FALSE,xlab="",ylab="",type="b",lwd=2)
axis(1,tick=FALSE);axis(2,tick=FALSE,line=-2)
text(1995,-300,"US-China bilateral trade balance \n  
     (in billion USD, nominal)",cex=2)

#### Trade relative to GDP for OECD ####
# Download data from WDI
require(WDI)
iso2c<-c("CA","US","GB","DK","IS","NO","TR","ES","PT","FR","IE","BE","DE","GR",
         "SE","CH","AT","NL","LU","IT","JP","FI","AU","NZ","MX","CZ","HU","PL",
         "KR","SK","CL","SI","IL","EE")
trade_m<-WDIsearch("trade",field="name",short=FALSE) # 108

wdi<-WDI(iso2c,trade_m[108,1],start=2015,end=2015) 
wdi<-wdi[order(wdi[,3]),]

# Plot data
par(mar=c(5,12,0,2),las=1,bty="n",cex.lab=2)
barplot(wdi[,3],xaxt="n",yaxt="n",ylab="",xlab="Trade relative to GDP",
        border=F,width=c(.35),space=1.8,xlim=c(0,420),
        horiz=TRUE,col="black")

axis(2,at=(1:34)-.5,labels=wdi$country, tick=F,cex.axis=1.2)
axis(1,tick=F)
abline(v=seq(0,420,50),col="white",lwd=3)
abline(v=0,col="gray",lwd=2)

#### Terms of trade Australia ####

# Prepare data
aus<-read.csv("data_raw/aus_tot.csv")
tot<-ts(aus$terms_of_trade,start=c(1901,1))

# Plot data
par(mar=c(5,5,2,1))
plot(tot,xlab="Terms of trade index (2014=100)",ylab="",
     axes=FALSE,lwd=2,type="b")
axis(1,tick=FALSE);axis(2,tick=FALSE,lin=-1)

#### UK trade over time ####
# Prepare data
uk<-read.csv("data_raw/uk_trade.csv")
ex<-ts(uk$export_volume/uk$gdp*100,start=c(1280,1),frequency=1)
im<-ts(uk$import_volume/uk$gdp*100,start=c(1280,1),frequency=1)

ex<-window(ex,start=c(1800,1))
im<-window(im,start=c(1800,1))

par(mar=c(5,5,2,2))
plot(ex,xlab="",ylab="",axes=FALSE,lwd=2,type="b",
     ylim=c(0,40),xlim=c(1800,2018))
lines(im,col="steelblue4",lty=2,lwd=2,type="b",pch=0)

text(1950,3,"Exports (% GDP)",cex=1.2)
text(1950,20,"Imports (% GDP)",cex=1.2)
axis(1,tick=FALSE);axis(2,tick=FALSE,line=-1)


#### UK export volumes
ev<-ts(uk$export_volume,start=c(1280,1),frequency=1)
ev<-window(ev,start=c(1790,1))/1000

plot(ev,log="y",axes=FALSE,xlab="",ylab="",lwd=2,type="b")
axis(1,tick=FALSE)
axis(2,tick=FALSE,line=-2,at=c(1,5,10,50,100,500),lab=c(1,5,10,50,100,500))
text(1820,300,"UK export volume",cex=2)

#### World trade data ####

# Prepare data
require(zoo)
un<-read.csv("data_raw/norbom.csv")
df<-data.frame(year=1900:1960)
df<-merge(df,un,all.x=TRUE)

manu<-ts(df$manufactured/df$total*100,start=c(1900,1),frequency=1)
other<-ts(df$other/df$total*100,start=c(1900,1),frequency=1)

# Plot data
plot(manu,ylim=c(30,70),xlab="",ylab="",type="b",
     pch=19,cex=2,axes=FALSE)
lines(na.approx(manu))
points(other,pch=18,col="steelblue4",cex=2)
lines(na.approx(other),col="steelblue4")

text(1958,manu[61]+2,"Manufactured",cex=1.5)
text(1960,other[61]-2,"Other",cex=1.5)
text(1910,70,"Trade in goods (% total)",cex=2)
axis(1,tick=FALSE);axis(2,tick=FALSE,line=-1)

#### Foreign direct investment ####
# https://data.oecd.org/fdi/fdi-stocks.htm
fdi<-read.csv("data_raw/oecd_fdi.csv",stringsAsFactors=FALSE)

# Prepare data
fdi<-fdi[fdi$TIME==2015,]
fdi<-fdi[order(fdi$LOCATION,fdi$SUBJECT),]

df<-data.frame(iso=unique(fdi$LOCATION),
               inw=fdi$Value[fdi$SUBJECT=="INWARD"],
               outw=fdi$Value[fdi$SUBJECT=="OUTWARD"])
df<-df[order(df$inw),]

# Plot data
par(mar=c(6,4,2,2))
plot(df$inw,pch=19,cex=2,ylim=c(0,400),axes=FALSE,xlab="",ylab="")
points(df$outw,pch=5,cex=2)
axis(1,at=1:46,label=df$iso,las=2,tick=FALSE,cex.axis=1.2)
axis(2,tick=FALSE,cex.axis=2,line=-1.5)

text(43,391,"Inward",cex=1.5)
text(43,355,"Outward",cex=1.5)
text(10,390,"Foreign Direct Invesments \n (% GDP)",cex=2)