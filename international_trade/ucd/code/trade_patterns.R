## Figures lecture Trade Patterns
setwd("~/Dropbox/github/Teaching/international_trade/ucd")

#### Trade balance USA and China ####

# Prepare data
oecd<-read.csv("data_raw/MEI_TRD.csv")
usa<-ts(oecd$Value[oecd$LOCATION=="USA"],start=c(1990,1),frequency=12)
chn<-ts(oecd$Value[oecd$LOCATION=="CHN"],start=c(1990,1),frequency=12)

# Plot figure
par(mar=c(5,5,2,2),las=1,bty="n",cex.lab=2,cex.axis=2)

plot(usa,axes=FALSE,xlab="",ylab="US dollars (billions)",ylim=c(-80,120),lwd=2)
lines(chn,lty=2,col="firebrick3",lwd=2)
axis(1,tick=FALSE);axis(2,tick=FALSE,line=-1)

text(1992,10,"China",cex=2)
text(1992,-15,"USA",cex=2)

#### Bilateral trade balance USA-China ####
# Load data
d<-read.csv("data_raw/usa_chn_trade.csv")
im<-ts(d$Imports,start=c(1985,1))
ex<-ts(d$Exports,start=c(1985,1))
tb<-(ex-im)/1000

# Plot figure
plot(tb,axes=FALSE,xlab="",ylab=" Nominal US dollars (billions)",type="b",lwd=2)
axis(1,tick=FALSE);axis(2,tick=FALSE,line=-2)

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
par(las=1)
barplot(wdi[,3],xaxt="n",yaxt="n",ylab="",border=F,width=c(.35),space=1.8,
        horiz=TRUE,col="black")
axis(2,at=(1:34)-.26,labels=wdi$iso2c, tick=F,cex.axis=1.2)
axis(1,tick=F)
abline(v=seq(0,420,50),col="white",lwd=3)
abline(v=0,col="gray",lwd=2)

#### Terms of trade Australia ####
# Prepare data
aus<-read.csv("data_raw/australia.csv")
tot<-ts(aus$terms_of_trade,start=c(1901,1))

# Plot data
plot(tot,xlab="",ylab="2014=100",axes=FALSE,lwd=2)
axis(1,tick=FALSE);axis(2,tick=FALSE,lin=-1)

#### UK trade over time ####
# Prepare data
uk<-read.csv("data_raw/uk_trade.csv")
ex<-ts(uk$export_volume/uk$gdp*100,start=c(1280,1),frequency=1)
im<-ts(uk$import_volume/uk$gdp*100,start=c(1280,1),frequency=1)

plot(ex,xlab="",ylab="Percentage of GDP",axes=FALSE,lwd=2)
lines(im,col="steelblue4",lty=2,lwd=2)
axis(1,tick=FALSE);axis(2,tick=FALSE,line=-1)

# Export volume
ev<-ts(uk$export_volume,start=c(1280,1),frequency=1)
ev<-window(ev,start=c(1790,1))/1000

plot(ev,log="y",axes=FALSE,xlab="",ylab="Export volume",lwd=2)
axis(1,tick=FALSE);axis(2,tick=FALSE,line=-2)

#### World trade data ####
# Prepare data
require(zoo)
un<-read.csv("data_raw/norbom.csv")
df<-data.frame(year=1900:1960)
df<-merge(df,un,all.x=TRUE)

manu<-ts(df$manufactured/df$total*100,start=c(1900,1),frequency=1)
other<-ts(df$other/df$total*100,start=c(1900,1),frequency=1)

# Plot data
plot(manu,ylim=c(30,70),xlab="",ylab="Percentage of total trade",type="b",
     pch=19,cex=2,axes=FALSE)
lines(na.approx(manu))
points(other,pch=18,col="steelblue4",cex=2)
lines(na.approx(other),col="steelblue4")

axis(1,tick=FALSE);axis(2,tick=FALSE,line=-1)