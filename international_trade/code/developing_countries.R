## Developing countries
options(scipen=8)
par(mar=c(5,5,2,2),las=1,bty="n",cex.lab=2,cex.axis=2)
setwd("~/Dropbox/github/teaching/international_trade/ucd")

# ISO2C
require(countrycode)
data(countrycode_data)
cc<-countrycode_data
iso2c<-cc$iso2c

#### GDP per capita ####
require(WDI)
gdp_m<-WDIsearch("gdp",field="name",short=FALSE) # 50
wdi<-WDI("all",gdp_m[94,1],start=1990,end=1990)

# Drop country aggregates
d<-na.omit(wdi[wdi$iso2c %in% iso2c,])
d$gdp<-d[,3]/d[,3][d$country=="United States"]
x<-sort(d$gdp,decreasing=TRUE)

# Plot data
plot(x,axes=FALSE,xlab="",ylab="",cex=2)
abline(h=1,lty=2,lwd=2)
axis(1,tick=FALSE)
axis(2,tick=FALSE)

text(140,1.5,"GDP per capita (1990) \n proportion of USA",cex=2)

#### Natural resource rents vs. GDP per capita ####
# Download data from World Bank Development Indicators
require(WDI)
gdp_m<-WDIsearch("gdp",field="name",short=FALSE) # 94,101
wdi<-WDI("all",c(gdp_m[98,1],gdp_m[101,1]),start=2015,end=2015)

# Drop country aggregates
d<-na.omit(wdi[wdi$iso2c %in% iso2c,])

# Plot data
plot(d[,4],d[,5],log="x",axes=FALSE,
     xlab="GDP per capita",ylab="Resource rents (% GDP)")
axis(1,tick=FALSE);axis(2,tick=FALSE,line=-1)

#### Historical price copper and sugar ####
# https://www.sfu.ca/~djacks/data/index.html
# Load data and create time-series objects
d<-read.csv("data_raw/jacks_commodity_prices.csv")
copper<-ts(d$Copper,start=1850)
sugar<-ts(d$Sugar,start=1850)

# Set 1850 as 100
copper=copper/copper[1]*100
sugar=sugar/sugar[1]*100

# Plot
plot(sugar,lwd=2,axes=FALSE,xlim=c(1850,2019),ylim=c(0,150),xlab="",ylab="")
lines(copper,col="steelblue4",lwd=2)
axis(1,tick=FALSE);axis(2,tick=FALSE)
text(2000,145,"Prices (1850=100)",cex=1.7)
text(2018,copper[166]-5,"Copper",cex=1.2)
text(2018,sugar[166]-5,"Sugar",cex=1.2)

#### Coffee prices and exports ####
# http://www.ico.org/new_historical.asp?section=Statistics
d<-read.csv("data_raw/coffee.csv")
price<-ts(d$average_price,start=1991)
export<-ts(d$exports,start=1991)

# Plot data
plot(export,price,cex=1.2,axes=FALSE,
     xlab="Coffee exports (million 60Kg bags)",ylab="Coffee price (US cents/lb)")
axis(1,tick=FALSE);axis(2,tick=FALSE,line=-1)

#### Coffee prices ####
d<-read.csv("data_raw/coffee_prices.csv")
uga<-ts(d$uganda,start=1990)
png<-ts(d$p_new_guinea,start=1990)

# Plot
plot(uga,xlim=c(1990,2018.5),ylim=c(0,110),lwd=2,xlab="",
     ylab="Coffee price (US cents/lb)",axes=FALSE)
lines(png,col="steelblue4",lwd=2)
axis(1,tick=FALSE);axis(2,tick=FALSE,line=-1)
text(2017.5,uga[27]-2,"Uganda",cex=1.5)
text(2017.3,png[27]-2,"Papua \n New Guinea",cex=1.5)

#### Tariff rates ####
# Get World Bank data
tar_m<-WDIsearch("tariff",field="name",short=FALSE) # 14
gdp_m<-WDIsearch("gdp",field="name",short=FALSE) # 94
wdi<-WDI("all",c(tar_m[14,1],gdp_m[94,1]),start=1989,end=2016)

# Subset to remove country groupings
d<-na.omit(wdi[wdi$iso2c %in% iso2c,])

# Indicator for developing countries
gdp<-d[d$year==2015,]
gdp$dev<-ifelse(gdp[,5]<12500,1,0)
d<-d[d$iso2c %in% gdp$iso2c[gdp$dev==1],]

dev<-gdp$iso2c[gdp$dev==1]

# Aggregate data
require(plyr)
d<-ddply(d,.(year),summarise,
              tariff=mean(TM.TAX.MANF.WM.AR.ZS,na.rm=TRUE))
trf<-ts(d$tariff,start=min(d$year))

# Plot data
plot(trf,axes=FALSE,ylab="",xlab="",lwd=2)
axis(1,tick=FALSE);axis(2,tick=FALSE)
text(2010,40,"Tariff rates",cex=2)

#### Imports and exports ####
# Download data from WDI
x_m<-WDIsearch("export",field="name",short=FALSE) # 75
m_m<-WDIsearch("import",field="name",short=FALSE) # 65
wdi<-WDI("all",c(x_m[75,1],m_m[65,1]),start=1960,end=2016)
d<-na.omit(wdi[wdi$iso2c %in% iso2c,])

# Aggregate to global level
d2<-ddply(d,.(year),summarise,
         exports=mean(NE.EXP.GNFS.ZS,na.rm=TRUE),
         imports=mean(NE.IMP.GNFS.ZS),na.rm=TRUE)

# Time-series object
x<-ts(d2$exports,start=1960)
m<-ts(d2$imports,start=1960)

# Plot
plot(x,xlim=c(1960,2019),ylim=c(20,60),lwd=2,
     axes=FALSE,xlab="",ylab="Percentage of GDP")
lines(m,col="steelblue4",lwd=2)
axis(1,tick=FALSE);axis(2,tick=FALSE,line=-1)
text(2016,m[57]-2,"Imports",cex=1.2)
text(2015,x[57]-2,"Exports",cex=1.2)

#### Exports per region ####
# Data downloaded in previous section

# Aggregate per region
d$region<-countrycode(d$iso2c,"iso2c","region",warn=TRUE)
x_r<-ddply(d,.(region,year),summarise,exports=mean(NE.EXP.GNFS.ZS,na.rm=TRUE))

# Time-series object
ca<-ts(x_r$exports[x_r$region=="Central America"],start=1960)
sa<-ts(x_r$exports[x_r$region=="South America"],start=1960)
sea<-ts(x_r$exports[x_r$region=="South-Eastern Asia"],start=1960)
ea<-ts(x_r$exports[x_r$region=="Eastern Asia"],start=1960)
wa<-ts(x_r$exports[x_r$region=="Western Africa"],start=1960)

# Plot data
plot(ca,xlim=c(1960,2019),ylim=c(0,100),lwd=2,col="forestgreen",lty=2,
     axes=FALSE,xlab="",ylab="")
lines(sa,col="steelblue3",lwd=2,lty=2)
lines(sea,col="red3",lwd=2,lty=2)
lines(ea,col="goldenrod4",lwd=2)
lines(wa,col="black",lwd=2)

axis(1,tick=FALSE);axis(2,tick=FALSE)
text(1970,90, "Exports \n (percentage of GDP)",cex=1.7)

text(2017,ca[57]-2,"C-America",cex=1.2)
text(2017,sa[57]-3,"S-America",cex=1.2)
text(2017,sea[57]-3,"SE-Asia",cex=1.2)
text(2017,ea[57]+3,"E-Asia",cex=1.2)
text(2017,wa[57]+2,"W-Africa",cex=1.2)

#### India and Brazil ####
# Download data
gdp_m<-WDIsearch("gdp growth",field="name",short=FALSE) # 2
wdi<-WDI(c("IN","BR"),gdp_m[2,1],start=1960,end=2016)

# Time-series object
bra<-ts(rev(wdi[,3][wdi$iso2c=="BR"]),start=1960)
ind<-ts(rev(wdi[,3][wdi$iso2c=="IN"]),start=1960)

# Calculate moving average
ma<-function(x,n=5){filter(x,rep(1/n,n),sides=1)}
bra.m<-ma(bra)
ind.m<-ma(ind)
                
# Plot
plot(bra.m,ylim=c(-1,15),xlim=c(1965,2019),lwd=2,
     axes=FALSE,xlab="",ylab="GDP growth")
lines(ind.m,lwd=2,col="steelblue4")
axis(1,tick=FALSE);axis(2,tick=FALSE)

text(2018,bra.m[57],"Brazil",cex=1.2)
text(2018,ind.m[57],"India",cex=1.2)

#### Mexico ####
x_m<-WDIsearch("exports",field="name",short=FALSE) # 61, 86, 91
gdp_m<-WDIsearch("growth",field="name",short=FALSE) # 28
wdi<-WDI("MX",c(gdp_m[28,1],x_m[c(61,86,91),1]),start=1960,end=2016)

# Time series objects
exp<-ts(wdi[,5],start=1960)
oil<-ts(wdi[,6],start=1960)
manf<-ts(wdi[,7],start=1960)
gdp<-ts(wdi[,4],start=1960)

# Plot: exports
plot(exp,lwd=2,axes=FALSE,xlab="",ylab="")
axis(1,tick=FALSE);axis(2,tick=FALSE)
text(1965,35,"Exports \n (percentage GDP)",cex=1.7)

# Plot: export composition
plot(manf,lwd=2,col="steelblue4",xlim=c(1960,2019),ylim=c(0,100),
     axes=FALSE,xlab="",ylab="Percentage of merchandise exports")
lines(oil,lwd=2)
axis(1,tick=FALSE);axis(2,tick=FALSE,line=-1)
text(2017,manf[57],"Manufactured \n goods",cex=1.2)
text(2017,oil[57],"Oil",cex=1.2)

# Plot; exports and GDP per capita growth 
plot(exp,lwd=2,axes=FALSE,xlab="",ylab="",xlim=c(1960,2019),ylim=c(-10,40))
lines(gdp,col="steelblue4",lwd=2)
axis(1,tick=FALSE);axis(2,tick=FALSE)
text(2012,exp[57],"Exports \n (% GDP)",cex=1.2)
text(2017,gdp[57],"GDP \n growth",cex=1.2)
abline(v=1994,lty=2,lwd=1.5)
text(1991,35,"NAFTA",cex=1.2)

#### Asian tigers ####
# Download data from WDI
gdp_m<-WDIsearch("growth",field="name",short=FALSE) # 28
wdi<-WDI(c("TW","KR","SG","HK"),gdp_m[28,1],start=1960,end=2016)
wdi<-wdi[order(wdi$iso2c,-wdi$year),]

# Time-series objects
kr<-ts(wdi[,3][wdi$iso2c=="KR"],start=1960)
sg<-ts(wdi[,3][wdi$iso2c=="SG"],start=1960)
hk<-ts(wdi[,3][wdi$iso2c=="HK"],start=1960)

# Plot data
par(mfrow=c(3,1))
plot(kr,lwd=2,ylim=c(-10,15),
     axes=FALSE,ylab="",xlab="",main="GDP growth Korea")
abline(h=mean(kr,na.rm=TRUE),lty=2)
axis(2,tick=FALSE)

plot(sg,lwd=2,ylim=c(-10,15),
     axes=FALSE,ylab="",xlab="",main="GDP growth Singapore")
abline(h=mean(sg,na.rm=TRUE),lty=2)
axis(2,tick=FALSE)

plot(hk,lwd=2,ylim=c(-10,15),
     axes=FALSE,ylab="",xlab="",main="GDP growth Hong Kong")
abline(h=mean(hk,na.rm=TRUE),lty=2)
axis(2,tick=FALSE)
axis(1,tick=FALSE)

#### Asian tigers 2 ####
# Download data from WDI
gdp_m<-WDIsearch("growth",field="name",short=FALSE) # 28
wdi<-WDI(c("ID","PH","TH"),gdp_m[28,1],start=1960,end=2016)
wdi<-wdi[order(wdi$iso2c,-wdi$year),]

# Time-series objects
id<-ts(wdi[,3][wdi$iso2c=="ID"],start=1960)
ph<-ts(wdi[,3][wdi$iso2c=="PH"],start=1960)
th<-ts(wdi[,3][wdi$iso2c=="TH"],start=1960)

# Plot data
par(mfrow=c(3,1))
plot(id,lwd=2,ylim=c(-15,15),
     axes=FALSE,ylab="",xlab="",main="GDP growth Indonesia")
abline(h=mean(id,na.rm=TRUE),lty=2)
axis(2,tick=FALSE)

plot(ph,lwd=2,ylim=c(-15,15),
     axes=FALSE,ylab="",xlab="",main="GDP growth Philippines")
abline(h=mean(ph,na.rm=TRUE),lty=2)
axis(2,tick=FALSE)

plot(th,lwd=2,ylim=c(-15,15),
     axes=FALSE,ylab="",xlab="",main="GDP growth Thailand")
abline(h=mean(th,na.rm=TRUE),lty=2)
axis(2,tick=FALSE)
axis(1,tick=FALSE)


#### Openness of trade ####
gdp_m<-WDIsearch("growth",field="name",short=FALSE) # 27
trade_m<-WDIsearch("trade",field="name",short=FALSE) # 108
wdi<-WDI(iso2c,c(gdp_m[27,1],trade_m[108,1]),start=2000,end=2015)

# Aggregate data
d<-ddply(wdi,.(iso2c),summarise,
         gdp_g=mean(NY.GDP.MKTP.KD.ZG,na.rm=TRUE),
         trade=mean(NE.TRD.GNFS.ZS,na.rm=TRUE))

# Plot data
par(mfrow=c(1,1))
plot(d$gdp_g,d$trade,xlim=c(-5,15),ylim=c(5,420),axes=FALSE,
     xlab="GDP growth",ylab="Trade relative to GDP",log="y")
axis(1,tick=FALSE);axis(2,tick=FALSE,line=-1)

