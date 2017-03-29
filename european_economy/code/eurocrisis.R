# Figures for lecture on the Eurocrisis
setwd("~/Dropbox/github/Teaching/european_economy")

## Function for drawing lines
lifeLines<-function(series,col="black",hcol="black",lwd=1,hlwd=2){
  for (i in 1:length(series[,1])){
    lines(startYear:endYear,series[i,],col=col,lwd=lwd)
  }
}

#------------------------------------------------------------------------------
# Start with looking at the general macroeconomic trends since roughly 1999 focusing on i) GDP growth, ii) unemployment, iii) budget surplus, and iv) government debt.
#------------------------------------------------------------------------------
#### GDP growth ####
d<-read.csv("data_raw/eurostat_gdp_growth.csv",header=TRUE)
d<-d[d$TIME>=1999,]

# Process data
require(reshape2)
dat.l<-d[,c("GEO","TIME","Value")]
dat.l<-dat.l[order(dat.l$GEO,dat.l$TIME),]
dat.w<-reshape(dat.l,timevar="TIME",idvar=c("GEO"),direction="wide")

rownames(dat.w)<-dat.w$GEO;m<-dat.w[,-1]
startYear<-min(dat.l$TIME);endYear<-max(dat.l$TIME)

# Plot data
par(las=1,tck=0.02,mar=c(4,5,3,5),mgp=c(2.8,0.3,2.8),
    cex.lab=1.2,cex.axis=1.2,cex.main=1.7)

plot(0,xlim=c(startYear,endYear),ylim=c(-15,30),type="n",bty="n",
     main="GDP growth",xlab="",ylab="",axes=FALSE)
 
lifeLines(m,col="grey60")
lines(startYear:endYear,m[9,],col="black",lwd=2.5)       # Euro area
lines(startYear:endYear,m[27,],col="firebrick3",lwd=2.5) # Spain
lines(startYear:endYear,m[13,],col="steelblue4",lwd=2.5) # Greece
axis(1,tick=FALSE); axis(2,tick=FALSE)

#------------------------------------------------------------------------------
#### Unemployment ####
d<-read.csv("data_raw/eurostat_unemployment.csv",header=TRUE)
d<-d[d$TIME>=1999,]

# Process data
dat.l<-d[,c("GEO","TIME","Value")]
dat.l<-dat.l[order(dat.l$GEO,dat.l$TIME),]
dat.w<-reshape(dat.l,timevar="TIME",idvar=c("GEO"),direction="wide")

rownames(dat.w)<-dat.w$GEO;m<-dat.w[,-1]
startYear<-min(dat.l$TIME);endYear<-max(dat.l$TIME)

# Plot data
plot(0,xlim=c(startYear,endYear),ylim=c(0,30),type="n",bty="n",
     main="Unemployment rate",xlab="",ylab="",axes=FALSE)
 
lifeLines(m,col="grey60")
lines(startYear:endYear,m[9,],col="black",lwd=2.5)       # Euro area
lines(startYear:endYear,m[27,],col="firebrick3",lwd=2.5) # Spain
lines(startYear:endYear,m[12,],col="gold",lwd=2.5)     # Germany
axis(1,tick=FALSE); axis(2,tick=FALSE)

#------------------------------------------------------------------------------
#### Deficit ####
d<-read.csv("data_raw/eurostat_budget_deficit.csv",header=TRUE)
d<-d[bd$TIME>=1999,]

# Process data
dat.l<-d[,c("GEO","TIME","Value")]
dat.l<-dat.l[order(dat.l$GEO,dat.l$TIME),]
dat.w<-reshape(dat.l,timevar="TIME",idvar=c("GEO"),direction="wide")

rownames(dat.w)<-dat.w$GEO;m<-dat.w[,-1]
startYear<-min(dat.l$TIME);endYear<-max(dat.l$TIME)

# Plot data
plot(0,xlim=c(startYear,endYear),ylim=c(-35,10),type="n",bty="n",
     main="Budget surplus (% of GDP)",xlab="",ylab="",axes=FALSE)
abline(h=-3,lwd=2);abline(h=0,lwd=2)
 
lifeLines(m,col="grey60")
lines(startYear:endYear,m[9,],col="black",lwd=2.5)       # Euro area
lines(startYear:endYear,m[14,],col="steelblue4",lwd=2.5) # Greece
lines(startYear:endYear,m[13,],col="gold",lwd=2.5)      # Germany
axis(1,tick=FALSE); axis(2,tick=FALSE)

#------------------------------------------------------------------------------
#### Government debt ####
d<-read.csv("data_raw/eurostat_public_debt.csv",header=TRUE)
d<-d[debt$TIME>=1999,]

# Process data
dat.l<-d[,c("GEO","TIME","Value")]
dat.l<-dat.l[order(dat.l$GEO,dat.l$TIME),]
dat.w<-reshape(dat.l,timevar="TIME",idvar=c("GEO"),direction="wide")

rownames(dat.w)<-dat.w$GEO;m<-dat.w[,-1]
startYear<-min(dat.l$TIME);endYear<-max(dat.l$TIME)

# Plot data
plot(0,xlim=c(startYear,endYear),ylim=c(0,180),type="n",bty="n",
     main="Government debt (% of GDP)",xlab="",ylab="",axes=FALSE)
abline(h=60,lwd=2)
 
lifeLines(m,col="grey60")
lines(startYear:endYear,m[9,],col="black",lwd=2.5)       # Euro area
lines(startYear:endYear,m[14,],col="steelblue4",lwd=2.5) # Greece
lines(startYear:endYear,m[13,],col="gold",lwd=2.5)      # Germany
axis(1,tick=FALSE); axis(2,tick=FALSE)

#------------------------------------------------------------------------------
#### Situation in Spain ####
# For illustrattion focus on single country: Spain
deficit<-read.csv("data_raw/eurostat_budget_deficit.csv",header=TRUE)
debt<-read.csv("data_raw/eurostat_public_debt.csv",header=TRUE)

x<-deficit[deficit$GEO=="Spain",]$Value
y<-debt[debt$GEO=="Spain",]$Value
years<-1995:2015

# Plot
par(mar=c(5,5,1,1),bty="n",las=1,cex.axis=1.5,cex.lab=1.5,cex.main=1.7)
plot(x,y,type="b",cex=0,lty=3,lwd=.75,
     xlab="Budget surplus (% of GDP)",ylab="Public debt (% of GDP)",axes=FALSE)
abline(h=60,lty=2);abline(v=-3,lty=2)
text(x,y,label=years,cex=1.2)
axis(1,tick=FALSE);axis(2,tick=FALSE)

#------------------------------------------------------------------------------
#### Government bond yields ####
# Bond yields for PIGS economies
# (Include Germany as reference)
bonds<-read.csv("data_raw/eurostat_bond_yields.csv",header=TRUE)

# Data to time-series
portugal<-ts(bonds[bonds$GEO=="Portugal",]$Value,start=c(1980,1),frequency=12)
ireland<-ts(bonds[bonds$GEO=="Ireland",]$Value,start=c(1980,1),frequency=12)
greece<-ts(bonds[bonds$GEO=="Greece",]$Value,start=c(1980,1),frequency=12)
spain<-ts(bonds[bonds$GEO=="Spain",]$Value,start=c(1980,1),frequency=12)
germany<-ts(bonds[bonds$GEO=="Germany (until 1990 former territory of the FRG)",]$Value,start=c(1980,1),frequency=12)

# Subset to last 10 years
portugal<-window(portugal,start=c(2006,1),frequency=12)
ireland<-window(ireland,start=c(2006,1),frequency=12)
greece<-window(greece,start=c(2006,1),frequency=12)
spain<-window(spain,start=c(2006,1),frequency=12)
germany<-window(germany,start=c(2006,1),frequency=12)

# Plot data
par(las=1,bty="n",cex.axis=1.5,cex.lab=1.5,mar=c(4,5,1,1))
plot(portugal,ylim=c(0,30),col="chartreuse4",axes=FALSE,xlab="",
     ylab="Bond yields (%)",lwd=2)
lines(ireland,col="steelblue4",lwd=2,lty=2)
lines(greece,col="dodgerblue2",lwd=2)
lines(spain,col="firebrick3",lwd=2)
lines(germany,lty=2,lwd=2)

axis(1,tick=FALSE);axis(2,tick=FALSE)

text(2012,.5,"Germany",cex=1.2)
text(2012,4,"Spain",cex=1.2)
text(2012,10,"Ireland",cex=1.2)
text(2012,14.2,"Portugal",cex=1.2)
text(2012,17,"Greece",cex=1.2)

#------------------------------------------------------------------------------
# Continue with looking at the division in Europe in the period since the crisis. Illustrating how economic convergence across country has come undone.
#------------------------------------------------------------------------------
#### Economic growth since 2007 ####
# - Eurozone, Germany, France, Italy, Spain, UK,
d<-read.csv("data_raw/eurostat_real_gdp.csv",header=TRUE)

# Process data
eur<-d$Value[d$GEO==d[1,2]];eur=eur/eur[1]*100
deu<-d$Value[d$GEO=="Germany (until 1990 former territory of the FRG)"]
deu=deu/deu[1]*100
fra<-d$Value[d$GEO=="France"]; fra=fra/fra[1]*100
esp<-d$Value[d$GEO=="Spain"]; esp=esp/esp[1]*100
ita<-d$Value[d$GEO=="Italy"]; ita=ita/ita[1]*100
gbr<-d$Value[d$GEO=="United Kingdom"]; gbr=gbr/gbr[1]*100


# Plot data
par(mar=c(5,2,2,5))
plot(eur,type="l",ylim=c(90,110),axes=FALSE,xlab="Growth since 2007 (2007=100)",
     ylab="",lwd=2,lty=2)
axis(1,tick=FALSE,at=1:10,label=2007:2016);axis(4,tick=FALSE)
text(10,eur[10]-.85,"Eurozone")

lines(deu,col="gold",lwd=2); text(10,deu[10]+.5,"Germany")
lines(esp,col="firebrick3",lwd=2); text(10,esp[10]+.5,"Spain")
lines(fra,col="steelblue4",lwd=2); text(10,fra[10]+.5,"France")
lines(ita,col="dodgerblue2",lwd=2); text(10,ita[10]-.5,"Italy")
lines(gbr,col="magenta",lwd=2); text(10,gbr[10]-.5,"UK")

#------------------------------------------------------------------------------
#### Unemployment averages ####
# i) Eurozone, ii) Austria, Germany, the Netherlands, iii) Greece, Italy, Spain
d<-read.csv("data_raw/eurostat_unemployment.csv",header=TRUE)
d<-d[d$TIME>=2007,]

# Process data
require(plyr)
eur<-d$Value[d$GEO==d[1,2]]
north<-c("Germany (until 1990 former territory of the FRG)","Austria",
         "Netherlands")
south<-c("Greece","Italy","Spain","Portugal")
d<-d[d$GEO %in% c(north,south),]
d$group<-ifelse(d$GEO %in% north,1,0)

d2<-ddply(d,.(group,TIME),summarise,un.rate=mean(Value,na.rm=TRUE))
n.eur<-d2$un.rate[d2$group==1]
s.eur<-d2$un.rate[d2$group==0]
                      
# Plot data
plot(eur,type="l",ylim=c(5,25),axes=FALSE,xlab="Unemployment rate",
     ylab="",lwd=2,lty=2)
axis(1,tick=FALSE,at=1:10,label=2007:2016);axis(4,tick=FALSE)
text(10,eur[10]-.85,"Eurozone")

lines(n.eur,col="steelblue4",lwd=2); text(10,n.eur[10]+.5,"Rhine")
lines(s.eur,col="steelblue4",lwd=2); text(9.8,s.eur[10]+.5,"Mediterranean")

#------------------------------------------------------------------------------
#### Investment levels ####
# Eurozone, Germany, France, Italy, UK
d<-read.csv("data_raw/eurostat_capital_formation.csv",header=TRUE)

# Process data
eur<-d$Value[d$GEO==d[1,2]];eur=eur/eur[1]*100
deu<-d$Value[d$GEO=="Germany (until 1990 former territory of the FRG)"]
deu=deu/deu[1]*100
fra<-d$Value[d$GEO=="France"]; fra=fra/fra[1]*100
esp<-d$Value[d$GEO=="Spain"]; esp=esp/esp[1]*100
ita<-d$Value[d$GEO=="Italy"]; ita=ita/ita[1]*100
gbr<-d$Value[d$GEO=="United Kingdom"]; gbr=gbr/gbr[1]*100


# Plot data
par(mar=c(5,2,2,5))
plot(eur,type="l",ylim=c(60,120),axes=FALSE,xlab="Capital formation (2007=100)",
     ylab="",lwd=2,lty=2)
axis(1,tick=FALSE,at=1:10,label=2007:2016);axis(4,tick=FALSE)
text(10,eur[10]-1.5,"Eurozone")

lines(deu,col="gold",lwd=2); text(10,deu[10]+.5,"Germany")
lines(esp,col="firebrick3",lwd=2); text(10,esp[10]+.5,"Spain")
lines(fra,col="steelblue4",lwd=2); text(10,fra[10]+.5,"France")
lines(ita,col="dodgerblue2",lwd=2); text(10,ita[10]-.5,"Italy")
lines(gbr,col="magenta",lwd=2); text(10,gbr[10]-.5,"UK")

#------------------------------------------------------------------------------
#### Exports ####
# Portugal, Italy, Ireland, Greece, Spain
require(WDI)
x_m<-WDIsearch("exports",field="name",short=FALSE)
wdi<-WDI("all",x_m[57,1],start=1999,end=2015) 

# Calculate export shares relative to world demand
ex<-wdi$NE.EXP.GNFS.KD[wdi$country=="World"]
d<-wdi[wdi$country %in% c("Portugal","Italy","Ireland","Greece","Spain"),]
d$ex<-rep(ex,5)
d$ex.s<-d$NE.EXP.GNFS.KD/d$ex
d<-d[order(d$country,d$year),]

# Data to vectors
por<-d$ex.s[d$country=="Portugal"]; por=por/por[1]*100
ita<-d$ex.s[d$country=="Italy"]; ita=ita/ita[1]*100
ire<-d$ex.s[d$country=="Ireland"]; ire=ire/ire[1]*100
gre<-d$ex.s[d$country=="Greece"]; gre=gre/gre[1]*100
esp<-d$ex.s[d$country=="Spain"]; esp=esp/esp[1]*100

# Plot data
plot(por,type="l",ylim=c(60,165),axes=FALSE,
     xlab="Ratio between export and export markets (1999=100)",
     ylab="",lwd=2)
axis(1,tick=FALSE,at=1:17,label=1999:2015);axis(4,tick=FALSE)
text(17,por[17]+3,"Portugal")

lines(ita,col="dodgerblue2",lwd=2); text(17,ita[17]-3,"Italy")
lines(ire,col="gold",lwd=2); text(17,ire[17]+3,"Ireland")
lines(gre,col="steelblue4",lwd=2); text(17,gre[17]+3,"Greece")
lines(esp,col="firebrick3",lwd=2); text(17,esp[17]+3,"Spain")





