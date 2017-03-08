# Figures for lecture on the Eurocrisis
setwd("~/Dropbox/github/Teaching/european_economy")
par(mar=c(5,5,1,1),bty="n",las=1,cex.axis=1.5,cex.lab=1.5)

#------------------------------------------------------------------------------
#### Public debt ####
debt<-read.csv("data_raw/eu_debt.csv",header=TRUE)

# Data to time-series objects
# Focus on Greece and Italy, use Eurozone as reference
eurozone_debt<-ts(debt[debt$GEO=="Euro area (17 countries)",]$Value,
                  start=c(1995,1),freq=1)
greece_debt<-ts(debt[debt$GEO=="Greece",]$Value, start=c(1995,1),freq=1)
italy_debt<-ts(debt[debt$GEO=="Italy",]$Value,start=c(1995,1),freq=1)

# Plot data for Greece, comparing it with Italy
plot(greece_debt,ylim=c(50,180),xlim=c(1995,2019),lwd=2,axes=FALSE,
     xlab="", ylab="")
text(2000,170,"Government debt \n (percentage of GDP)",cex=1.7)
text(2016.3,greece_debt[21],"Greece",cex=1.5)

lines(italy_debt,col="blue");text(2016.3,italy_debt[21],"Italy",cex=1.5)
lines(eurozone_debt,lty=2);text(2016.3,eurozone_debt[21],"Eurozone",cex=1.5)
abline(h=60)

axis(1,tick=FALSE)
axis(2,tick=FALSE,line=-1)

#------------------------------------------------------------------------------
#### Budget deficit ####
deficit<-read.csv("data_raw/eu_deficit.csv",header=TRUE)

# Data to time-series objects
# Focus on Greece and Germany, use Eurozone as reference
eurozone_deficit<-ts(deficit[deficit$GEO=="Euro area (17 countries)",]$Value,
                  start=c(1995,1),freq=1)
greece_deficit<-ts(deficit[deficit$GEO=="Greece",]$Value, 
                   start=c(1995,1),freq=1)
germany_deficit<-ts(deficit[
  deficit$GEO=="Germany (until 1990 former territory of the FRG)",]$Value,
                    start=c(1995,1),freq=1)

# Plot data for Greece, comparing it to Germany
plot(greece_deficit,ylim=c(-16,5),xlim=c(1995,2020),lwd=2,axes=FALSE,
     xlab="", ylab="")
text(2000,3,"Government budget surplus \n (percentage of GDP)",cex=1.7)
text(2016.3,greece_deficit[21],"Greece",cex=1.5)
lines(germany_deficit,col="blue")
text(2016.4,germany_deficit[21],"Germany",cex=1.5)
lines(eurozone_deficit,lty=2)
text(2016.5,eurozone_deficit[21],"Eurozone",cex=1.5)
abline(h=-3)
axis(1,tick=FALSE)
axis(2,tick=FALSE,line=-1)

#------------------------------------------------------------------------------
#### Government bond yields ####
# Bond yields for PIGS economies
# (Include Germany as reference)

# Load data
bonds<-read.csv("data_raw/eu_bonds_lt.csv",header=TRUE)

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
plot(portugal,ylim=c(0,30),col="chartreuse4",axes=FALSE,xlab="",ylab="%",
     lwd=2)
lines(ireland,col="steelblue4",lwd=2,lty=2)
lines(greece,col="dodgerblue2",lwd=2)
lines(spain,col="firebrick3",lwd=2)
lines(germany,lty=2,lwd=2)
axis(1,tick=FALSE)
axis(2,tick=FALSE)

text(2012,.5,"Germany",cex=1.2)
text(2012,4,"Spain",cex=1.2)
text(2012,10,"Ireland",cex=1.2)
text(2012,14.2,"Portugal",cex=1.2)
text(2012,17,"Greece",cex=1.2)

#------------------------------------------------------------------------------
#### Greece GDP ####
# Greece GDP quarterly
grc<-read.csv("data_raw/greece_gdp_quarterly.csv")
par(mar=c(3,3,1,1),las=1,bty="n",cex.axis=1.5,cex.lab=1.5)

# Create time-series objects
# Use Chain linked volume, raw data and adjusted data
grc.un<-grc[grc$UNIT=="Chain linked volumes, index 2010=100" &
              grc$S_ADJ=="Unadjusted data (i.e. neither seasonally adjusted nor calendar adjusted data)",]$Value
grc.un<-ts(grc.un,start=c(1995,1),frequency=4)

grc.adj<-grc[grc$UNIT=="Chain linked volumes, index 2010=100" &
              grc$S_ADJ=="Seasonally and calendar adjusted data",]$Value
grc.adj<-ts(grc.adj,start=c(1995,1),frequency=4)

# Plot data
plot(grc.adj,ylim=c(60,120),xlab="",ylab="",axes=FALSE,lwd=2)
lines(grc.un,col="grey50",type="b")
text(1998,115,"Chain linked volumes \n index 2010=100",cex=1.7)
axis(1,tick=FALSE,line=-1)
axis(2,tick=FALSE,line=-1)

abline(h=grc.adj[87],lty=3)
