# Some statistics on debt and budget deficits in the EU
setwd("~/Dropbox/github/Teaching/eu_economics")
par(mar=c(5,5,1,1),bty="n",las=1,cex.axis=1.5,cex.lab=1.5)

#------------------------------------------------------------------------------
#### Government debt ####
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