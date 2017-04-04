# Figures for lecture on the Eurocrisis: Greek Depression

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


#------------------------------------------------------------------------------
#### Contraction of economy ####
cont<-window(grc.adj,start=c(2007,2),frequency=4)
cont<-cont/cont[1]*100

# Plot
par(mar=c(4,5,1,1))
plot(cont,axes=FALSE,xlab="",ylab="",lwd=2)
axis(1,tick=FALSE);axis(2,tick=FALSE)

#------------------------------------------------------------------------------
#### Public spending and tax revenue ####
ps<-read.csv("data_raw/eurostat_public_spending.csv")
tr<-read.csv("data_raw/eurostat_tax_revenue.csv")
x<-ts(ps$Value[ps$GEO=="Greece"],start=c(1999,1),frequency=1)
y<-ts(tr$Value[tr$GEO=="Greece"],start=c(1999,1),frequency=1)

# Plot
par(mar=c(4,5,1,5))
plot(x,ylim=c(30,70),type="b",axes=FALSE,xlab="",ylab="Percentage of GDP",pch=0)
text(2015,x[17]+2,"Public \n spending")
lines(y,lty=2,type="b")
text(2015,y[17]+2,"Tax \n revenue")

axis(1,tick=FALSE,at=seq(1999,2015,2),label=seq(1999,2015,2));axis(2,tick=FALSE)

#------------------------------------------------------------------------------
#### Labour costs ####
d<-read.csv("data_raw/oecd_labour_costs.csv")

gre<-ts(d$Value[d$Country=="Greece"],start=c(1999,1),frequency=4)
eur<-ts(d$Value[d$Country!="Greece"],start=c(1999,1),frequency=4)

# Plot
plot(gre,ylim=c(60,110),lwd=2,
     ylab="",xlab="Unit labour costs (2010=100)",axes=FALSE)
lines(eur,lty=2,lwd=2)
text(2000,85,"Eurozone");text(2000,70,"Greece")

axis(1,tick=FALSE,at=seq(1999,2015,2),label=seq(1999,2015,2));axis(2,tick=FALSE)


###



d<-read.csv("data_raw/oecd_potential_gdp.csv")
d<-d[order(d$Time),]

gdp<-ts(d$Value[d$VARIABLE=="GDP"]/(1*10^9),start=c(1995,1),frequency=1)
gdptr<-ts(d$Value[d$VARIABLE=="GDPTR"]/(1*10^9),start=c(1995,1),frequency=1)


# Plot
par(mar=c(5,5,1,1))
plot(gdp,ylim=c(90,250),lwd=2,axes=FALSE,ylab="",xlab="")
lines(gdptr,lwd=2,lty=2)
axis(1,tick=FALSE);axis(2,tick=FALSE)
