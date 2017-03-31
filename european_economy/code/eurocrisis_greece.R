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

#### Contraction of economy ####
grec2<-window(grc.adj,start=c(2007,2),frequency=4)
test<-grec2/grec2[1]*100

#### Public spending and tax revenue ####
a<-read.csv("data_raw/eurostat_public_spending.csv")
b<-read.csv("data_raw/eurostat_tax_revenue.csv")
x<-ts(a$Value[a$GEO=="Greece"],start=c(1999,1),frequency=1)
y<-ts(b$Value[b$GEO=="Greece"],start=c(1999,1),frequency=1)


plot(x,ylim=c(30,70),type="b")
lines(y,lty=2,type="b")

#### Labour costs ####
d<-read.csv("data_raw/oecd_labour_costs.csv")

gre<-ts(d$Value[d$Country=="Greece"],start=c(1999,1),frequency=4)
eur<-ts(d$Value[d$Country!="Greece"],start=c(1999,1),frequency=4)


plot(gre,ylim=c(60,110))
lines(eur,lty=2)
