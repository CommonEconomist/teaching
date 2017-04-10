# Macroeconomic indicators Irish economy
# Add FDI


ire<-read.csv("data_raw/ireland.csv",header=TRUE)          # Load data
par(mar=c(5,5,1,1),las=1,bty="n",cex.lab=1.5,cex.axis=1.5) # Plot settings

# GDP per capita
gdpcap<-ts(ire$gdp_capita,start=c(1960,1),frequency=1)
plot(gdpcap,lwd=2,xlab="",ylab="in '000",axes=FALSE)
axis(1,tick=FALSE)
axis(2,tick=FALSE)

# GDP per capita growth
gdpcap.l<-lag(gdpcap,k=-1)
gdp.g=(gdpcap-gdpcap.l)/gdpcap.l*100

plot(gdp.g,lwd=2,xlab="",ylab="%",axes=FALSE) # Peculiar jump in data
abline(h=0,lty=2)
axis(1,tick=FALSE)
axis(2,tick=FALSE)

# Terms of trade
tot<-ts(ire$terms_of_trade,start=c(1960,1),frequency=1)

plot(tot,lwd=2,xlab="",ylab="%",axes=FALSE) 
abline(h=100,lty=2)
axis(1,tick=FALSE)
axis(2,tick=FALSE)
text(1962,102,"2010=100",cex=1.5)

# Unemployment rate
unem.r<-ire$unemployment_total/ire$population*100
unem.r<-ts(unem.r,start=c(1960,1),frequency=1)
plot(unem.r,lwd=2,xlab="",ylab="%",axes=FALSE,ylim=c(0,10)) 
axis(1,tick=FALSE)
axis(2,tick=FALSE)

# Government deficit
deficit<-(ire$total_revenue-ire$total_expenditure)/ire$gdp*100
deficit<-na.omit(ts(deficit,start=c(1960,1),frequency=1))
debt<-na.omit(ts(ire$gross_debt_percentage,start=c(1960,1),frequency=1))

plot(deficit,lwd=2,xlab="",ylab="Percentage of GDP",axes=FALSE,ylim=c(-35,120)) 
abline(h=0,lty=2)
abline(h=100,lty=2)
lines(debt,lwd=2,col="red")
axis(1,tick=FALSE)
axis(2,tick=FALSE,at=seq(-40,120,20))

