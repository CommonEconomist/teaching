# Example oil prices
# Data taken from: http://www.eia.gov/petroleum/data.cfm
par(bty="n",las=1,cex.axis=1.5,cex.lab=1.5)

oil<-read.csv("data/oil_prices.csv",header=TRUE)

# EIA deciced to present the data in a bit of a weird order. 
oil$i<-1:nrow(oil)
oil<-oil[order(-oil$i),]
oil.real<-ts(oil[,2],start=c(1976,1),freq=12)

# Plot data
plot(oil.real,type="l",axes=FALSE,xlab="",ylab="",lwd=2)
axis(1,tick=FALSE)
axis(2,tick=FALSE)
