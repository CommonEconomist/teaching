# GDP Germany 
par(mar=c(5,5,4,2),las=1,bty="n",cex.lab=2,cex.axis=2,cex.main=2) 
d<-read.csv('data/gdp_germany_q.csv')
gdp<-ts(d[,2],start=c(1991,1),frequency=4)

# Plot
plot(gdp,type='l',lwd=2,xlab='',ylab='',main='Germany GDP',axes=FALSE,log='y') 
axis(1,tick=FALSE)
axis(2,tick=FALSE,line=-3)

