## Example GDP per capita growth Liberia
par(mar=c(5,5,2,2),bty='n',las=1,pty='m',cex.lab=2,cex.axis=2,cex.main=2)
require(pwt9);data("pwt9.0");pwt9.0

# Calculate growth rate
Y<-pwt9.0$rgdpna[pwt9.0$country=='Liberia']
emp<-pwt9.0$emp[pwt9.0$country=='Liberia']
dY=ts(diff(log(Y/emp))*100,start=1950)

# Plot data
plot(dY,lwd=2,axes=FALSE,xlab='',ylab='dY',main='GDP per capita growth Liberia')
axis(1,tick=FALSE)
axis(2,tick=FALSE,line=-1)
