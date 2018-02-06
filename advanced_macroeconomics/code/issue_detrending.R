## Example problem linear detrending of stochastic trend model

# Set parameters
N=100
g=3                             # Growth rate
set.seed(42);e=rnorm(N,0,5)     # Random shocks 
y=100                           # Starting value

# Generate data over time
for(i in 2:N){
  y[i]<-g+y[i-1]+e[i]
}

# First-difference model
Ly=c(NA,y[-N])
Dy=y[-1]-Ly[-1]

# Plot results
par(mar=c(4,4,2,2),mfrow=c(2,1),las=1,bty="n",
    cex.axis=1.5,cex.lab=1.5,cex.main=1.7)

plot(y,xlab="",ylab="",main=expression(y),axes=FALSE,type="l",lwd=2)
axis(2,tick=FALSE)
plot(Dy,xlab="",ylab="",main=expression(paste(Delta,y)),
     axes=FALSE,type="l",lwd=2)
abline(h=3,lty=2)
axis(1,tick=FALSE);axis(2,tick=FALSE)
