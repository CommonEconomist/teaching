#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#
# Example Gibbs sampler
# Last update: 2018 04 02
#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#
N=5000
rho=.5

# Gibbs sampler
x=y=vector()
x[1]=y[1]=0
for(i in 2:N){
  x[i] = rnorm(1,rho*y[i-1],sqrt(1-rho^2))
  y[i] = rnorm(1,rho*x[i],sqrt(1-rho^2))
}

sim=cbind(x,y)

# Plot estimate relative to sample size
par(mar=c(5,5,3,2),las=1,cex.lab=2,cex.axis=2,cex.main=2,bty='n')

# Sample size
q<-c()
for(i in 1:N){  q[i]=mean(x[1:i])}
plot(q,type='l',log='x',axes=F,xlab='Sample size',ylab='x')
axis(1,tick=F);axis(2,tick=F,line=-2)

# Convergence
plot(ts(sim[,1]))
plot(ts(sim[,2]))

# Plots of the marginals and the joint distribution
hist(sim[,1])
hist(sim[,2])

# Plot sampling
par(pty='s',mfrow=c(1,2))
plot(sim[1:100,],type="b",axes=F,xlab='x',ylab='y')
plot(sim[1:1000,],type="b",axes=F,xlab='x',ylab='y')
