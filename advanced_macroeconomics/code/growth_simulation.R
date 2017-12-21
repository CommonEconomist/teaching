## Simulation technology levels over time

#### 1) Set parameters ####
t=0:100
g=.02
lambda=.01
sigma=.04
e=exp(1)


#### 2) Generate data ####

# Technology levels
# For leader country A_t=A_0e^gt
A=1*e^(.02*t)

# Solution for follower country takes form
# A_jt = (sigma_j/sigma_j+g-lambda_j)A_t + D_j0 e^-(sigma_j-lambda_j)t
D=(sigma/(sigma+g-lambda))*A+-.5*e^-((sigma-lambda)*t)
D2=(sigma/(sigma+g-lambda))*A+.5*e^-((sigma-lambda)*t)

# Ratio follower/leader
R=D/A
R2=D2/A

# Growth rate technology level follower
require(data.table)
G=(D-shift(D,n=1,type="lag"))/shift(D,n=1,type="lag")
G2=(D2-shift(D2,n=1,type="lag"))/shift(D2,n=1,type="lag")

### 3) Create figures ###
par(las=1,bty="n",cex.axis=1.5,cex.lab=1.5,cex.main=1.7,mfrow=c(1,2))

# Technology level over time
plot(A,type="l",axes=FALSE,xlab="",ylab="",ylim=c(0,10),lwd=2,
     main="Follower starts below eq.")
lines(D,lwd=2,col="steelblue4")
axis(1,tick=FALSE);axis(2,tick=FALSE)

plot(A,type="l",axes=FALSE,xlab="",ylab="",ylim=c(0,10),lwd=2,
     main="Follower starts above eq.")
lines(D2,lty=2,lwd=2,col="steelblue4")
axis(1,tick=FALSE);axis(2,tick=FALSE)

# Ratio follower/leader
par(las=1,bty="n",cex.axis=1.5,cex.lab=1.5,mfrow=c(1,1))
plot(R,type="l",axes=FALSE,xlab="",ylab="",ylim=c(.3,1.3),
     lwd=2,col="steelblue4")
lines(R2,col="steelblue4",lwd=2,lty=2)
abline(h=.8,lwd=1.5)
axis(1,tick=FALSE);axis(2,tick=FALSE)

# Technology growth rate
plot(G,type="l",axes=FALSE,xlab="",ylab="",ylim=c(0,.11),lwd=2,col="steelblue4")
lines(G2,lwd=2,col="steelblue4",lty=2)
abline(h=.02,lwd=1.5)
axis(1,tick=FALSE);axis(2,tick=FALSE)

#### 4) Growth miracle ####
# Economy changes from having sigma_j=0.005 to sigma_j=0.04 in period 21
# Let the economy start at having economy at 30% of technology level leader
sigma0=.005
L=1/3-.3
m=(sigma0/(sigma0+g-lambda))*A-L*e^-((sigma0-lambda)*t)

# Growth rate changes at period 21. Need to adjust D_j0
d=(sigma/(sigma+g-lambda))*A[21]-m[21]
m[22:101]=(sigma/(sigma+g-lambda))*A[22:101]+-d*e^-((sigma-lambda)*t[1:80])

# Technology levels over time
plot(A,type="l",axes=FALSE,xlab="",ylab="",ylim=c(0,10),lwd=2)
lines(m,lwd=2,col="steelblue4")
axis(1,tick=FALSE);axis(2,tick=FALSE)

# Ratio follower leader
R3=m/A
plot(R3,type="l",axes=FALSE,xlab="",ylab="",ylim=c(.3,.8),
     lwd=2,col="steelblue4")
abline(h=.8,lwd=1.5)
axis(1,tick=FALSE);axis(2,tick=FALSE)

# Growth rate technology
G3=(m-shift(m,n=1,type="lag"))/shift(m,n=1,type="lag")
plot(G3,type="l",axes=FALSE,xlab="",ylab="",ylim=c(0,.11),lwd=2,col="steelblue4")
abline(h=.02,lwd=1.5)
axis(1,tick=FALSE);axis(2,tick=FALSE)
