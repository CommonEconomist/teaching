#require(haven)
#x<-read_dta("~/Downloads/col_regfile09.dta") # Large file
load("col_regfile09.RData")

m1<-lm(log(flow)~log(gdp_o)+log(gdp_d)+log(distw),x[x$year==2006 & x$flow!=0,])
summary(m1)


par(mar=c(5,5,2,2),las=1,bty="n",cex.lab=2,cex.axis=2)
plot(m1$model[,4],m1$model[,1],cex=.5,col="grey50",
     xlab="log of distance",ylab="log of trade flow",axes=FALSE)
abline(lm(m1$model[,1]~m1$model[,4]),col="firebrick3")
axis(1,tick=FALSE);axis(2,tick=FALSE,line=-1)


theta<-c()
y<-1949:2006
N<-length(y)

for(i in 1:N){
  print(i)
  m<-lm(log(flow)~log(gdp_o)+log(gdp_d)+log(distw),
        x[x$year==y[i] & x$flow!=0,])
  theta[i]<-as.numeric(coef(m)[4])
}

theta<-ts(theta,start=y[1])

# Plot results
par(mar=c(4,4,2,2))
plot(theta,type="b",axes=FALSE,xlab="",ylab="")
axis(1,tick=FALSE);axis(2,tick=FALSE,line=-1)
text(1995,-.5,"Estimated coefficient on distance",cex=1.5)
