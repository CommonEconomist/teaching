setwd('~/github/teaching/european_economy')
d<-read.csv('data/ert_h_eur_q_1_Data.csv')

deu=1/d$Value[d$CURRENCY=='German mark']
m<-data.frame(deu=deu,
              fra=1/d$Value[d$CURRENCY=='French franc']/deu,
              ita=1/d$Value[d$CURRENCY=='Italian lira']/deu,
              nld=1/d$Value[d$CURRENCY=='Dutch guilder']/deu,
              bel=1/d$Value[d$CURRENCY=='Belgian franc']/deu,
              lux=1/d$Value[d$CURRENCY=='Luxembourg franc']/deu,
              irl=1/d$Value[d$CURRENCY=='Irish pound']/deu)

t<-ts(m,start=c(1971,1),frequency=4)              
t<-window(t,start=c(1972,2),end=c(1992,4))

# Divide by first value
t[,1]=t[,1]/t[,1]
t[,2]=t[,2]/t[1,2]
t[,3]=t[,3]/t[1,3]
t[,4]=t[,4]/t[1,4]
t[,5]=t[,5]/t[1,5]
t[,6]=t[,6]/t[1,6]
t[,7]=t[,7]/t[10,7]


par(mar=c(5,5,3,2),bty='n',las=1,cex.lab=2,cex.axis=2,cex.main=2)
plot(t[,1],ylim=c(.2,1.025),axes=F,xlab='',ylab='',lwd=2)
lines(t[,2],lwd=2)
lines(t[,3],lwd=2)
lines(t[,4],lwd=2)
lines(t[,5],lwd=2)
lines(t[,6],lwd=2)
lines(t[,7],lwd=2)

abline(h=1.025,lty=2)
abline(h=.975,lty=2)

axis(1,tick=F)
axis(2,tick=F,line=-2)
