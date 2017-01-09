sterling<-read.csv("data_raw/sterling_euro_exchange_daily.csv")

par(las=1,cex.lab=1.5,cex.axis=1.5,mar=c(4,5,1,1))
plot(sterling$Value,type="b",axes=FALSE,xlab="",ylab="",lwd=2,pch=19,
     ylim=c(0.7,1))
axis(2,tick=FALSE,line=-1)
text(40,.97,"Euro/Pound exchange rate \n (trading days only)",cex=1.7)

text(130,0.74,"Brexit referendum",cex=1.5)
segments(125,.743,125,0.7595,lwd=1.5,col="black")

segments(139,.81,139,0.83,lwd=1.5,col="black")
text(145,0.80,"Cameron \n resignation",cex=1.5)

segments(197,.87,197,.83,lwd=1.5,col="black")
text(197,.82,"Conservatives \n conference",cex=1.5)

segments(126,mean(sterling[-1:-125,]$Value,na.rm=TRUE),261,lty=2)
segments(1,mean(sterling[1:125,]$Value,na.rm=TRUE),125,lty=2)

text(126,.87,"0.87 (0.02)",cex=1.5)
text(4.5,.785,"0.78 (0.02)",cex=1.5)
axis(1,tick=FALSE,at=c(22,43,67,87,109,131,152,175,200,218,240),
     label=c("Feb","Mar","Apr","May","Jun","Jul","Aug","Sep","Oct","Nov","Dec"))
