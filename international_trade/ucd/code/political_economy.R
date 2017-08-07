setwd("~/Dropbox/github/teaching/international_trade/ucd")
d<-read.csv("data_raw/oecd_cpi.csv",stringsAsFactors=FALSE)

oecde<-ts(d$Value[d$LOCATION=="OECDE"],start=c(2013,1),frequency=12)
rus<-ts(d$Value[d$LOCATION=="RUS"],start=c(2013,1),frequency=12)

o<-oecde/oecde[1]*100
r<-rus/rus[1]*100

plot(o,ylim=c(100,150))
lines(r,lty=2)
