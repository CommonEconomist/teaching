#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#
# Unemployment
# i) Eurozone, ii) Austria, Germany, the Netherlands, iii) Greece, Italy, Spain
# Last update: 2018 02 20
#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#
library(plyr)
d<-read.csv("data/eurostat_unemployment.csv",header=TRUE)
d<-d[d$TIME>=2007,]

# Process data
eur<-d$Value[d$GEO==d[1,2]]
north<-c("Germany (until 1990 former territory of the FRG)","Austria",
         "Netherlands")
south<-c("Greece","Italy","Spain","Portugal")
d<-d[d$GEO %in% c(north,south),]
d$group<-ifelse(d$GEO %in% north,1,0)

d2<-ddply(d,.(group,TIME),summarise,un.rate=mean(Value,na.rm=TRUE))
n.eur<-d2$un.rate[d2$group==1]
s.eur<-d2$un.rate[d2$group==0]

# Plot data
plot(eur,type="l",ylim=c(5,25),axes=FALSE,xlab="Unemployment rate",
     ylab="",lwd=2,lty=2)
axis(1,tick=FALSE,at=1:10,label=2007:2016);axis(4,tick=FALSE)
text(10,eur[10]-.85,"Eurozone")

lines(n.eur,col="steelblue4",lwd=2); text(10,n.eur[10]+.5,"Rhine")
lines(s.eur,col="steelblue4",lwd=2); text(9.8,s.eur[10]+.5,"Mediterranean")
