#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#
# PIGS exports
# Last update: 2018 04 19
#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#
library(WDI)

x_m<-WDIsearch("exports",field="name",short=FALSE)
wdi<-WDI("all",x_m[57,1],start=1999,end=2015) 

# Calculate export shares relative to world demand
ex<-wdi$NE.EXP.GNFS.KD[wdi$country=="World"]
d<-wdi[wdi$country %in% c("Portugal","Italy","Ireland","Greece","Spain"),]
d$ex<-rep(ex,5)
d$ex.s<-d$NE.EXP.GNFS.KD/d$ex
d<-d[order(d$country,d$year),]

# Data to vectors
por<-d$ex.s[d$country=="Portugal"]; por=por/por[1]*100
ita<-d$ex.s[d$country=="Italy"]; ita=ita/ita[1]*100
ire<-d$ex.s[d$country=="Ireland"]; ire=ire/ire[1]*100
gre<-d$ex.s[d$country=="Greece"]; gre=gre/gre[1]*100
esp<-d$ex.s[d$country=="Spain"]; esp=esp/esp[1]*100

# Plot data
plot(por,type="l",ylim=c(60,170),axes=FALSE,lwd=2,
     xlab="",ylab="",main="Ratio between export and export markets (1999=100)")
axis(1,tick=FALSE,at=1:17,label=1999:2015);axis(4,tick=FALSE)
text(17,por[17]+3,"Portugal")

lines(ita,col="dodgerblue2",lwd=2); text(17,ita[17]-3,"Italy")
lines(ire,col="gold",lwd=2); text(17,ire[17]+3,"Ireland")
lines(gre,col="steelblue4",lwd=2); text(17,gre[17]+3,"Greece")
lines(esp,col="firebrick3",lwd=2); text(17,esp[17]+3,"Spain")
