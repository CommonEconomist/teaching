#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#
# Economic growth since 2007 
# - Eurozone, Germany, France, Italy, Spain, UK,
# Last update: 2018 02 20
#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#
d<-read.csv("data/eurostat_real_gdp.csv",header=TRUE)

# Process data
eur<-d$Value[d$GEO==d[1,2]];eur=eur/eur[1]*100
deu<-d$Value[d$GEO=="Germany (until 1990 former territory of the FRG)"]
deu=deu/deu[1]*100
fra<-d$Value[d$GEO=="France"]; fra=fra/fra[1]*100
esp<-d$Value[d$GEO=="Spain"]; esp=esp/esp[1]*100
ita<-d$Value[d$GEO=="Italy"]; ita=ita/ita[1]*100
gbr<-d$Value[d$GEO=="United Kingdom"]; gbr=gbr/gbr[1]*100

# Plot data
par(mar=c(5,2,2,5))
plot(eur,type="l",ylim=c(90,110),axes=FALSE,xlab="Growth since 2007 (2007=100)",
     ylab="",lwd=2,lty=2)
axis(1,tick=FALSE,at=1:10,label=2007:2016);axis(4,tick=FALSE)
text(10,eur[10]-.85,"Eurozone")

lines(deu,col="gold",lwd=2); text(10,deu[10]+.5,"Germany")
lines(esp,col="firebrick3",lwd=2); text(10,esp[10]+.5,"Spain")
lines(fra,col="steelblue4",lwd=2); text(10,fra[10]+.5,"France")
lines(ita,col="dodgerblue2",lwd=2); text(10,ita[10]-.5,"Italy")
lines(gbr,col="magenta",lwd=2); text(10,gbr[10]-.5,"UK")
