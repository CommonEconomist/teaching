# Public debt Eurozone
d<-read.csv("data/eurostat_public_debt_q.csv",stringsAsFactors=FALSE)

# Prepare data
d<-d[d$TIME=="2008Q2",]

# Change name for Germany
d$GEO[d$GEO=="Germany (until 1990 former territory of the FRG)"]<-"Germany"
d$iso2c<-countrycode(d$GEO,"country.name","iso2c",warn=TRUE)
d<-na.omit(d[order(d$Value),])
d<-d[d$iso2c %in% c('GR','IT','BE','PT','AT','FR','CY','DE','ES','IE',
                    'NL','FI','LU','SI','MT','SK'),]


# Plot data
barplot(d$Value,xaxt="n",yaxt="n",ylab="",border=F,width=c(.35),space=1.8,
        horiz=TRUE,col="black",main='Public debt relative to GDP (2008:2)')
axis(2,at=(1:16)-.26,labels=d$iso2c, tick=F,cex.axis=1.3)
axis(1,tick=F)
abline(v=seq(0,130,10),col="white",lwd=3)
abline(v=0,col="gray",lwd=2)
abline(v=60,lty=2)
