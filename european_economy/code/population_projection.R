#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#
# Population projections
# Last update: 2018 02 20
#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#
par(mar=c(5,5,3,1),bty="n",las=1,cex.axis=2,cex.lab=2,cex.main=2,
    mfrow=c(2,1))

# Prepare data
d<-read.csv("data/eurostat_population.csv",stringsAsFactors=FALSE)
pop_total<-ts(d$Value[d$INDIC_DE=="Population on 1 January - total "],
              start=c(2015,1),frequency=1)/1000000
pop_work<-ts(d$Value[d$INDIC_DE!="Population on 1 January - total "],
             start=c(2015,1),frequency=1)

# Plot
plot(pop_total,axes=FALSE,xlab="",ylab="",main="Total population (millions)",
     lwd=2,ylim=c(500,530))
axis(2,tick=FALSE)

plot(pop_work,axes=FALSE,xlab="",ylab="",ylim=c(50,70),
     main="Proportion of population aged 15-64",lwd=2)
axis(2,tick=FALSE);axis(1,tick=FALSE)
