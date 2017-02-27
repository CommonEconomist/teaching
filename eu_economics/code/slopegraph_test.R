setwd("~/Dropbox/github/Teaching/eu_economics")
eb<-read.csv("data_raw/eurobarometer.csv",stringsAsFactors=FALSE)
require(reshape)
eb<-reshape(eb,timevar="Date",idvar=c("Country"),direction="wide")

rownames(eb)<-eb$Country
eb[,-1]<-eb[,-1]*100; eb<-round(eb[,-1],0)
colnames(eb)<-c("Mar-1990","Mar-1991","Oct-1991","Mar-1992",
                     "Oct-2005","Sep-2006")
source_url("https://raw.githubusercontent.com/leeper/slopegraph/master/R/slopegraph.R")
source_url("https://raw.githubusercontent.com/leeper/slopegraph/master/R/segmentize.R")
source_url("https://raw.githubusercontent.com/leeper/slopegraph/master/R/bump_overlaps.R")

slopegraph(eb,,col.line='black',xlim=c(-1,ncol(eb)+2),
           col.xaxt="transparent", main="")
