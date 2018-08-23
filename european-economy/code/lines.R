## Function for drawing lines 
lifeLines<-function(series,col="black",hcol="black",lwd=1,hlwd=2){
  for (i in 1:length(series[,1])){
    lines(startYear:endYear,series[i,],col=col,lwd=lwd)
  }
}