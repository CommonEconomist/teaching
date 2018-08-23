## Suggested solutions tutorial "Introduction to R"

## Q1:
v<-c(16,2,3,13,5,11,10,8,9,7,6,12,4,14,15,1)
M=matrix(v,4,4)

## Q2:
# The matrix you creates is what is called a magic matrix, so the mean and 
# sum will be the same for each row and column. 

## Q3:
df<-data.frame(country=rep(c("Australia","South Africa"),each=5),
               year=rep(2005:2009,2),
               iron=c(85,98,86,86,119,68,62,59,70,94),
               wheat=c(46,64,66,58,39,19,24,21,28,9),
               iron.p=rep(c(28,33,37,61,72),2),
               wheat.p=rep(c(520,640,732,793,516),2))
df$p.index=df$iron*df$iron.p+df$wheat*df$wheat.p

## Q4:
# Above average in 7 years. 
# At least one standard deviation above average in 3 years.

## Q5:
Fh2C<-function(x){(x-32)*5/9}
cel<-Fh2C(sf$TAVG)
year<-1921:2015
plot(year,cel,type="l")   # 'type' set the type of graph, line here. 
abline(h=mean(cel),lty=2) # 'lty' specifies the line type. 

## Q6: 
tswing<-sf$TMAX-sf$TMIN
plot(year,tswing,type="l",ylab="T range",xlab="")
