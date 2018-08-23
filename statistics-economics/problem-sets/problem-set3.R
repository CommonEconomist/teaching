## R problem set 1: Frequencies
par(bty="n")

# 1: Mean and standard deviation mortality former Eastern Bloc
mean(df$mortality[df$eastbloc==1])
var(df$mortality[df$eastbloc==1])

mean(df$mortality)
var(df$mortality)

# 2: Check density of the data
plot(density(df$mortality))
lines(density(df$mortality[df$eastbloc==1]),lty=2,col="steelblue4")

# 3: Test if mortality rate is different in former Eastern Bloc
t.test(df$mortality[df$eastbloc==1],df$mortality[df$eastbloc==0])

# 4: Sample regions and test whether mortality rate is different
set.seed(42)
s1<-sample(df$mortality[df$structural.fund==1],50,replace=FALSE)
s2<-sample(df$mortality[df$structural.fund==0],50,replace=FALSE)
t.test(s1,s2)

# 5 Test if female mortality rate is lower
t.test(df$female.mortality,df$mortality,alternative="less")
t.test(df$female.mortality[df$structural.fund==1],
       df$mortality[df$structural.fund==1],alternative="less")


# 6: Use a z-test to test whether
# i) mortality rate is different in former Eastern Bloc countries
# ii) is different in structural fund regions
z.test<-function(x,mu,var){
  zeta=(mean(x)-mu)/(sqrt(var/length(x)))
  return(zeta)
}

z.test(df$mortality[df$eastbloc==1],
       mean(df$mortality,na.rm=TRUE),
       var(df$mortality,na.rm=TRUE))

z.test(na.omit(df$mortality[df$structural.fund==1]),
       mean(df$mortality,na.rm=TRUE),
       var(df$mortality,na.rm=TRUE))


z.test(na.omit(df$mortality[df$structural.fund==1]),
       mean(df$mortality[df$eastbloc==1],na.rm=TRUE),
       var(df$mortality[df$eastbloc==1],na.rm=TRUE))
