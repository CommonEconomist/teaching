## t-test example

# Population
set.seed(42)
db<-rnorm(150000,182.5,6)

s1<-sample(db,100,replace=FALSE)
s2<-sample(db,100,replace=FALSE)
t.test(s1,s2)

# Average temperature: Madrid and Chicago
set.seed(42)
t1<-rnorm(50,14.5,7.1)
t2<-rnorm(50,9.7,10.5)
t.test(t1,t2)

# Winning time of the Oxford-Cambridge boat race
boat_race_90s<-c(17.22,16.59,17.44,17.00,18.09,
                 18.04,16.58,17.38,16.19,16.41)
boat_race_00s<-c(18.04,19.59,16.54,18.06,18.47,
                 16.42,18.26,17.49,20.53,17.00)
t.test(boat_race_90s,boat_race_00s,alternative="less")


# Z-test
z.test<-function(x,mu,var){
  zeta=(mean(x)-mu)/(sqrt(var/length(x)))
  return(zeta)
}

nl<-c(180,188,184,186,181,178,186)
z.test(nl,mean(db),var(db))
