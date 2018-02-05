## R problem set 1: Frequencies

# 1: Number of regions per country
table(df$ccode)

# 2: Average general mortality and infant mortality rate for all regions and regions in former Eastern Bloc countries
mean(df$mortality)
mean(df$infant.mortality)

mean(df$mortality[df$eastbloc==1])
mean(df$infant.mortality[df$eastbloc==1])

# 3: Average for regions that qualify for structural funds
mean(df$infant.mortality[df$structural.fund==1],na.rm=TRUE)
mean(df$infant.mortality[df$candidate==1],na.rm=TRUE)

# 4: Structural funding regions in former East Bloc countries
d<-table(df$structural.fund)
prop.table(d) # 32%

d<-table(df$structural.fund,df$eastbloc)
prop.table(d)   # 14% of total sample
prop.table(d,1) # 43% of regions that qualify for EU funding

# 5: Probability of above average mortality rate
funding<-df$structural.fund
hi.mortality<-df$mortality.hi
d<-table(funding,hi.mortality)
prop.table(d) #0.33

# 6: East Bloc regions in highest income group
d<-table(df$income,df$eastbloc)
prop.table(d,2) # 1.9%

# 7: Middle income with above average mortality rates
# NB 
d<-table(df$income,df$mortality.hi)
prop.table(d,1) # 55% for lower-middle, 54 for upper middle

# 8: About Ireland
mean(df$mortality[df$ccode=="IE"])
a<-mean(df$eur.pc[df$ccode=="IE"])
test<-ifelse(df$eur.pc>a,1,0)
sum(test,na.rm=TRUE)/length(test)
