## R problem set 1: Frequencies
load("~/Dropbox/github/Teaching/statistics_economics/data/Eurostat.RData")
par(bty="n")

# 1: Plot distribution of mortality rate
hist(df$mortality,breaks=100)

# 2: Boxplot of mortality rate
boxplot(df$mortality,horizontal=TRUE)

# 3: Plot mortality rate across income groups
boxplot(df$mortality~df$income,horizontal=TRUE)

# 4: Distribution across income groups for various mortality rates
boxplot(df$infant.mortality~df$income,horizontal=TRUE)
boxplot(df$female.mortality~df$income,horizontal=TRUE)
boxplot(df$middle.aged.mortality~df$income,horizontal=TRUE)

# 5: Plot mortality rate versus female mortality rate  
plot(df$mortality,df$female.mortality)

# 6: Plot income versus mortality rate
plot(df$eur.pc,df$mortality,log="x")
points(df$eur.pc[df$structural.fund==1],
       df$mortality[df$structural.fund==1],
       pch=19)

# 7: Doctors per inhabitants and the general and infant mortality rate
plot(df$inh.per.doc,df$mortality)
plot(df$inh.per.doc,df$infant.mortality)
