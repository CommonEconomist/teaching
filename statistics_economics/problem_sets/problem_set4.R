## R problem set 4: Regression analysis
load("~/Dropbox/github/Teaching/statistics_economics/data/Eurostat.RData")

# 1: Regions that qualify for EU funding
m1<-lm(mortality~structural.fund,df)
summary(m1)
m2<-lm(mortality~structural.fund+eastbloc,df) # Adding Eastern Bloc dummy
summary(m2)

# 2: Regress mortality rate on log of income per capita
m3<-lm(mortality~log(eur.pc),df)
summary(m3)

# 3: Plot residuals against fitted values and fitted values against observed
plot(m3$residuals,m3$fitted)
plot(m3$model$mortality,m3$fitted)

# 4: Use purchasing power instead
m4<-lm(mortality~log(pps.pc),df)
summary(m4)

plot(m4$residuals,m4$fitted)
plot(m4$model$mortality,m4$fitted)

# 5: Adding indicator variable for regions that qualify for EU funding
m5<-lm(mortality~structural.fund+log(eur.pc),df)
summary(m5)

# 6: Model including various variables
m6<-lm(mortality~log(eur.pc)+eastbloc+euro2012,df)
summary(m6)

plot(m6$residuals,m6$fitted)
plot(m6$model$mortality,m6$fitted)


# 7: Checking effect of the number of inhabitants per doctor
m7<-lm(mortality~log(inh.per.doc)+log(eur.pc)+eastbloc,df)
summary(m7)

m8<-lm(infant.mortality~log(inh.per.doc)+log(eur.pc)+eastbloc,df)
summary(m8)


# 9: Log-transforming the outcome
m9<-lm(log(mortality)~log(eur.pc)+eastbloc,df)
summary(m9)

plot(m9$residuals,m9$fitted)
plot(m9$model[,1],m9$fitted)
