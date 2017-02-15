# Clean eurostat data on mortality
setwd("~/Dropbox/github/Teaching/statistics_economics")

#------------------------------------------------------------------------------
#### 1) Population ####
pop<-read.csv("data_raw/demo_r_pjangroup_1_Data.csv");str(pop)

population<-data.frame(GEO=unique(pop$GEO),
                    pop=pop[pop$AGE=="Total" & pop$SEX=="Total" & 
                              pop$TIME==2014,]$Value,
                    pop.male=pop[pop$AGE=="Total" & pop$SEX=="Males" &
                                   pop$TIME==2014,]$Value,
                    pop.female=pop[pop$AGE=="Total" & pop$SEX=="Females" &
                                     pop$TIME==2014,]$Value,
                    pop.u5=pop[pop$AGE=="Less than 5 years" & pop$SEX=="Total" &
                                 pop$TIME==2014,]$Value,
                    pop.middle.aged=pop[pop$AGE=="From 45 to 49 years" & 
                                          pop$SEX=="Total" & 
                                          pop$TIME==2014,]$Value +
                      pop[pop$AGE=="From 50 to 54 years" & pop$SEX=="Total"& 
                            pop$TIME==2014,]$Value)

#------------------------------------------------------------------------------
#### 2) Mortality ####
mort<-read.csv("data_raw/demo_r_magec3_1_Data.csv");str(mort)

mortality<-data.frame(GEO=unique(mort$GEO),
                     deaths=mort[mort$AGE=="Total" & mort$SEX=="Total" &
                                   mort$TIME==2014,]$Value,
                     deaths.male=mort[mort$AGE=="Total" & mort$SEX=="Males" &
                                         mort$TIME==2014,]$Value,
                     deaths.female=mort[mort$AGE=="Total" &
                                          mort$SEX=="Females" &
                                          mort$TIME==2014,]$Value,
                     deaths.u5=mort[mort$AGE=="Less than 5 years" & 
                                       mort$SEX=="Total" & 
                                       mort$TIME==2014,]$Value,
                     deaths.middle.aged=mid_life<-mort[
                       mort$AGE=="From 45 to 49 years" & 
                         mort$SEX=="Total" & mort$TIME==2014,]$Value +
                         mort[mort$AGE=="From 50 to 54 years" & 
                                mort$SEX=="Total" & mort$TIME==2014,]$Value)

#------------------------------------------------------------------------------
#### 3) GDP ####
gdp<-read.csv("data_raw/nama_10r_2gdp_1_Data.csv");str(gdp)

unit<-as.vector(unique(gdp$UNIT))
income<-data.frame(GEO=unique(gdp$GEO),
                       eur.pc=gdp[gdp$UNIT==unit[1] & gdp$TIME==2014,]$Value,
                       eur.pc.per=gdp[gdp$UNIT==unit[2]& gdp$TIME==2014,]$Value,
                       pps.pc=gdp[gdp$UNIT==unit[3] & gdp$TIME==2014,]$Value,
                       pps.pc.per=gdp[gdp$UNIT==unit[4] & gdp$TIME==2014,]$Value,
                       eur.mil=gdp[gdp$UNIT==unit[5]& gdp$TIME==2014,]$Value,
                       eur.pps.mil=gdp[gdp$UNIT==unit[6]& gdp$TIME==2014,]$Value)

#------------------------------------------------------------------------------  #### 4) Doctors ####                  
doc<-read.csv("data_raw/hlth_rs_prsrg_1_Data.csv");str(doc)
doctors<-data.frame(GEO=unique(doc$GEO),inh.per.doc=doc[doc$TIME==2014,]$Value)

#------------------------------------------------------------------------------
#### 5) Country code ####
require(stringr)
nuts2<-unique(as.character(mort$GEO))
geo<-(str_extract(nuts2,"[aA-zZ]+"))
ccode<-substring(geo,1,2)

#------------------------------------------------------------------------------
#### 6) Merge data ####
df<-data.frame(GEO=unique(mort$GEO),CCODE=ccode)
dat<-merge(df,mortality,all.x=TRUE)
dat<-merge(dat,population,all.x=TRUE)
dat<-merge(dat,income,all.x=TRUE)
dat<-merge(dat,doctors,all.x=TRUE)

#------------------------------------------------------------------------------
#### 7) Create variables ####
# - Mortality rate
# - Infant mortality rate
# - Female mortality rate
# - Mortality rate middle aged
dat$mortality<-dat$deaths/(dat$pop+dat$deaths)*1000
dat$infant.mortality<-dat$deaths.u5/(dat$pop.u5+dat$deaths.u5)*1000
dat$female.mortality<-dat$deaths.female/(dat$pop.female+dat$deaths.female)*1000
dat$middle.aged.mortality<-dat$deaths.middle.aged/
  (dat$pop.middle.aged+dat$deaths.middle.aged)*1000

#------------------------------------------------------------------------------
#### 8) Create some categorical variables ####
# - Dummy variable for former Warsaw pact countries and Soviet states
# - Dummy for areas that qualify for structural funds
# - Income groups (4)
# - Round reached at Euro 2012

# Eastbloc
wp<-c("BG","CZ","SK","HU","PL","RO","EE","LT","LV")
dat$eastbloc<-ifelse(dat$CCODE %in% wp,1,0)

# Structural funds
# Qualifies when GDP is lower than 75% of EU average
dat$structural.fund<-ifelse(dat$eur.pc< .75*mean(dat$eur.pc,na.rm=TRUE),1,0)

# Income groups
q<-as.numeric(quantile(dat$eur.pc,probs=c(.25,.5,.75),na.rm=TRUE))
dat$q1<-ifelse(dat$eur.pc<=q[1],1,0)
dat$q2<-ifelse(dat$eur.pc>q[1] & dat$eur.pc<=q[2],2,0)
dat$q3<-ifelse(dat$eur.pc>q[2] & dat$eur.pc<=q[3],3,0)
dat$q4<-ifelse(dat$eur.pc>q[3],4,0)
dat$income=dat$q1+dat$q2+dat$q3+dat$q4

# Euro 2012
euro2012<-data.frame(CCODE=unique(ccode),
                     euro2012=c(0,0,2,1,3,0,1,2,4,2,1,4,0,0,0,0,
                             0,0,1,0,1,3,0,0,0,0,1,2,0,0,0,0,
                             0,0,0,0))
dat<-merge(dat,euro2012,all.x=TRUE)

# Above average mortality rate
dat$mortality.hi<-ifelse(dat$mortality>mean(dat$mortality,na.rm=TRUE),1,0)

#------------------------------------------------------------------------------
#### 9) Subset and save data ####
eurostat<-dat[,c("GEO","CCODE","mortality","infant.mortality",
                 "female.mortality","middle.aged.mortality","mortality.hi",
                 "eur.pc","pps.pc","inh.per.doc","income",
                 "structural.fund","eastbloc","euro2012")]
# Drop Non-EU countries
eurostat$noneu<-ifelse(eurostat$CCODE %in% c("AL","CH","NO","TR","IS"),1,0)
df<-eurostat[eurostat$noneu==0,]
df$noneu<-NULL
df$CCODE<-factor(df$CCODE)

save(df,file="data/Eurostat.RData")
