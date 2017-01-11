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
                     deaths.middle.ages=mid_life<-mort[
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
#### 5) Indicators ####
require(stringr)
nuts2<-unique(as.character(mort$GEO))
country<-(str_extract(nuts2,"[aA-zZ]+"))

#------------------------------------------------------------------------------
#### 6) Merge data ####
df<-data.frame(GEO=unique(mort$GEO),ISO2C=country)
dat<-merge(df,mortality,all.x=TRUE)
dat<-merge(dat,population,all.x=TRUE)
dat<-merge(dat,income,all.x=TRUE)
dat<-merge(dat,doctors,all.x=TRUE)
