## Example contingency table

# First generate some data simulating the relation between revolutions following military defeat
set.seed(42)
R<-rgamma(200,1,7)                 # Baseline risk of revolution
defeat<-rbinom(200,1,prob=.12)     # Probability of military defeat
R<-R+.4*defeat+rnorm(200,.01,.001) # Adjust probability of revolution
revolution<-ifelse(R>.5,1,0)       # Dummy variable for revolution

# Look at cross-tabulation
#table(defeat);table(revolution)
table(defeat,revolution)

# Can calcute probability
7/(168+7)  # Revolution when there was no defeat
12/(13+12) # Revolution when there was military defeat 

# Save table and look at proportions
DR<-table(defeat,revolution)

prop.table(DR)
prop.table(DR,1) # Row as margin
prop.table(DR,2) # Column as margin

# Can very easily conduct a Chi-square test using 'summary'
#summary(DR)

# Produce a mosaicplot
#plot(DR,main="Proportions",col=c("grey90","grey60"))
