## Q1: 
# Countries with larger populations tend to produce more in absolute terms
# but are not necessarily more productive as shown by GDP per capita. 

## Q2:
# First we need to calculate the growth rate:
wdi15$gdp.g=(wdi15$gdp_constant-wdi15$gdp_constant.l)/wdi15$gdp_constant.l

# Plot the data
plot(wdi15$urban_population,wdi15$gdp_constant,
     log="xy",xlab="Urban population",ylab="GDP")
# Here we see that larger shares of urban population seems to be associated
# with higher GDPs

plot(wdi15$urban_population,wdi15$gdp_pc_constant,
     log="xy",xlab="Urban population",ylab="GDP per capita")
# Similar trend for GDP per capita

plot(wdi15$urban_population,wdi15$gdp.g,xlab="Urban population",ylab="GDP")
# No pattern with the growth rate. 

## Q3:
# Here we see that there seems to be a very strong correlation where higher
# levels of GDP per capita are associated with lower infant mortality rates.

## Q4:
# On average European countries have high GDP per capita and low infant 
# mortality rates while African countries tend to have low income levels and 
# high infant mortality rates. 
# However, we also see that the top African countries in terms of GDP per capita
# overlap with the European countries in the lower and middle-low income levels.
points(wdi15[wdi15$continent=="Americas",]$gdp_pc_constant,
      wdi15[wdi15$continent=="Americas",]$under5_mortality_per1000,
      col="red",pch=19)
points(wdi15[wdi15$continent=="Asia",]$gdp_pc_constant,
      wdi15[wdi15$continent=="Asia",]$under5_mortality_per1000,
      col="yellow",pch=19)
points(wdi15[wdi15$continent=="Oceania",]$gdp_pc_constant,
      wdi15[wdi15$continent=="Oceania",]$under5_mortality_per1000,
      col="green",pch=19)

# Looking at the other continents we see that there is more of a spread. 

## Q5:
# Both countries have had pretty low life expectancies over time, Sierra Leone
# more so than Rwanda. 
# We see that there is a clear shock for Rwanda where life expectancy dips to
# the lowest observed levels. 
# Sierra Leone also experiences a decrease but not so profound. 
# The good news is that both countries did see an overal increase in life
# expectancy over time. 

## Q6:
# Definitely the genocide. 

## Q7: 
table(wdi[wdi$year>=1981 & wdi$under5_mortality_per1000>330,]
      [,c("country","year")])

lines(wdi[wdi$country=="Niger",]$under5_mortality_per1000,col="black")

table(wdi[wdi$year>=2000 & wdi$under5_mortality_per1000>200,]
      [,c("country","year")])

lines(wdi[wdi$country=="Sierra Leone",]$under5_mortality_per1000,col="blue")
lines(wdi[wdi$country=="Haiti",]$under5_mortality_per1000,col="green")

# We can identify three countries with outliers: Niger, Sierra Leone, and Haiti. 
# For Niger the likely cause is just general underdevelopment. 
# Sierra Leone experienced a pretty brutal civil war, while Haiti, which is 
# already one of the poorest countries in the world, was hit by a major earth
# quake which severly hampered civil society. 