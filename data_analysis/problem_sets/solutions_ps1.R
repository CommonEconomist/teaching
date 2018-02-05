## Suggested solutions problem set 1

#### Question 1 (1) #### 
# Use a boxplot to visualise the distribution of the export data. 
# What are the average and median level of coffee exports? 
boxplot(coffee$export)
mean(coffee$export)   # Mean=207.5
median(coffee$export) # Median=12

#### Question 2 (2) ####
# Which are the top three coffee exporters, and what is their 
# share of world trade?
# NB - to order the data frame you create you can use NAME[order(-NAME$VAR),]
# Where NAME is the name you give the data frame and VAR the name of 
# your variable.
coffee_pc<-aggregate(export~country,coffee,sum)
coffee_pc$share=coffee_pc$export/sum(coffee_pc$export)
coffee_pc<-coffee_pc[order(-coffee_pc$export),]
# The three largest exporters are 1) Brazil, 2) Vietnam, and 3) Colombia and 
# the control 27, 21, and 10 procent of world trade respectively. 

#### Question 3 (2) #### 
# Plot the distribution of coffee exports per region, analyse your results
# with regard to the variability in the data. Adjust the data to exclude 
# 0 exports and plot the data again. Are there any noticable changes?
par(mar=c(4,10,1,1))
boxplot(coffee$export~coffee$region,horizontal=TRUE)
boxplot(coffee[coffee$export>0,]$export~coffee[coffee$export>0,]$region,
        horizontal=TRUE)
# In general there is a lot of variability with some large exporters and a lot
# of countries that produce and export some coffee. 
# The data contains coffee producing countries that haven't exported during 
# the given time period but excluding these doesn't change the pattern in the 
# data

#### Question 4 (1) ####
# Arabica coffee is the most traded coffee in the world. The plant that
# produces the beans is indigenous to the highlands of Ethiopia. 
# What is the Ethiopian share in the global coffee trade?
# Plot their exports over time.
coffee_pc[coffee_pc$country=="Ethiopia",]$share # 3%
plot(coffee[coffee$country=="Ethiopia",]$export,type="b",ylab="",xlab="")


#### Question 5 (1) #### 
# What was the total revenue, in million dollars, of Ethiopian exports?
arabica.p<-c(154.20,155.40,165.85,172.35,170.34,178.07) # Price vector
eth.x<-coffee[coffee$country=="Ethiopia",]$export       # Export vector

# The price is in cents per pound, but the data is in 1000s 60 kg bags,
# so we need to convert this:
eth.x<-eth.x*1000*60*2.2    # In pounds
eth.rev=eth.x*arabica.p/100 # In Dollars
sum(eth.rev)/1000000        # 403 million dollars total revenue

#### Question 6 (1) ####
# Vietnam is a large coffee exporter and the world's main exporter of 
# Robusta coffee which was introduced there by the French. 
# In which month did Vietnam have the largest revenue?
robusta.p<-c(87.60,91.03,92.61,96.98,96.68,101.77)     # Price vector
vnm.x<-coffee[coffee$country=="Vietnam",]$export       # Export vector
vnm.x<-vnm.x*1000*60*2.2            # In pounds
vnm.rev=vnm.x*robusta.p/100/1000000 # In Million Dollars
plot(vnm.rev,type="b",xlab="",ylab="")

#### Question 7 (2) ####
# Brazil is the largest coffee producer in the world. 
# Their production consists for 70% of Arabica and the remaining 30% of Robusta. 
# Calculate the price index, in million dollars, for Brazil.
# In how many months were exports above average? 
# And in how many months were revenues above average?
bra.x<-coffee[coffee$country=="Brazil",]$export       # Export vector
bra.x<-bra.x*1000*60*2.2    # In pounds
bra.rev=(bra.x*.3*robusta.p+bra.x*.7*arabica.p)/100/1000000

plot(bra.x,type="b");abline(h=mean(bra.x),lty=2)
plot(bra.rev,type="b");abline(h=mean(bra.rev),lty=2)

# Exports were above avera in three months (May, August, September), while
# revenues were only above average in two months (August, September) due to 
# price differences. 

