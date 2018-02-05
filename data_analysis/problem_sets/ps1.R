#------------------------------------------------------------------------------
				            # Problem set 1: Coffee exports
#------------------------------------------------------------------------------
# For this first problem set you are going to look at global coffee exports. 
# In the dataset you will find information compiled by the International 
# Coffee Organization on coffee exports, measured in 1000s 60 kg bags, by 
# coffee producing countries between April and September 2016. 
# Use the data to answer the question below (points in parentheses). 
# You can write your answers, and the code you used to get the answer in the 
# script itself. 
load("coffee_exports.RData") # Alternatively use 'Ctrl+O' to open the file

#### Question 1 (1) #### 
# Use a boxplot to visualise the distribution of the export data. 
# What are the average and median level of coffee exports? 

#### Question 2 (2) ####
# Which countries are in the top three of coffee exporters, and what is their 
# share of world trade?
# NB - to order the data frame with aggregates you can use 
# NAME[order(-NAME$VAR),]
# Where NAME is the name you give the data frame and VAR the name of 
# your variable.

#### Question 3 (1) #### 
# Plot the distribution of coffee exports per region, analyse the variability in
# the data. Adjust the data to exclude 0 exports and plot the data again. 
# Are there any noticable changes?

#### Question 4 (1) ####
# Arabica coffee is the most traded coffee in the world. The plant that
# produces the beans is indigenous to the highlands of Ethiopia. 
# What is the Ethiopian share in the global coffee trade?

# The IMF provides data on international commodity prices, and for 
# April-September the coffee prices, given in cents per pound 
# (1 kg = 2.2 pounds), for Arabica and Robusta were as follows:
#         April May June  July  August  September
# Arabica 154.20  155.40  165.85  172.35  170.34  178.07
# Robusta 87.60   91.03   92.61    96.98    96.68   101.77

#### Question 5 (2) #### 
# What was the total revenue, in million dollars, of Ethiopian exports?

#### Question 6 (1) ####
# Vietnam is a large coffee exporter and the world's main exporter of 
# Robusta coffee which was introduced there by the French. 
# In which month did Vietnam have the largest revenue?

#### Question 7 (2) ####
# Brazil is the largest coffee producer in the world. 
# Their production consists for 70% of Arabica and the remaining 30% of Robusta. 
# Calculate the price index, in million dollars, for Brazil.
# In how many months were exports above average? 
# And in how many months were revenues above average?