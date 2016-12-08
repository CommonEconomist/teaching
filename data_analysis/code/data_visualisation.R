#------------------------------------------------------------------------------
				# Tutorial 2: Data visualisation
#------------------------------------------------------------------------------

setwd("~/Dropbox/ucd/data_analysis/") # Set working directory
par(las=1,bty="n")                    # Plot settings

# In this tutorial we are going to have a look at a number of different
# probability distributions. 
# We are going to look at the characteristics of these distributions and 
# see how weel these fit observed data. 
# However, we first start with generating random data before we move on
# to real life data. 

#------------------------------------------------------------------------------
#### 1) Generating probability distributions ####
# We start with generating some random data.
# The normal distribution is often a good vantage point:
set.seed(42);x<-rnorm(1000) # NB - 'set.seed' used for replicable results

# When working with data, one of the first things you do is examining the 
# properties by looking and the descriptive statistics and plotting the data.
# This can often reveal relevant information such as the presence of missing
# values or whether the data is skewed. 
# Important aspect to be considered when further processing the data and using
# it for more formal statistical analysis. 
# For our randomly distributed data, 
# let's have a look at the summary statistics:
summary(x)
sd(x)

# Plot the data in a histogram, adding the mean as a vertical line
hist(x) # You can use 'prob=TRUE' to get probabilities rather than frequencies.
abline(v=mean(x),col="red") 

## Q1: Generate the following distributions, all with size 1000:
# Normal distribution with mean 2 and sd 1;
# Uniform distribution with min 0 and max 10;
# Poisson distribution with mean 2;
# Gamma distribution with mean 2. 
# See '?Distributions' for help. 
# Plot the results using the code given below. 
par(mfrow=c(2,2)) # 2x2 plot
hist(Normal);hist(Uniform);hist(Poisson);hist(Gamma) # Note use of ';'
dev.off() # To remove plot and reset settings

## Q2; For each distribution, think of real life example where the data would
# fit that distribution. 

# A recent study has shown that the Dutch men are the tallest in the world. 
# http://www.bbc.com/news/science-environment-36888541
# The average height of the Dutch man is 182.5 cm with a standard deviation of 
# 6 cm. 
# Generate the height distribution for a population the size of 's-Hertogenbosch
# (150,000) and plot the data in a histogram indicating where the mean is. 

## Q3: How many man will be at least 188.5 cm tall?

## Q4: The average Dutch woman has a height of 169 cm with a standard deviation
# of 2 cm. 
# On a population of 8.5 million Dutch women, how many will be at least as tall
# as the average Dutch man?
# Hint: you have to use the 'pnorm' function here, check '?pnorm'. 
# You can take the square root using 'sqrt'. 

#------------------------------------------------------------------------------
                            #******************#
                            #### Tukey Time ####
                            #******************#

# John Tukey (1915-2000) was an American mathematician who made a number of 
# important contributions to the field of statistics. 
# For instance, he is responsible for laying out the foundations of 
# exploratory data analysis, a central theme in this part of the course.
# One of his most famous contributions was the boxplot. 
# The boxplot is a method to graphically display the distribution of the data,
# specifically focusing on the quartiles. 
# Let's use the boxplot to visually summarise the height data you just generated. 
boxplot(x,horizontal=TRUE) # Can change orientation using 'horizontal=TRUE'

# In a boxplot, the box shows the range of the data between the first and third
# quartile, where the thick black line indicates the median. 
# The whiskers in this case extend to 1.5 of the Inter Quartile Range (Q3-Q1)
# of the lower or upper quartile. 
# The dots outside of the whiskers represent the outliers. 
# To get a summary we can get Tukey's Five Numbers, which will give the 
# minimum, lower-hinge, median, upper-hing, and maximum. 
fivenum(x)

# The boxplot is useful to compare data across groups. 
# Let's look at the average height of women again aross groups of different 
# size.
## Q5: What indication of sample size does the boxplot give?
index<-c(rep(1,30),rep(2,200),rep(3,1500),rep(4,20000))  # Generate groups
data<-c(rnorm(30,169,2),rnorm(200,169,2),
        rnorm(1500,169,2),rnorm(20000,169,2))            # Generate data
boxplot(data~index)

#------------------------------------------------------------------------------
#### 2) The size of whales ####
# From Dutch women we move on to the size of whales. 
# You should have downloaded a csv-file called 'whales.csv'. 
# The data is taken from a paper called "How Large Should Whales Be?", which
# of course is an important scientific question. 
# http://journals.plos.org/plosone/article?id=10.1371/journal.pone.0053967
# The data consists of estimates of whale size for different species. 
# Load said file using the following command:
whales<-read.csv("whales.csv",header=TRUE,sep=",",stringsAsFactors=FALSE)

# Let's check the data we just loaded
str(whales) 
head(whales)

# A useful command in R is 'unique'. 
# Let's find out the number of different groups and families.
length(unique(whales$group))
length(unique(whales$family))

## Q6: Examine the distribution of the size of whales. 
# Do you think that the average size is a good measure of centrality?
# Explain why or why not. 

# We can use the boxplot to summarise the size distribution per group.
# Due to the length of the names we first need to adjust some of the plot
# settings
par(mar=c(4,10,2,2),las=1)# 'mar' sets graph margins (bottom,right,top,left).
boxplot(mass_kg~family,whales,horizontal=TRUE)

# Figure shows that there is quite some variation. 
# The large scale differences obscure details for some families. 
# One thing we could do is use a log-scale. 
# This is easily implemented in the plot function specifying the 'log' argument. 
options(scipen=4) # To keep axis without scientific notations
boxplot(mass_kg~family,whales,horizontal=TRUE,log="x")

## Q7: Examine why the Physeteridae family produces such a peculiar boxplot. 

#------------------------------------------------------------------------------
#### 3) A classic: Prussian cavalry officers kicked to death by horses ####
# This is a classic example in statistics based on the work by 
# Ladislaus Bortkiewicz (1868-1931), a Russian economist and statistician, 
# whose parents were Polish and lived and worked mostly in Germany. 
# For his most famous work he used data on the number of officers in the 
# Prussian cavalry which were kicked to deaths. 
# The data spans 20 years and covers 14 corps, corps as in military units. 
prussia<-read.csv("prussian.csv",stringsAsFactors=FALSE)

## Q8: Examine the fatality data and discuss what type of distribution would
# best fit the data. 

# We continue by calculating the proportion of observations with size X. 
# So we're interested in what percentage of the observations have 0 fatalities,
# the percentage with 1 fatality and so on. 
table(prussia$deaths)                # Frequency of fatalities
V<-as.vector(table(prussia$deaths))  # Vectorise the table
perc<-V/sum(V)*100                   # Calculate proportion
dev.off()                            # Reset plot area
plot(0:4,perc,type="b",ylim=c(0,60),
     xlab="Number of fatalities",ylab="Percentage") # Plot the results

## Q9: Use your anwer for question 8 to predict the proportion of 
# observations of size X. 
# Add your result to the existing plot using the code given below.
# Does your distribution fit the data well?
lines(0:4,'YOUR PREDICTIONS',lty=2,type="b") 

# Let's examine the toal number of fatalities per year. 
# Aggregating the data to annual level. 
kicks.yr<-aggregate(deaths~year,prussia,sum)
plot(kicks.yr$year,kicks.yr$deaths,type="b",
     xlim=c(1875,1894),ylim=c(0,20)) # Use 'xlim' and 'ylim' to adjust axis

## Q10: Suppose you're a young cavalry officer in the Prussian army. 
# Which corps would you rather avoid being enlisted to? 
# How much larger is the average fatality rate for this corps compared to all
# cavalry units?
# And which corps have below average fatality rates?
