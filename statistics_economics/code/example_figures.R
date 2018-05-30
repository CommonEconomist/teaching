## Example exploratory data analysis
set.seed(42)

# Simulate data 
X<-rgamma(1200,1,6)              # Some explanatory variable
G<-sample(1:3,1200,replace=TRUE) # A randomly assigned group

y<-.2*X+.33*G^2+rnorm(1200,0,1) # Generate outcome variable (with added noise)

# Check distribution of outcome variable
hist(y)
boxplot(y,horizontal=TRUE,notch=TRUE)
plot(density(y))

# Can use boxplot to plot distribution of y across groups
boxplot(y~G)

# Scatterplot of X versus y
plot(X,y)
plot(X,y,pch=G-1)
