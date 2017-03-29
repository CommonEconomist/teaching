## Linear regression example

# Generate outcome
set.seed(42)
x=runif(1000) # Exogenous variable x
y=2*x         # Outcome variable y

# Add some randomness
X=x+.1*rnorm(1000,0,.5)
Y=y+.1*rnorm(1000,0,.5)

# Run regression
m1<-lm(Y~X)
summary(m1)

# Plot data along with regression line
plot(X,Y)            # Plot the data
abline(m1,col="red") # Add the model


# Extract fitted values and residuals
yhat<-fitted(m1) 
u<-m1$residuals  # Extract residuals

# Plot 
par(mfrow=c(1,2),pty="s",las=1) # 'pty="s"' makes a square plot
plot(Y,yhat) 
plot(u,yhat) 

# Adding a variable to model
set.seed(42)
Z=X+rnorm(1000,0,.5)
cor(X,Z)

m2<-lm(Y~X+Z)
summary(m2)
