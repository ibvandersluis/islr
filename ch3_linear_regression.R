# ISLR Chapter 3: Linear Regression

# Load history
loadhistory()
# Clear workspace
rm(list=ls())



# 3.6.1 Libraries

# Libraries are groups of functions and datasets not included in base R distribution
# Load libraries MASS and ISLR
library(MASS)
library(ISLR)



# 3.6.2 Simple Linear Regression

# Goal: predict medv (median house value)
# Inspect Boston dataset
fix(Boston)
names(Boston)
?Boston

# Simple linear regression with medv as response and lstat as predictor
# Form: lm(y~x,data). y is response, x is predictor, data is dataset that stores them
# Returns error, need to specify dataset:
lm.fit=lm(medv~lstat)
# Provide data reference
lm.fit=lm(medv~lstat,data=Boston)
# Alternative method
attach(Boston)
lm.fit=lm(medv~lstat)

# Get info about lm.fit
lm.fit
summary(lm.fit)
names(lm.fit)
coef(lm.fit)
# Get confidence interval for coefficient estimates
confint(lm.fit)
# predict() confidence intervals and prediction intervals of medv for given value lstat
predict(lm.fit,data.frame(lstat=c(5,10,15)),interval="confidence")
predict(lm.fit,data.frame(lstat=c(5,10,15)),interval="prediction")

# Plot medv and lstat with least squares regression
plot(lstat,medv)
# abline() can draw any line abline(a,b) with intercept a and slope b
abline(lm.fit)
# abline() with different options. lwd=3 increases width by factor of 3
abline(lm.fit,lwd=3)
abline(lm.fit,lwd=3,col="red")
plot(lstat,medv,col="red")
plot(lstat,medv,pch=20)
plot(lstat,medv,pch="+")
plot(1:20,1:20,pch=1:20)

# Some diagnostic plots
# Split display into 2x2 grid
par(mfrow=c(2,2))
# Plot
plot(lm.fit)

# Use residuals() and rstudent() to return residuals and studentised residuals
plot(predict(lm.fit), residuals(lm.fit))
plot(predict(lm.fit), rstudent(lm.fit))
# Compute leverate statistics
plot(hatvalues(lm.fit))
# Return index of largest element in vector
which.max(hatvalues(lm.fit))
# For some reason, this function returns the same value twice



# 3.6.3 Multiple Linear Regression

# lm() for multiple regression syntax is lm(y~x1+x2+x3) for 3 predictors
lm.fit=lm(medv~lstat+age,data=Boston)
# Get coefficients
summary(lm.fit)
# Use . shorthand to denote all variables in order
lm.fit=lm(medv~.,data=Boston)
# Get coefficients
summary(lm.fit)
# Just get R squared
summary(lm.fit)$r.sq

# If not installed: install.packages("car")
# Ran into a problem installing the car package due to unsuccessful installation of lme4. This was resolved.
# See https://community.rstudio.com/t/issue-installing-car-package/58319 for my response detailing what I did.
# Load car package
library(car)
# Compute variance inflation factors
vif(lm.fit)

# Perform regression using all but one variable (age)
lm.fit1=lm(medv~.-age,data=Boston)
summary(lm.fit1)
# Alternative method
lm.fit1=update(lm.fit, ~.-age)
summary(lm.fit1)



# 3.6.4 Interaction Terms

# Syntax of interaction terms
# a:b includes an interaction term between a and b
# a*b includes a, b, and the interaction term aXb
# In otherwords a*b = a+b+a:b
summary(lm(medv~lstat*age,data=Boston))



# 3.6.5 Non-linear Transformations of the Predictors

# lstat^2 must be stored in the function I()
lm.fit2=lm(medv~lstat+I(lstat^2))
summary(lm.fit2)
# Quantify benefit of quadratic fit over linear fit
lm.fit=lm(medv~lstat)
anova(lm.fit,lm.fit2)
# Note difference between Model 1 and Model 2 (the predictors)
# anova() performs hypothesis test comparing Model 1 and Model 2
# H-null: the two models fit the data equally well
# H-alt: the full model is superior

# Show lack of discernable pattern in residuals
par(mfrow=c(2,2))
plot(lm.fit2)

# Use poly() function for high-order polynomials within lm()
# 5th order polynomial plot:
lm.fit5=lm(medv~poly(lstat,5))
summary(lm.fit5)

# Example of logarithmic transformation
summary(lm(medv~log(rm),data=Boston))



# 3.6.6 Qualitative Predictors

# Goal: attempt to predict Sales in 400 locations
# Examine Carseats
fix(Carseats)
names(Carseats)
# R generates dummy variables for qualitative variables automatically
# Multiple regression model with some interaction terms
lm.fit=lm(Sales~.+Income:Advertising+Price:Age,data=Carseats)
summary(lm.fit)

# contrasts() returns coding that R uses for dummy variables
attach(Carseats)
contrasts(ShelveLoc)
?contrasts



# 3.6.7 Writing Functions

# Error: no such object
LoadLibraries
# Error: no such function
LoadLibraries()
# Create function LoadLibraries()
# { signals R to allows carriage returns for more commands, } signals are that we are done entering commands
LoadLibraries=function(){
  library(ISLR)
  library(MASS)
  print("The libraries have been loaded.")
}

# Typing the function name returns the contents of the function
LoadLibraries
# Calling the function executes the instructions in the function
LoadLibraries()

# Save history
savehistory()
# Quit
q()