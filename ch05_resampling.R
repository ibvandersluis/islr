# ISLR Chapter 5: Resampling Methods

# Load history
loadhistory()
# Clear workspace
rm(list=ls())



# 5.3.1 The Validation Set Approach

# Load the ISLR library
library(ISLR)
# Set seed for reproducible pseudorandom results
set.seed(1)
# Split observations into two halves
train=sample(392,196)
# Use subset option in lm() to fit a linear regression with observations corresponding to training data
lm.fit=lm(mpg~horsepower,data=Auto,subset=train)
# Predict response for 392 observations
# Calculate MSE of the 196 observations in validation set
attach(Auto)
mean((mpg-predict(lm.fit,Auto))[-train]^2)
# My answer differs here, as I got 23.27 and the book got 26.14

# Use poly() to estimate test error for polynomial and cubic regressions
lm.fit2=lm(mpg~poly(horsepower,2),data=Auto,subset=train)
mean((mpg-predict(lm.fit2,Auto))[-train]^2)
# Answers differ again. I got 18.71, book got 19.82

lm.fit3=lm(mpg~poly(horsepower,3),data=Auto,subset=train)
mean((mpg-predict(lm.fit3,Auto))[-train]^2)
# I got 18.79, book got 19.78

set.seed(2)
train=sample(392,196)
lm.fit=lm(mpg~horsepower,subset=train)
mean((mpg-predict(lm.fit,Auto))[-train]^2)
# I got 25.73, book got 23.30

lm.fit2=lm(mpg~poly(horsepower,2),data=Auto,subset=train)
mean((mpg-predict(lm.fit2,Auto))[-train]^2)
# I got 20.43, book got 18.90

lm.fit3=lm(mpg~poly(horsepower,3),data=Auto,subset=train)
mean((mpg-predict(lm.fit3,Auto))[-train]^2)
# I got 20.36, book got 19.26



# 5.3.2 Leave-One-Out Cross-Validation

# Using glm() function without passing the family argument, it performs linear regression
glm.fit=glm(mpg~horsepower,data=Auto)
coef(glm.fit)

lm.fit=lm(mpg~horsepower,data=Auto)
coef(lm.fit)

library(boot)
glm.fit=glm(mpg~horsepower,data=Auto)
cv.err=cv.glm(Auto,glm.fit)
cv.err$delta

# Repeat for complex polynomial fits
# Use for() to automate the program
cv.error=rep(0,5)
for (i in 1:5){
  glm.fit=glm(mpg~poly(horsepower,i),data=Auto)
  cv.error[i]=cv.glm(Auto,glm.fit)$delta[1]
}
cv.error



# 5.3.3 k-Fold Cross-Validation

set.seed(17)
cv.error.10=rep(0,10)
for (i in 1:10){
  glm.fit=glm(mpg~poly(horsepower,i),data=Auto)
  cv.error.10[i]=cv.glm(Auto,glm.fit,K=10)$delta[1]
}
cv.error.10
# Results differ slightly



# 5.3.4 The Bootstrap

alpha.fn=function(data,index){
  X=data$X[index]
  Y=data$Y[index]
  return ((var(Y)-cov (X,Y))/(var(X)+var(Y) -2* cov(X,Y)))
}
alpha.fn(Portfolio,1:100)

set.seed(1)
alpha.fn(Portfolio,sample(100,100,replace=T))
# I got 7.37, the book got 0.59

boot(Portfolio,alpha.fn,R=1000)
# Bias and standard error differ significantly

boot.fn=function(data,index)
  return(coef(lm(mpg~horsepower,data=data,subset=index)))
boot.fn(Auto,1:329)         
# Not sure what the error is
# UPDATE 7 Apr: I had to check my parentheses and spacing. Should work now

set.seed(1)
# Still some variance in results but the function runs
boot.fn(Auto,sample(392,392,replace=T))
boot.fn(Auto,sample(392,392,replace=T))
boot(Auto,boot.fn,1000)
summary(lm(mpg~horsepower,data=Auto))$coef
# Summary is correct


boot.fn=function(data,index)
coefficients(lm(mpg~horsepower+I(horsepower^2),data=data,subset=index))
set.seed(1)
boot(Auto,boot.fn,1000)

summary(lm(mpg~horsepower+I(horsepower^2),data=Auto))$coef

# All the functions run but some results vary

# Save history()
savehistory()
q()
