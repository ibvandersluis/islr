# ISLR Chapter 6: Linear Model Selection and Regularization

# Load history
loadhistory()
# Clear workspace
rm(list=ls())



# 6.5 Lab 1: Subset Selection Methods

# 6.5.1 Best Subset Selection

# Our goal is to predict a baseball player's salary based on certain performance indicators from the previous year
# Load ISLR library
library(ISLR)
fix(Hitters)
names(Hitters)
dim(Hitters)
# Count how many players have missing Salary elements
sum(is.na(Hitters$Salary))
# Remove all rows with any missing values
Hitters=na.omit(Hitters)
dim(Hitters)
sum(is.na(Hitters))


# If not installed, install.packages('leaps')
# Load leaps library for regsubsets() function
library(leaps)
# Use regsubsets() to perform best subset election by ID-ing best model that contains a given number of predictors
# 'Best' is quantified using RSS
# Same syntax as lm()
regfit.full=regsubsets(Salary~.,Hitters)
# Get best set of variables for each model size
summary(regfit.full)

# By default, regsubsets() only reports results up to the best 8-variable model
# nvmax option can override
# Fit up to 19-variable model
regfit.full=regsubsets(Salary~.,data=Hitters,nvmax=19)
reg.summary=summary(regfit.full)
names(reg.summary)
# Let's show the R-squared stat for all models, 1-19 variables
# We can see that it increases each time a variable is added (as expected)
reg.summary$rsq

# Plot RSS, adjusted R-squared, C-sub-p, and BIC for all models to help us pick a model
# Use type="l" (as in the letter L, not a one) to tell R to connect plotted points with lines
par(mfrow=c(2,2))
plot(reg.summary$rss,xlab="Number of Variables",ylab="RSS",type="l")
plot(reg.summary$adjr2,xlab="Number of Variables",ylab="Adjusted RSq",type="l")
# points() works like plot() except it adds points to existing plots instead of starting a new one
# Use which.max() to identify a max point in a vector
which.max(reg.summary$adjr2)
points(11,reg.summary$adjr2[11],col="red",cex=2,pch=20)
# Plot C-sub-p and BIC, and use which.mn to find the smallest statistics
plot(reg.summary$cp,xlab="Number of Variables",ylab="Cp",type="l")
which.min(reg.summary$cp)
points(10,reg.summary$cp[10],col="red",cex=2,pch=20)
which.min(reg.summary$bic)
plot(reg.summary$bic,xlab="Number of Variables",ylab="BIC",type="l")
points(6,reg.summary$bic[6],col="red",cex=2,pch=20)

# regsubsets() has built-in plot() command which can display selected variables for the best model
plot(regfit.full,scale="r2")
plot(regfit.full,scale="adjr2")
plot(regfit.full,scale="Cp")
plot(regfit.full,scale="bic")
# If you get 'Error in plot.new(): figure margins too large',
# Just make the plot area in RStudio larger to fit the plots
# Or run par(mfrow=c(1,1)) and view the plots one at a time

# Use coef() to see coefficient estimates
coef(regfit.full,6)



# 6.5.2 Forward and Backward Stepwise Selection

# regsubsets() can be used to perform forward/backward stepwise selection
# Use method option to specify "forward" or "backward"
regfit.fwd=regsubsets(Salary~.,data=Hitters,nvmax=19,method="forward")
summary(regfit.fwd)
regfit.bwd=regsubsets(Salary~.,data=Hitters,nvmax=19,method="backward")
summary(regfit.bwd)

# Seven-variable models identified by each method are all different
coef(regfit.full,7)
coef(regfit.fwd,7)
coef(regfit.bwd,7)



# 6.5.3 Choosing Among Models Using the Validation Set Approach and Cross-Validation

# Create random vector train where value is TRUE if the observation is in the training set, else FALSE
set.seed(1)
train=sample(c(TRUE,FALSE),nrow(Hitters),rep=TRUE)
test=(!train)
# Apply regsubsets() to training set to perform best subset selection
regfit.best=regsubsets(Salary~.,data=Hitters[train,],nvmax=19)

# Compute validation set error for best model of each size
# Make a model matrix from test data
test.mat=model.matrix(Salary~.,data=Hitters[test,])

val.errors=rep(NA,19)
# For each size i,
for(i in 1:19){
  # extract coefficients from regfit.best,
  coefi=coef(regfit.best,id=i)
  # multiply them into columns of test model matrix,
  pred=test.mat[,names(coefi)]%*%coefi
  # and compute test MSE
  val.errors[i]=mean((Hitters$Salary[test]-pred)^2)
}

# The best model is the one with 10(?) variables
val.errors
# My results differ here
which.min(val.errors)
# This shows 7 to be the optimal number of variables
# Note that according to the book, 10 is the optimal number of variables
# Since my results differ I am going with 7 instead
coef(regfit.best,7)

# Make a predict function for regsubsets
predict.regsubsets=function(object,newdata,id,...){
  form=as.formula(object$call[[2]])
  mat=model.matrix(form,newdata)
  coefi=coef(object,id=id)
  xvars=names(coefi)
  mat[,xvars]%*%coefi
}

regfit.best=regsubsets(Salary~.,data=Hitters,nvmax=19)
coef(regfit.best,7)

# Use cross-validation
# We must perform best subset selection within each of the k training sets
# Create a vector that allocates each observation to one of k=7 folds
k=7
set.seed(1)
folds=sample(1:k,nrow(Hitters),replace=TRUE)
# Create a matrix to store results
cv.errors=matrix(NA,k,19,dimnames=list(NULL,paste(1:19)))

# Write a for loop that performs cross validation
# For jth fold, elements of folds = j are in test set, remainder are in training set
# Make predictions for each model size
# Compute test errors on appropriate subset and store in cv.errors
for(j in 1:k){
  best.fit=regsubsets(Salary~.,data=Hitters[folds!=j,],nvmax=19)
  for(i in 1:19){
    pred=predict(best.fit,Hitters[folds==j,],id=i)
    cv.errors[j,i]=mean((Hitters$Salary[folds==j]-pred)^2)
  }
}
# Results in 7x19 matrix,
# where element (i,j) corresponds to test MSE for ith cross validation for best j-variable model

# Use apply() to average over the columns of this matrix to get vector
# for which the jth element is the cross-validation error for the j-variable model
mean.cv.errors=apply(cv.errors,2,mean)
mean.cv.errors
par(mfrow=c(1,1))
plot(mean.cv.errors,type="b")
# Cross-validation selects an 11-variable model

# Perform best subset selection on full dataset to get 11-variable model
reg.best=regsubsets(Salary~.,data=Hitters,nvmax=19)
coef(reg.best,11)
# These results match the book



# 6.6 Lab 2: Ridge Regression and the Lasso

# Syntax: pass in an x matrix and y vector
# y~x syntax not used
x=model.matrix(Salary~.,Hitters)[,-1]
y=Hitters$Salary
# model.matrix() useful here as it automatically transforms qualitative variables into dummy variables
# This is important because glmnet() can only take quantiative inputs

# 6.6.1 Ridge Regression

# If not installed, install.packages('glmnet')
library(glmnet)
grid=10^seq(10,-2,length=100)
# glmnet()'s alpha option determines model type. 0 for ridge regression, 1 for lasso
ridge.mod=glmnet(x,y,alpha=0,lambda=grid)
# Implements ridge regression from lambda=10^10 to lambda=10^-2
# glmnet() standardises variables to same scale by default (override with standardize=FALSE)

# Each value of lambda is a vector of ridge regression coefficients
# Stored in a matrix accessible using coef()
dim(coef(ridge.mod))
# Coefficients when lambda = 11,498
ridge.mod$lambda[50]
coef(ridge.mod)[,50]
sqrt(sum(coef(ridge.mod)[-1,50]^2))
# Coefficients when lambda = 705
ridge.mod$lambda[60]
coef(ridge.mod)[,60]
sqrt(sum(coef(ridge.mod)[-1,60]^2))

# Use predict() to get ridge regression coefficients for new value of lambda, in this case 50
predict(ridge.mod,s=50,type="coefficients")[1:20,]

# Need to split samples into training/test sets
# Two ways to randomly split the data:
# 1) Produce random vector of TRUE and FALSE elements
# 2) Randomly choose a subset of numbers between 1 and n
# We use the second approach here
set.seed(1)
train=sample(1:nrow(x),nrow(x)/2)
test=(-train)
y.test=y[test]

# Fit ridge regression on training set and evaluate MSE on test set, using lambda = 4
# In this function call, newx replaces type="coefficients" to get predictions for test set
ridge.mod=glmnet(x[train,],y[train],alpha=0,lambda=grid,thresh=1e-12)
ridge.pred=predict(ridge.mod,s=4,newx=x[test,])
mean((ridge.pred-y.test)^2)
# Different results: I got MSE of 142199, book got 101037

# If we had fit a model with an intercept,
# we could predict each test observation using the mean of training observations
mean((mean(y[train])-y.test)^2)
# More variance: I got 224670, book got 193253

# Could get the same result fitting a ridge regression model with very large lambda
ridge.pred=predict(ridge.mod,s=1e10,newx=x[test,])
mean((ridge.pred-y.test)^2)
# Fitting ridge regression model with lambda = 4 yields lower test MSE than a model with just an intercept

# Check whether ridge regression with lambda = 4 is any better than least squares regression
# (least squares is just ridge regression with lambda = 0)
ridge.pred=predict(ridge.mod,s=0,newx=x[test,],x=x,y=y,exact=T)
# Got this error from previous function call:
# Error: used coef.glmnet() or predict.glmnet() with `exact=TRUE`
# so must in addition supply original argument(s)  x and y  in order to safely rerun glmnet
# So I added the arguments x=x and y=y above. It ran but I'm not sure if that's the proper solution.
mean((ridge.pred-y.test)^2)
# I got 94050, book got 114783
lm(y~x,subset=train)
predict(ridge.mod,s=0,x=x,y=y,exact=T,type="coefficients")[1:20,]

# In general we should use cross-validation to choose the tuning parameter lambda
# Do this using cv.glmnet(), which performs ten-fold cross validation by default
set.seed(1)
cv.out=cv.glmnet(x[train,],y[train],alpha=0)
plot(cv.out)
bestlam=cv.out$lambda.min
bestlam
# I got 326, book got 212

# Get MSE associated with this value
ridge.pred=predict(ridge.mod,s=bestlam,newx=x[test,])
mean((ridge.pred-y.test)^2)
# I got 139856, book got 96016

# Fit ridge regression model on full dataset and examine coefficient estimates
out=glmnet(x,y,alpha=0)
predict(out,type="coefficients",s=bestlam)[1:20,]
# No coefficients are zero because ridge regression does not perform variable selection



# 6.6.2 The Lasso

# To fit lasso model, use glmnet() with alpha=1
lasso.mod=glmnet(x[train,],y[train],alpha=1,lambda=grid)
plot(lasso.mod)
# Now perform cross-validation and compute associated test error
set.seed(1)
cv.out=cv.glmnet(x[train,],y[train],alpha=1)
plot(cv.out)
bestlam=cv.out$lambda.min
lasso.pred=predict(lasso.mod,s=bestlam,newx=x[test,])
mean((lasso.pred-y.test)^2)
# I got 143674, book got 100743

# Advantage: resulting coefficients are sparse
out=glmnet(x,y,alpha=1,lambda=grid)
lasso.coef=predict(out,type="coefficients",s=bestlam)[1:20,]
lasso.coef
lasso.coef[lasso.coef!=0]



# 6.7 Lab 3: PCR and PLS Regression

# 6.7.1 Principal Components Regression

# If not installed, install.packages('pls')
library(pls)
set.seed(2)
pcr.fit=pcr(Salary~.,data=Hitters,scale=TRUE,validation="CV")
# scale=TRUE standardises the predictors
# validation="CV" causes pcr() to compute ten-fold cross-validation error for each possible M (num components)
# Examine resulting fit
summary(pcr.fit)
# summary() gives us the percentage of variance explained in predictors and in response

# Plot cross-validation scores using validationplot()
# val.type="MSEP" causes cross-validation MSE to be plotted
validationplot(pcr.fit,val.type="MSEP")
# Minimum error at M=16

# Perform PCR on training data and evaluate test set performance
set.seed(1)
pcr.fit=pcr(Salary~.,data=Hitters,subset=train,scale=TRUE,validation="CV")
validationplot(pcr.fit,val.type="MSEP")
# Variance: my result shows lowest error at M=5, book got M=7
pcr.pred=predict(pcr.fit,x[test,],ncomp=5)
mean((pcr.pred-y.test)^2)
# My results here differ substantially whether ncomp=5 or ncomp=7
# Book gets 96556 and I get answers in 140,000-143,000 range

# Fit PCR on full dataset, using M=5 (or M=7...)
pcr.fit=pcr(y~x,scale=TRUE,ncomp=5)
summary(pcr.fit)
# Oddly, my answers here are identical, even with M=7



# 6.7.2 Partial Least Squares

# Syntax for PLS is just like the syntax for PCR
set.seed(1)
pls.fit=plsr(Salary~.,data=Hitters,subset=train,scale=TRUE,validation="CV")
summary(pls.fit)
# My results differ
validationplot(pls.fit,val.type="MSEP")
# Also different, I get minimum at M=1, not M=2
pls.pred=predict(pls.fit,x[test,],ncomp=2)
mean((pls.pred-y.test)^2)
# Different again. I get 145368, book gets 101417
# (Apparently) the MSE is slightly higher than that obtained with ridge regression, lasso, and PCR

# Perform PLS using full data set and M=1 (or M=2??)
pls.fit=plsr(Salary~.,data=Hitters,scale=TRUE,ncomp=1)
summary(pls.fit)
# Results in summary are the same

# Save history
savehistory()
# Quit
q()
