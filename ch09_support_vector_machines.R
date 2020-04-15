# ISLR Chapter 9: Support Vector Machines

# Load history
loadhistory()
# Clear workspace
rm(list=ls())



# 9.6.1 Support Vector Classifier

# If not already installed, install.packages('e1071')
library(e1071)

# Demonstrate with 2D example
set.seed(1)
x=matrix(rnorm(20*2),ncol=2)
y=c(rep(-1,10),rep(1,10))
x[y==1,]=x[y==1,]+1
plot(x,col=(3-y))
# Classes are not linearly separable

# Create data frame with respsonse coded as a factor
dat=data.frame(x=x,y=as.factor(y))
library(e1071)

# Create support vector classifier for a given cost value
# scale=FALSE indicates that the features should not be scaled to have mean of 0 and standard deviation of 1
svmfit=svm(y~.,data=dat,kernel="linear",cost=10,scale=FALSE)
# Plot the support vector
plot(svmfit,dat)
# Support vectors are X's and other observations are O's
# Only one misclassification

# Determine identity of support vectors
svmfit$index
# Get information about the fit
summary(svmfit)

# Try with smaller cost
svmfit=svm(y~.,data=dat,kernel="linear",cost=0.1,scale=FALSE)
plot(svmfit,dat)
svmfit$index
# Note that we have more support vectors with a smaller cost value, because the margin is wider

# Use tune() to perform cross-validation
set.seed(1)
tune.out=tune(svm,y~.,data=dat,kernel="linear",ranges=list(cost=c(0.001,0.01,0.1,1, 5, 10, 100)))
summary(tune.out)
# Some variance in outputs
# Lowest cross-validation error rate is 0.1
# Access the best model from tune() like this:
bestmod=tune.out$best.model
summary(bestmod)

# Use predict() to categorise the label on a set of test obsrevations
xtest=matrix(rnorm(20*2),ncol=2)
ytest=sample(c(-1,1),20,rep=TRUE)
xtest[ytest==1,]=xtest[ytest==1,]+1
testdat=data.frame(x=xtest,y=as.factor(ytest))
# Make predictions
ypred=predict(bestmod,testdat)
table(predict=ypred,truth=testdat$y)
# My answers have some variance

# Try again with cost=0.01
svmfit=svm(y~.,data=dat,kernel="linear",cost=0.1,scale=FALSE)
ypred=predict(svmfit,testdat)
table(predict=ypred,truth=testdat$y)

# Make linearly separable
x[y==1,]=x[y==1,]+0.5
plot(x,col=(y+5)/2,pch=19)

# Fit model with high cost value so that no observations are misclassified
dat=data.frame(x=x,y=as.factor(y))
svmfit=svm(y~.,data=dat,kernel="linear",cost=1e5)
summary(svmfit)
plot(svmfit,dat)
# No training errors, 3 support vectors
# But narrow margin may lead to poor performance on test data

# Try smaller cost
svmfit=svm(y~.,data=dat,kernel="linear",cost=1)
summary(svmfit)
plot(svmfit,dat)
# One misclassified observations but wider margin, will probably perfrom better on test data



# 9.6.2 Support Vector Machine

# Other kenrel options: kernel="polynomial" and kernel="radial"
# Generate data with nonlinear class boundary
set.seed(1)
x=matrix(rnorm(200*2),ncol=2)
x[1:100,]=x[1:100,]+2
x[101:150,]=x[101:150,]-2
y=c(rep(1,150),rep(2,50))
dat=data.frame(x=x,y=as.factor(y))
plot(x,col=y)

# Split into test/train groups
train=sample(200,100)
# Fit using svm() function
svmfit=svm(y~.,data=dat[train,],kernel="radial",gamma=1,cost=1)
plot(svmfit,dat[train,])
summary(svmfit)
# Results differ a bit here

# Inreasing cost will reduce training errors but at the price of irrecular boundary and maybe overfitting
svmfit=svm(y~.,data=dat[train,],kernel="radial",gamma=1,cost=1e5)
plot(svmfit,dat[train,])

# Perform cross-validation with tune to get best gamma and cost
set.seed(1)
tune.out=tune(svm,y~.,data=dat[train,],kernel="radial",ranges=list(cost=c(0.1,1,10,100,1000),gamma=c(0.5,1,2,3,4)))
summary(tune.out)
# My results give best cost as 100 and est gamma as 3
table(true=dat[-train,"y"],pred=predict(tune.out$best.model,newdata=dat[-train,]))



# 9.6.3 ROC Curves

# If not already installed, install.packages('ROCR')
library(ROCR)
rocplot=function(pred,truth,...){
  predob=prediction(pred,truth)
  perf=performance(predob,"tpr","fpr")
  plot(perf,...)
}

# Use decision.values=TRUE to obtain fitted values for a given SVM model fit
svmfit.opt=svm(y~.,data=dat[train,],kernel="radial",gamma=2,cost=1,decision.values=T)
fitted=attributes(predict(svmfit.opt,dat[train,],decision.values=TRUE))$decision.values
par(mfrow=c(1,2))
rocplot(fitted,dat[train,"y"],main="Training Data")

# We can increase gamma to get a more flexible fit and further improve accuracy
svmfit.flex=svm(y~.,data=dat[train,],kernel="radial",gamma=50,cost=1,decision.values=T)
fitted=attributes(predict(svmfit.flex,dat[train,],decision.values=T))$decision.values
rocplot(fitted,dat[train,"y"],add=T,col="red")

# Compute ROC curves on test data
fitted=attributes(predict(svmfit.opt,dat[-train,],decision.values=T))$decision.values
rocplot(fitted,dat[-train,"y"],main="Test Data")
fitted=attributes(predict(svmfit.flex,dat[-train,],decision.values=T))$decision.values
rocplot(fitted,dat[-train,"y"],add=T,col="red")



# 9.6.4 SVM with Multiple Cases

# If response is a factor of more than two levels, svm() does multi-class classification using 1v1 approach

# Generate a third class of observations
set.seed(1)
x=rbind(x,matrix(rnorm(50*2),ncol=2))
y=c(y,rep(0,50))
x[y==0,2]=x[y==0,2]+2
dat=data.frame(x=x,y=as.factor(y))
par(mfrow=c(1,1))
plot(x,col=y+1)

# Fit SVM to data
svmfit=svm(y~.,data=dat,kernel="radial",cost=10,gamma=1)
plot(svmfit,dat)



# 9.6.5 Application to Gene Expression Data

# Gene expression measurements are available for each tissue sample
library(ISLR)
names(Khan)
dim(Khan$xtrain)
dim(Khan$xtest)
length(Khan$ytrain)
length(Khan$ytest)
# Training and test sets hold 63 and 20 observations respectively
table(Khan$ytrain)
table(Khan$ytest)

# Relatively few observations compared with features --> use linear kernel
dat=data.frame(x=Khan$xtrain,y=as.factor(Khan$ytrain))
out=svm(y~.,data=dat,kernel="linear",cost=10)
summary(out)
table(out$fitted,dat$y)
# No training errors

# Try on test data
dat.te=data.frame(x=Khan$xtest,y=as.factor(Khan$ytest))
pred.te=predict(out,newdata=dat.te)
table(pred.te,dat.te$y)
# 2 test set errors

# Save history
savehistory()
# Quit
q()
