# ISLR Chapter 8: Tree-Based Methods

# Load history
loadhistory()
# Clear workspace
rm(list=ls())



# 8.3.1 Fitting Classification Trees

# If not already installed, install.packages('tree')
library(tree)

# Task: analyse Carseats data
# Recode Sales as binary variable
library(ISLR)
attach(Carseats)
High=ifelse(Sales<=8,"no","yes")
# Creat data frame to merge High with Carseats
Carseats=data.frame(Carseats,High)

# Fit classification tree to predict High using all variables except Sales
tree.carseats=tree(High~.-Sales,Carseats)
# Summarise tree
summary(tree.carseats)
# Training error is 9%

# Plot the tree, with pretty=0 to include category names
plot(tree.carseats)
text(tree.carseats,pretty=0)
tree.carseats

# Split into test and train sets and predict results
set.seed(2)
train=sample(1:nrow(Carseats),200)
Carseats.test=Carseats[-train,]
High.test=High[-train]
tree.carseats=tree(High~.-Sales,Carseats,subset=train)
tree.pred=predict(tree.carseats,Carseats.test,type="class")
table(tree.pred,High.test)
# Books gets different values here, but my output had more correct predictions
(104+50)/200
# 77% accuracy for this tree fit

# Use pruning to attempt to improve model
set.seed(3)
# Determine optimal tree complexity with cv.tree()
# FUN=prune.misclass indicates we want classification error rate to guide cv and pruning
# As opposed to the cv.tree() default which is deviance
cv.carseats=cv.tree(tree.carseats,FUN=prune.misclass)
names(cv.carseats)
cv.carseats
# Different values from book

# Plot error rate as function of size and k
par(mfrow=c(1,2))
plot(cv.carseats$size,cv.carseats$dev,type="b")
plot(cv.carseats$k,cv.carseats$dev,type="b")
# Apply prune.misclass() to prune tree down to 8 nodes (book says 9 but my results differ)
prune.carseats=prune.misclass(tree.carseats,best=8)
plot(prune.carseats)
text(prune.carseats,pretty=0)

# Determine accuracy
tree.pred=predict(prune.carseats,Carseats.test,type="class")
table(tree.pred,High.test)
(89+62)/200
# 75.5% accuracy with pruning
# The book's results improved, mine are slightly worse

# Increase the value of best for larger pruned tree with lower accuracy
prune.carseats=prune.misclass(tree.carseats,best=15)
plot(prune.carseats)
text(prune.carseats,pretty=0)
tree.pred=predict(prune.carseats,Carseats.test,type="class")
table(tree.pred,High.test)



# 8.3.2 Fitting Regression Trees

# Fit regression tree to Boston data set
library(MASS)
set.seed(1)
train=sample(1:nrow(Boston),nrow(Boston)/2)
tree.boston=tree(medv~.,Boston,subset=train)
summary(tree.boston)
# Variance in results

plot(tree.boston)
text(tree.boston,pretty=0)

# Determine if pruning helps
cv.boston=cv.tree(tree.boston)
plot(cv.boston$size,cv.boston$dev,type='b')

prune.boston=prune.tree(tree.boston,best=5)
plot(prune.boston)
text(prune.boston,pretty=0)

# Use unpruned tree to make predictions
yhat=predict(tree.boston,newdata=Boston[-train,])
boston.test=Boston[-train,"medv"]
plot(yhat,boston.test)
abline(0,1)
mean((yhat-boston.test)^2)
# Results differ



# 8.3.3 Bagging and Random Forests

# Apply bagging and random forests to Boston data
# If not already installed, install.packages('randomForest')
library(randomForest)
set.seed(1)
bag.boston=randomForest(medv~.,data=Boston,subset=train,mtry=13,importance=TRUE)
bag.boston

yhat.bag=predict(bag.boston,newdata=Boston[-train,])
plot(yhat.bag)
abline(0,1)
# That doesn't look right...
mean((yhat.bag-boston.test)^2)
# Results differ again, but beats  pruned tree

# Change number of trees grown by randomForest() using ntree arg
bag.boston=randomForest(medv~.,data=Boston,subset=train,mtry=13,tree=25)
yhat.bag=predict(bag.boston,newdata=Boston[-train,])
mean((yhat.bag-boston.test)^2)

# Try random forest with mtry=6
set.seed(1)
rf.boston=randomForest(medv~.,data=Boston,subset=train,mtry=6,importance=TRUE)
yhat.rf=predict(rf.boston,newdata=Boston[-train,])
mean((yhat.rf-boston.test)^2)
# Improved error rate (different from book, shocker)

# View importance of variables
importance(rf.boston)
varImpPlot(rf.boston)



# 8.3.4 Boosting

# If not already installed, install.packages('gbm')
library(gbm)

# Run gbm() with distribution="gaussian" because this is regression
# If for classification, use distribution="bernoulli"
# Specify number of trees with n.trees=5000
# Limit depth with interaction.depth=4
set.seed(1)
boost.boston=gbm(medv~.,data=Boston[train,],distribution="gaussian",n.trees=5000,interaction.depth=4)
summary(boost.boston)

# Produce partial dependence plots, showing marginal effect of selected variables on response
par(mfrow=c(1,2))
plot(boost.boston,i="rm")
plot(boost.boston,i="lstat")

# Use boosted model to predict medv on test set
yhat.boost=predict(boost.boston,newdata=Boston[-train,],n.trees=5000)
mean((yhat.boost-boston.test)^2)

# Perform boosting with different shrinkage parameter (lambda). Default = 0.001
boost.boston=gbm(medv~.,data=Boston[train,],distribution="gaussian",n.trees=5000,interaction.depth=4,shrinkage=0.2,verbose=F)
yhat.boost=predict(boost.boston,newdata=Boston[-train,],n.trees=5000)
mean((yhat.boost-boston.test)^2)

# Save history
savehistory()
# Quit
quit()
