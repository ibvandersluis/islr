# ISLR Chapter 4: Classification

# Load history
loadhistory()
# Clear workspace
rm(list=ls())



# 4.6.1 The Stock Market Data

# Load ISLR library
library(ISLR)
# Inspect Smarket
names(Smarket)
dim(Smarket)
summary(Smarket)
pairs(Smarket)

# cor() gives a matrix containing all pairwise correlations from predictors in the dataset
# Gives error, Direction variable is qualitative
cor(Smarket)
# Exclude Direction
cor(Smarket[,-9])
# Shows little correlation between today's returns and yesterday's returns
# Notable correlation: Year-Volume
# Plot Volume
attach(Smarket)
plot(Volume)



# 4.6.2 Logistic Regression

# glm() fits generalised linear models, which includes logistic regression
# Use arg family=binomial to indicate R should run logistic regression
glm.fit=glm(Direction~Lag1+Lag2+Lag3+Lag4+Lag5+Volume,data=Smarket,family=binomial)
summary(glm.fit)
# Get coefficients for this model
coef(glm.fit)
summary(glm.fit)$coef
summary(glm.fit)$coef[,4]
# Use predict() to determine probability that market will go up given the predictors
# type="response" tells R to output probabilities of P(Y=1|X) instead of other info
# If predict() receives no dataset, probabilities are computed using log regression training data
glm.probs=predict(glm.fit,type="response")
glm.probs[1:10]
# Show dummy variable
contrasts(Direction)
# Create a vector of class predictions based on whether probability of market increase > 0.5
# Create vector of 1,250 'Down' elements
glm.pred=rep("Down",1250)
# Change elements to 'Up' if probability > 0.5
glm.pred[glm.probs>.5]="Up"
# Use table() to produce confusion matrix for determining correct/incorrect classifications
table(glm.pred,Direction)
# From output, calculate how many were correct
# i.e. in how many cases did the market go up when we predicted up, or down when we predicted down
(507+145)/1250
# Compute fraction of days for which prediction was correct
mean(glm.pred==Direction)
# WARNING: we trained and tested using the same dataset
# Which means that 100 - 52.2 = 47.8% is the TRAINING error rate
# We need to train and test with separate data if we want realistic error rates
# So let's do that

# Create boolean vector corresponding to observations from 2001-2004
# True if < 2005, false otherwise
train=(Year<2005)
# Separate 2005 data, AKA data that is not being used for training
Smarket.2005=Smarket[!train,]
dim(Smarket.2005)
# Separate 2005 direction data for assessing accuracy of our model
Direction.2005=Direction[!train]

# Fit logistic regression model using observations < 2005
glm.fit=glm(Direction~Lag1+Lag2+Lag3+Lag4+Lag5+Volume,data=Smarket,family=binomial,subset=train)
# Obtain predictions of stock market for 2005 observations
glm.probs=predict(glm.fit,Smarket.2005,type="response")

# Compare predictions to observations for 2005
# Make vector of 252 'Down' values
glm.pred=rep("Down",252)
# If prob > 0.5, set to 'Up'
glm.pred[glm.probs>.5]="Up"
# Check accuracy
table(glm.pred,Direction.2005)
mean(glm.pred==Direction.2005)
# Get error rate
mean(glm.pred!=Direction.2005)
# 48% accuracy means it's worse than guessing

# Let's try removing predictors that are less helpful: Lag3, Lag4, Lag5
# Refit logistic regression with only Lag1 and Lag2
glm.fit=glm(Direction~Lag1+Lag2,data=Smarket,family=binomial,subset=train)
glm.probs=predict(glm.fit,Smarket.2005,type="response")
glm.pred=rep("Down",252)
glm.pred[glm.probs>.5]="Up"
table(glm.pred,Direction.2005)
# Get accuracy
mean(glm.pred==Direction.2005)
# 56% accuracy
# Accuracy on days when a market increase is predicted
106/(106+76)
# 58% accuracy

# Predict returns associated with values Lag1=1.2 & Lag2=1.1, and Lag1=1.5 & Lag2=-0.8
predict(glm.fit,newdata=data.frame(Lag1=c(1.2,1.5),Lag2=c(1.1,-0.8)),type="response")



# 4.6.3 Linear Discriminant Analysis (LDA)

# Load MASS
library(MASS)
# Use lda() function. Syntax same as for lm(), and glm() except for absence of family option
# Fit model using observations before 2005
lda.fit=lda(Direction~Lag1+Lag2,data=Smarket,subset=train)
lda.fit
plot(lda.fit)
# Get predictions for 2005 observations
lda.pred=predict(lda.fit,Smarket.2005)
names(lda.pred)
# Store vector of predictions
lda.class=lda.pred$class
# Compare results
table(lda.class,Direction.2005)
# Check accuracy
mean(lda.class==Direction.2005)
# 56% accuracy, essentially the same accuracy as logistic regression

# Apply 50% threshold to posterior probabilites to recreate predictions form lda.pred$class
sum(lda.pred$posterior[,1]>=.5)
sum(lda.pred$posterior[,1]<.5)
# Note that posterior probability output is the probability of market decrease
lda.pred$posterior[1:20,1]
lda.class[1:20]
# Change the threshold to 90%
sum(lda.pred$posterior[,1]>.9)



# 4.6.4 Quadratic Discriminant Analysis (QDA)

# Fit QDA model to Smarket. Syntax of qda() identical to lda()
qda.fit=qda(Direction~Lag1+Lag2,data=Smarket,subset=train)
qda.fit
qda.class=predict(qda.fit,Smarket.2005)$class
table(qda.class,Direction.2005)
mean(qda.class==Direction.2005)
# 60% accuracy - pretty good



# 4.6.5 K-Nearest Neighbours

# Load class library for knn() function
library(class)
# Unlike the previous functions where we get predictions by first fitting the model,
# knn() forms predictions with a single command
# knn() inputs:
# - A matrix containing predictors associated with training data (train.X)
# - A matrix containing predictors associated with testing data (text.X)
# - A vector containing class labels for training observations (train.Direction)
# - A value for K, the number of neighbours used by the classifier
# Use column bind cbind() to bind Lag1 and Lag2
train.X=cbind(Lag1,Lag2)[train,]
test.X=cbind(Lag1,Lag2)[!train,]
train.Direction=Direction[train]
# Seed random
set.seed(1)
# Get predictions
knn.pred=knn(train.X,test.X,train.Direction,k=1)
# Get accuracy
table(knn.pred,Direction.2005)
(83+43)/252
# 50% accuracy, not great
# Try again with different value for K
knn.pred=knn(train.X,test.X,train.Direction,k=3)
table(knn.pred,Direction.2005)
mean(knn.pred==Direction.2005)
# 53.6% accuracy is better but not by much
# Increasing K further does not improve results
# QDA gives best results



# 4.6.6 An Application to Caravan Insurance Data

# Apply KNN to Caravan dataset
# 85 predictors for 5,822 people
# Response variable: Purchase. Does the individual purchase an insurance policy?
dim(Caravan)
attach(Caravan)
summary(Purchase)
348/5822
# 5.97% Purchased insurance
# Use scale() function to scale the data
# Exclude col 86 (Purchase)
standardized.X=scale(Caravan[,-86])
var(Caravan[,1])
var(Caravan[,2])
var(standardized.X[,1])
var(standardized.X[,2])
# All columns now have standard deviation of 1 and a mean of zero
# Split observations into test set (1,000) and a training set (remainder)
# Set size of test set
test=1:1000
# Assign all but first 1000 observations to train set
train.X=standardized.X[-test,]
# Assign first 1000 observations to test set
test.X=standardized.X[test,]
# Vector of purchase responses for training
train.Y=Purchase[-test]
# Vector of purchase responses for testing
test.Y=Purchase[test]
set.seed(1)
# Fit KNN model on training data using K=1
knn.pred=knn(train.X,test.X,train.Y,k=1)
mean(test.Y!=knn.pred)
# Error rate 11.8%
mean(test.Y!="No")
# But positive response rate is about half that at 5.9%

table(knn.pred,test.Y)
9/(68+9)
# K=1 -- 11.7% of those predicted to buy actually did
# Try for K=3
knn.pred=knn(train.X,test.X,train.Y,k=3)
table(knn.pred,test.Y)
5/26
# K=3 -- 19.2%
# Try for K=5
knn.pred=knn(train.X,test.X,train.Y,k=5)
table(knn.pred,test.Y)
4/15
# K=5 -- 26.6%

# Fit logistic regression model to data
glm.fit=glm(Purchase~.,data=Caravan,family=binomial,subset=-test)
glm.probs=predict(glm.fit,Caravan[test,],type="response")
glm.pred=rep("No",1000)
glm.pred[glm.probs>.5]="Yes"
table(glm.pred,test.Y)
# Only 7 people are predicted to buy at a 0.5 threshold, and all 7 predictions were wrong
# Change threshold to 0.25
glm.pred=rep("No",1000)
glm.pred[glm.probs>.25]="Yes"
table(glm.pred,test.Y)
11/(22+11)
# With 0.25 threshold, 33 people predicted to buy, and 33% of them actually did
# Over 5 times better than just guessing at random

# Save history
savehistory()
# Quit
q()
