# ISLR Chapter 7: Moving Beyond Linearity

# Load history
loadhistory()
# Clear workspace
rm(list=ls())



# 7.8 Lab: Non-Linear Modeling

library(ISLR)
attach(Wage)

# 7.8.1 Polynomial Regression and Step Functions

# Fit a linear model to predict wage using a 4-degree polynomial poly(age,4)
fit=lm(wage~poly(age,4),data=Wage)
coef(summary(fit))

# We can use poly() to get age, age^2, age^3, age^4 directly with raw=TRUE
# This option affects the coefficients but not the fitted values
fit2=lm(wage~poly(age,4,raw=T),data=Wage)
coef(summary(fit2))
# Which is equivalent to this:
fit2a=lm(wage~age+I(age^2)+I(age^3)+I(age^4))
coef(fit2a)
# Or this:
fit2b=lm(wage~cbind(age,age^2,age^3,age^4),data=Wage)
coef(fit2b)

# Create grid of values for age
agelims=range(age)
age.grid=seq(from=agelims[1],to=agelims[2])
# Call predict() specifying that we want standard errors with se=TRUE
preds=predict(fit,newdata=list(age=age.grid),se=TRUE)
se.bands=cbind(preds$fit+2*preds$se.fit,preds$fit-2*preds$se.fit)
# Plot data and add the fit from deg-4 polynomial
# Divide plot area into 2 tall subplots. mar and oma allow us to control the margins
par(mfrow=c(1,2),mar=c(4.5,4.5,1,1),oma=c(0,0,4,0))
plot(age,wage,xlim=agelims,cex=.5,col="darkgrey")
# Title spans both subplots
title("Degree-4 Polynomial",outer=T)
lines(age.grid,preds$fit,lwd=2,col="blue")
matlines(age.grid,se.bands,lwd=1,col="blue",lty=3)

# Fitted values are the same regardless of whether poly() produces orthogonal basis functions
preds2=predict(fit2,newdata=list(age=age.grid),se=TRUE)
max(abs(preds$fit-preds2$fit))
# I got 9.05e-12, book got 7.39e-13

# Must decide which degree of polynomial to use
# Use the analysis of variance anova() function to test H-null that M-1 sufficiently explains the data
# H-alt is that a more complex model M-2 is required
# M-1 and M-2 must be nested models (M-1's predictors must be a subset of M-2)
# Fit 5 models and sequentially compare the simpler model
fit.1=lm(wage~age,data=Wage)
fit.2=lm(wage~poly(age,2),data=Wage)
fit.3=lm(wage~poly(age,3),data=Wage)
fit.4=lm(wage~poly(age,4),data=Wage)
fit.5=lm(wage~poly(age,5),data=Wage)
anova(fit.1,fit.2,fit.3,fit.4,fit.5)

# Could have also used the orthogonal polynomials resulting from poly() to get p-values
coef(summary(fit.5))

# The square of the t-statistics are equal to the F-statistics from anova()
(-11.983)^2

# ANOVA works whether or not we use orthogonal polynomials
# Also works with other terms in the model
# We can compare these three models with anova()
fit.1=lm(wage~education+age,data=Wage)
fit.2=lm(wage~education+poly(age,2),data=Wage)
fit.3=lm(wage~education+poly(age,3),data=Wage)
anova(fit.1,fit.2,fit.3)
# We can also choose polynomial degree using cross-validation

# Let's predict whether someone earns more that $250k per year
# Similar to before, except we first make an appropriate response vector
# And apply glm() using family="binomial" to fit a polynomial logistic regression
fit=glm(I(wage>250)~poly(age,4),data=Wage,family=binomial)
# wage>250 evaluates to a boolean TRUE or FALSE, while glm() turns it into binary (T=1, F=0)
# Make predictions
preds=predict(fit,newdata=list(age=age.grid),se=T)
# See the book for the very mathy equation for this command (bottom of p. 291):
pfit=exp(preds$fit)/(1+exp(preds$fit))
se.bands.logit=cbind(preds$fit+2*preds$se.fit,preds$fit-2*preds$se.fit)
se.bands=exp(se.bands.logit)/(1+exp(se.bands.logit))

# Note: could have directly computed probabilities by selecting type="response" in predict()
preds=predict(fit,newdata=list(age=age.grid),type="response",se=T)
# But the confidence intervals would have ended up with negative probabilities

# Make other plot (often called a rug plot):
plot(age,I(wage>250),xlim=agelims,type="n",ylim=c(0,.2))
# jitter() jitters the age values so that observations of same age don't cover each other up
points(jitter(age),I((wage>250)/5),cex=.5,pch="|",col="darkgrey")
lines(age.grid,pfit,lwd=2,col="blue")
matlines(age.grid,se.bands,lwd=1,col="blue",lty=3)

# Fit a step function with cut(), which automatically picks cutpoints
table(cut(age,4))
fit=lm(wage~cut(age,4),data=Wage)
coef(summary(fit))



# 7.8.2 Splines

# If you try to install.packages('splines') you'll be told it's a base package. Just library() it
library(splines)
# bs() generates matrix of basis functions
# Cubic splines produced by default

# Fit wage to age:
fit=lm(wage~bs(age,knots=c(25,40,60)),data=Wage)
pred=predict(fit,newdata=list(age=age.grid),se=T)
plot(age,wage,col="gray")
lines(age.grid,pred$fit,lwd=2)
lines(age.grid,pred$fit+2*pred$se,lty="dashed")
lines(age.grid,pred$fit-2*pred$se,lty="dashed")

# Use df option to produce a spline with knots at uniform quantiles (25th, 50th, 75th percentiles)
dim(bs(age,knots=c(25,40,60)))
dim(bs(age,df=6))
attr(bs(age,df=6),"knots")

# Use ns() to fit a natural spline
fit2=lm(wage~ns(age,df=4),data=Wage)
pred2=predict(fit2,newdata=list(age=age.grid),se=T)
lines(age.grid,pred2$fit,col="red",lwd=2)

# Fit a smoothing spline
plot(age,wage,xlim=agelims,cex=.5,col="darkgrey")
title("Smoothing Spline")
fit=smooth.spline(age,wage,df=16)
fit2=smooth.spline(age,wage,cv=TRUE)
# I got a warning that cross validation with non-unique x values seems doubful
# I kept going, it was fine
fit2$df
lines(fit,col="red",lwd=2)
lines(fit2,col="blue",lwd=2)
legend("topright",legend=c("16 DF","6.8 DF"),col=c("red","blue"),lty=1,lwd=2,cex=.8)

# Perform local regression with loess() function
plot(age,wage,xlim=agelims,cex=.5,col="darkgrey")
title("Local Regression")
fit=loess(wage~age,span=.2,data=Wage)
fit2=loess(wage~age,span=.5,data=Wage)
lines(age.grid,predict(fit,data.frame(age=age.grid)),col="red",lwd=2)
lines(age.grid,predict(fit2,data.frame(age=age.grid)),col="blue",lwd=2)
legend("topright",legend=c("Span=0.2","Span=0.5"),col=c("red","blue"),lty=1,lwd=2,cex=.8)
# The span indicates how much of the observations are in each neighbourhood
# As we can see, the larger the span is, the smoother the fit



# 7.8.3 GAMs

# Fit GAM to predict wage using year and age, treating education as a qualitative predictor
gam1=lm(wage~ns(year,4)+ns(age,5)+education,data=Wage)
# Fit model using smoothing splines instead of natural splines
# Need gam library. If not installed, install.packages('gam')
library(gam)
# s() indicates we want a smoothing spline
# We specify 4 deg of freedom for year and 5 for age
# Qualitative variables can be left as they are, as it will be converted into dummy variables
# Use gam() to fit the function
gam.m3=gam(wage~s(year,4)+s(age,5)+education,data=Wage)
# I got a warning for non-list contrasts argument ignored and continued

# Plot the figure
par(mfrow=c(1,3))
plot(gam.m3,se=TRUE,col="blue")

# We can still use plot.gam() on gam1 even though it is an lm
plot.Gam(gam1,se=TRUE,col="red")
# Error resolved, use plot.Gam not plot.gam

# We can use ANOVA tests to determine which model is best
# M-1: a GAM that excludes year
# M-2: a GAM that uses a linear function of year
# M-3: a GAM that uses a spline function of year
gam.m1=gam(wage~s(age,5)+education,data=Wage)
gam.m2=gam(wage~year+s(age,5)+education,data=Wage)
anova(gam.m1,gam.m2,gam.m3,test="F")
# M-2 is the preferred model

summary(gam.m3)

# Make predictions
preds=predict(gam.m2,newdata=Wage)

# We can use local regression as building blocks in a GAM with lo()
gam.lo=gam(wage~s(year,df=4)+lo(age,span=0.7)+education,data=Wage)
# Remember to use plot.Gam not plot.gam
plot.Gam(gam.lo,se=TRUE,col="green")
# We can also use lo() to create interactions before calling the gam() function
gam.lo.i=gam(wage~lo(year,age,span=0.5)+education,data=Wage)
# I got warnings but moving on
# This fits a two-term model in which the first term is an interaction between year and age

# To plot resulting 2D surface, install akima
install.packages('akima')
library(akima)
plot(gam.lo.i)

# To fit logistic regression GAM, use I() to make binary response variable with family=binomial
gam.lr=gam(I(wage>250)~year+s(age,df=5)+education,family=binomial,data=Wage)
par(mfrow=c(1,3))
plot(gam.lr,se=T,col="green")

# There are no high earners in the <HS category
table(education,I(wage>250))
# So we can remove this category from the GAM and run again
gam.lr.s=gam(I(wage>250)~year+s(age,df=5)+education,family=binomial,data=Wage,subset=(education!="1. < HS Grad"))
plot(gam.lr.s,se=T,col="green")

# Save history
savehistory()
# Quit
q()
