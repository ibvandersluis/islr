# ISLR Chapter 10: Unsupervised Learning

# Load history
loadhistory()
# Clear workspace
rm(list=ls())



# 10.4 Lab 1: Principal Components Analysis

# Objective: perform PCA on USArrests dataset, part of the base R package
states=row.names(USArrests)
states
# Inspect the variables
names(USArrests)
# Examine data. apply() lets us apply a function to each row or column in the dataset. '2' indicates columns
apply(USArrests,2,mean)
# Examine variances
apply(USArrests,2,var)

# Perform principal components analysis
# scale=TRUE gives variables standard deviation 1.
pr.out=prcomp(USArrests,scale=TRUE)
names(pr.out)
# center and scale are the means and standard deviations of variables used for scaling
pr.out$center
pr.out$scale
# rotation matrix gives principal component loading vectors
pr.out$rotation
# NOTE: in general there are min(n-1,p) informative principal components in a set of n observations and p variables
# The x matrix has the principal component score vector
dim(pr.out$x)

# Plot first two principal components
# scale=0 ensures the arrows are scaled to represent the loadings
biplot(pr.out,scale=0)

# This is a mirror image from Figure 10.1 in the book
# This is because principal components are unique up to a sign change
# We can replicate Figure 10.1 by doing:
pr.out$rotation=-pr.out$rotation
pr.out$x=-pr.out$x
biplot(pr.out,scale=0)

# Get standard deviation of each principal component
pr.out$sdev

# Get variance explained by squaring standard deviations
pr.var=pr.out$sdev^2
pr.var

# Compute proportion of variance explained by each principal component
# Divide variance explained (by each principal component) by total variance explained (by all principal components)
pve=pr.var/sum(pr.var)
pve
# First principal component explains 62%, second explains 24.7%, etc

# Plot PVE per component
plot(pve,xlab="Principal Component",ylab="Proportion of Variance Explained",ylim=c(0,1),type="b")
# Plot cumulative PVE
plot(cumsum(pve),xlab="Principal Component",ylab="Cumulative Proportion of Variance Explained",ylim=c(0,1),type="b")

# cumsum() computes cumulative sum of elements of a numeric vector. Example:
a=c(1,2,8,-3)
cumsum(a)



# 10.5 Lab 2: Clustering

# 10.5.1 K-Means Clustering

# Simple example, in which there truly are two distinct clusters in the data
set.seed(2)
x=matrix(rnorm(50*2),ncol=2)
x[1:25,1]=x[1:25,1]+3
x[1:25,2]=x[1:25,2]-4

# Perform K-means clustering with K=2
km.out=kmeans(x,2,nstart=20)
# Get cluster assignments
km.out$cluster

# Plot data with colour-coded observations
plot(x,col=(km.out$cluster+1),main="K-Means Clustering Results with K=2",xlab="",ylab="",pch=20,cex=2)

# Try with K=3. nstart indicates the number of initial cluster assignments for R to use. Best results returned
set.seed(4)
km.out=kmeans(x,3,nstart=20)
km.out
# Try with K=3 and nstart=1 vs nstart=20
set.seed(3)
km.out=kmeans(x,3,nstart=1)
km.out$tot.withinss
km.out=kmeans(x,3,nstart=20)
km.out$tot.withinss
# (results differed slightly)
# km.out$tot.withinss is total within-cluster sum of squares
# Individual within-cluster sum-of-squares in km.out$withinss
# It is strongly recommended always performing K-means with a large nstart value (i.e. 20-50)
# And don't forget to set.seed(n)



# 10.5.2 Hierarchical Clustering

# Use hclust() to use hierarchical clustering. dist() computes a 50x50 inter-observation Euclidian distance matrix
# Compute with complete linkage
hc.complete=hclust(dist(x),method="complete")
# Perform hierarchical clustering with average or single linkage
hc.average=hclust(dist(x),method="average")
hc.single=hclust(dist(x),method="single")

# Plot the dendrograms
par(mfrow=c(1,3))
plot(hc.complete,main="Complete Linkage",xlab="",sub="",cex=.9)
plot(hc.average,main="Average Linkage",xlab="",sub="",cex=.9)
plot(hc.single,main="Single Linkage",xlab="",sub="",cex=.9)

# Determine cluster labels for each observation associated with a given cut of the dendrogram
cutree(hc.complete,2)
cutree(hc.average,2)
cutree(hc.single,2)
# Note that single linkage as only one observation in its own cluster at clusters=2
# Try single linkage with 4 clusters (two singletons still remain)
cutree(hc.single,4)

# Use scale() to scale variables before performing clustering
xsc=scale(x)
par(mfrow=c(1,1))
plot(hclust(dist(xsc),method="complete"),main="Hierarchical Clustering with Scaled Features")

# Compute correlation-based distance with as.dist()
# This only makes sense for data with > 3 features
# Cluster a 3-dimensional dataset
x=matrix(rnorm(30*3),ncol=3)
dd=as.dist(1-cor(t(x)))
plot(hclust(dd,method="complete"),main="Complete Linkage with Correlation-Based Distance",xlab="",sub="")



# 10.6 Lab 3: NCI60 Data Example

# Use PCA and hierarchical clustering on NCI60 cancer cell line microarray data
# 6,830 gene expression measurements on 64 cancer cell lines
library(ISLR)
nci.labs=NCI60$labs
nci.data=NCI60$data
dim(nci.data)
nci.labs[1:4]
table(nci.labs)



# 10.6.1 PCA on the NCI60 Data

# Perform PCA after scaling data
pr.out=prcomp(nci.data,scale=TRUE)

# Create a function that assigns color to each element of a numeric vector
Cols=function(vec){
  cols=rainbow(length(unique(vec)))
  return(cols[as.numeric(as.factor(vec))])
}

# Plot first few principal component score vectors
par(mfrow=c(1,2))
plot(pr.out$x[,1:2],col=Cols(nci.labs),pch=19,xlab="Z1",ylab="Z2")
plot(pr.out$x[,c(1,3)],col=Cols(nci.labs),pch=19,xlab="Z1",ylab="Z3")

# Get summary of proportion of variance explained
summary(pr.out)

# Plot variance explained
plot(pr.out)

# Plot PVEs
pve=100*pr.out$sdev^2/sum(pr.out$sdev^2)
par(mfrow=c(1,2))
# Plot PVE of each principal component
plot(pve,type="o",ylab="PVE",xlab="Principal Component",col="blue")
# Plot cumulative PVE
plot(cumsum(pve),type="o",ylab="Cumulative PVE",xlab="Principal Component",col="brown3")



# 10.6.2 Clustering the Observations of the NCI60 Data

# Use hierarchical clustering to determine if observations cluster into distinct types of cancer

# Standardise variables to have mean 0 and sd 1
sd.data=scale(nci.data)

# Perform hierarchical clustering using complete, single, and average linkage
par(mfrow=c(1,3))
data.dist=dist(sd.data)
plot(hclust(data.dist),labels=nci.labs,main="Complete Linkage",xlab="",sub="",ylab="")
plot(hclust(data.dist,method="average"),labels=nci.labs,main="Average Linkage",xlab="",sub="",ylab="")
plot(hclust(data.dist,method="single"),labels=nci.labs,main="Single Linkage",xlab="",sub="",ylab="")
# We will use complete linkage for the rest of the analysis

# Cut the dendrogram at a height that will yield 4 clusters
hc.out=hclust(dist(sd.data))
hc.clusters=cutree(hc.out,4)
table(hc.clusters,nci.labs)

# Plot this cut
par(mfrow=c(1,1))
plot(hc.out,labels=nci.labs)
abline(h=139,col="red")

# Print output of hclust
hc.out

# Try k-means clustering with K=4
set.seed(2)
km.out=kmeans(sd.data,4,nstart=20)
km.clusters=km.out$cluster
table(km.clusters,hc.clusters)
# (Results vary somewhat)

# Perform hierarchical clustering on first few principal component score vectors
hc.out=hclust(dist(pr.out$x[,1:5]))
plot(hc.out,labels=nci.labs,main="Hier. Clust. on First Five Score Vectors")
table(cutree(hc.out,4),nci.labs)
# In this case, PCA denoised the data

# Save history
savehistory()
# Quit
q()
