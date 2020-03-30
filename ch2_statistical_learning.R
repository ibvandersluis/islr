# ISLR Chapter 2: Statistical Learning

# Clear workspace
rm(list=ls())



# 2.3.1 Basic Commands

# Assign values to x
x <- c(1,3,2,5)
# Print x
x
# Assign values to x and y using =
x = c(1,6,2)
y = c(1,4,3)
# Get lengths of x and y
length(x)
length(y)
# Add x and y
x+y

# List workspace
ls()
# Clear workspace
rm(list=ls())

# Assign matrix to x
x = matrix(data=c(1,2,3,4), nrow=2, ncol=2)
# Print x
x
# Remake matrix with byrow=TRUE
matrix(c(1,2,3,4),2,2,byrow=TRUE)
# Math on matrix x (performs operation on each object)
sqrt(x)
x^2

# Assign x 50 numbers with normal distribution. Default mean=0 sd=1
x=rnorm(50)
# Assign y similar numbers but changes distribution settings
y=x+rnorm(50,mean=50,sd=.1)
# Calculatre correlation of x and y
cor(x,y)

# Set the seed to 1303
set.seed(1303)
# Test for consistency
rnorm(50)

# Set the seed to 3
set.seed(3)
# Assign new normal distribution to y
y=rnorm(100)
# Get mean of y
mean(y)
# Get variance of y
var(y)
# Get standard deviation of y (two methods)
sqrt(var(y))
sd(y)



# 2.3.2 Graphics

# Assign normal distributions to x and y
x=rnorm(100)
y=rnorm(100)
# Plot x and y
plot(x,y)
# Plot x and y with labels
plot(x,y,xlab="this is the x-axis",ylab="this is the y-axis",main="Plot of X vs Y")
# Save as .pdf
# Create empty .pdf file
pdf("Figure.pdf")
# Make a plot for the .pdf
plot(x,y,col="green")
# Tell R we're done making the .pdf
dev.off()

# Make sequence 1-10 (inclusive) and assign to x
x=seq(1,10)
# Print x
x
# Alternative method
x=1:10
# Print x again
x
# Assign x a sequence of 50 evely spaced numbers from -pi to pi
x=seq(-pi,pi,length=50)

# Make a contour (topo map) plot. Arguments: vectors x and y, and a matrix for z
y=x
f=outer(x,y,function(x,y)cos(y)/(1+x^2))
contour(x,y,f)
contour(x,y,f,nlevels=45,add=T)
fa=(f-t(f))/2
contour(x,y,fa,nlevels=15)
# Make an image (colour-coded plot)
image(x,y,fa)
# Make a 3D plot, at different angles. Theta is angle on a 2D plane, phi is elevation
persp(x,y,fa)
persp(x,y,fa,theta=30)
persp(x,y,fa,theta=30,phi=20)
persp(x,y,fa,theta=30,phi=70)
persp(x,y,fa,theta=30,phi=40)



# 2.3.3 Indexing Data

# Store data in a matrix in A
A=matrix(1:16,4,4)
# Print A
A
# Select cell from row 2, column 3
A[2,3]

# Select portions of matrix
# Select rows 1 and 3, and cols 2 and 4
A[c(1,3), c(2,4)]
# Select rows 1-3, and cols 2-4
A[1:3,2:4]
# Select rows 1-2 and all cols
A[1:2,]
# Select all rows and cols 1-2
A[,1:2]
# Use - sign to exclude that section
# Exclude rows 1 and 3
A[-c(1,3),]
# Exclude rows 1 and 3, and cols 1, 3 and 4
A[-c(1,3),-c(1,3,4)]

# Print dimensions of A
dim(A)



# 2.3.4 Loading Data

# Load Auto.data into R, store as data frame
Auto=read.table("Auto.data")
# View data in spreadsheet like window
fix(Auto)
# Load data, correctly sorting out header ('header=T') and missing values ('na.strings="?"')
Auto=read.table("Auto.data",header=T,na.strings="?")
fix(Auto)

# Load from CSV
Auto=read.csv("Auto.csv",header=T,na.strings="?")
fix(Auto)
dim(Auto)
# Show first 4 rows of file
Auto[1:4,]
# Omit rows with absent values
Auto=na.omit(Auto)
dim(Auto)
# View variable names
names(Auto)



# 2.3.5 Additional Graphical and Numerical Summaries

# Tell R to plot cylinders, mpg (will produce error)
plot(cylinders, mpg)
# Till R which dataset to look in for the variable name
plot(Auto$cylinders, Auto$mpg)
# Alternatively, attach R to the dataset
attach(Auto)
plot(cylinders, mpg)
# Convert cylinders from quantitative to qualitative
cylinders=as.factor(cylinders)
# plot() automatically uses boxplots if x-axis is categorical
# Here are some plot functions with various options
plot(cylinders, mpg)
plot(cylinders, mpg, col="red")
plot(cylinders, mpg, col="red", varwidth=T)
plot(cylinders, mpg, col="red", varwidth=T, horizontal=T)
plot(cylinders, mpg, col="red", varwidth=T, xlab="cylinders", ylab="MPG")

# Plot a histogram (with some options)
hist(mpg)
hist(mpg,col=2)
hist(mpg,col=2,breaks=15)

# Create a scatterplot matrix with pairs()
pairs(Auto)
# Create a scatterplot with only a subset of variables
pairs(~ mpg + displacement + horsepower + weight + acceleration, Auto)

# identify() allows us to interact with a plot to get values on specific data points
# 3 args: x-axis, y-axis, the variable to be examined
plot(horsepower,mpg)
identify(horsepower,mpg,name)
# esc to exit identify()
# The outputs are the rows for the selected points

# Get numerical summary of variables in a dataset
summary(Auto)
# Or just one variable
summary(mpg)

# Save commands from session
savehistory()
# Quit
q()
