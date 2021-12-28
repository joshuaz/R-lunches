
###################################
##     Predictive Modeling -     ##
##        Regression             ##
##      R Learning Lunch         ##
###################################

# use this to clear your working environment 
rm(list=ls())

#set working directory

#install packages we need.
install.packages("gradDescent")
library(gradDescent)

# Read in data and explore
wine = read.csv("wine.csv")
str(wine)
summary(wine)
head(wine)
# AGST = average growing season temperature

##########Linear Regression##########

plot(wine$AGST, wine$Price)
# Linear Regression (one variable)
model1 = lm(Price ~ AGST, data=wine)
## model1 = name of model
## lm = function
## price = dependent variable
## AGST = independent variable

summary(model1)
#y=m*x+b
#m=0.6351
#b=-3.4178 
#r=0.435
plot(wine$AGST, wine$Price)
abline(model1)

# Sum of Squared Errors
model1$residuals
SSE = sum(model1$residuals^2)
SSE

# Linear Regression (two variables)
model2 = lm(Price ~ AGST + HarvestRain, data=wine)
summary(model2)

# Sum of Squared Errors
SSE = sum(model2$residuals^2)
SSE

# Linear Regression (all variables)
model3 = lm(Price ~ AGST + HarvestRain + WinterRain + Age + FrancePop, data=wine)
summary(model3)

# Sum of Squared Errors
SSE = sum(model3$residuals^2)
SSE

# Remove FrancePop
model4 = lm(Price ~ AGST + HarvestRain + WinterRain + Age, data=wine)
summary(model4)

# Correlations
cor(wine$WinterRain, wine$Price)
cor(wine$Age, wine$FrancePop)
cor(wine)

# Remove Age and FrancePop
model5 = lm(Price ~ AGST + HarvestRain + WinterRain, data=wine)
summary(model5)

##########Normal Equation##########
# #Given a matrix equation, 
# #X*theta=y
# #the normal equation is that which minimizes the sum of the square differences between the
# #left and right sides:
# #(X^(T) * X)theta = X^(T)y.
# #Here, X^(T)X is a normal matrix.
# 
# #Normal Equation = theta = (X^(T) * X)^(-1) * X^(T) * y
# #(X^(T) * X)^(-1) is the inverse of matrix (X^(T) * X)
# #theta is list of coefficients
# 
# #Helpful video: https://www.youtube.com/watch?v=B-Ks01zR4HY

y <- wine$Price
#X0 is just a matrix of ones to account for X0 in y = aX0 + bX1 + cX2 + ... 
x0 <- matrix(1, 25, 1)
x1 <- wine$WinterRain
x2 <- wine$AGST
x3 <- wine$HarvestRain
x4 <- wine$Age
x5 <- wine$FrancePop
Y <- as.matrix(y)
X <- as.matrix(cbind(x0,x1,x2,x3,x4,x5))
beta = solve(t(X) %*% X) %*% (t(X) %*% Y) ;
beta

##########Gradient Descent (with one independent variable)##########
# #Gradient descent is an optimization algorithm that finds the optimal coefficients (a,b) that 
# #reduces  prediction error.
# 
# #At a theoretical level, gradient descent is an algorithm that minimizes functions. 
# #Given a function defined by a set of parameters, 
# #gradient descent starts with an initial set of parameter values and 
# #iteratively moves toward a set of parameter values that minimize the function. 
# #This iterative minimization is achieved using calculus, 
# #taking steps in the negative direction of the function gradient.
# 
# ##Intuition
# #Think of a large bowl like what you would eat cereal out of or store fruit in. 
# #This bowl is a plot of the cost function (f).
# #A random position on the surface of the bowl is the cost of the current values of the 
# #coefficients (cost).
# #The bottom of the bowl is the cost of the best set of coefficients, the minimum of the function.
# #The goal is to continue to try different values for the coefficients, evaluate their cost 
# #and select new coefficients that have a slightly better (lower) cost.
# #Repeating this process enough times will lead to the bottom of the bowl 
# #and you will know the values of the coefficients that result in the minimum cost.
# 
# ##Lets now go step by step to understand the Gradient Descent algorithm:
# #Step 0: standardize the data as it makes the optimization process faster.
# #Step 1: Initialize the coefficients(a & b) with random values and calculate Error (SSE)
# #Step 2: Calculate the gradient i.e. change in SSE when the coefficients (a & b) are changed by a very small value from their original randomly initialized value. This helps us move the values of a & b in the direction in which SSE is minimized.
# #Step 3: Adjust the coefficients with the gradients to reach the optimal values where SSE is minimized
# #Step 4: Use the new coefficients for prediction and to calculate the new SSE
# #Step 5: Repeat steps 2 and 3 till further adjustments to coefficients doesn't significantly reduce the Error
# 
# ##Another explanation
# #  1) The procedure starts off with initial values for the coefficient or coefficients for the 
# #  function. These could be 0.0 or a small random value.
# #  1a) coefficient = 0.0
# #  2) The cost of the coefficients is evaluated by plugging them into the function and calculating the cost.
# #  2a) cost = f(coefficient) or cost = evaluate(f(coefficient))
# #  3) The derivative of the cost is calculated. The derivative is a concept from calculus and 
# #  refers to the slope of the function at a given point. We need to know the slope so that we know the direction (sign) 
# #  to move the coefficient values in order to get a lower cost on the next iteration.
# #  3a) delta = derivative(cost)
# #  4) Now that we know from the derivative which direction is downhill, we can now update the coefficient values. 
# #  A learning rate parameter (alpha) must be specified that controls how much the coefficients can change on each update.
# #  4a) coefficient = coefficient - (alpha * delta)
# #  5) This process is repeated until the cost of the coefficients (cost) is 0.0 or close enough to zero to be good enough.
# 
# ##helpful websites:
# #https://machinelearningmastery.com/gradient-descent-for-machine-learning/
# #https://www.kdnuggets.com/2017/04/simple-understand-gradient-descent-algorithm.html
# #https://www.youtube.com/watch?v=LN0PLnDpGN4
# #https://www.youtube.com/watch?v=kWq2k1gPyBs
# #https://www.youtube.com/watch?v=7LqYTTwuu0k (Especially 4:44 through 7:47)
# #https://www.r-bloggers.com/regression-via-gradient-descent-in-r/

# #Why do we use it instead of just OLS?
# Analytical solutions are strongly connected to the model,
# so implementing them can be inefficient if you plan to generalize/change your models
# in the future. They are sometimes less efficient then their numerical approximations,
# and sometimes there are simply harder to implement.

# To sum: use gradient descent over an analytical solution if:
# you are considering changes in the model, generalizations, 
# OR adding some more complex terms/regularization/modifications
# you need a generic method because you do not know much about the future of the code and
# the model (you are only one of the developers)

# analytical solution is more expensive computationaly, and you need efficiency
# analytical solution requires more memory, which you do not have
# analytical solution is hard to implement and you need easy, simple code

#Revisit original model1
model1 = lm(Price ~ AGST, data=wine)
summary(model1)

# create the x- matrix of explanatory variables
x <- as.matrix(cbind(x0,x2))

# create the y-matrix of dependent variables
y <- as.matrix(y)
m <- nrow(Y)
m

# Implement feature scaling. This makes it move more quickly 
# (https://www.youtube.com/watch?v=e1nTgoDI_m8).
# The algorithm will reach the minimum cost faster if the shape of the cost function is not skewed and distorted.
x.scaled <- x
x.scaled[,2] <- (x[,2] - mean(x[,2]))/sd(x[,2])

# analytical results with matrix algebra
solve(t(x)%*%x)%*%t(x)%*%y # w/o feature scaling
solve(t(x.scaled)%*%x.scaled)%*%t(x.scaled)%*%y # w/ feature scaling

# define the gradient function dJ/dtheata: 1/m * (h(x)-y))*x where h(x) = x*theta
# in matrix form this is as follows:
grad <- function(x, y, theta) {
  gradient <- (1/m)* (t(x) %*% ((x %*% t(theta)) - y))
  return(t(gradient))
}

# define gradient descent update algorithm
grad.descent <- function(x, maxit){
  theta <- matrix(c(0, 0), nrow=1) # Initialize the parameters


#The learningRate variable controls how large of a step we take downhill during each iteration. If we take too large of a step, we may step over the minimum. However, if we take small steps, it will require many iterations to arrive at the minimum. 
#If alphaα is too small: slow convergence.
#If alpha is too large: ￼may not decrease on every iteration and thus may not converge.
 
  alpha = .05 # set learning rate/tuning parameter
  for (i in 1:maxit) {
    theta <- theta - alpha  * grad(x, y, theta)   
  }
  return(theta)
}

# results without feature scaling
print(grad.descent(x,1000))

# results with feature scaling
print(grad.descent(x.scaled,1000))

#Trade-Offs

##Gradient Descent
#Need to choose a (tuning parameter).
#Needs many iterations.
#Works well even when n is large.

##Normal Equation
#No need to choose a (tuning parameter)
#Do not need to iterate.
#Need to compute (A^(T)A)^(-1)
#Slow if n is very large.


# Predicions

# Read in test set
wineTest = read.csv("wine_test.csv")
str(wineTest)

# Make test set predictions
predictTest = predict(model4, newdata=wineTest)
predictTest

# Compute R-squared
SSE = sum((wineTest$Price - predictTest)^2)
SST = sum((wineTest$Price - mean(wine$Price))^2)
1 - SSE/SST

