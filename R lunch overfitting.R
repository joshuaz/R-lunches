# Overfitting (Ridge and Lasso Regressions)
# “Everything should be made simple as possible, but not simpler – Albert Einstein”
rm(list=ls())

#Sources:
#https://www.datacamp.com/community/tutorials/tutorial-ridge-lasso-elastic-net
#https://towardsdatascience.com/ridge-regression-for-better-usage-2f19b3a202db
#https://www.analyticsvidhya.com/blog/2017/06/a-comprehensive-guide-for-linear-ridge-and-lasso-regression/
#https://www.youtube.com/watch?v=_3xMSbIde2I
#https://www.tutorialspoint.com/statistics/adjusted_r_squared.htm
#https://blog.minitab.com/blog/adventures-in-statistics-2/multiple-regession-analysis-use-adjusted-r-squared-and-predicted-r-squared-to-include-the-correct-number-of-variables
#https://statisticsbyjim.com/regression/interpret-adjusted-r-squared-predicted-r-squared-regression/


#Ridge Regression - Shrinks coefficients to non-zero values to prevent overfit, but keeps all variables
#Lasso regression - shrinks regression coefficients, with some shrunk to zero. Thus, it also helps with feature selection
#Elastic Net regression - mix of ridge and lasso


#Install packaages
install.packages("caTools")
library(caTools)
install.packages("Metrics")
library(Metrics)
library(caret)
library(glmnet)
library(mlbench)
library(psych)

#Generate Random data
set.seed(42)
X_data <- 2 + 2 * rnorm(n = 30, mean = 0, sd = 1)
Y_data <- 3 - 3 * rnorm(n = 30, mean = 0, sd = 1)
plot(X_data, Y_data)
df <- data.frame("X" = X_data, "Y" = Y_data)
View(df)

# Split up the data using subset
library(caTools)
split = sample.split(df, SplitRatio = 0.5)
train = subset(df, split==TRUE)
test = subset(df, split==FALSE)

#Fit very high order polynomial to training data
poly_model1 <- lm(Y ~ poly(X,15, raw = TRUE), data=train)
predicted_values <- predict(poly_model1, train)
rmse(train$Y,predicted_values)

predicted_values2 <- predict(poly_model1, test)
rmse(test$Y,predicted_values2)
#As you can see, although it fits the training data very well (acounts for all the random noise), it poorly fits the testing data.


#Boston Housing Data
data("BostonHousing")
data <- BostonHousing

?BostonHousing
# The original data are 506 observations on 14 variables, medv being the target variable:

# crim	  per capita crime rate by town
# zn	    proportion of residential land zoned for lots over 25,000 sq.ft
# indus	  proportion of non-retail business acres per town
# chas	  Charles River dummy variable (= 1 if tract bounds river; 0 otherwise)
# nox	    nitric oxides concentration (parts per 10 million)
# rm	    average number of rooms per dwelling
# age	    proportion of owner-occupied units built prior to 1940
# dis	    weighted distances to five Boston employment centres
# rad	    index of accessibility to radial highways
# tax	    full-value property-tax rate per USD 10,000
# ptratio	pupil-teacher ratio by town
# b	      1000(B - 0.63)^2 where B is the proportion of blacks by town
# lstat	  percentage of lower status of the population
# medv	  median value of owner-occupied homes in USD 1000's



#Adjusted R squared

#Problems with R^2
#Problem 1: Every time you add a predictor to a model, the R-squared increases, even if due to chance alone. It never decreases. Consequently, a model with more terms may appear to have a better fit simply because it has more terms.
#Problem 2: If a model has too many predictors and higher order polynomials, it begins to model the random noise in the data. This condition is known as overfitting the model and it produces misleadingly high R-squared values and a lessened ability to make predictions.

#R^2 equation: 1-Sum Squared Regression Error/Sum Squared Total Error
#adjusted R^2 equation: 1- [(1-R^2)/N-K-1]
#As you can see, it "punishes" adding additional weights (K term)
#The adjusted R-squared is a modified version of R-squared that has been adjusted for the number of predictors in the model. The adjusted R-squared increases only if the new term improves the model more than would be expected by chance. It decreases when a predictor improves the model by less than expected by chance. The adjusted R-squared can be negative, but it’s usually not.  It is always lower than the R-squared.

#Example
try1 <- lm(medv ~ crim, data=data)
summary(try1)
#R^2=0.1508
#adjusted R^2=0.1491

try2 <- lm(medv ~ crim + rm, data=data)
summary(try2)
#R^2=0.542
#adjusted R^2=0.5401

try3 <- lm(medv ~ crim + rm + nox + age + dis + rad + tax, data=data)
summary(try3)
#R^2=0.6078
#adjusted R^2=0.6023

try4 <- lm(medv ~ ., data=data)
summary(try4)
#R^2=0.7406
#adjusted R^2=0.7338

data$uselessvariable <- rnorm(n = 506, mean = 0, sd = 1)
try6 <- lm(medv ~ ., data=data)
summary(try6)
#R^2=0.7406
#adjusted R^2=0.7332

#As you can see, adding a useless variable actually made adjusted R squared go down


#Lasso/Ridge/ElasticNet

#Definitions:
#alpha is a tuning parameter that controls how much we want to regularize the data
#For a linear model, regularization is typically achieved by constraining the weights of the model.
data <- BostonHousing

# Data
data("BostonHousing")
data <- BostonHousing

# Data Partition
set.seed(222)
ind <- sample(2, nrow(data), replace = T, prob = c(0.7, 0.3))
train <- data[ind==1,]
test <- data[ind==2,]

# Custom Control Parameters
custom <- trainControl(method = "repeatedcv",
                       number = 10,
                       repeats = 5,
                       verboseIter = T)
#In 10-fold cross-validation, training data is broken into 10 parts
#model is made from 9 parts and 1 part is used for error estimation
#This is repeated 10 times with a different part used for error estimation.
#verboseIter is a method to see R's computing.

# Linear Model
set.seed(1234)
lm <- train(medv ~ .,
            train,
            method = 'lm',
            trControl = custom)

# Results
lm
lm$results
summary(lm)
#MAE=mean absolute error
plot(lm$finalModel)

## Ridge Regression
# It shrinks the parameters, therefore it is mostly used to prevent multicollinearity.
# It reduces the model complexity by coefficient shrinkage.
# It uses L2 regularization technique.
# Tries to shrink coefficients but keeps all variables in the model

#Ridge Regression (also called Tikhonov regularization) is a regularized version of Linear Regression: a regularization term equal to is added to the cost function. 
#This forces the learning algorithm to not only fit the data but also keep the model weights as small as possible. Note that the regularization term should only be added to the cost function during training. 
#Once the model is trained, you want to evaluate the model’s performance using the unregularized performance measure.
#The hyperparameter alpha controls how much we want to regularize the model. If alpha = 0, then ridge regression is just a linear regression. If it is very large, then all weights end up close to zero.

set.seed(1234)
ridge <- train(medv ~ .,
               train,
               method = 'glmnet',
               tuneGrid = expand.grid(alpha = 0,
                                      lambda = seq(0.0001, 1, length=5)),
               trControl = custom)
#lambda is basically strength of penalty on coefficients (higher lambda = more penalty)
#It helps to make coefficients that are not contributing close to zero

# Plot Results
ridge
plot(ridge)
plot(ridge$finalModel, xvar = "lambda", label = T)
plot(ridge$finalModel, xvar = 'dev', label=T)
plot(varImp(ridge, scale=T))

# Lasso Regression (Least Absolute Shrinkage Selector Operator)
#It uses L1 regularization technique
#It is generally used when we have more number of features, because it automatically does feature selection.
#If there is multicollinearity, it selects only one of the variables and ignores others.
#tends to completely eliminate the weights of the least important features
set.seed(1234)
lasso <- train(medv ~ .,
               train,
               method = 'glmnet',
               tuneGrid = expand.grid(alpha=1,
                                      lambda = seq(0.0001, 1, length = 5)),
               trControl = custom)

# Plot Results
lasso
plot(lasso)
plot(lasso$finalModel, xvar = 'lambda', label=T)

# Elastic Net Regression
#combination of ridge and lasso
set.seed(1234)
en <- train(medv ~ .,
            train,
            method = 'glmnet',
            tuneGrid = expand.grid(alpha = seq(0,1,length=10),
                                   lambda = seq(0.0001,.2,length=5)),
            trControl = custom)


# Plot Results
plot(en)
plot(en$finalModel, xvar = 'lambda', label=T)
plot(en$finalModel, xvar = 'dev', label=T)
plot(varImp(en))

#
#So when should you use Linear Regression, Ridge, Lasso, or Elastic Net? It is almost always preferable to have at least a little bit of regularization, so generally you should avoid plain Linear Regression.
#Ridge is a good default,
#if you suspect that only a few features are actually useful, you should prefer Lasso or Elastic Net since they tend to reduce the useless features’ weights down to zero.
#In general, Elastic Net is preferred over Lasso since Lasso may behave erratically when the number of features is greater than the number of training instances or when several features are strongly correlated.

# Compare Models
model_list <- list(LinearModel = lm, Ridge = ridge, Lasso = lasso, ElasticNet = en)
res <- resamples(model_list)
summary(res)
bwplot(res)
xyplot(res, metric = 'RMSE')

# Best Model
en$bestTune
#because alpha is closer to 0 than 1, it is more based on a ridge than lasso model
best <- en$finalModel
coef(best, s = en$bestTune$lambda)

# Save Final Model for Later Use
saveRDS(en, "final_model.rds")
fm <- readRDS("final_model.rds")
print(fm)

# Prediction
p1 <- predict(fm, train)
sqrt(mean((train$medv-p1)^2))

p2 <- predict(fm, test)
sqrt(mean((test$medv-p2)^2))