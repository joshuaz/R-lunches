###############################################
##             Decision Trees                ##
##        R Learning Lunch June 30th         ##
###############################################

###############################################
##     Code Pulled from 
## An Introduction to Statistical Learning   ##
###############################################

######################################
## This next line of code will clear #
## your working environment to you   #
## can start "fresh"                 #
rm(list=ls())
######################################

## Install and Load Packages ##
install.packages("ISLR")
install.packages("tree")
install.packages("rpart")
install.packages("gbm")
install.packages('rpart.plot')
install.packages('RColorBrewer')
library(rpart.plot)
library(RColorBrewer)
library(tree)
library(ISLR)
library(gbm)

###########################
####Load and Explore the Data####
###########################
attach(Carseats)

#Explore the Data
dim(Carseats)
summary(Carseats)
head(Carseats)
str(Carseats)
  
#Create new variable called "High"
  High=ifelse(Sales<=8,"Low","High")
    #Add "High" to the original dataset
    Carseats=data.frame(Carseats,High)

# Create a classification tree with High as the outcome (dependent variable)
#We include everything except "Sales." We use a minus "-" sign to do this. 
tree.carseats=tree(High~.-Sales,Carseats)

#Summary returns variables used, number of nodes, residual mean deviance, and misclassification error rate
summary(tree.carseats)

png("tree.png", width=1000, height=800, antialias="cleartype")
  plot(tree.carseats)
    text(tree.carseats,pretty=0)
      dev.off()

#A nicer plot using rpart.plot() http://www.milbo.org/rpart-plot/prp.pdf

#Each node box displays the classification, the probability 
#of each class at that node (i.e. the probability of the class 
#conditioned on the node) and the percentage of observations used 
#at that node.
png("Pretty Tree.png", width=1000, height=800, antialias="cleartype") 
  tree.carseats.rpart=rpart(High~.-Sales,data=Carseats)
   rpart.plot(tree.carseats.rpart)
     rpart.plot(tree.carseats.rpart, # middle graph
                 extra=104, box.palette="GnBu",
                 branch.lty=3, nn=TRUE)
      dev.off()

### Estimating the test error
### We split the observations into a training set and a test
### set, build the tree using the training set, and evaluate its performance.

## We break the dataset into 2 sets: the training set and the testing set. 
## We build the model based on the training dataset. We then apply the model to the testing 
## dataset and save the predicted values. We then compare the predicted values 
## from the testing dataset to the actual values of the testing set.

set.seed(2)
# Randomly sample  half of the observations for training and the rest for test
train=sample(1:nrow(Carseats), 200)
  Carseats.test=Carseats[-train,]

  tree.carseats=tree(High~.-Sales,Carseats,subset=train)
    tree.pred=predict(tree.carseats,Carseats.test,type="class")
      #type="class" instructs R to return the actual class prediction
      #print the results of tree.pred
        tree.pred
          High.test=High[-train]
            table(tree.pred,High.test)
              # Baseline Accuracy
              (89+57)/200

#####Cross Validation####   
            
## The function cv.tree() performs cross-validation in order to
## determine the optimal level of tree complexity; cost complexity pruning
## is used in order to select a sequence of trees for consideration. 
set.seed(3)
cv.carseats=cv.tree(tree.carseats,FUN=prune.misclass)
  #FUN=prune.misclass indicates that we want the classification error rate 
  #to guide the cross-validation and pruning process,
  #rather than deviance (the default)

names(cv.carseats)
  # size = the number of terminal nodes of each tree considered
  # dev = the cross validation error rate
  # k which corresponds to alpha - the cost complexity parameter
  # a small alpha corresponds to a larger tree. 
cv.carseats

#Let's Plot this#

plot(cv.carseats$size,cv.carseats$dev,type="b" , main = "Number of Terminal Nodes by the Cross Validation Error")
 text(9, 60, "Ideal Tree Size", pos=3)
  arrows(9, 59, 9, 52)    

# We decide on 9 terminal nodes (the point at which the cross validation error is minimized)
  
   dev.off()
   
#Pruning
  prune.carseats=prune.misclass(tree.carseats,best=9)


    png("pruned tree.png", width=1000, height=800, antialias="cleartype")
      plot(prune.carseats)
        text(prune.carseats,pretty=0)
         dev.off()
         
tree.pred=predict(prune.carseats,Carseats.test,type="class")
  table(tree.pred,High.test)
    #New Accuracy | An improvement from  .715
    (94+60)/200
## If we increase the value of best, we obtain 
## a larger pruned tree with lower
## classification accuracy
prune.carseats=prune.misclass(tree.carseats,best=15)

 plot(prune.carseats)
  text(prune.carseats,pretty=0)
   tree.pred=predict(prune.carseats,Carseats.test,type="class")
    table(tree.pred,High.test)
# Accuracy with 15 nodes | An improvement from the original (.715), 
  #but not as good as 9 nodes | Why? 15 nodes overfits
(86+62)/200

    
# Fitting Regression Trees
# First, we create a training set, and fit the tree to the training data.
library(MASS)
set.seed(1)

# Let's explore the Boston data ##
  ?Boston
    head(Boston)
      tail(Boston)
        str(Boston)

train = sample(1:nrow(Boston), nrow(Boston)/2)
  tree.boston=tree(medv~.,Boston,subset=train)
    summary(tree.boston)

plot(tree.boston)
  text(tree.boston,pretty=0)
  
# Even though there are eight nodes, only three variables were used. 
  
#Cross Validation
# Uses function "cv.tree()" which runs a K-fold cross-validation 
# experiment to find the deviance or number of misclassifications 
# as a function of the cost-complexity parameter k.
  cv.boston=cv.tree(tree.boston)
 
  names(cv.boston)
  cv.boston
   plot(cv.boston$size,cv.boston$dev,type="b" , main = "Number of Terminal Nodes by the Cross Validation Error")
    text(7, 11000, "Ideal Tree Size", pos=3)
     arrows(8, 10000, 8, 6500) 
  
# We decide on 8 terminal nodes (the point at which the 
# cross validation error is minimized)
  dev.off()

 
#In this case, the most complex tree is selected by cross-validation. However,
# if we wish to prune the tree, we could do so as follows, using the
        
prune.boston=prune.tree(tree.boston,best=5)
  plot(prune.boston)
    text(prune.boston,pretty=0)   

#In keeping with the cross-validation results, we use the unpruned tree to
#make predictions on the test set.
    
yhat=predict(tree.boston,newdata=Boston[-train,])
  boston.test=Boston[-train,"medv"]
    plot(yhat,boston.test)
      abline(0,1) 
        #This function adds one or more straight lines through the current plot
          mean((yhat-boston.test)^2) 
            #Mean Squared Error (MSE)

# Bagging and Random Forests
  #Random Forest is a fancy form of bagging that tries to de-correlate the trees
        
library(randomForest)
  set.seed(1)
    bag.boston=randomForest(medv~.,data=Boston,subset=train,mtry=13,importance=TRUE)
      #We chose 13 for mtry (number of predictors sampled for spliting at each node) because there are 13
        # independent variables we are using to predict the one dependent variable, medv
          bag.boston
            #Random Forest did 500 trees (iterations) and selected the optimal tree using majority vote.

#Make predictions on test set
  yhat.bag = predict(bag.boston,newdata=Boston[-train,])
    plot(yhat.bag, boston.test)
      abline(0,1)
        mean((yhat.bag-boston.test)^2)
          mean.500trees <- mean((yhat.bag-boston.test)^2)
            #Better fit :)
        
#Trying again with fewer trees (from 500 to 25)
  bag.boston=randomForest(medv~.,data=Boston,subset=train,mtry=13,ntree=25)
    bag.boston
    
#Make predictions on test set again
  yhat.bag = predict(bag.boston,newdata=Boston[-train,])
    mean((yhat.bag-boston.test)^2)
      mean.RF25trees <- mean((yhat.bag-boston.test)^2)
        #More trees is better at explaining the variance in the testing set (% Var explained: 86.65 vs. 83.01)
          #But may not be a better predictor on the testing set (MSE= 13.47349 vs. 13.43068)
      
#Now using fewer variables
  tuneRF(Boston[, -14], Boston[, 14], stepFactor=1.5)
    #tuneRF is a function in RandomForest that helps you choose search for the optimal value 
      #(with respect to Out-of-Bag error estimate) of mtry for randomForest.
        set.seed(1)
          rf.boston=randomForest(medv~.,data=Boston,subset=train,mtry=6,importance=TRUE)
            rf.boston
      
#Make predictions on test set again      
  yhat.rf = predict(rf.boston,newdata=Boston[-train,])
    mean((yhat.rf-boston.test)^2)
      mean.6variablesRF <- mean((yhat.rf-boston.test)^2)
        #Best MSE yet!
          plot(yhat.rf, boston.test)  
            abline(0,1)

#Look at importance for variables
  importance(rf.boston)
    #"%incMSE" is based upon the mean decrease of accuracy in predictions on the 
      #out-of-bag sample swhen a given variable is excluded from the model. When variable "crim" is excluded 
        #from the model, the accuracy decreases by an average of 12.384 percentage points.
          #"IncNodePurity" a measure of the total decrease in node impurity that results from 
        #splits over that variable, averaged over all trees.
      #Plot this
    varImpPlot(rf.boston)