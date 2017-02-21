#------------------------------------------knn() homework 03----------------------------------------

Dogs <- read.csv("/Users/ThomasPfeiffer/Documents/STA 3920/Dogs.csv")
library(class)#get knn()
#The goal of the knn() function is to create a function to fit our data that is based on a trial period
#in this case, the knn() function takes two inputs to determine the categorization: weight and whisker length
#Y represents our categorical variable: cat or dog
#X represents our inputs that our Yhat will be determined by; X is a matrix of our two variables we are using to predict
TrialX <- Dogs[1:15, 2:3] #this is the training data for the first 15 rows of weight and whisker length
TestX <- Dogs[16:25, 2:3] #this is the test data for the final 10 rows of weight and whisker length
TrialY <- Dogs$Class[1:15] #this is the trial actual Y values that we will use to calculate errors i.e. (Y-Yhat)
TestY <- Dogs$Class[16:25]
Dogsknn.pred <- knn(TrialX, TestX, TrialY, 3)
#knn() requires 4 inputs:
#1. a trial of the variables used to create the function
#2. the test variables that will be used to test the function
#3. a trial of the dependent variables that are used to create the function (by limiting the error)
#4. the number of nearest neighbors to use
table(Dogsknn.pred,TestY)
mean(Dogsknn.pred==TestY)

#Now I want to create a function that runs through different values of k
#Bestk <- function(dataStuff, d) #dataStuff is...?
