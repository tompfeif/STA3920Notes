#Homework 6
#Creating my own knn() function

matrix1 <- matrix(1:6,nrow = 3, ncol = 2, colnames(c(X,Y)), byrow = FALSE) #First, create a matrix that includes numbers 1-6
pro <- matrix(c(1.5,3.3), nrow = 3, ncol = 2, byrow = TRUE, colnames(c(X,Y))) #matrix of test points

#Now we will find the squared distance using Pythagorean's Theorem: a^2 + b^2 = c^2
#We can do this because we have 2 dimensions and we are trying to solve the third length of the triangle side
#Each column represents 
junk.probe <- matrix1 - pro #difference between the coordinates of probe and actual
SqJunk <- (junk.probe)^2 #squared difference between the points
Dist <- SqJunk[,1] + SqJunk[,2] #add the first 
#-------------------------------Question: WHAT DISTANCE ARE WE LOOKING FOR HERE???----------------------------------

#we can apply these calculations more elegantly by using the apply() function
apply(SqJunk,1,sum) #This takes a matrix(or df) and applies the third input to the matrix (by row=1 or col=2)
#This will add the matrix by row, thus A^2 + B^2 to get our distance between the points

#Finally we want to sort the data from low to high:
#????????????????
