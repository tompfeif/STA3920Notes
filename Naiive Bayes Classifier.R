#STA Homework 10 Naive Bayes Classification
plot(iris[1:4], main = "Iris Data  (red=setosa,green=versicolor,blue=virginica)",
      pch = 20, col = ifelse(iris$Species=="setosa", "red", ifelse(iris$Species== "versicolor", "green", "blue")))
#Or you can also use:
pairs(iris[1:4], main="Iris Data (red=setosa,green=versicolor,blue=virginica)", 
      pch=21, bg=c("red","green3","blue")[unclass(iris$Species)])

#Naive Bayes Classifier
install.packages('e1071', dependencies = TRUE) #install an R package with function 'naiveBayes'
library(e1071)
library(class)

classifier <- naiveBayes(iris[,1:4], iris[,5]) #Run naiveBayes classifier function
table(predict(classifier, iris[,-5]), iris[,5]) #Calculate correct classifications

#These are additional R packages that we can use for cross validation
install.packages("caret")
install.packages("klaR")
install.packages("lattice")
install.packages("ggplot2")
install.packages("MASS")
library(caret)
library(klaR)

train <- sample(150,100) #randomly select 1/3 of the iris data to use for testing purposes
x.train = iris[train,-5] #x inputs for training data
y.train = iris$Species[train] #y input for training predicted value
model = train(x.train,y.train,'nb',trControl=trainControl(method='cv',number=10))
#Method is cross validation, and it breaks the training data into 10 sections

table(predict(model$finalModel,x.test)$class,y.test) #Here we are testing the method developed above and outputting a table

#Question 10.6----------------------------------
train <- sample(150,100)
x.train = iris[train,1:2]
y.train = iris$Species[train]
model = train(x.train,y.train,'nb',trControl=trainControl(method='cv',number=10))
table(predict(model$finalModel,x.test)$class,y.test)


#HW 10------------------------------------------
data <- c(1:10)
train <- sample(10,3)
notTrain <- data[-train]
print(train)
print(notTrain)


#Extra stuff
#The job of managers is the management of variation. Statisticians measure uncertainty
#Bayes' inference: P(A|B) = P(B|A)*P(A) / P(B)
#P(B) = [P(B|A)*P(A) + P(B|A')P(A')]
#Therefore, P(A|B) = P(A)*P(B|A) / [P(B|A)*P(A) + P(B|A')*P(A')]

#Naive Bayes classification method
#Naive Bayes assumes there is no correlation between the variables

#3d scatterplot:
install.packages("scatterplot3d")
library(scatterplot3d)
scatterplot3d(iris[,1],iris[,2],iris[,3]) #there are better ones, but I need to do specifics for osx software

install.packages("e1071")
library(e1071)
classifier <- naiveBayes(iris[,1:4],iris[,5])
table(predict(classifier, iris[,-5]), iris[,5]) #'-5' means don't include column 5
#This bayes classifier results in 6 errors


#Use cross-validation to check that the data is not overfit
install.packages("caret")
install.packages("klaR")
library(caret)
library(klaR)

train <- sample(150,100) #sample() takes a random 100 variables
x.train <- iris[train,-5]
y.train <- iris$Species[train]
x.test <- iris[-train,-5]
y.test <- iris$Species[-train]

model <- train(x.train,y.train,'nb',trControl=trainControl(method='cv',number=10))
table(predict(model$finalModel, x.test)$class, y.test) #compare prediction column against y.test, the actual values

