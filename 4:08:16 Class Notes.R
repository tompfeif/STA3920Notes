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


