#Final project
#Write a loop for R that would output the classification error rate for different widths of the lag window
#when using k-nearest neighbors
library(class)

#Using Iris data, we must randomly select the data.
set.seed(1234) #This sets our random number generator so we may test repeatedly
ind <- sample(2, nrow(iris), replace=TRUE, prob=c(0.67, 0.33)) #we use this to randomize our iris data
#Our training data should be 2/3 of our data, thus probability ind==1 is 67%

iris.training <- iris[ind==1, 1:4] 
iris.test <- iris[ind==2, 1:4] 
iris.trainlabels <- iris[ind==1, 5] 
iris.testlabels <- iris[ind==2, 5]

#The below function is going to calculate the prediction error rate for our knn function for k up to m
knnForecast <- function(TrialX, TrialY, TestX, TestY, m) 
{
        errorRate <- vector(length = m) #placeholder vector for error rate of each knn 

        for(i in 1:m) {
                prediction <- knn(TrialX, TestX, TrialY, k=i)
                errorRate[i] <- 1 - mean(prediction==TestY)  #error rate, not prediction rate
        }
        return(errorRate)
}
errors <- knnForecast(TrialX = iris.training, TrialY = iris.trainlabels, TestX = iris.test, TestY = iris.testlabels, m = 100)
plot(errors, main = "Knn Error Rate for each k", xlab = "k", ylab = "Prediction Error Rate", pch = 1, cex = .5)

#here we test the above function
test.prediction <- knn(iris.training, iris.test, iris.trainlabels, 5)
table(test.prediction, iris.testlabels)
mean(test.prediction == iris.testlabels)