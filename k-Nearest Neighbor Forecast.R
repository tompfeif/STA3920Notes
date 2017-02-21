#02_13_2016 Class Notes
#Bell curves come from adding up random events! Sum up a random event and you get a normal distribution
#In order to generate a stochastic process, you must sum up a random series of events i.e. a random walk

#Is the underlying distribution stable?
#In order to forecast something, the distribution must be stable
#If the data, when broken up into smaller samples looks similar, i.e. slices of the main data will
#appear similar if the underlying distribution is stable

#Stock price is not stable, but daily stock returns is stable!
#Therefore, since stock price data is not stable, we can't forecast them
#However, we can forecast stock returns because they are stable!
#Scalable data mining tools do exist
#Programming is all about scalability; we want our programs to work for 500 or 5,000,000,000 data sets
#Stock options are just insurance on stocks. We can find a fair price by using stochastic processes


#Loading in some libraries to include valuable items
library(class) #this is an r package that helps us with classification
#we load this library because we want to use the knn() function
#The knn() function is the k-Nearest Neighbor Classification

#The knn() function requires four inputs. 
#1. A matrix containing the predictors associated with the training data,labeled train.X below. 
#2. A matrix containing the predictors associated with the data for which we wish to make predictions, labeled test.X below. 
#3. A vector containing the class labels for the training observations, labeled train.Direction below. 
#4. A value for K, the number of nearest neighbors to be used by the classifier

#This is the next function using the knn() command
#We are using this function to try to predict if the stock will be up or down tomorrow based on
#the returns for the last two days
SMarketTatum <- read.csv("/Users/ThomasPfeiffer/Documents/STA 3920/SMarketTatum.csv")
Lag1 <- SMarketTatum$Lag1
Lag2 <- SMarketTatum$Lag2
Year <- SMarketTatum$Year
train <- (Year < 2005)
SMarketTatum.2005 <- SMarketTatum[!train,]
Direction <- SMarketTatum$Direction
Direction.2005 <- Direction[!train]
train.X <- cbind(Lag1 ,Lag2)[train ,]
test.X <- cbind (Lag1 ,Lag2)[!train ,]
train.Direction <- Direction [train]
knn.pred <- knn(train.X,test.X,train.Direction,k=1)
table(knn.pred,Direction.2005)
mean(knn.pred==Direction.2005)
#I want to figure out how to use this above function because I currently don't understand it
#The resulting table of 4 values indicates how well the forecast worked
#We add our correct predictions divided by the total number of forecasts:
(84+43) / (84+43+68+57) #this equals .504
#The calculations above have been calculated on the daily direction of the stock data
#The model ended up predicting with a terrible 50% chance, because it is very difficult to predict 
#the movement of a stock because it is a stochastic process (random walk)

#Let us now consider something that we may be able to better forecast i.e. let's look at something
#that is not a sum of random events, but is a stable distribution! i.e. let's look at range!
#Now we are going to predict if tomorrow's range is going to be above or below the median range
#this is useful with regards to the options market and understanding the overall risk of the stock!!

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




#------------------------------knn() function for my stock data-------------------------------------------
 
#Now I want to run the knn() function on my updated csv spreadsheet for TUP
#The goal of our use of knn() here is to see if we can use range to determine up or down for the next day!
#knn() helps us to develop a model to predict our dependent variable
#our independent variables are daily return with lag1 and daily return with lag2
#our dependent variable is if the following day is up or down
TUPknn <- read.csv("/Users/ThomasPfeiffer/Documents/STA 3920/TUP 10 Year Historical Prices_Updated.csv")
library(class) #This library contains the knn() function
TUPLag1Range <- TUPknn$Lag1 #assign daily range lag1 vector (i.e. daily return with 1 day lag)
TUPLag2Range <- TUPknn$Lag2 #assign daily range lag2 vector (i.e. daily return with 2 days lag)
TUPYear <- TUPknn$Year
#to get part of the data for the algorith to learn, we partition the data (majority in training data)
trainingData <- (TUPYear < 2015) #step1 set the partition criteria; training data is from 2006 to the end of 2014
TUPafter2015 <- TUPknn[!trainingData,] #step2 set the test data using boolean subset; test data is from 2015 to 2016
TUPDirection <- TUPknn$Direction 
TUPDirectionAfter2015 <- TUPDirection[!trainingData] #the direction testing data vector; a vector of our Y values
train.Direction <- TUPDirection [trainingData] #this is the direction training data we use to minimize error and build our function
TUPknn.train <- cbind(TUPLag1Range, TUPLag2Range)[trainingData,] #combine our Lag1 and Lag2 variables into a matrix
#this means that this matrix contains our predictor variables; above is training predictor variables, below is test predictors
TUPknn.test <- cbind (TUPLag1Range, TUPLag2Range)[!trainingData,] 
#The input for this knn() function is:
#1. the training predictor (independent) variables; if more than one, we need a matrix i.e. multiple columns
#2. the testing predictor (independent) variables; our model uses these to make predictions based on the model created
     #this second input is basically our regression input variables: X1, X2, etc.
#3. the training actual values, which our model will use to create the function with minimal error
#4. the number of nearest neighbors to use.
TUPknn.pred <- knn(TUPknn.train, TUPknn.test, train.Direction,k=1)
table(TUPknn.pred,TUPDirectionAfter2015)
mean(TUPknn.pred==TUPDirectionAfter2015)#this outputs the probability of correct predictions

#Installing packages that are useful
install.packages("kknn")
library("kknn")
install.packages("ggplot2")
library("ggplot2")



#HW6---------------------------------------------------------------------------------------------------------

Dogs <- read.csv("/Users/ThomasPfeiffer/Documents/STA 3920/Dogs.csv")
library(class)#get knn()

TrialX <- Dogs[1:15, 2:3] #this is the training data for the first 15 rows of weight and whisker length
TestX <- Dogs[16:25, 2:3] #this is the test data for the final 10 rows of weight and whisker length
TrialY <- Dogs$Class[1:15] #this is the trial actual Y values that we will use to calculate errors i.e. (Y-Yhat)
TestY <- Dogs$Class[16:25]
Dogsknn.pred <- knn(TrialX, TestX, TrialY, 3)

table(Dogsknn.pred,TestY)
mean(Dogsknn.pred==TestY)

#Construct a probe to test distances
Probe <- matrix(c(10,2),nrow=25,ncol=2,byrow = TRUE)
#find the difference between data and probe
Diff <- Probe - Dogs[,c(2,3)]
#squared difference
SqDiff <- Diff^2
#Finding the distance between data points and (10,2)
Dist <- SqDiff[,1] + SqDiff[,2]
#sort the list according to the distance value
Dogs$Class[sort.list(Dist)]