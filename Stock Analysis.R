#STA3920

#First, get an idea of what the data looks like:
# head(IBM)
# summary(IBM)
# length(IBM)
# str(IBM)
# class(IBM)
# dim(IBM)

#Adjusted closing price considers two things: Stock splits, new share issues, and dividend payments

#R studio is an integrated development environment (IDE) similar to visual basic or xcode

#To import data, the slashes have to be switched 
#Ferralgas Partners
FGP <-read.csv("/Users/ThomasPfeiffer/Documents/STA 3920/FGP 10 Year Historical Prices.csv")
#IBM
IBM <-read.csv("/Users/ThomasPfeiffer/Documents/STA 3920/IBM 10 Year Historical Prices.csv")
#Tupperware
TUP <- read.csv("/Users/ThomasPfeiffer/Documents/STA 3920/TUP 10 Year Historical Prices.csv")


#To remove the NA values from your dataset, you can use the na.omit() function

#Getting the vectors for daily percentage change:
TUPret <- na.omit(TUP$Daily...Change)
FGPret <- na.omit(FGP$X..Daily.Change)
IBMret <- na.omit(IBM$Daily...Change)
#Calculating the MSFE for daily % change with moving average period lengths up to 500
TUPDailyChangeMSFEValues <- vectorMSFE(IBMret, 500)
FGPDailyChangeMSFEValues <- vectorMSFE(FGPret, 500)
IBMDailyChangeMSFEValues <- vectorMSFE(TUPret, 500)

#Getting the vectors for daily range:
TUPrange <- na.omit(TUP$Daily.Range)
FGPrange <- na.omit(TUP$Daily.Range)
IBMrange <- na.omit(IBM$DailyRange)
#Calculating the MSFE values for daily Range with d[1:500]
TUPDailyRangeMSFEValues <-vectorMSFE(TUPrange, 500)
FGPDailyRangeMSFEValues <-vectorMSFE(FGPrange, 500)
IBMDailyRangeMSFEValues <-vectorMSFE(IBMrange, 500)

#Getting the vectors for yesterday down or up
#our first value in up or down is empty (obviously) so we want to exclude the first value from our boxplot
#we can remove the first cell by subsetting with a negative number 
#i.e. x[-1] will get all values except for the first value in the set
#Remove the NA values using na.omit():
TUPdownOrup <- na.omit(TUP$PreviousUpOrDown)
FGPdownOrup <- na.omit(FGP$PreviousUporDown)
IBMdownOrup <- na.omit(IBM$PreviousUporDown)
#However, this has not worked in this case for the box plots. 
#It turns out that we can use either the factor() command or the droplevels() command to remove empty levels
IBMdownOrup <- IBMdownOrup[IBMdownOrup!=""]
IBMdownOrup <- factor(IBMdownOrup)
#OR I can also use:
IBMdownOrup <- IBMdownOrup[IBMdownOrup!=""]
IBMdownOrup <- droplevels(IBMdownOrup)
#Now we have our data with the desired number of factors, the problem now is that our vectors are of 
#different lengths, and boxplots must have equal lengths to plot against one another.
#I know that I want to leave off the values without daily up or down results, so I can use:
boxplot(tail(IBMret,2520)~ IBMdownOrup)
#Or I can also do:
boxplot(split(IBMret[-1], IBMdownOrup))
#but this only works because I know that the empty values occur in the first two columns
#if my empty columns existed elsewhere, I would have to fix my data before creating a boxplot
#similarly for TUP and FGP:
TUPdownOrup <- TUPdownOrup[TUPdownOrup!=""]
TUPdownOrup <- droplevels(TUPdownOrup)
FGPdownOrup <- FGPdownOrup[FGPdownOrup!=""]
FGPdownOrup <- droplevels(FGPdownOrup)

#boxplots:
boxplot(TUPret[-1]~TUPdownOrup, main= "TUP Daily Change")
boxplot(FGPret[-1]~FGPdownOrup, main= "FGP Daily Change")




#02_06_16 Class Notes
#Calculating Mean Squared Forecast Error (MSFE) - used to make comparisons and it is very useful

#to get a function window by itself, use the fix() function with the name of your desired function in parentheses

#This is a function to calculate the MSFE- mean squared forecast error of our moving average
#'m' is the length of the moving average window we want to use i.e. days
#'datacolumn' is the vector of values i.e. the column we take values from in vector form without any missing values
#The following program can be used to calculate MSFE for a moving average on any vector of data
MSFE <- function(datacolumn, m) 
        {
        colLength <- length(datacolumn)  #determines the length of the column of interest i.e. 'datacolumn'
        sfe <- vector(length = colLength - m)#placeholder for squared forecast errors for each column
        
        for(i in 1:(colLength-m)) { #calculate the SFE of each row: (value - moving average)**2
                sfe[i] <- (datacolumn[m+i] - mean(datacolumn[i:(i+m-1)]))**2
        }
        msfe <- mean(sfe) #we take the average of all squared forecast errors to get our measure of error
        return(msfe)
}

#this second function is going to calculate the MSFE for all moving average window lengths from 1 to d
#we are doing this to determine how long the moving average length should be to minimize msfe
#d is the maximum window length you want to use i.e. the upper bound of the moving average length
vectorMSFE <- function(datacolumn, d)
        {
        msfeValues <- vector(length= d)#placeholder for the Mean squared forecast errors
        
        for(i in 1:d)#running all moving averages from m=1 to m=d to see how the MSFE changes
                {
                        msfeValues[i] <- MSFE(datacolumn, i)#we use our previous MSFE function   
        }
        return(msfeValues)
}
#This function is going to output a function with the different MSFE values for each m up to d

#To find the minimum value of a dataset use the min() function
#To find the location of the dataset, use the match() function i.e. match(min(TUPMSFEValues), TUPMSFEValues)




#---------------------------------------Questions for Professor Tatum:
#1) ANSWERED: Why are we calculating MSFE on the Daily % change and not the Adjusted closing price?
#       Because when it comes to stocks, what we are interested in is what the stock is going to do tomorrow,
#       We are interested in the return of the stock tomorrow, not the price
#       Our Main question is "What will the stock return tomorrow?" i.e. we are forecasting returns, not price
#       It is impossible to forecast tomorrow's price because stock price is a stochastic process where the 
#       best guess for tomorrow is a naive forecast. However, returns is a different story.
#       COOL!!! Forecasting returns is how we forecast stocks, not by price but by returns

#2) Why does our "up" "down" column assign unchanged days to "down"?

#3) Why did Professor Tatum's box plot have the following arguments?:
#boxplot( split( IBM$IBMret[-1:-2], IBM$Prev1Up[-1:-2]))
#Why are the boundaries from -1:-2 and -1:-2?


#If my workspace gets too crowded, I can remove variables using the remove() command
#In order to clear a console, type: cat("\014") 


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
#I want to figure out how to use this above function because I currently don't understand it
#The resulting table of 4 values indicates how well the forecast worked
#We add our correct predictions divided by the total number of forecasts:
(84+43) / (84+43+68+57) #this equals .504
#The calculations above have been calculated on the daily direction of the stock data
#The model ended up predicting with a terrible 50% chance, but maybe we can more accurately predict
#if the next day will be up or down by using the range instead


#Now we are going to predict if tomorrow's range is going to be above or below the median range
#this is useful with regards to the options market and understanding the overall risk of the stock!!


#Now I want to run the knn() function on my updated csv spreadsheet for TUP
TUPknn <- read.csv("/Users/ThomasPfeiffer/Documents/STA 3920/TUP 10 Year Historical Prices_Updated.csv")
