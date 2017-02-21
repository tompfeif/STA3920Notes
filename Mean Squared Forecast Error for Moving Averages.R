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




#Extra stuff
#Class Notes 3/19/16
#Statistical comprehension is going to be my skill as a data scientist.
#I will use R, Tableau, and Excel to solve preoblems and make statistical inferences.
#The most important thing is understanding how the statistics works

#Rolling dice:
#When we double the number of die, we reduce expected value variance by half

#When considering the length of a moving average window, we must consider the bias-variance trade-off
#Increasing the window length will increase bias and reduce variance
#Decreasing window length will increase variance and reduce bias
#The bias-variance tradeoff is intrinsic when choosing the moving average window length

#Now we are going to simulate the Bias-Variance trade-off
x<- rep(c(rep(0,25),rep(1,25)),20) #This produces 25 repeating 0's followed by 25 repeating 1's, all repeated 20 times
#Now let's add some error terms to our x data by including:
x1<- x + rnorm(1000) #This adds a normal random variable to our 1000 values of x

#Here is our function that calculates the Mean Squared Forecast Error for a single moving average window length
MVG1 <- function (x,m) #x is our data, m is our moving average window length
{
        n <- length(x)
        y <- vector(length=(n-m))
        
        for(k in 1:(n-m)) {
                y[k] <- ( x[m+k] - mean(x[k:(k+m-1) ]) )**2
        }
        msfe <- mean(y)
        return(msfe)
}
#Here we created a function to present the MSFE for every window length up to d, which allows us to see how the 
#window length effects the MSFE of our moving averages
VSFE <- function (x,d) # x is data, d is max window length 
{
        z <- vector(length=d)
        for(j in 1:d) {
                
                z[j] <- MVG1(x,j)
        }
        return(z)
}
#It is important to understand that our MSFE information is based on historical data, and this is used as an *estimate* for 
#The MSFE associated with the different window lengths
#Let's plot the MSFE associated with our x1 data that varies from 0 to 1 periodically, including error terms
y <- VSFE(x1,100) 
plot(y)
