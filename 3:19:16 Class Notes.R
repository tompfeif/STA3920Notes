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

#-------------------------------------------------Homework 7---------------------------------------------------
#7.1
a <- c(rep(0,99),rep(3,6))





