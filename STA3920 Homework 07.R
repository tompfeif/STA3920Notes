Junk = rnorm(1000)
Junk[500] = Junk[500]+10
Junky = VSFE(Junk,600)

plot(Junky, main = "TPfeiffer MSFE Plot, mean shift at t=500", cex=.2, xlab = "Window Length", ylab = "Squared Forecast Error")

#Create a data set of random variables. create 10 random variables with mean 1 and standard deviation 1, then 
#create 10 random variables with mean 2 and standard deviation 2, and repeat this whole process 50 times.

TestJunk = rep(c(rnorm(10, 1,1), (rnorm(10, 2, 2))), 50)
plot(TestJunk, main = "Plot of data, mean and sd shift every t=20", cex=.4)
TestJunk100 = head(TestJunk, 100)
plot(TestJunk100, main = "First 100 data points, mean and sd shift", cex=.4)

TestJunky = vectorMSFE(TestJunk,1000)
plot(TestJunky, main = "TPfeiffer vectorSFE Plot, mean and sd shift", cex=.4, xlab = "Window Length", ylab = "Squared Forecast Error")
#finding the minimum:

match(min(TestJunky),TestJunky)