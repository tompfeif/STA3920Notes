#Class Notes 2/27/16

#Bias-Variance trade-off
#To reduce bias, increase the number of bins
#To reduce variance, decrease the number of bins

#E.g. let's create a moving average to estimate the expected value of a normal distribution with a jump in the average
#to measure bias, we have to compute the expected value of our moving average across the given structure
#Moving average = W[t] = sum(Y[t-3], Y[t-2], Y[t-1]) / 3
#As long as our expected value doesn't shift, our moving average w has no bias i.e. E[W[t]] = E[y]

#The longer a moving average, the greater the bias
#The longer the moving average, the shorter the variance