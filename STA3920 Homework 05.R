#STA 3920 Homework 5: Bias and variance notes

simulation <- cbind(rnorm(500),rnorm(500)) #sample of 500 x and y points with mean=0 and sd=1; a bivariate Normal distribution
simulationBias <- cbind(1.8+rnorm(500),1.4+rnorm(500))

#Plot our bivariate Normal Distribution
plot(simulation,xlim=c(-10,10),ylim=c(-10,10), xlab="x", ylab="y", main="Unbiased Aim, Low Variability: 500 draws",cex=0.5)
par(new=TRUE)#par allows us to lay one plot ontop of another plot
#Plot a point at the center: (0,0)
plot(0,0,pch=20,col=18,xlim=c(-10,10),ylim=c(-10,10),cex=2,xlab=" ",ylab=" ")

#Introduce bias into the model by adding some value so that the real mean is shifted
plot(simulationBias,xlim=c(-10,10),ylim=c(-10,10), xlab="x", ylab="y", main="Biased Aim, Low Variability: 500 draws",cex=0.5)
par(new=TRUE)#par allows us to lay one plot ontop of another plot
#Plot a point at the center: (0,0)
plot(0,0,pch=20,col=18,xlim=c(-10,10),ylim=c(-10,10),cex=2,xlab=" ",ylab=" ")
par(new=TRUE)
plot(1.8,1.4,pch=20,col="green", xlim = c(-10,10),ylim=c(-10,10),cex=2,xlab = " ",ylab = " ")

#Increase the variance by multiplying the variance by 2
plot(cbind(1.8+2*rnorm(500),1.4+2*rnorm(500)),xlim=c(-10,10),ylim=c(-10,10), xlab="x", ylab="y", 
     main="TPfeiffer Biased Aim, High Variability",cex=0.5,pch=1)
legend("topright",legend = c("Target","Point of Aim"), pch = c(19,13), col = c("red", "green"))
par(new=TRUE)#par allows us to lay one plot ontop of another plot
#Plot a point at the center: (0,0)
plot(0,0,pch=19,col="red",xlim=c(-10,10),ylim=c(-10,10),cex=2,xlab=" ",ylab=" ")
par(new=TRUE)
plot(1.8,1.4,pch=13,col="green", xlim = c(-10,10),ylim=c(-10,10),cex=2,xlab = " ",ylab = " ")
