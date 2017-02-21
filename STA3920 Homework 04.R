#Homework 04 Code and Notes with plotting for knn() data
load("/Users/ThomasPfeiffer/Documents/STA 3920/Lecture Notes 4 Practice Data.RData")

animalClass <- Dogs$Class
plot(Dogs$Weight, Dogs$Whisker, main = "Dogs and Cats, Thomas Pfeiffer 2-24-16", xlab = "Weight", ylab = "Whisker Length",
     pch = ifelse(Dogs$Class=="Dog", 17,16), col = ifelse(Dogs$Class=="Dog", "forestgreen", "red"), cex = 1)
legend("topleft", inset = 0.05, legend = c("Dog","Cat"), pch = c(17,16), 
       col = ifelse(Dogs$Class=="Dog", "forestgreen", "red"))
#pch= is the parameter that selects the type of character graphed
#col= is the color of our different plotted points
#cex= is the parameter that decides the size of the plotted points or text
#legend= is the legend parameter

#in order to include labels, you must add a text() argument outside of the plot function, and inside include
#labels= is the label parameter
#pos= is the position function that allows you to place the labels on any side of the plot point you'd like
#cex= is the size function that allows you to determine the size of your label

#Here is the code for a plot that creates the ideal plot for this data
animalClass <- Dogs$Class
plot(Dogs$Weight, Dogs$Whisker, main = "Dogs and Cats, Thomas Pfeiffer 2-24-16", xlab = "Weight", ylab = "Whisker Length",
        pch = ifelse(Dogs$Class=="Dog", "D","C"), col = ifelse(Dogs$Class=="Dog", "blue", "red"), cex = .7)
legend("topleft", inset = 0.05, legend = c("Dog","Cat"), pch = ifelse(Dogs$Class=="Dog", "D","C"),
       col = ifelse(Dogs$Class=="Dog", "blue", "red"))



#Now I will run the knn function on some new sample data with whisker and weight lengths that I've made
#----------------------------------------------Standardizing Data-------------------------------------------------
#To standardize data, we subtract the mean and divide by the standard deviation
#Standardizing data makes the mean=0 and the standard deviation equal to 1
SampleData <- read.csv("/Users/ThomasPfeiffer/Documents/STA 3920/CatDogSampleData.csv")
StandardizedSampleData <- SampleData
StandardizedSampleData$Weight <- ((SampleData$Weight - mean(SampleData$Weight))/sd(SampleData$Weight))
StandardizedSampleData$Whisker <- ((SampleData$Whisker - mean(SampleData$Whisker))/sd(SampleData$Whisker))

#----------------------------Understanding how knn() works--------------------------------------
#Now I am going to display the knn() function by displaying its classification space
#To do this we need a dataset that I've created using excel, which is called "Creating a grid (Probe function in R).csv"
#ProbeX is a dataset that includes every value from 0-20 on a .1 scale, which we will use to creat a classification space
ProbeX <- read.csv("/Users/ThomasPfeiffer/Documents/STA 3920/Creating a grid (Probe function in R).csv")

library(class)
TrialX <- StandardizedSampleData[,2:3] #TrialX is the predictor variables: whisker length and weight
TrialY <- StandardizedSampleData[,4] #TrialY is the predicted varaibles: cat or dog

#Below is Professor Tatum's function ProbeKnn, which plots the knn algorithm so that we can see the classification space
NewProbeKnn <- function (TrialX, ProbeX, TrialY, k) 
{
        library(class)
        ProbeYhat = knn(TrialX,ProbeX,TrialY,k) 
        #ProbeYhat is the output of the predictions of the knn() function for the ProbeX test data
        #ProbeYhat is used to generate the classification space
        MinX <- min(TrialX[,1])
        MaxX <- max(TrialX[,1])
        MinY <- min(TrialX[,2])
        MaxY <- max(TrialX[,2])
       
        plot(ProbeX[,1],ProbeX[,2], main=paste("TPfeiffer knn Classification Space", "k=",k), xlab="Weight", ylab="Whisker", cex=1, 
             pch=3, col=ifelse(ProbeYhat=="Dog","deepskyblue","salmon1"), xlim=c(MinX, MaxX), ylim=c(MinY, MaxY))

        par(new=TRUE)#the par() attribute allows us to lay one plot ontop of another
        
        plot(TrialX[,1],TrialX[,2], ann=FALSE, pch=ifelse(TrialY=="Dog",17,16),
             col=ifelse(TrialY=="Dog","blue","red"), xlim=c(MinX, MaxX), ylim=c(MinY, MaxY),cex=1)
        legend("topright", legend = c("Dog","Cat"), pch = c(17,16), 
               col = ifelse(Dogs$Class=="Dog", "blue", "red"), border="black",cex=.65)
        return()
}
NewProbeKnn(TrialX,ProbeX,TrialY,7)