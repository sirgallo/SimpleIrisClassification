#clear console
rm(list = ls())

#set up directory and libraries
setwd("~/MachineLearningProjects/Homework1")
library(tidyverse)
library(class)

#euclidean distance 
edistance <- function(x,y,n) {
  dis = 0
  #n is the dimensions of the vector or # of components
  for (i in 1:n)
    dis = dis + (x[i] - y[i])^2
  dis = sqrt(dis)
  return(dis)
}

#load the iris data
data(iris)
irislength <- length(iris$Species)
#temp data frame
iristemp <- iris
#unfactorize the factorized classes
iristemp$Species <- as.character(iristemp$Species)

#convert the strings to integer classes
for(i in 1:length(iris$Species)) {
  if(iristemp$Species[i] == "setosa") 
    iristemp$Species[i] <- 1
  else if(iristemp$Species[i] == "versicolor")
    iristemp$Species[i] <- 2
  else
    iristemp$Species[i] <- 3
}

#convert chr to num
iristemp$Species <- as.numeric(iristemp$Species)

#select indexes for test points, sampling from full data set
testsetindexes = sort(sample(1:irislength, 50, replace = F), decreasing = TRUE)
#create data frame for test set
testdataset <- data.frame(Sepal.Length = numeric(), Sepal.Width = numeric(), Petal.Length = numeric(), Petal.Width = numeric(), Species = integer())

#find the indexes where the data are, place into test data frame
for(i in 1:50) {
  objectindex <- testsetindexes[i]
  #set up test set
  testdataset[i,] <- iristemp[objectindex,]
  #set up training set, manipulating the original dataframe
  iristemp <- iristemp[-c(objectindex),]
}

#transfer to trainingdataset
trainingdataset <- iristemp

#raw data without classes, for calculating distances
trainingnoclass <- trainingdataset[,-5]
testnoclass <- testdataset[,-5]
#attributes in data
features <- ncol(testnoclass)

#dataframe to hold expected and actual values, as well as temp storage for top nn

classvotevec <- integer()
expected <- integer()
actual <- integer()
neighbors <- integer()
topnn <- integer()
#nearest neighbor k
knn = 3

#loop to run through all test points for knn
for(i in 1:nrow(testdataset)) {
  #initialize vector for distances
  dist <- numeric()
  #get value for current expected class
  expclass <- testdataset$Species[i]
  expected[i] <- expclass
  
  #find distances to each training point from the current test point
  for(j in 1:nrow(trainingdataset))
    dist[j] <- edistance(testnoclass[i,], trainingnoclass[j,], features)
  
  #load neighbors from classes and distances into data frame
  neighbors <- cbind(trainingdataset$Species, dist)
  neighbors <- as.data.frame(neighbors)
  colnames(neighbors) <- c("Species", "Distance")
  
  #order the results based on distance, smallest -> largest
  neighborsordered <- neighbors[order(unlist(neighbors$Distance)),]
  
  #pick the top three nearest neighbors (since k = 3)
  topnn <- neighborsordered$Species[1:knn]
  #sums for votes for each class
  class1vote = 0
  class2vote = 0
  class3vote = 0
  
  #votes for each class per run
  for(k in 1:knn) {
    if(topnn[k] == 1)
      class1vote = class1vote + 1
    else if(topnn[k] == 2)
      class2vote = class2vote + 1
    else
      class3vote = class3vote + 1
  }
  
  #return the actual value, based on the majority class
  sumall <- cbind(class1vote, class2vote, class3vote)
  actual[i] <- which(sumall == max(sumall))
  classvotevec <- cbind(expected, actual)
}

#convert to dataframe
classvote <- as.data.frame(classvotevec)
colnames(classvote) <- c("expected", "actual")

#check accuracy, compare expected to actual, and find the accuracy for each class
accuracy <- numeric()
sum = 0
sum1 = 0
sum2 = 0
sum3 = 0
amntcl1 = 0
amntcl2 = 0
amntcl3 = 0
for(i in 1:nrow(testdataset)) {
  if(classvote$expected[i] == 1)
    amntcl1 = amntcl1 + 1
  else if(classvote$expected[i] == 2)
    amntcl2 = amntcl2 + 1
  else
    amntcl3 = amntcl3 + 1
  if(classvote$expected[i] == classvote$actual[i]) {
    sum = sum + 1
    if(classvote$expected[i] == 1)
      sum1 = sum1 + 1
    else if(classvote$expected[i] == 2)
      sum2 = sum2 + 1
    else
      sum3 = sum3 + 1
  } 
}

#calculate and print accuracy and error
accuracy <- sum/nrow(testdataset)
error <- (nrow(testdataset) - sum)/nrow(testdataset)
classacc <- cbind(sum1/amntcl1, sum2/amntcl2, sum3/amntcl3)
classacc <- as.data.frame(classacc)
colnames(classacc) <- c("setosa", "versicolor", "virginica")
cat("The accuracy for our model is: ", accuracy*100, "% and the error for our model is: ", error*100, "%.\n")
cat("The accuracy for class setosa is: ", classacc$setosa*100, "%, the accuracy for class versicolor", classacc$versicolor*100, "%, and the accuracy for virginica is: ", classacc$virginica*100, "%.")