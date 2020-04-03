##Assignment 5 Sample Code
###Import Dataset###
datFlight <- read.csv(file.choose(), stringsAsFactors = TRUE)
str(datFlight)
datFlight$carrier <- factor(datFlight$carrier)
datFlight$dest <- factor(datFlight$dest)
datFlight$origin <- factor(datFlight$origin)
datFlight$weather <- factor(datFlight$weather)
datFlight$dayweek <- factor(datFlight$dayweek)
datFlight$daymonth <- factor(datFlight$daymonth)
datFlight$delay <- factor(datFlight$delay)
str(datFlight)
prop.table(table(datFlight$delay))

####PartitionData#####
library(caret)
set.seed(100)
inTrain <- createDataPartition(y=datFlight$delay, p=0.60, list=FALSE)
length(inTrain)
traindata <- datFlight[inTrain,]
testdata <- datFlight[-inTrain,]
nrow(testdata)
inTest <- createDataPartition(y=testdata$delay, p=0.50, list=FALSE)
nrow(inTest)
testdata1 <- testdata[inTest,]
testdata2 <- testdata[-inTest,]

nrow(traindata)
nrow(testdata1)
nrow(testdata2)
prop.table(table(traindata$delay))
prop.table(table(testdata1$delay))
prop.table(table(testdata2$delay))
str(traindata)

####Build Naive Bayes Model
##Partition the data
#Define dataTrain as a list to store n different train datasets
datTrain<-list()
#Define a list to store n different test datasets
datTest<-list()
#set the number of groups of traindata and test data
n <- 3
# initialize the average number of instances in test sets, Sum_Test_cars, to zero
Sum_Test_flights <- 0
#Use a loop to generate n different groups of traindata and testdata with n different seeds
for (i in 1:n)
{ 
  seed_Number<-100+400*(i-1) # seed will be 100, 500, 900 i=1, 2 and 3
  set.seed(seed_Number)
  inTrain <- createDataPartition(datFlight$delay, p=0.67, list=FALSE)
  ##Store the training dataset as the element at index i of the training list
  datTrain[[i]] <- datFlight[inTrain,]
  ##Store the testing dataset at index i of the testing list
  datTest[[i]] <- datFlight[-inTrain,]
  #Avg_Test_cars <- Avg_Test_cars + nrow(datTest[[i]])/n # update Avg_Test_cars
  Sum_Test_flights <-Sum_Test_flights + nrow(datTest[[i]]) #get the total number of test dataset
  #generate and show proportion tables of train and test data 
  #paste - merge differnt strings/values together
  print(paste("the proportion table of traindata",i,sep=""))
  print(prop.table(table(datTrain[[i]]$delay)),sep="")
  print(paste("the proportion table of testdata",i,sep=""))
  print(prop.table(table(datTest[[i]]$delay)))
}

Avg_Test_flights<-Sum_Test_flights/n # show Average # of cars in the test sets
Avg_Test_flights


###Build Models and Evaluations
library(e1071)
library(rminer)
NB_sum_TP <- 0
NB_sum_TN <- 0
NB_sum_FP <- 0
NB_sum_FN <- 0
#Define an array to store all the results (and we will use it to calculate the average of the results)
NB_total_result<-array()

for(i in 1:n)
{
  #model building
  NBmodel <- naiveBayes(delay ~ ., data=datTrain[[i]])
  ##show each model
  print(NBmodel)
  #generate predictions for test dataset using the model
  NBpredictions <- predict(NBmodel, datTest[[i]])
  #generate confusion matrix
  NB_confusionMatrix<-confusionMatrix(NBpredictions,datTest[[i]]$delay,  positive = "ontime",  dnn = c("Prediction","True"))
  print(NB_confusionMatrix)  #show the confusion matrix
  # update the sum values of TP, FP, TN and FN with the new confusion matrix
  NB_sum_TP <- NB_sum_TP+NB_confusionMatrix$table[2,2]
  NB_sum_FP <- NB_sum_FP+NB_confusionMatrix$table[2,1]
  NB_sum_TN <- NB_sum_TN+NB_confusionMatrix$table[1,1]
  NB_sum_FN <- NB_sum_FN+NB_confusionMatrix$table[1,2]
  #use mmetric to calculate the accuracy, precision, recall and F-measure
  NB_result<-mmetric(datTest[[i]]$delay, NBpredictions,c("ACC","PRECISION","TPR","F1"))
  #use cbind to include the result in to the array
  NB_total_result<-cbind(NB_total_result,NB_result)
}
# Take a look at NB_total_result. Are there similar discpreancies in 
# F-measures of "BadBuys" between C50 and NB results?
NB_total_result
#calculate and show the mean value of each metric
rowMeans(NB_total_result[1:7,-1])
# show the mean values of the confusion matrics for all test sets
NB_mean_TP <- NB_sum_TP/n
NB_mean_TN <- NB_sum_TN/n
NB_mean_FP <- NB_sum_FP/n
NB_mean_FN <- NB_sum_FN/n

