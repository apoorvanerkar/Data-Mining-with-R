##Assignment 4 Decision Tree Sample Code
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

#Train a C50
library(C50)
C50model <- C5.0(delay~., data = traindata)

#Print and summary the model
C50model
summary(C50model)


#Prediction on testdata1 and testdata2
C50predictions1 <- predict(C50model, testdata1)
summary(C50predictions1)
C50predictions2 <- predict(C50model, testdata2)
summary(C50predictions2)


#ConfusionMatrix and mmetric
confusionMatrix(C50predictions1, testdata1$delay,  positive = "ontime",  dnn = c("Prediction", "True"))
confusionMatrix(C50predictions2, testdata2$delay, positive = "ontime",  dnn = c("Prediction", "True"))
library(rminer)
mmetric(testdata1$delay, C50predictions1,c("ACC","PRECISION","TPR","F1"))
mmetric(testdata2$delay, C50predictions2,c("ACC","PRECISION","TPR","F1"))

###C5.0 Tree prunning
#tree building
C50model_prune <- C5.0(delay ~ ., data=traindata,control=C5.0Control(CF = .05))
C50model_prune
#Prediction on testdata1 and testdata2
C50predictions1 <- predict(C50model_prune, testdata1)
summary(C50predictions1)
C50predictions2 <- predict(C50model_prune, testdata2)
summary(C50predictions2)
#tree evaluation
confusionMatrix(C50predictions1, testdata1$delay, positive = "ontime",  dnn = c("Prediction", "True"))
confusionMatrix(C50predictions2, testdata2$delay, positive = "ontime", dnn = c("Prediction", "True"))
mmetric(testdata1$delay, C50predictions1,c("ACC","PRECISION","TPR","F1"))
mmetric(testdata2$delay, C50predictions2,c("ACC","PRECISION","TPR","F1"))

###Model with two predictors
##C5.0
C50model_2p <- C5.0(delay ~ weather+distance, data=traindata)
C50model_2p
summary(C50model_2p)
#Prediction on testdata1 and testdata2
C50predictions1 <- predict(C50model_2p, testdata1, type = "class")
summary(C50predictions1)
C50predictions2 <- predict(C50model_2p, testdata2, type = "class")
summary(C50predictions2)
#tree evaluation
confusionMatrix(C50predictions1, testdata1$delay, positive = "ontime",  dnn = c("Prediction", "True"))
confusionMatrix(C50predictions2, testdata2$delay, positive = "ontime", dnn = c("Prediction", "True"))
mmetric(testdata1$delay, C50predictions1,c("ACC","PRECISION","TPR","F1"))
mmetric(testdata2$delay, C50predictions2,c("ACC","PRECISION","TPR","F1"))