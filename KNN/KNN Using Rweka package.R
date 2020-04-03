## KNN using Rweka package

## Import details
income <- read.csv(file.choose())
prop.table(table(income$income))

## select a subset from the original data with 2000 examples
library(caret)
sample_index <- sample(1:32561,2000)
sample_index

## Create the subset by using the sample index
income_sample <- income[sample_index,]
str(income_sample)

## Check the distribution for the sample data
prop.table(table(income_sample$income))

##Partition the data randomly
# training: 50%, testing: 1:25%, testing 2:25%
set.seed(100)
inTrain <- createDataPartition(income_sample$income, p = 0.5, list = FALSE)
inTrain
traindata <- income_sample[inTrain,]
testdata <- income_sample[-inTrain,]

inTest <- createDataPartition(testdata$income, p = 0.5, list = FALSE)
test1data <- testdata[inTest,]
test2data <- testdata[-inTest,]

## Check the ditribution of the target variable for training, test1, test2
prop.table(table(traindata$income))
prop.table(table(test1data$income))
prop.table(table(test2data$income))

## Build the knn model

k = 20
library(RWeka)
##KNN model 1 with two predictors
knn_model1 <- IBk(income ~ age + education, data = traindata, control = 
                    Weka_control(K = 20, X = TRUE))
knn_model1

#to use all the predictors '
knn_model2 <- IBk(income ~., data = traindata, control =  Weka_control(K = 20, X=T))  #X = T is for getting the best k value from 1 to 20
knn_model2

#assign weight based on the on the nearest neighbors
#option1: 1/distance (used only when K>1)
knn_model_weight1 <- IBk(income ~., data = traindata, control =  Weka_control(K = 20, X=T, I = T))  #I stands for inverse which is used for 1/distance
knn_model_weight1

#option2: 1-distance (used only when K>1)
knn_model_weight2 <- IBk(income ~., data = train_data, control =  Weka_control(K = 20, X=T, F = T))  #F = T is for 1-distance
knn_model_weight2

## Evaluations

## Predictions for the testing samples
prediction1data <- predict(knn_model2, testdata)
prediction1data

## Confusion matrix

library(caret)
confusionMatrix(prediction1data, test1data$income, positive = ">50k", dnn = c("TRUE","PREDICTION"))
library(miner)
mmetric(test1data$income, prediction1data, c("ACC","PRECISION", "TPR","F1"))

## Which class is class 1
str(prediction1data)
