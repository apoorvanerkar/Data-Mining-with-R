## REgression Model

## Impoer data
insurance <- read.csv(file.choose())
str(insurance)

## Explanatory power
## Simple linear regression
simple_model <- lm(expenses ~ age, data = insurance)
simple_model

## Check more evaluaions using summary
summary(simple_model)
## if p< alpha, predictor is a significant predictor
##*** close to zero
##* less than 0.01

## Multiple linear regression
multi_model <- lm(expenses ~., data = insurance)
multi_model
#if age is increased or if we add 1 to age as per the age coefficient the age will increase as the vale is positive
# if we move from smoker 1 to smoker yes, the expenses increase by 23847.5
# if you move out of southeast expenses will be reduced by 1035.6
# y = -119416.6 + 256.8 * age+..... - 959.3 * SW
summary(multi_model)

## Build linear regression model for prediction
## Partition data

## 50% for training and 50% for testing
library(caret)
set.seed(500)
inTrain <- createDataPartition(y = insurance$expenses, p = 0.5, list = FALSE)
insur_train <- insurance[inTrain,]
insur_test <- insurance[-inTrain,]

## Building the model based on the training sample
predict_model <- lm(expenses~., data = insur_train)

## Predict the expenses for the examples in the testing samples
insur_prediction <- predict(predict_model, insur_test)
insur_prediction

## Evaluations using error measurements
library(rminer)
mmetric(insur_test$expenses, insur_prediction, c("MAE", "RMSE", "MAPE", "RMSPE", 
                                                 "RRSE", "RAE", "R2", "COR"))

## Build the regression tree
install.packages("rpart")
library(rpart)

## Build the tree on training sample
rpart_model <- rpart(expenses~., data = insur_train)
# Check structure of this regression tree
rpart_model # stars means leave, reached final part of the tree
# if smoker =no and age< 43.5, then reach the end of the tree
# limitations - irrespective of the number of testing samples, there will always be 5 limited values fro future predictions

## Plot the regression tree
install.packages("rpart.plot")
library(rpart.plot)
rpart.plot(rpart_model)

## Build the model tree on training sample
library(RWeka)
m5p_model <- M5P(expenses~., data = insur_train)
## Check the model
m5p_model

## Evaluate the models based on the testing sample
rpart_prediction <- predict(rpart_model, insur_test)
m5p_prediction <- predict(m5p_model, insur_test)
mmetric(insur_test$expenses, rpart_prediction, c("MAE", "RMSE", "MAPE", "RMSPE", 
                                                 "RRSE", "RAE", "R2", "COR"))

mmetric(insur_test$expenses, m5p_prediction, c("MAE", "RMSE", "MAPE", "RMSPE", 
                                                 "RRSE", "RAE", "R2", "COR"))
