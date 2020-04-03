## KNN clasification model

## Import the breast cancer data

bc_data <- read.csv(file.choose())

str(bc_data)

## New data without ID
new_bc <- bc_data[,-1]

str(new_bc)

## Check the distribution of the target variable
table(new_bc$diagnosis)
prop.table(table(new_bc$diagnosis))

## Standardize the predictors
s_bc <- as.data.frame(lapply(new_bc[,-1], scale))
s_bc <- as.data.frame(lapply(new_bc[,2:31], scale))

## Summary of the original bc_data and the new standardized data
summary(new_bc)
summary(s_bc)

## Partition the data: one training first 50%, test1 the following 25%, test2 the last 25%
bc_train <- s_bc[1:285,]
bc_test1 <- s_bc[286:427,]
bc_test2 <- s_bc[428:569,]

## Partition the labels
bc_train_label <- new_bc[1:285,1]
bc_test1_label <- new_bc[286:427,1]
bc_test2_label <- new_bc[428:569,1]

## Check the structure for training predictors and labels

str(bc_train)
str(bc_train_label)

## skip the training phases
## Jump into prediction

library(class)
bc_test1_predictions <- knn(train = bc_train, test = bc_test1, cl = bc_train_label, k = 20)

bc_test1_predictions

## Evaluation phase
install.packages("caret")
install.packages("e1071")
library(caret)
library(e1071)
confusionMatrix(bc_test1_predictions, bc_test1_label, positive = "B", 
                dnn = c("prediction", "True"))

install.packages("rminer")
library(rminer)
mmetric(bc_test1_label, bc_test1_predictions, c("ACC", "PRECISION", "TPR", "F1"))

## See in the evaluation which class is class 1
str(bc_test1_predictions)

## Prediction for test2
bc_test2_predictions <- knn(train = bc_train, test= bc_test2, 
                            cl = bc_train_label, k=20)
bc_test2_predictions

## Evaluate the predictions for test2
confusionMatrix(bc_test2_predictions, bc_test2_label, positive = "M",
                 dnn = c("Prediction", "True"))
mmetric(bc_test2_label, bc_test2_predictions, c("ACC","PRECISION","TPR","F1"))

str(bc_test2_predictions)
