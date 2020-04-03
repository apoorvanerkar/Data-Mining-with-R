## Cost Sensitive Learning

##Define the following benefit and costs:(Yes is positive, No is negative)
## Profit when a dealer buy and resell a good car: $600 (-600 FP)
## Loss when a dealer buy a bad car that canot be resold: $5000 (-5000 FN)
## Opportunity cost of passing upon a good car: $600 (TN +6000)

## Before the cost sensitive learning: decision tree from c50 package
## Assume we already have the data imported and partioned randomly for n times
## Define the variables saving the sum of the cell values

TP_sum_before <- 0
FP_sum_before <- 0
TN_sum_before <- 0
FN_sum_before <- 0
n <- 3
library(C50)

for(x in 1:n)
{
  ## Building decision tree model for n times
  DT_model <- C5.0(IsBadBuy ~., data = datTrain[[x]])
  ## generate the prediction for testing model
  DT_prediction <- predict(DT_model,datTest[[x]])
  ##Generate confusion matrix based on the predictions
  library(caret)
  DT_matrix <- confusionMatrix(DT_prediction, datTest[[x]]$IsBadBuy, positive = "Yes", 
                               dnn = c("prediction", "True"))
  #print(DT_matrix)
  TP_sum_before <- TP_sum_before + DT_matrix$table[2,2] ## [2,2]Index number of true positive
  FP_sum_before <- FP_sum_before + DT_matrix$table[2,1]
  TN_sum_before <- TN_sum_before + DT_matrix$table[1,1]
  FN_sum_before <- FN_sum_before + DT_matrix$table[1,2]
  
}

## Show the values in the variables:
TP_sum_before
FP_sum_before
TN_sum_before
FN_sum_before

## Calculate average of the cell values

TP_average_before <- TP_sum_before/n
FP_average_before <- FP_sum_before/n
TN_average_before <- TN_sum_before/n
FN_average_before <- FN_sum_before/n

## Overall net benefit

ONB <- TN_average_before*600 - FN_average_before*5000 - FP_average_before*600

## Net benefit per car
ONB/(TP_average_before + FP_average_before + TN_average_before + FN_average_before)

## Define the cost matrix
matrix_dimnames <- list(c("predicted No", "predicted Yes"), c("No","Yes"))
cost_matrix <- matrix(c(0,1,10,0), nrow = 2, dimnames = matrix_dimnames)
cost_matrix

## Building cost sensitive decision tree models

TP_sum_cost <- 0
FP_sum_cost <- 0
TN_sum_cost <- 0
FN_sum_cost <- 0
n <- 3
library(C50)

for(x in 1:n)
{
  ## Building decision tree model for n times
  DT_cost_model <- C5.0(IsBadBuy ~., data = datTrain[[x]], cost = cost_matrix)
  ## generate the prediction for testing model
  DT_cost_prediction <- predict(DT_model,datTest[[x]])
  ##Generate confusion matrix based on the predictions
  library(caret)
  DT_cost_matrix <- confusionMatrix(DT_cost_prediction, datTest[[x]]$IsBadBuy, positive = "Yes", 
                               dnn = c("prediction", "True"))
  print(DT_cost_matrix)
  TP_sum_cost <- TP_sum_cost + DT_cost_matrix$table[2,2] ## [2,2]Index number of true positive
  FP_sum_cost <- FP_sum_cost + DT_cost_matrix$table[2,1]
  TN_sum_cost <- TN_sum_cost + DT_cost_matrix$table[1,1]
  FN_sum_cost <- FN_sum_cost + DT_cost_matrix$table[1,2]
  
}

## After the cost sensitive learning
## overall net benefit (benefit - cost)
ONB_cost <- TN_sum_cost/n*600 - FN_sum_cost/n*5000 - FP_sum_cost/n*600

## Average onb
ONB_cost/(TN_sum_cost/n + FN_sum_cost/n + FP_sum_cost/n + TP_sum_cost/n)
