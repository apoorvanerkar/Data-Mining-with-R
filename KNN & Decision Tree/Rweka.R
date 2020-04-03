##K Nearest Neighbor using R Weka
##Import blood donation data
donation <- read.csv(file.choose())

##Check the structure and summary of the data
str(donation)
summary(donation)

##Transform the target variable to factor type
donation$B <- factor(donation$B)

##Partition data to training and testing
##the first 50% of examples as training
##the continous 25% of examples as test1 and the rest 25% of examples as test2
traindata_donation <- donation[1:374, ]
testdata1_donation <- donation[375:561, ]
testdata2_donation <- donation[562:748, ]

##Check how many rows in training, test1, test2
nrow(traindata_donation)
nrow(testdata1_donation)
nrow(testdata2_donation)
##374 in training, 187 in test1 and 187 in test2

##Load the RWeka Package
library(RWeka)

##Use training data to build the knn model
donation_model_knn <- IBk(B ~ ., data = traindata_donation, control = Weka_control(K = 20, X = TRUE))
donation_model_knn
##16 nearest neighbors for classification

##Giving weight to nearest neighbor: the inverse of their distance (use when k >1)
donation_model_knn_I <- IBk(B ~ ., data = traindata_donation, control = Weka_control(K = 20, X = TRUE, I = TRUE))
donation_model_knn_I
##12 nearest neighbors for classification

##Giving weight to nearest neighbor: 1- their distance (use when k >1)
donation_model_knn_F <- IBk(B ~ ., data = traindata_donation, control = Weka_control(K = 20, X = TRUE, F = TRUE))
donation_model_knn_F
##16 nearest neighbors for classification

##Create predictions for test1 and test2
##Use the first model: donation_model_knn
knn_prediction1 <- predict(donation_model_knn, testdata1_donation)
knn_prediction1
library(rminer)
mmetric(testdata1_donation$B, knn_prediction1, c("ACC", "PRECISION", "TPR", "F1"))
##See which class is class 1
str(knn_prediction1)
##Class 1 is "0" and Class 2 is "1"
##Overall accuracy:76.68
##Precision for class "0" is 81.55%; Precision for class "1" is 63.16%
##Recall for class "0" is 95.14%; For class "1" is 27.9%
##F measure for class "0" is 87.82%; For class "1" is 38.71%
##It is not a good model, because it performs only good for predicting class "0", but not class "1"

##Use the second testing sample
library(caret)
library(rminer)
knn_prediction2 <- predict(donation_model_knn, testdata2_donation)
knn_prediction2
confusionMatrix(knn_prediction2, testdata2_donation$B, positive = "1", dnn = c("True", "Prediction"))
mmetric(testdata2_donation$B, knn_prediction2, c("ACC", "PRECISION", "TPR", "F1"))
##See which class is class 1
str(knn_prediction2)
##Class 1 is "0" and Class 2 is "1"
##There's no huge difference between test1 and test2
##If we are trying to predict class "1" using this model, it is not a good model
##If we are trying to predict class "0" using this model, it is a good model;