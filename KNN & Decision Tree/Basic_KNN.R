##K Nearest Neighbor using R Weka
##Import blood donation data
donation <- read.csv(file.choose())

##Check the structure and summary of the data
str(donation)
summary(donation)

##Transform the target variable to factor type
donation$B <- factor(donation$B)

##Standardize the variable values
standardize <- lapply(donation[,1:4], scale)

##Create a new standardized data
donation_z <- as.data.frame(standardize)

##Partition data to training and testing
##the first 50% of examples as training
##the continous 25% of examples as test1 and the rest 25% of examples as test2
traindata_donation <- donation_z[1:374, ]
testdata1_donation <- donation_z[375:561, ]
testdata2_donation <- donation_z[562:748, ]

##Check how many rows in training, test1, test2
nrow(traindata_donation)
nrow(testdata1_donation)
nrow(testdata2_donation)
##374 in training, 187 in test1 and 187 in test2

##Partition the labels (target variable values) to training and testing
traindata_label <- donation[1:374, 5]
testdata1_label <- donation[375:561,5 ]
testdata2_label <- donation[562:748, 5]

##Build the model and make the predictions for the testing data
library(class)
test1_prediction1 <- knn(train = traindata_donation, test = testdata1_donation, cl=traindata_label, k = 20)
test1_prediction1
test2_prediction2 <- knn(train = traindata_donation, test = testdata2_donation, cl = traindata_label, k=20)
test2_prediction2

##Evaluate the first testing sample
install.packages("rminer")
library(rminer)
mmetric(testdata1_label, test1_prediction1, c("ACC", "PRECISION", "TPR", "F1"))
##See which class is class 1
str(test1_prediction1)

##Evaluate the second testing sample
mmetric(testdata2_label, test2_prediction2, c("ACC", "PRECISION", "TPR", "F1"))
##See which class is class 1
str(test2_prediction2)
