## Support Vector Machine

## install the package
install.packages("kernlab")
library(kernlab)

## the function for support vector machine
svm_model <- ksvm(target variable~ predictors, data = traindata)

svm_model<-ksvm(target variable ~ predictors, data = traindata,
                kernel = "rbfdot", C = penalties for the examples within the margins)
