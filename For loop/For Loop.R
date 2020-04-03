## LAB 4
 
## 3.	Use for loop to randomly partition and store the carAuction dataset for one training sample 
#   (70% of the total sample size) and two test samples 1 and 2(15% of the total sample size each) for 10 times. 
#   Use the seed number 100, 200, ., to 1000 for the random partition. Define a variable to store the total 
#   testing sample 1 size in the loop. Outside the loop, calculate the average testing sample 1 size from the 10 iterations.

CarAuctionDat <- read.csv(file.choose())
str(CarAuctionDat)

library(caret)
datTrain <- list()
datTest1 <- list()
datTest2 <- list()
total <- 0
n <- 10

for (i in 1:n) {
  seednumber = i * 100
  set.seed(seednumber)
  print(paste("Run number is",i, " and the set seed number is", seednumber))
  InTrain <- createDataPartition(CarAuctionDat$IsBadBuy, p = 0.7, list = FALSE)
  datTrain[[i]] <- CarAuctionDat[InTrain,]
  datTest <- CarAuctionDat[-InTrain,]
  InTest <- createDataPartition(datTest$IsBadBuy, p = 0.5, list = FALSE)
  datTest1[[i]] <- datTest[InTest,]
  datTest2[[i]] <- datTest[-InTest,]
  datTest1_observations <- nrow(datTest1[[i]])
  total <- total + datTest1_observations
  print(paste("Total testing sample1 size is ", total))
}

avg <- total / n
print(avg)

