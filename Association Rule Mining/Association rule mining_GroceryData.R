## Association rule mining

## Install the package
install.packages("arules")

##Load a package
library(arules)

## Read the file into R
groceries <- read.transactions(file.choose(), sep=",", rm.duplicates=FALSE)
?read.transactions

## SUmmary of the transaction matrix
summary(groceries)

##  Look at the First five transactions
inspect(groceries[1:5])

## Check the Frequency of items or (Support)
itemFrequency(groceries[,1:5]) # support # sorted in aplhabetical order

## Plot the frequency/support giving different conditions
itemFrequencyPlot(groceries, topN = 20) #by default in decending order
itemFrequencyPlot(groceries, support=0.1) # support greater than 10%

## a visualization of the sparse matrix for the first 5 transactions
image(groceries[1:5]) # empty space represent all zeros, 1 are all items

##Randomly select 100 transactions
image(sample(groceries, 100))

## Generate association rules
apriori(groceries)

## Set new benchmarks (checking both sides)
groceryrules <- apriori(groceries, parameter = 
                          list(support = 0.006, confidence = 0.25, minlen = 2))
groceryrules

## Summary of the rules
summary(groceryrules)

## Check the first 3 rules
inspect(groceryrules[1:3])

## Sort the rules and show the rule sin descending order by certain benchmark
## Sort the rules by confidence and inspect
inspect(sort(groceryrules, by = "confidence")[1:5])
inspect(sort(groceryrules, by = "support")[1:5])

## Find all the rules containing any berry items
berryrules <- subset(groceryrules, items %in% "berries")
berryrules
inspect(berryrules)

## Save the rules into csv
write(groceryrules, 
      file = "groceryrules.csv", 
      sep = "," , row.names=F)

rules_df <- as(groceryrules,"data.frame")
str(rules_df)



install.packages("RWeka")
install.packages("kmeans")
