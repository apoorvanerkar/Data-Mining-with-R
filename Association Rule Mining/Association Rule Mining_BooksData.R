# Lab 1

## 1.	Load the data into R in a routine way. Remember to set "header = FALSE" in the parameter so R will not look for the column headers. 
book <- read.csv(file.choose(), header = FALSE, sep =",")


## 2.	Summarize the book data loaded in R. How many vectors "V" does R automatically create?
summary(book)
# In total we automatically created 55 vectors


## 3.	Load the book data into a sparse matrix. Remember to remove the duplicate records using "rm.duplicates = TRUE" when you load the data. 
#Summarize the matrix using summary (). 
#How many columns/items/books are included in this matrix? What is the density of this matrix and what does density mean in this scenario? 
install.packages("arules")
library(arules)
book <- read.transactions(file.choose(), sep = ",", rm.duplicates = TRUE)
summary(book)
# Columns = 7787
# Density = 0.0008099037
# It means that from this huge books table, only 0.08 % of the cells in this matrix are actually having value and the value is 1.


## 4.	Check the most frequent items. Which book has the highest frequency? Inspect the itemset length distribution. 
#  How many users rated/purchased 20 books in the matrix?

# The most frequest items are -
  #a painted house : 286     
  #a time to kill : 268
  #1st to die: a novel : 208        
  #2nd chance : 200
  #a walk to remember : 165  

#Book with highhest frequency is - a painted house : 286 

# Above 15 users out of all read/purchased 20 books in the matrix.


## 5.	Look at the first five transactions/users' book information. Do the records match the first five rows from the original data set? 
#Use itemFrequency to check the proportion of certain books. What are the proportions of books in column from 1 to 5? 

inspect(book[1:5]) # look at first 5 transactions
# Yes, the records match the first five rows from the original data set.

itemFrequency(book[,1:5]) # examine frequency of first 5 items
# 0.0287% of the users have actually read or purchased the books


## 6.	Create a histogram plotting the books have more than 5 percent support. How many books in the matrix have at least 5 percent support? Which book has the third highest support among the books shown in the histogram? Plot the first 10 books with highest support. Which book has the 8th highest support?

itemFrequencyPlot(book, support=0.05) # Plot frequency of items
# 4 books in the matrix have at least 5 percent support.
# "1st to die: a novel" has the third highest support among the books shown in the histogram.

itemFrequencyPlot(book, topN = 10)
# 'a prayer for owen meany' has the 8th highest support.


## 7.	Use apriori () function to generate association rules on the matrix. Use default parameters to generate association rules. How many rules have been generated? 

apriori(book)
# Zero rules have been generated.

## 8.	Adjust the parameters: support = 0.01, confidence = 0.05 and minlen = 2. How many association rules have been generated?

bookrules <- apriori(book, parameter = list(support = 0.01, confidence = 0.05, minlen = 2))
# 12 rules have been generated.

bookrules # Also check the number of rules by calling the name 'bookrules'


## 9.	Summarize the information of association rules from Q9. How many rules have size 3? What are the means of support, confidence and lift? 

summary(bookrules)
# Zero rules have size 3
# Means for support : 0.01465 ; Means for Confidence : 0.2193 ; Means for lift : 3.278.


## 10.	Check the first ten rules. If a user likes "a painted house", which books else should be recommended to this user? 
#Sort the rule list by lift and inspect the first ten rules with highest lift. Which rule has the 3rd highest lift?

inspect(bookrules[1:10]) # look at first 10 rules

# If a user likes "a painted house", books that should be recommended to this user are
  # {a painted house}     => {1st to die: a novel} 
  # {a painted house}     => {2nd chance}

inspect(sort(bookrules, by = "lift")[1:10]) # sorting bookrules by lift

# Rule that has the 3rd highest lift -
  # {a painted house}     => {a time to kill} 


## 11.	Find subsets of rules containing any "1st to die: a novel". How many rules include "1st to die: a novel"? 
#Find subsets of rules containing any "a time to kill". How many rules include "a time to kill"? Sort the "a time to kill" rules by support. 
#Which rule has the 3rd highest support? If a user likes "a time to kill", which book should be recommended to him/her according to the rule 
#with highest support?

firstrules <- subset(bookrules, items %in% "1st to die: a novel")
inspect(firstrules)
# 6 rules include "1st to die: a novel"

timerules <- subset(bookrules, items %in% "a time to kill")
inspect(timerules)
# 6 rules include "a time to kill"

inspect(sort(timerules, by="support"))
# Rule that has the 3rd highest support -
  # {2nd chance}          => {a time to kill}      0.01206203 0.2100000  2.728433 42

# "a painted house" should be recommended if a user likes "a time to kill", to him/her according to the rule with highest support


## 12.	You can also save the rules into a csv files and convert the rules into a data frame. Check the variables of this data frame. 
# What is the maximal value of Lift?

write(bookrules, file = "bookrules.csv",
      sep = "," , quote = TRUE, row.names= FALSE)

bookrules_df <- as(bookrules,"data.frame")
str(bookrules_df)
summary(bookrules_df$lift)      
# The maximum value of lift is 6.864


# Apriori algorithm

# 1.	Giving the following large 3 itemsets, what are the candidate 4 itemsets?
#{1,5,6}
#{1,2,3}
#{2,5,7}
#{1,5,7}
#{2,4,9}
#{2,3,6}
#{1,5,6,7},{1,2,3,6},{1,2,5,7}

# 2.	Giving the following large 4 itemsets, what are the candidate 5 itemsets?
#{1,2,3,6}
#{1,2,3,7}
#{1,2,5,6}
#{2,3,7,9}
#{1,2,8,0}
#{1,2,3,6,7},{1,2,3,5,6},{1,2,3,7,9}

