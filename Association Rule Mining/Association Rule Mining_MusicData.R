# Assignment 2: Association Rule Mining

# (1). Load the music data into a sparse matrix. Since each user may mention one artist multiple times, we set "rm.duplicates=TRUE" when we load the data into a sparse matrix.

library(arules)
music <- read.transactions(file.choose(), sep = ",", rm.duplicates = TRUE)


# (2). Use summary () function to inspect the sparse matrix. How many users and how many columns does this matrix have? 
#      What does density mean? What is the most frequent artist/item? How many rows/users have 30 items/artists? 

summary(music)

# How many users and how many columns does this matrix have?
  # Ans. This matric has 14593 users and 1004 columns.

# What does density mean?
  # Ans. From the huge matrix/table, only 1.97% of the cells in this matrix is having value and the value is 1.

# What is the most frequent artist/item?
  # Ans. "radiohead" is the most frequent artist/item with frequency fo 2703.

# How many rows/users have 30 items/artists?
  # Ans. 340 rows/users have 30 items/artists.


# (3). Inspect the first 5 users' artists. Do the records match the first five rows of the original csv file? Use itemFrequency to check the frequencies/supports of users' artists in column from 4 to 7.

# Inspect the first 5 users' artists.
inspect(music[1:5])

# Do the records match the first five rows of the original csv file?
  # Ans: Yes, the records match the first five rows of the original csv file.

# Use itemFrequency to check the frequencies/supports of users' artists in column from 4 to 7.

  itemFrequency(music[,4:7])
  
  #  3 doors down 30 seconds to mars                311      36 crazyfists 
  #  0.031796067        0.033714795        0.008497225        0.008086069 
  
  
# (4). Create a histogram plotting the artists have more than 10 percent support (itemFrequencyPlot). How many artists in the matrix have at least 10 percent support? Plot the first 20 artists with highest support. Which artist has the 15th highest support?
  
# Create a histogram plotting the artists have more than 10 percent support (itemFrequencyPlot).
  itemFrequencyPlot(music, support=0.10)

# How many artists in the matrix have at least 10 percent support? 
  # Ans: 10 artists in the matrix have at least 10 percent support.

# Plot the first 20 artists with highest support. 
  itemFrequencyPlot(music, topN = 20)

# Which artist has the 15th highest support?
  # Ans: "Placebo" artist has the 15th highest support.
  
# (5). Generate a visualization (image) of the sparse matrix for the first 100 users' preference. Then generate a visualization of a random sample of 500 users' artist selection. 
  
# Generate a visualization (image) of the sparse matrix for the first 100 users' preference.
  image(music[1:100])

# Then generate a visualization of a random sample of 500 users' artist selection. 
  image(sample(music, 500))
  
  
#  (6). Use apriori() function to train the association rules on the music data. How many rules do we have when we use the default setting? In order to learn more rules, we adjust support level to 0.01, minlen =2 and confidence level to 0.25. How many rules do we have then?
  
# Use apriori() function to train the association rules on the music data.  
  apriori(book)
  
# How many rules do we have when we use the default setting?
  # Ans. set of 0 rules have been generated.
  
# In order to learn more rules, we adjust support level to 0.01, minlen =2 and confidence level to 0.25. How many rules do we have then?
  musicrules <- apriori(music, parameter = list(support = 0.01, confidence = 0.25, minlen = 2))
  musicrules
  # Ans: 788  rules have been generated.
  
  
# (7). Summarize the rules generated from adjusted parameters. How many rules have size 3 among all the rules? Check the first ten rules. If a user likes "the pussycat dolls", which artist should online radio recommend to this user? Sort the rule list by lift and inspect the first five rules with highest lift. Which rule has the fourth highest lift?
  
# Summarize the rules generated from adjusted parameters.  
  summary(musicrules)

# How many rules have size 3 among all the rules? 
  # Ans: 3 rules have size 3 among all the rules
  
# Check the first ten rules. 
  inspect(musicrules[1:10])
  
# If a user likes "the pussycat dolls", which artist should online radio recommend to this user? 
  #Ans:
    # lhs                     rhs           support    confidence lift      count
    # {the pussycat dolls} => {rihanna}     0.01069006 0.5777778  13.092409 156  
  
# Sort the rule list by lift and inspect the first five rules with highest lift. 
  inspect(sort(musicrules, by = "lift")[1:5]) 

# Which rule has the fourth highest lift?
  # Ans:
    # lhs                     rhs       support    confidence lift     count
    # {beyoncc}            => {rihanna} 0.01432194 0.4696629  10.64253 209
  
  
# (8). Find subsets of rules containing any cold play. How many rules includes "cold play"? Sort the rules by support and inspect the first five cold play rules with highest support. What rule has the 2nd highest support?
  
# Find subsets of rules containing any cold play.   
  firstmusicrules <- subset(musicrules, items %in% "coldplay")
  inspect(firstmusicrules)
  
# How many rules includes "cold play"?
  # Ans: 172 rules includes "cold play"
  
# Sort the rules by support and 
  sort(firstmusicrules, by="support")

# Inspect the first five cold play rules with highest support
  inspect(sort(firstmusicrules, by="support")[1:5])

# What rule has the 2nd highest support?
  # Ans:
  # lhs              rhs           support    confidence lift     count
  # {radiohead}   => {coldplay}    0.05612280 0.3029967  1.860956 819
  
  
# (9). You can write these rules to a csv file and save the rules into a data frame. How many variables does this data frame contain?
  
# write these rules to a csv file and save the rules into a data frame.
  
  write(musicrules, file = "musicrules.csv",
        sep = "," , quote = TRUE, row.names= FALSE)
  
  musicrules_df <- as(musicrules,"data.frame")
  
# How many variables does this data frame contain?
  str(musicrules_df)
  # Ans : This data frame contains 5 variables.
  