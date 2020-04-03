## K means clustering

## Import the data
bartrider <- read.csv(file.choose())

str(bartrider)

## CHeck the frequencies of the levels of the factor variables
table(bartrider$HomeType)
prop.table(table(bartrider$HomeType))
table(bartrider$Ethnic)
table(bartrider$Gender)
table(bartrider$MarStatus)

## Use aggregate function to check the average age for people live in different home types
aggregate(data=bartrider, Age ~ HomeType, mean)

## Transform all the factor type to dummy variables
bartrider$HomeType_dummy <- ifelse(bartrider$HomeType == "House",1,0)
bartrider$Gender_dummy <- ifelse(bartrider$Gender == "F",1,0)
bartrider$Ethnic_dummy <- ifelse(bartrider$Ethnic == "White",1,0)
bartrider$MarStatus_dummy <- ifelse(bartrider$MarStatus == "Married",1,0)

## Check the structure again
str(bartrider)

## Create a dataset with only numeric variables
bart_dummy <- bartrider[,c(-4,-5,-6,-8)]
bart_dummy <- bartrider[,c(-(4:6),-8)]
str(bart_dummy)

##kmeans clustering
k = 2
set.seed(100) ## can have any numeric numbers.To have same output consistently
bart_2means <- kmeans(bart_dummy, k)
bart_2means

## Which cluster does each subject belong to
bart_2means$cluster
bartrider$clusterID <- bart_2means$cluster

## Check the centers of each cluster
bart_2means$centers

## Check the within variance
bart_2means$withinss
bart_2means$tot.withinss

## Check the between variance
bart_2means$betweenss

## CHeck the cluster sizes
bart_2means$size

## Try different k values
k=3
set.seed(100)
bart_3means <- kmeans(bart_dummy, k)
bart_3means$centers

## Standardization for each of the variables
summary(bart_dummy)
bart_scale <- lapply(bart_dummy, scale) # scale is function for standardization # lapply returns a list
bart_standard <- as.data.frame(bart_scale)
summary(bart_standard)

## Try k means clustering after the standardization
k=2
set.seed(100)
bart_2means_standard <- kmeans(bart_standard,k)
bart_2means_standard$size
bart_2means$size
