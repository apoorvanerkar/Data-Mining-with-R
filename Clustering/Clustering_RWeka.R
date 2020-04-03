## Kmeans clustering using Rweka package
install.packages("RWeka")

## Load the package
library(RWeka)

## Import data
bartrider2 <- read.csv(file.choose())
str(bartrider2)

## Kmeans clustering
k =2
clustering_Rweka <- SimpleKMeans(bartrider2,Weka_control(N = k , V = TRUE))
clustering_Rweka

## Use Manhattan distance instead
clustering_Manhattan <- SimpleKMeans(bartrider2, Weka_control(N = k,V = T, A = 
                                                                "weka.core.ManhattanDistance"))
clustering_Manhattan

