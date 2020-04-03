## Hierarchical clustering

## Import the data

unemployment <- read.csv(file.choose())
str(unemployment)

## Remove the first column from the original data. (Calculate distance based on two columns)
unemployment_num <- unemployment[,2:3]
unemployment_num <- unemployment[,-1]
unemployment_num <- unemployment[,c(2,3)]
unemployment_num <- unemployment[,c("mean","stddev")]

##Calculate distances between any two objects
d <- dist(unemployment_num)

## Generate the hierarchical clusters
hier_clusters <- hclust(d)

## Generate hierarchical clusters using one command
hier_clusters <- hclust(dist(unemployment[,-1]))

## By default, it is complete link: farthest distance as the similarity
## Plot the hier_clusters
plot(hier_clusters)

#k =3: three clusters
k=3
clusterCut <- cutree(hier_clusters,k)
clusterCut

## Assign the cluster IDs back to the orginal data
unemployment$clusterID <- clusterCut
table(unemployment$clusterID)

##Change closest distance as the similarity calculation
cluster_s <- hclust(d, method = "single")
#single is the closest path as the distance
plot(cluster_s)

## Use average diatance as the similarity calculation
cluster_a <- hclust(d, method = "average")
plot(cluster_a)

## Order the states by cluster ID
orderindex <- order(unemployment$clusterID)
cluster_state_inorder <- data.frame(state = unemployment$state[orderindex],
                                    cluster = unemployment$clusterID[orderindex])
cluster_state_inorder
