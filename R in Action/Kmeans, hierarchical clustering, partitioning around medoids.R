# basic hierarchical clustering
# every observation starts as a cluster, the closest points get joined into a single cluster, and then the next two, and so on
# until every observation is in the same cluster
# hierarchical clustering works way better when the number of observations is small
data(nutrient, package = 'flexclust')
row.names(nutrient) <- tolower(row.names(nutrient))
nutrient.scaled <- scale(nutrient)                                            # remember to scale the data first
d <- dist(nutrient.scaled)                                                    # calculates the euclidean distance matrix between each pair of observations
fit.average <- hclust(d, method = 'average')                                  # does the clustering; other options: single, complete, average, centroid, ward
plot(fit.average, hang = -1, cex = 0.8, main = 'Average Linkage Clustering')

# selecting an appropriate number of clusters
library(NbClust)
devAskNewPage(ask = TRUE)
nc <- NbClust(nutrient.scaled, distance = 'euclidean', min.nc = 2, max.nc = 15, method = 'average')
table(nc$Best.n[1, ]) # after applying multiple indices, we see that 2, 3, 5 and 15 are all good options to test out

# applying the clustering
clusters <- cutree(fit.average, k = 5) # we choose 5 clusters
table(clusters)
aggregate(nutrient, by = list(cluster = clusters), median)
aggregate(as.data.frame(nutrient.scaled), by = list(cluster = clusters), median)
plot(fit.average, hang = -1, cex = 0.8, main = 'Average Linkage Clusters\n5 Cluster Solution')
rect.hclust(fit.average, k = 5) # now we can see the dendogram with our applied clusters

# basic k-means clustering
# select k centroids to be randomly placed in the multidimensional space, points are assigned to their closest centroids,
# centroids are recalculated as the average of all the data points in the cluster, reassign points again to their closest
# cluster, keep doing this until no more changes are made or a stopping criterion is reached
wssplot <- function(data, nc = 14, seed = 1234){
  wss <- (nrow(data)-1)*sum(apply(data, 2, var))
  for (i in 2:nc){
    set.seed(seed)
    wss[i] <- sum(kmeans(data, centers = i)$withinss)
  }
  plot(1:nc, wss, type = 'b', xlab = 'number of clusters', ylab = 'within groups sum of squares')
}
data(wine, package = 'rattle')
head(wine)
df <- scale(wine[-1]) # we get rid of the type column, this is the actual clusters that each wine belongs to
wssplot(df)           # from the scree plot, it looks like 3 is the number of clusters to go with
set.seed(1234)
devAskNewPage(ask = TRUE)
nc <- NbClust(df, min.nc = 2, max.nc = 15, method = 'kmeans') # using the clustering indices method chooses 3 as well
par(mfrow = c(1,1))
barplot(table(nc$Best.nc[1, ]), 
        xlab = 'number of clusters', 
        ylab = 'number of criteria', 
        main = 'number of clusters chosen by criteria')
set.seed(1234)
fit.km <- kmeans(df, 3, nstart = 25) # fits the actual k means clustering with 3 clusters
fit.km$size
fit.km$centers                       # shows the centroid values for each cluster
aggregate(wine[-1], by = list(cluster = fit.km$cluster), mean) # shows the cluster means, unscaled
ct.km <- table(wine$Type, fit.km$cluster) 
ct.km # looks like our clustering was pretty accurate here

# partitioning around medoids (PAM)
# k means is sensitive to outliers (because of the means), so PAM is essentially a more robust method
# instead of selecting points as centroids, we begin by randomly selecting k observations to be medoids,
# for every other point, calculate the distance between itself and all medoids, it will join the cluster of the closest
# medoid, after every point is assigned, calculate the total sum of distance from every point to the medoid for each cluster,
# select a point that is NOT a medoid, and make it the medoid of its cluster, then reassign every point to its nearest cluster
# again, recalculate the total sum of distance, if the distance is smaller then keep the change, otherwise revert back
library(cluster)
set.seed(1234)
fit.pam <- pam(wine[-1], k = 3, stand = TRUE) # stand means standardize; we do want to standardize the wine data
fit.pam$medoids
clusplot(fit.pam, main = 'Bivariate Cluster Plot') # takes the 2 highest principal components and graphs each point
ct.pam <- table(wine$Type, fit.pam$clustering)
ct.pam # clearly it is not as accurate as the normal k means, but this is the more robust approach and is expected

# What if the clusters aren't real?
library(fMultivar)
set.seed(1234)
df <- rnorm2d(1000, rho = 0.5) # generates 1000 points from a bivariate normal distribution with correlation 0.5
df <- as.data.frame(df)
plot(df, main = 'Bivariate Normal Distribution with rho=0.5') # there are no clusters in this data
wssplot(df) # suggests 3 clusters
library(NbClust)
nc <- NbClust(df, min.nc = 2, max.nc = 15, method = 'kmeans') # suggests 2 clusters
par(mfrow = c(1,1))
barplot(table(nc$Best.nc[1, ]), 
        xlab = 'Number of Clusters',
        ylab = 'Number of Criteria',
        main = 'Number of Clusters Chosen by 26 Criteria')
library(ggplot2)
library(cluster)
fit <- pam(df, k = 2)
df$clustering <- factor(fit$clustering)
ggplot(df, aes(x = V1, y = V2, color = clustering, shape = clustering)) +
  geom_point() +
  ggtitle('Clusting of Bivariate Normal Data') # this looks super cool but the clustering doesn't mean anything here
plot(nc$All.index[, 4], type = 'o', ylab = 'CCC', xlab = 'Number of Clusters', col = 'blue')
# this plots the cubic cluster criteria (CCC); USUALLY, when the CCC is all negative and decreasing for 2 or more clusters,
# the distribution is typically unimodal

















