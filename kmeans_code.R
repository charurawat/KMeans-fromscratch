
# ---------------------------------------------------------------------------------------------------------

# install packages
library(dplyr)
library(ggplot2)
library(tidyr)

# get IRIS data
require("datasets")
data("iris") # load Iris Dataset
str(iris) # view structure of dataset

iris.new <- iris[,c(1,2,3,4)] # excluding last column because unsupervised learning...
head(iris.new)

# normalize the data 
normalize <- function(x){
  return ((x-min(x))/(max(x)-min(x)))
}

iris.new$Sepal.Length<- normalize(iris.new$Sepal.Length)
iris.new$Sepal.Width<- normalize(iris.new$Sepal.Width)
iris.new$Petal.Length<- normalize(iris.new$Petal.Length)
iris.new$Petal.Width<- normalize(iris.new$Petal.Width)
head(iris.new)

iris_kdata <- iris.new[,c(1,2)] # keeping only the first 2 columns
head(iris_kdata) 

# ------------------------------------------------------------------------------------------------------

"
Steps followed - 
1. Randomly created k points for starting cluster centers or centroids
2. Each point in the dataset is assigned to the cluster of the nearest centroid based on closest euclidean distance
For every point in data and every centroid, while any point changes cluster assignment do the foll - 
3. Calculated the distance between the centroid and point
4. Assigned the point to the cluster with the min distance
5. For every cluster:
   - calculated  mean of the points in that cluster; assigned this as new coordinates of centroid
6. Repeated until dataset cluster assignments do not change (dist_error = 0) or reaches convergnce..

"

# defining distance function based on euclidian distance
euclid_dist <- function(x, y) {
  distance = 0  
  # loops through number of columns in x
  for (i in (1:length(x))) {  
    distance = distance + (x[[i]] - y[[i]])^2                                                    
  }
  distance = sqrt(distance)  
  return (distance)
}

# main function that calculates k nearest clusters, argument - data and clusters
cluster.kmeans  <- function(km_data, k) {
  cluster_init <- km_data[sample.int(nrow(km_data),k),] # initializing random cluster datapoints 
  cluster_init_old <- as.data.frame(matrix(0, ncol = 2, nrow = k)) # store old cluster coordinates
  cluster_loc <- vector("numeric", nrow(km_data)) # store datapoint and its cluster assignment
  clu_assign = as.data.frame(matrix(0, ncol = 2, nrow = nrow(km_data)))
  
  # dist_err: the distance between old and new cluster location 
  # algorithm reaches convergence when clusters no longer change (dist_err = 0)
  dist_err <- sum(euclid_dist(cluster_init, cluster_init_old))
  # while loop will continue until dist_err is not greater than 0 or convergence not reached
  while (dist_err > 0) {
    euc_dist = vector("numeric", nrow(cluster_init)) # vector to store distances between points and clusters 
    for (i in 1:nrow(km_data)) { # for loop to assign point to closest cluster
      euc_dist <- euclid_dist(km_data[i,], cluster_init[1:k,])  # distance between each point and the clusters 
      cluster_loc[i] <- which.min(euc_dist) # select min distance                  
    }
    cluster_init_old <- cluster_init # assigns old cluster locations before modifying 
    # iterate through cluster assignments 
    for (i in 1:k) {
      cluster_length <- length(which(cluster_loc == 1)) # determines df size based on cluster selections 
      clu_assign = as.data.frame(matrix(0, ncol = 2, nrow = nrow(km_data))) # resets points df with 0 after every iteration
      for (j in 1:nrow(km_data)) { # iterate through each row
        if (cluster_loc[j] == i) { # checks if the cluster assignment matches 
          clu_assign[j,] <- km_data[j,]
        }
      }
      clu_assign[clu_assign == 0] <- NA # replace 0 with NA
      clu_assign <- na.omit(clu_assign) # omits NA value    
      cluster_init[i,1] <- mean(clu_assign[,1])  # calculates the mean of all x coords
      cluster_init[i,2] <- mean(clu_assign[,2]) 
      cluster_init[cluster_init == 0] <- NA  
      cluster_init <- na.omit(cluster_init) # new location for k cluster  
      #  recalcuates the dist_err from new cluster location and old cluster location 
      #  after # itertion, will converge and ist_err = 0
      dist_err <- sum(euclid_dist(cluster_init, cluster_init_old)) # re-assign
    }
  }
  # dataframe to store within sum of squares for each cluster 
  wss = as.data.frame(matrix(0, ncol = 1, nrow = nrow(cluster_init)))
  # loop based on number of clusters 
  for (i in 1:k) {
    # wss: Total distance of data points from their respective cluster centroids 
    wss_intv = as.data.frame(matrix(0, ncol = 1, nrow = nrow(km_data)))   # store distances of individual datapoints to final cluster coordinates
    for (j in 1:nrow(km_data)) {
      if (cluster_loc[j] == i)  # if point in cluster then calc euc dist between point and cluster
        wss_intv[j,] <-  euclid_dist(km_data[j,], cluster_init[k,]) 
    }
    wss_intv[wss_intv == 0] <- NA    
    wss_intv <- na.omit(wss_intv)       
    wss[k,] <- sum(wss_intv)           
  }
  # sums all within sum of squares to calculate total within sum of squares
  totalss <- sum(wss) 
  return(list(twss = totalss, cluster_loc = cluster_loc))
}

# ------------------------------------------------------------------------------------------------------
# when k = 1
# Run function on data and plot
res_kmeans <- cluster.kmeans (iris_kdata, 1)
ggplot(iris_kdata, aes(x = Sepal.Length, y = Sepal.Width, color = factor(res_kmeans$cluster_loc))) + geom_point() + labs(color = "Cluster") + ggtitle("Iris dataset - Kmeans Clustering")
res_kmeans$twss # total within var

# when k = 2
# Run function on data and plot
res_kmeans <- cluster.kmeans (iris_kdata, 2)
ggplot(iris_kdata, aes(x = Sepal.Length, y = Sepal.Width, color = factor(res_kmeans$cluster_loc))) + geom_point() + labs(color = "Cluster") + ggtitle("Iris dataset - Kmeans Clustering")
res_kmeans$twss # total within var

# when k = 3
# Run function on data and plot
res_kmeans <- cluster.kmeans (iris_kdata, 3)
ggplot(iris_kdata, aes(x = Sepal.Length, y = Sepal.Width, color = factor(res_kmeans$cluster_loc))) + geom_point() + labs(color = "Cluster") + ggtitle("Iris dataset - Kmeans Clustering")
res_kmeans$twss # total within var

# when k = 4
# Run function on data and plot
res_kmeans <- cluster.kmeans (iris_kdata, 4)
ggplot(iris_kdata, aes(x = Sepal.Length, y = Sepal.Width, color = factor(res_kmeans$cluster_loc))) + geom_point() + labs(color = "Cluster") + ggtitle("Iris dataset - Kmeans Clustering")
res_kmeans$twss # total within var

# when k = 5
# Run function on data and plot
res_kmeans <- cluster.kmeans (iris_kdata, 5)
ggplot(iris_kdata, aes(x = Sepal.Length, y = Sepal.Width, color = factor(res_kmeans$cluster_loc))) + geom_point() + labs(color = "Cluster") + ggtitle("Iris dataset - Kmeans Clustering")
res_kmeans$twss # total within var

# when k = 6
# Run function on data and plot
res_kmeans <- cluster.kmeans (iris_kdata, 6)
ggplot(iris_kdata, aes(x = Sepal.Length, y = Sepal.Width, color = factor(res_kmeans$cluster_loc))) + geom_point() + labs(color = "Cluster") + ggtitle("Iris dataset - Kmeans Clustering")
res_kmeans$twss # total within var

# when k = 7
# Run function on data and plot
res_kmeans <- cluster.kmeans (iris_kdata, 7)
ggplot(iris_kdata, aes(x = Sepal.Length, y = Sepal.Width, color = factor(res_kmeans$cluster_loc))) + geom_point() + labs(color = "Cluster") + ggtitle("Iris dataset - Kmeans Clustering")
res_kmeans$twss # total within var

# when k = 8
# Run function on data and plot
res_kmeans <- cluster.kmeans (iris_kdata, 8)
ggplot(iris_kdata, aes(x = Sepal.Length, y = Sepal.Width, color = factor(res_kmeans$cluster_loc))) + geom_point() + labs(color = "Cluster") + ggtitle("Iris dataset - Kmeans Clustering")
res_kmeans$twss # total within var

# when k = 9
# Run function on data and plot
res_kmeans <- cluster.kmeans (iris_kdata, 9)
ggplot(iris_kdata, aes(x = Sepal.Length, y = Sepal.Width, color = factor(res_kmeans$cluster_loc))) + geom_point() + labs(color = "Cluster") + ggtitle("Iris dataset - Kmeans Clustering")
res_kmeans$twss # total within var

# when k = 10
# Run function on data and plot
res_kmeans <- cluster.kmeans (iris_kdata, 10)
ggplot(iris_kdata, aes(x = Sepal.Length, y = Sepal.Width, color = factor(res_kmeans$cluster_loc))) + geom_point() + labs(color = "Cluster") + ggtitle("Iris dataset - Kmeans Clustering")
res_kmeans$twss # total within var
