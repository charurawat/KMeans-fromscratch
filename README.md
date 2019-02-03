# KMeans-fromscratchinR

## Implementing k-means algorithm from scratch in R

Contains code in R with implementation of k-means clustering in R from scratch without the use of external modules or libraries.
Used the iris dataset to implement the algorithm.

Steps followed for the algorithm -
1. Randomly created k points for starting cluster centers or centroids
2. Each point in the dataset is assigned to the cluster of the nearest centroid based on closest euclidean distance
For every point in data and every centroid, while any point changes cluster assignment do the foll - 
3. Calculated the distance between the centroid and point
4. Assigned the point to the cluster with the min distance
5. For every cluster:
   - calculated  mean of the points in that cluster; assigned this as new coordinates of centroid
6. Repeated until dataset cluster assignments do not change (dist_error = 0) or reaches convergnce..
