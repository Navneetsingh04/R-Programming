# k-means clustering is a unsupervised learning algorithm used to partition a dataset 
# into k distinct clusters based on feature similarity.
# It works by iteratively assigning data points to the nearest cluster centroid and
# updating the centroids until convergence.
# The algorithm is sensitive to the initial placement of centroids, so it's common to
# run it multiple times with different initializations and choose the best result based
# on a chosen metric (e.g., inertia).


library(cluster)
data <- read.csv("C:/Users/Navneet singh/Downloads/Mall_Customers.csv")

# Selecting relevant features (Columns 4 and 5)
x <- data[, 4:5]  

set.seed(123)

# Elbow method to find the optimal number of clusters
wcss <- numeric(10)  # Initialize with correct size

for (i in 1:10) {
        kmeans <- kmeans(x, centers = i, nstart = 10)  # Use nstart=10
        wcss[i] <- kmeans$tot.withinss  # Store total within-cluster sum of squares
}

# Plot the Elbow method
plot(x = 1:10, y = wcss, type = "b", pch = 19, col = "blue",
     xlab = "Number of clusters", ylab = "WCSS",
     main = "Elbow Method for Optimal k")

kmeans = kmeans(x = x, centers = 5, iter.max = 300, nstart = 10)  # Use nstart=10 for better initialization

# Visualizing the clusters

clusplot(x = x,
         clus = kmeans$cluster,
         main = "Clusters of Customers",
         xlab = "Annual Income (k$)",
         ylab = "Spending Score (1-100)",
         lines = 0,
         shade = TRUE,
         color = TRUE,
         labels = 2,
         plotchar = FALSE,
         span = TRUE,
         sub = paste("K-Means with", nrow(kmeans$centers), "clusters")
)
