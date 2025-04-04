# Apply K-Means clustering to the "Mall_Customers.csv" dataset. 
# Visualize the clusters and explain how the number of clusters was chosen.

library(ggplot2)
library(cluster)

data <- read.csv("C:/Users/Navneet singh/Downloads/Mall_Customers.csv")
data$CustomerID <- NULL 
data$Genre <- factor(data$Genre)

# Select only the numeric columns we want to cluster
# (Annual Income and Spending Score)
cluster_data <- data[, c("Annual.Income..k..", "Spending.Score..1.100.")]

# Scale the data
cluster_data <- scale(cluster_data)

# Determine optimal number of clusters using Elbow Method
set.seed(123)
wcss <- vector()
for(i in 1:10) {
        kmeans_model <- kmeans(cluster_data, centers = i, nstart = 20)
        wcss[i] <- kmeans_model$tot.withinss
}

# Plot the Elbow Method
plot(1:10, wcss, type = "b", col = "blue",
     xlab = "Number of Clusters", ylab = "WCSS",
     main = "Elbow Method for Optimal k")

# From the plot, k=5 appears optimal (where the elbow bends)

# Apply K-Means with k=5
final_model <- kmeans(cluster_data, centers = 5, nstart = 20)

# Add cluster assignments to original data
data$Cluster <- as.factor(final_model$cluster)

# Visualize the clusters
ggplot(data, aes(x = Annual.Income..k.., y = Spending.Score..1.100., 
                 color = Cluster)) +
        geom_point(size = 3) +
        ggtitle("Customer Segments by Income and Spending") +
        xlab("Annual Income (k$)") +
        ylab("Spending Score (1-100)") +
        theme_minimal()

clusplot(cluster_data, 
         final_model$cluster, 
         main = "K-Means Clustering of Mall Customers", 
         color = TRUE, 
         shade = TRUE, 
         labels = 2, 
         lines = 0)

