#Qustion1:
data <- data.frame(
  ID = c('a', 'b', 'c', 'd', 'e', 'f', 'g'),
  x = c(3, 7, 5, 4, 1, 5, 6),
  y = c(6, 2, 6, 5, 5, 3, 7),
  Cluster = c(1, 1, 1, 1, 2, 2, 2)
)

# Calculate Manhattan distance
manhattan_distance <- function(x1, y1, x2, y2) {
  abs(x1 - x2) + abs(y1 - y2)
}

# Function to assign clusters based on Manhattan distance
assign_clusters <- function(data, centroids) {
  apply(data[,c('x', 'y')], 1, function(row) {
    distances <- apply(centroids, 1, function(centroid) {
      manhattan_distance(row[1], row[2], centroid['x'], centroid['y'])
    })
    return(which.min(distances))
  })
}

# Calculate initial centroids
centroids <- aggregate(cbind(x, y) ~ Cluster, data, median)

# Assign points to the nearest centroids
data$Cluster <- assign_clusters(data, centroids)

# Recalculate the centroids after reassigning the points
new_centroids <- aggregate(cbind(x, y) ~ Cluster, data, median)

# Print out the updated data with new cluster assignments
print(data)




#Question3:
library(factoextra)
library(cluster)

data <- read.csv('hw8_p3.csv')

# Standardize data
data_scaled <- scale(data)

wss_values <- numeric(10)
for (k in 1:10) {
  set.seed(123)
  kmeans_result <- kmeans(data_scaled, centers = k, nstart = 25)
  wss_values[k] <- kmeans_result$tot.withinss
}

wss_values

# Plot the elbow method
fviz_nbclust(data_scaled, kmeans, method = "wss")

# Compute average silhouette method
silhouette_scores <- fviz_nbclust(data_scaled, kmeans, method = "silhouette")

print(silhouette_scores)


set.seed(31)
k2 <- kmeans(data_scaled, centers = 2, nstart = 25)

cluster_stats <- data.frame(cluster=rep(1:2, each=5), 
                            Variable=rep(colnames(data_scaled), 2),
                            Mean=numeric(10), Max=numeric(10), Min=numeric(10))

# Calculate the mean, max, and min for each variable in each cluster
for(i in 1:2){
  for(variable in colnames(data_scaled)){
    cluster_data <- data_scaled[k2$cluster == i, variable]
    cluster_stats[cluster_stats$cluster == i & cluster_stats$Variable == variable, "Mean"] <- mean(cluster_data)
    cluster_stats[cluster_stats$cluster == i & cluster_stats$Variable == variable, "Max"] <- max(cluster_data)
    cluster_stats[cluster_stats$cluster == i & cluster_stats$Variable == variable, "Min"] <- min(cluster_data)
  }
}

cluster_stats_wide <- reshape(cluster_stats, timevar = "Variable", idvar = "cluster", direction = "wide")

print(cluster_stats_wide)




#Question4
library(cluster)
library(factoextra)

data <- read.csv("hw8_p4.csv")

# Convert character columns to factors
data[] <- lapply(data, function(x) {
  if(is.character(x)) factor(x) else x
})

# Compute the Gower distance matrix
gower_dist <- daisy(data, metric = "gower")

# Initialize a vector to store the average silhouette widths
avg_sil_width <- numeric(9)

# Run the PAM algorithm for k=2 to k=10 and calculate silhouette scores
for(k in 2:10) {
  pam_fit <- pam(gower_dist, diss = TRUE, k = k)
  sil_width <- silhouette(pam_fit)
  avg_sil_width[k - 1] <- mean(sil_width[, 'sil_width'])
}

# Find the k value corresponding to the largest average silhouette width
optimal_k <- which.max(avg_sil_width) + 1

# Plot the silhouette scores
plot(2:10, avg_sil_width, type='b', pch=19, frame=FALSE, 
     xlab="Number of clusters k", ylab="Average Silhouette Width")
title(main="Silhouette Method for Optimal k")

# Print the optimal k value
print(paste("The optimal number of clusters k is:", optimal_k))



#Question5
library(cluster)

data <- read.csv('hw8_p5.csv')

# Convert categorical columns to factors
categorical_columns <- c("job", "marital", "education", "housing", "loan", "contact")
data[categorical_columns] <- lapply(data[categorical_columns], as.factor)

# Calculate the Gower distance
gower_dist <- daisy(data, metric = "gower")

# Execute the clustering algorithm
methods <- c("average", "single", "complete", "ward")
agnes_models <- lapply(methods, function(m) agnes(gower_dist, method = m))

# Collect and plot the agglomerative coefficients
agglomerative_coefficients <- sapply(agnes_models, function(model) model$ac)
barplot(agglomerative_coefficients, main="Agglomerative Coefficients", names.arg = methods, ylab = "Coefficient")

# From the chart, we see that the ward method is the best
best_method <- "ward"

# Cluster using the best method
best_model <- agnes(gower_dist, method = best_method)

# Cut the tree with k = 3. 
clusters <- cutree(as.hclust(best_model), k = 3)

# Output
cluster_list <- list()
for (k in 1:max(clusters)) {
  cluster_rows <- which(clusters == k)
  cluster_list[[k]] <- cluster_rows
  cat(paste("Cluster", k, "rows:", toString(cluster_rows)), "\n")
}
