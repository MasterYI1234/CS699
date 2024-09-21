#Q1:
#This need to change to your directory.
setwd("C:/Users/ASUS/Desktop/Master/R/R DATA")
data = read.csv("hw2_p1.csv")

library(ggplot2)

#(1)
# Calculate statistical values for temp
mean_temp <- mean(data$temp)
sd_temp <- sd(data$temp)
min_temp <- min(data$temp)
max_temp <- max(data$temp)

cat("Mean of temp:", mean_temp, "\n")
cat("Standard deviation of temp:", sd_temp, "\n")
cat("Minimum of temp:", min_temp, "\n")
cat("Maximum of temp:", max_temp, "\n")

(2)
# Function to calculate percentiles
percentile <- function(data, p) {
  n <- length(data)
  loc <- p * (n + 1)
  if (floor(loc) == loc) {
    return(data[loc])
  } else {
    lower <- data[floor(loc)]
    upper <- data[ceiling(loc)]
    return((lower + upper) / 2)
  }
}

# Calculate Q1, Q2, Q3 for temp
sorted_temp <- sort(data$temp)
Q1_temp <- percentile(sorted_temp, 0.25)
Q2_temp <- percentile(sorted_temp, 0.5)
Q3_temp <- percentile(sorted_temp, 0.75)

cat("25th percentile of temp:", Q1_temp, "\n")
cat("50th percentile of temp:", Q2_temp, "\n")
cat("75th percentile of temp:", Q3_temp, "\n")

#(3):
# Plot boxplot for RH
ggplot(data, aes(y = RH)) + geom_boxplot() + ggtitle("Boxplot of RH")


#(4):
# Detect outliers for wind using the IQR method
sorted_wind <- sort(data$wind)
Q1_wind <- percentile(sorted_wind, 0.25)
Q3_wind <- percentile(sorted_wind, 0.75)
IQR_wind <- Q3_wind - Q1_wind
lower_bound <- Q1_wind - 1.5 * IQR_wind
upper_bound <- Q3_wind + 1.5 * IQR_wind
outliers <- data$wind[data$wind < lower_bound | data$wind > upper_bound]

cat("Outliers in wind:", outliers, "\n")




#Q3:
O1 <- c(0, 1, 4, 1, 3, 1, 4, 1, 1, 2)
O2 <- c(2, 2, 1, 5, 0, 4, 0, 3, 5, 2)
O3 <- c(3, 0, 5, 2, 1, 3, 2, 0, 3, 4)

# Function to compute cosine similarity
cosine_similarity <- function(A, B) {
  dot_product <- sum(A * B)
  magnitude_A <- sqrt(sum(A^2))
  magnitude_B <- sqrt(sum(B^2))
  
  return(dot_product / (magnitude_A * magnitude_B))
}

# Calculate cosine similarity
#(1)
cosine_O1_O2 <- cosine_similarity(O1, O2)
print(paste("Cosine similarity between O1 and O2: ", cosine_O1_O2))

#(2)
cosine_O1_O3 <- cosine_similarity(O1, O3)
print(paste("Cosine similarity between O1 and O3: ", cosine_O1_O3))
