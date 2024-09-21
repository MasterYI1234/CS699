data <- read.csv("/Users/aminabauyrzan/Downloads/project_dataset.csv")

library(caTools)

set.seed(123)  # Set a random seed for reproducibility
split <- sample.split(data$o_bullied, SplitRatio = 0.8)
training_data <- data[split, ]
testing_data <- data[!split, ]


write.csv(training_data, "/Users/aminabauyrzan/Desktop/CS699/training_data.csv", row.names = FALSE)
write.csv(testing_data, "/Users/aminabauyrzan/Desktop/CS699/testing_data.csv", row.names = FALSE)

data<- testing_data

head(data)
summary(data)
str(data)

# Check for missing values
missing_values <- sum(is.na(data))

# Impute missing values or remove rows/columns
data <- na.omit(data)  # Remove rows with missing values

# Calculate variances for each column
variances <- apply(data, 2, var)

# Find the indices of columns with non-zero variance
library(caret)
# Check for near-zero variance columns
nzv_cols <- nearZeroVar(data, names = TRUE)
nzv_cols

# Remove columns with near-zero variance
data_clean <- data[, !names(data) %in% nzv_cols]

# Calculate the correlation matrix
corr_matrix <- cor(data_clean)

highCorr <- findCorrelation(corr_matrix, cutoff = 0.7, names = TRUE)

data1 <- data_clean[, -which(names(data_clean) %in% highCorr)]

data_initial<- data
data <- data1


# Load necessary packages
library(rJava)
library(C50)  # For CFS
library(Boruta)  # For Boruta
library(FSelector)  # For Information Gain
library(Formula)
library(rpart)

# CFS
cfs_subset <- cfs(o_bullied ~., data)
subset.cfs <- as.simple.formula(cfs_subset, "o_bullied")
subset.cfs

f <- as.simple.formula(data, "o_bullied")

# Boruta
boruta_result <- Boruta(o_bullied ~ ., data)
boruta_subset <- getSelectedAttributes(boruta_result)
boruta_result
# PCA
pca_result <- prcomp(data[, -data$o_bullied], scale = TRUE)
num_pca_components <- 10  # Choose the number of components you want to keep
pca_subset <- pca_result$x[, 1:num_pca_components]

# Information Gain
#df <- as.data.frame(unclass(data), stringsAsFactors = TRUE)
subset.infogain <- information.gain(o_bullied ~ . , data)
sorted.features[1:10] # top 10


info_gain <- information.gain(o_bullied ~ ., data)
info_gain_subset <- head(sort(info_gain, decreasing = TRUE), num_features_to_keep)
info_gain_subset <- names(info_gain_subset)


# Assuming 'target_column' is the column index or name of the target variable
subset_cfs <- data[, c(cfs_subset, target_column)]
subset_boruta <- data[, c(boruta_subset, target_column)]
subset_pca <- cbind(pca_subset, data[, target_column])
subset_info_gain <- data[, c(info_gain_subset, target_column)]


