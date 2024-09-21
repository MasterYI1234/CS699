library(ROSE)
library(caret)
library(randomForest)
library(e1071)
library(class)
library(knitr)
library(ggplot2)
library(rpart)

# Read the dataset
data <- read.csv("project_dataset.csv")

#Feature Selection
set.seed(123)
# Build a random forest model to identify important features
rf_model <- randomForest(o_bullied ~ ., data=data, importance=TRUE)


#Ensure Data Integrity
if(sum(is.na(data)) > 0) {
  stop("There are missing values in the dataset.")
} 
#No missing values.

# Select the top 20 important features
important_features <- importance(rf_model)
top_features <- rownames(important_features[order(important_features[,1], decreasing=TRUE)[1:20],])
# Select only the top features along with the target variable
data_selected <- data[, c(top_features, "o_bullied")]


# Function to detect outliers in a numeric vector
detect_outliers_in_vector <- function(x) {
  if(is.numeric(x)) {
    boxplot_stats <- boxplot.stats(x)
    return(boxplot_stats$out)  # Return outlier values
  } else {
    return(NULL)  # Return NULL for non-numeric columns
  }
}

# Check outliers for the top 20 important features
outliers_detected <- lapply(data_selected[top_features], detect_outliers_in_vector)

# Print outliers for each of the top 20 features that has outliers
outliers_present <- sapply(outliers_detected, function(x) !is.null(x) && length(x) > 0)

if(any(outliers_present)) {
  cat("Outliers detected in the following top features:\n")
  outliers_names <- names(outliers_detected)[outliers_present]
  print(outliers_names)
} else {
  cat("No outliers detected in the top features.")
}


# Loop through features with outliers and plot boxplots
for(feature in names(outliers_detected)[outliers_present]) {
  print(
    ggplot(data_selected, aes(x = factor(1), y = !!sym(feature))) +
      geom_boxplot() +
      labs(title=paste("Boxplot for feature:", feature), x="", y=feature) +
      theme(axis.title.x=element_blank(),
            axis.text.x=element_blank(),
            axis.ticks.x=element_blank())
  )
}


#Display a table of the top 20 important features, based on the Random Forest model.
kable(important_features[order(important_features[,1], decreasing=TRUE)[1:20],], caption="Top 20 Important Features")

# Convert the matrix to a data frame and preserve feature names
top_20_features_df <- as.data.frame(important_features[order(important_features[,1], decreasing=TRUE)[1:20],])
top_20_features_df$Feature <- rownames(top_20_features_df)

colnames(top_20_features_df)
#> colnames(top_20_features_df)
#[1] "%IncMSE"       "IncNodePurity" "Feature"  

# Using ggplot2 to make graph
ggplot(top_20_features_df, aes(x=reorder(Feature, -`%IncMSE`), y=`%IncMSE`)) +
  geom_bar(stat="identity") +
  coord_flip() + 
  labs(title="Top 20 Important Features", x="Features", y="Feature Importance (%IncMSE)")



#Data Standardization
# Apply data standardization using the 'range' method
preProcess_range <- preProcess(data_selected, method='range')
data_normalized <- predict(preProcess_range, data_selected)

#Handling Imbalanced Class Distribution - Oversampling
# Check the class distribution
dim(data_normalized)
table(data_normalized$o_bullied)
# Convert the target variable 'o_bullied' to a factor
data_normalized$o_bullied <- as.factor(data_normalized$o_bullied)
# Perform oversampling to balance the class distribution
data_oversampled <- ovun.sample(o_bullied ~ ., data = data_normalized, method = "over", p = 0.5)$data
# Check the class distribution after oversampling
table(data_oversampled$o_bullied)

# Stratified Split of the Dataset
set.seed(123)
index <- createDataPartition(data_oversampled$o_bullied, p=0.7, list=FALSE)
training_data <- data_oversampled[index,]
test_data <- data_oversampled[-index,]




# Build various classification models
# Logistic Regression
# Ensure that the levels of the target variable match between training and test data
training_data$o_bullied <- factor(training_data$o_bullied, levels = levels(test_data$o_bullied))
# Check if the levels of the target variable match
levels(training_data$o_bullied)
levels(test_data$o_bullied)

# Adjusting the Logistic Regression Model
# Build a logistic regression model using the glm function
model_glm <- glm(o_bullied ~ ., data=training_data, family="binomial")
pred_glm <- predict(model_glm, newdata=test_data, type="response")
# Convert predicted probabilities to binary classes based on a threshold (usually 0.5)
predicted_classes <- as.factor(ifelse(pred_glm > 0.5, 1, 0))

# Decision Tree
model_rpart <- rpart(o_bullied ~ ., data=training_data, method="class")
pred_rpart <- predict(model_rpart, newdata=test_data, type="class")

# Random Forest
model_rf <- randomForest(o_bullied ~ ., data=training_data)
pred_rf <- predict(model_rf, newdata=test_data)

# Support Vector Machine (SVM)
model_svm <- svm(o_bullied ~ ., data=training_data, probability=TRUE)
pred_svm <- predict(model_svm, newdata=test_data)

# k-Nearest Neighbors (k-NN)
train_knn <- as.data.frame(training_data[, -ncol(training_data)])
test_knn <- as.data.frame(test_data[, -ncol(test_data)])
cl <- as.factor(training_data$o_bullied)
pred_knn <- knn(train_knn, test_knn, cl, k=5)

# Naive Bayes
model_nb <- naiveBayes(o_bullied ~ ., data=training_data)
pred_nb <- predict(model_nb, newdata=test_data)

# Summarize model performance
model_results <- list(
  Logistic_Regression = confusionMatrix(predicted_classes, test_data$o_bullied),
  Decision_Tree = confusionMatrix(pred_rpart, test_data$o_bullied),
  Random_Forest = confusionMatrix(pred_rf, test_data$o_bullied),
  SVM = confusionMatrix(pred_svm, test_data$o_bullied),
  kNN = confusionMatrix(pred_knn, test_data$o_bullied),
  Naive_Bayes = confusionMatrix(pred_nb, test_data$o_bullied)
)

# Print the accuracy of each model
lapply(model_results, function(x) x$overall['Accuracy'])

# Determine the best-performing model
accuracies <- sapply(model_results, function(x) x$overall['Accuracy'])
best_model <- names(which.max(accuracies))
print(paste("The best model is:", best_model))

#Summarize the accuracy of each model in a table for easier comparison.
accuracies_table <- data.frame(Model = names(accuracies), Accuracy = as.numeric(accuracies))
kable(accuracies_table, caption="Model Accuracy Comparison")

#Create a bar graph to display the accuracy of each model for a visual comparison.
ggplot(accuracies_table, aes(x=Model, y=Accuracy, fill=Model)) +
  geom_bar(stat="identity") +
  labs(title="Model Accuracy Comparison", x="Model", y="Accuracy") +
  theme_minimal()