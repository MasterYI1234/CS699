library(ROSE)
library(caret)
library(randomForest)
library(e1071)
library(class)
library(knitr)
library(ggplot2)
library(rpart)
library(pROC)


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
model_glm <- glm(o_bullied ~ ., data=training_data, family=binomial)
predicted_classes <- ifelse(predict(model_glm, newdata=test_data, type='response') > 0.5, "yes", "no")

# Decision Tree
model_rpart <- rpart(o_bullied ~ ., data=training_data, method="class")
pred_rpart <- predict(model_rpart, newdata=test_data, type="class")

# Random Forest
model_rf <- randomForest(o_bullied ~ ., data=training_data)
pred_rf <- predict(model_rf, newdata=test_data)

# SVM
model_svm <- svm(o_bullied ~ ., data=training_data)
pred_svm <- predict(model_svm, newdata=test_data)

# kNN
model_knn <- train(o_bullied ~ ., data=training_data, method="knn", preProcess=c("center", "scale"), tuneLength=10)
pred_knn <- predict(model_knn, newdata=test_data)

# Naive Bayes
model_nb <- naiveBayes(o_bullied ~ ., data=training_data)
pred_nb <- predict(model_nb, newdata=test_data)







# Adjust the class levels of the data
test_data$o_bullied <- factor(test_data$o_bullied, levels = c("0", "1"))
predicted_classes <- as.factor(ifelse(predict(model_glm, newdata=test_data, type='response') > 0.5, "1", "0"))

# Create a function to adjust the levels of predicted classes
adjust_predictions <- function(predictions) {
  return(factor(predictions, levels = c("0", "1")))
}

predicted_classes_adj <- adjust_predictions(predicted_classes)
pred_rpart_adj <- adjust_predictions(pred_rpart)
pred_rf_adj <- adjust_predictions(pred_rf)
pred_svm_adj <- adjust_predictions(pred_svm)
pred_knn_adj <- adjust_predictions(pred_knn)
pred_nb_adj <- adjust_predictions(pred_nb)

# Modify the metrics calculation function for correct computation of ROC
compute_metrics_table <- function(predictions, actuals) {
  predictions_numeric <- as.numeric(as.character(predictions))
  actuals_numeric <- as.numeric(as.character(actuals))
  cm <- confusionMatrix(factor(predictions_numeric), factor(actuals_numeric))
  
  TP <- as.numeric(cm$table[2,2])
  TN <- as.numeric(cm$table[1,1])
  FP <- as.numeric(cm$table[1,2])
  FN <- as.numeric(cm$table[2,1])
  
  MCC <- (TP * TN - FP * FN) / sqrt((TP+FP)*(TP+FN)*(TN+FP)*(TN+FN))
  FPR <- FP / (FP + TN)  # Calculate FPR manually
  TPR <- TP / (TP + FN)
  Precision <- TP / (TP + FP)
  Recall <- TPR  # Same as TPR
  ROC_Area <- auc(roc(actuals_numeric, predictions_numeric))
  Kappa <- cm$overall["Kappa"]
  
  # Compute F-measure
  F_measure <- ifelse(!is.na(Precision) && !is.na(Recall), 
                      2 * (Precision * Recall) / (Precision + Recall), 
                      NA)
  
  # Create the metrics table
  metrics_table <- data.frame(
    TPR = c(TPR, cm$byClass["Sensitivity"], NA),  # Assuming Class 1 is the positive class
    FPR = c(FPR, 1 - cm$byClass["Specificity"], NA),  # Assuming Class 1 is the positive class
    Precision = c(cm$byClass["Precision"], Precision, NA),  # Assuming Class 0 is the negative class
    Recall = c(1 - cm$byClass["Sensitivity"], Recall, NA),  # Assuming Class 0 is the negative class
    F_measure = c(F_measure, F_measure, NA),
    ROC_Area = c(NA, ROC_Area, ROC_Area),  # Assuming ROC Area is the same for both classes
    MCC = c(MCC, MCC, NA),
    Kappa = c(Kappa, Kappa, NA)
  )
  rownames(metrics_table) <- c("Class 0", "Class 1", "Wt. Average")
  
  # Compute weighted averages
  n0 <- sum(actuals_numeric == 0)
  n1 <- sum(actuals_numeric == 1)
  n_total <- n0 + n1
  
  metrics_table["Wt. Average",] <- (n0 * metrics_table["Class 0",] + n1 * metrics_table["Class 1",]) / n_total
  
  return(metrics_table)
}

# Compute evaluation metrics table for each model using adjusted predictions
model_evaluation_tables <- list(
  Logistic_Regression = compute_metrics_table(predicted_classes_adj, test_data$o_bullied),
  Decision_Tree = compute_metrics_table(pred_rpart_adj, test_data$o_bullied),
  Random_Forest = compute_metrics_table(pred_rf_adj, test_data$o_bullied),
  SVM = compute_metrics_table(pred_svm_adj, test_data$o_bullied),
  kNN = compute_metrics_table(pred_knn_adj, test_data$o_bullied),
  Naive_Bayes = compute_metrics_table(pred_nb_adj, test_data$o_bullied)
)

# Print the evaluation metrics table for each model
lapply(model_evaluation_tables, print)





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
best_model <- names(which.max(accuracies))[1]
best_model <- unlist(strsplit(best_model, "\\."))[1]
print(paste("The best model is:", best_model))

#Summarize the accuracy of each model in a table for easier comparison.
accuracies_table <- data.frame(Model = names(accuracies), Accuracy = as.numeric(accuracies))
kable(accuracies_table, caption="Model Accuracy Comparison")

#Create a bar graph to display the accuracy of each model for a visual comparison.
ggplot(accuracies_table, aes(x=Model, y=Accuracy, fill=Model)) +
  geom_bar(stat="identity") +
  labs(title="Model Accuracy Comparison", x="Model", y="Accuracy") +
  theme_minimal()




write.csv(data_selected, "preprocessed_data.csv", row.names = FALSE)
write.csv(training_data, "initial_train.csv", row.names = FALSE)
write.csv(test_data, "initial_test.csv", row.names = FALSE)

# Determine the best model based on accuracy
best_model_obj <- switch(best_model,
                         Logistic_Regression = model_glm,
                         Decision_Tree = model_rpart,
                         Random_Forest = model_rf,
                         SVM = model_svm,
                         kNN = model_knn,
                         Naive_Bayes = model_nb)

# Save the parameters of the best model
save(best_model_obj, file = paste0(best_model, "_params.RData"))

# Display the parameters of the model
summary(best_model_obj)





