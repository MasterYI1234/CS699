library(vcd)
library(xgboost)
library(caret)
library(e1071)

df <- read.csv("drug_consumption_cannabis.csv")

# Split the dataset
set.seed(123)
trainIndex <- createDataPartition(df$C6, p = 0.75, list = FALSE)
tr <- df[trainIndex,]
ts <- df[-trainIndex,]

# Set parameter grid for tuning
grid <- expand.grid(nrounds = c(50, 100, 150),
                    max_depth = c(4, 6, 8),
                    colsample_bytree = c(0.6, 0.8, 1),
                    eta = c(0.01, 0.05, 0.1),
                    gamma = 0,
                    min_child_weight = 1,
                    subsample = 1)

tr$C6 <- as.factor(tr$C6)

# Build model using cross-validation
set.seed(123)
model <- train(
  C6 ~ ., data = tr, method = "xgbTree",
  trControl = trainControl(method = "cv", number = 5),
  tuneGrid = grid, metric = "Accuracy"
)

# Print parameter list
print(grid)
best_parameters <- model$bestTune
print(best_parameters)

# Forecast
predictions <- predict(model, ts)

# Convert ts$C6 to factor
ts$C6 <- as.factor(ts$C6)

# Convert predictions to factor with same levels as ts$C6
predictions <- factor(predictions, levels = levels(ts$C6))

# Calculate confusion matrix
confusion_matrix <- confusionMatrix(predictions, ts$C6)

# Function to extract performance metrics
extract_metrics <- function(confusion_matrix) {
  tp <- confusion_matrix[2, 2]
  fp <- confusion_matrix[1, 2]
  tn <- confusion_matrix[1, 1]
  fn <- confusion_matrix[2, 1]
  
  tpr <- tp / (tp + fn)
  fpr <- fp / (fp + tn)
  precision <- tp / (tp + fp)
  recall <- tpr
  f_measure <- 2 * (precision * recall) / (precision + recall)
  mcc_numerator <- as.numeric(tp) * as.numeric(tn) - as.numeric(fp) * as.numeric(fn)
  mcc_denominator <- sqrt(as.numeric(tp + fp) * as.numeric(tp + fn) * as.numeric(tn + fp) * as.numeric(tn + fn))
  mcc <- mcc_numerator / mcc_denominator
  kappa_obj <- vcd::Kappa(as.table(confusion_matrix))
  kappa_value <- kappa_obj$statistic["Kappa"]
  
  return(c(TPR = tpr, FPR = fpr, Precision = precision, Recall = recall, F_measure = f_measure, MCC = mcc, Kappa = kappa))
}

# Calculate metrics for each class
class_0_metrics <- extract_metrics(confusion_matrix$table)
class_1_metrics <- extract_metrics(matrix(c(confusion_matrix$table[2,2], confusion_matrix$table[2,1], confusion_matrix$table[1,2], confusion_matrix$table[1,1]), nrow = 2))

# Calculate weighted average metrics
n_class_0 <- sum(df$C6 == 0)
n_class_1 <- sum(df$C6 == 1)
weighted_avg_metrics <- (n_class_0 * class_0_metrics + n_class_1 * class_1_metrics) / (n_class_0 + n_class_1)

# Display metrics
cat("Class 0 Metrics:\n")
print(class_0_metrics)

cat("Class 1 Metrics:\n")
print(class_1_metrics)

cat("Weighted Average Metrics:\n")
print(weighted_avg_metrics)