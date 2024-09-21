library(caret)
library(e1071)
library(rpart)
library(rpart.plot)

# Read the data
data <- read.csv("heart_failure.csv")

# (1) Convert DEATH_EVENT to a factor
data$DEATH_EVENT <- as.factor(data$DEATH_EVENT)

# (2) Stratified splitting of the dataset into training and test sets
set.seed(123) 
splitIndex <- createDataPartition(data$DEATH_EVENT, p = 0.66, list = FALSE)
train_data <- data[splitIndex, ]
test_data <- data[-splitIndex, ]

# (3) Build a Naïve Bayes model using the training dataset
nb_model <- naiveBayes(DEATH_EVENT ~ ., data = train_data)

# (4) Test the model on the test dataset
nb_predictions <- predict(nb_model, test_data)

# (5) Display the confusion matrix and prediction accuracy for each class
nb_confusion_matrix <- table(Predicted = nb_predictions, Actual = test_data$DEATH_EVENT)
print(nb_confusion_matrix)

class_1_accuracy <- nb_confusion_matrix[2,2] / sum(nb_confusion_matrix[2,])
cat("Accuracy for Class 1:", class_1_accuracy, "\n")

class_0_accuracy <- nb_confusion_matrix[1,1] / sum(nb_confusion_matrix[1,])
cat("Accuracy for Class 0:", class_0_accuracy, "\n")

# (6) Build a decision tree model using the rpart algorithm on the training dataset
tree_model <- rpart(DEATH_EVENT ~ ., data = train_data, method = "class", parms = list(split = "information"))

# (7) Plot the tree of the model
rpart.plot(tree_model, main="Decision Tree", under=TRUE, faclen=0)

# (8) Test the model on the test dataset
tree_predictions <- predict(tree_model, test_data, type = "class")

# (9) Display the confusion matrix and prediction accuracy for each class
tree_confusion_matrix <- table(Predicted = tree_predictions, Actual = test_data$DEATH_EVENT)
print(tree_confusion_matrix)

tree_class_1_accuracy <- tree_confusion_matrix[2,2] / sum(tree_confusion_matrix[2,])
cat("Accuracy for Class 1 :", tree_class_1_accuracy, "\n")

tree_class_0_accuracy <- tree_confusion_matrix[1,1] / sum(tree_confusion_matrix[1,])
cat("Accuracy for Class 0 :", tree_class_0_accuracy, "\n")