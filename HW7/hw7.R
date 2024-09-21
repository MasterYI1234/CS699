#Question1：
library(arules)

transactions <- list(
  c("2", "3", "4", "5", "6", "8"),
  c("1", "2", "3", "5", "6"),
  c("1", "4", "5", "7", "8"),
  c("2", "3", "4", "5", "6"),
  c("1", "2", "3", "4", "5", "7"),
  c("1", "3", "8")
)

trans <- as(transactions, "transactions")

min_support <- 0.5
max_len <- 10 
frequent_itemsets_list <- list()

# Generate frequent itemsets of each length through looping
for (len in 1:max_len) {
  frequent_itemsets <- eclat(trans, parameter = list(support = min_support, maxlen = len))
  
  # If the number of new frequent itemsets is 0, exit the loop
  if (length(frequent_itemsets) == 0) {
    break
  }
  
  frequent_itemsets_list[[len]] <- frequent_itemsets
}

# Print all frequent itemsets
for (len in 1:length(frequent_itemsets_list)) {
  cat(paste0("\nFrequent ", len, "-itemsets:\n"))
  inspect(frequent_itemsets_list[[len]])
}

# Find all association rules with a confidence greater than or equal to 0.8
rules <- apriori(trans, parameter = list(support = min_support, confidence = 0.8))
inspect(rules)


#Question2：
# Contingency table data
matrix_data <- matrix(c(287, 45, 195, 143), ncol=2, byrow=TRUE)
rownames(matrix_data) <- c("Tea = Yes", "Tea = No")
colnames(matrix_data) <- c("Coffee = Yes", "Coffee = No")

# Calculate association measures
a <- matrix_data[1,1]
b <- matrix_data[1,2]
c <- matrix_data[2,1]
d <- matrix_data[2,2]
n <- sum(matrix_data)

lift <- (a * n) / ((a + c) * (a + b))
all_confidence <- min(a / (a + b), a / (a + c))
cosine <- a / (sqrt((a + b) * (a + c)))
kulczynski <- 0.5 * (a / (a + b) + a / (a + c))
imbalance_ratio <- abs(a / (a + b) - a / (a + c)) / (a / (a + b) + a / (a + c) - 2 * (a^2) / n)

# Print association measures
cat("Lift:", lift, "\n")
cat("All-confidence:", all_confidence, "\n")
cat("Cosine:", cosine, "\n")
cat("Kulczynski:", kulczynski, "\n")
cat("Imbalance Ratio:", imbalance_ratio, "\n")

# Chi-squared test
chisq_test <- chisq.test(matrix_data)
cat("Chi-squared:", chisq_test$statistic, "\n")
cat("P-value:", chisq_test$p.value, "\n")
if(chisq_test$p.value < 0.05) {
  cat("Given the p-value is less than 0.05, we reject the null hypothesis and conclude that buying coffee and buying tea are correlated.\n")
} else {
  cat("Given the p-value is greater than 0.05, we fail to reject the null hypothesis and conclude that buying coffee and buying tea are not correlated.\n")
}


#Question3:
library(arules)

# Load the data
data <- read.csv("hw7.csv", stringsAsFactors = TRUE)
transactions <- as(data, "transactions")

# (1). Mine all frequent itemsets with the minimum support = 0.005
frequent_itemsets <- apriori(transactions, parameter = list(support = 0.005, target = "frequent itemsets"))
cat("Summary of all frequent itemsets:\n")
summary(frequent_itemsets)

# (2). Total number of frequent itemsets and top 5 based on support
cat("\nTotal number of frequent itemsets:", length(frequent_itemsets), "\n")
cat("Top 5 frequent itemsets based on support:\n")
inspect(head(sort(frequent_itemsets, by="support"), 5))

# (3). Total number of frequent 1-itemsets and top 5
frequent_1_itemsets <- subset(frequent_itemsets, size(items) == 1)
cat("\nTotal number of frequent 1-itemsets:", length(frequent_1_itemsets), "\n")
cat("Top 5 frequent 1-itemsets based on support:\n")
inspect(head(sort(frequent_1_itemsets, by="support"), 5))

# (4). Total number of frequent 2-itemsets and top 5
frequent_2_itemsets <- subset(frequent_itemsets, size(items) == 2)
cat("\nTotal number of frequent 2-itemsets:", length(frequent_2_itemsets), "\n")
cat("Top 5 frequent 2-itemsets based on support:\n")
inspect(head(sort(frequent_2_itemsets, by="support"), 5))

# (5). Total number of frequent 3-itemsets and the one with highest support
frequent_3_itemsets <- subset(frequent_itemsets, size(items) == 3)
cat("\nTotal number of frequent 3-itemsets:", length(frequent_3_itemsets), "\n")
cat("The frequent 3-itemset with the highest support:\n")
inspect(head(sort(frequent_3_itemsets, by="support"), 1))

# (6). Total number of frequent 4-itemsets and the one with highest support
frequent_4_itemsets <- subset(frequent_itemsets, size(items) == 4)
cat("\nTotal number of frequent 4-itemsets:", length(frequent_4_itemsets), "\n")
cat("The frequent 4-itemset with the highest support:\n")
inspect(head(sort(frequent_4_itemsets, by="support"), 1))

# (7). Mine all rules with minimum support = 0.005 and minimum confidence = 0.15
rules <- apriori(transactions, parameter = list(support = 0.005, confidence = 0.15, target = "rules"))
cat("\nSummary of all rules:\n")
summary(rules)

# (8). Top 5 rules based on confidence
cat("\nTop 5 rules based on confidence:\n")
inspect(head(sort(rules, by="confidence"), 5))

# (9). Top 5 rules based on support
cat("\nTop 5 rules based on support:\n")
inspect(head(sort(rules, by="support"), 5))

# (10). Rules that include coffee
cat("\nRules that include coffee:\n")
coffee_rules_lhs <- subset(rules, subset = lhs %in% "coffee")
coffee_rules_rhs <- subset(rules, subset = rhs %in% "coffee")
coffee_rules <- unique(c(coffee_rules_lhs, coffee_rules_rhs))
inspect(coffee_rules)


