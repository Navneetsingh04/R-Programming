# Use the "spam-emails.csv" dataset to classify emails as spam or not spam using
# Naive Bayes Compare the model's performance before and after feature selection.

library(e1071)
library(caret)
library(caTools)

data <- read.csv("C:/Users/Navneet singh/Downloads/spam-emails.csv")
data$spam <- factor(data$spam)

# Check for missing values
sum(is.na(data))

# Split the dataset into training and testing sets
set.seed(123)
split <- sample.split(data$spam, SplitRatio = 0.8)
train_data <- subset(data, split == TRUE)
test_data <- subset(data, split == FALSE)

# Separate features and target variable
train_x <- train_data[, -ncol(train_data)]
train_y <- train_data[, ncol(train_data)]
test_x <- test_data[, -ncol(test_data)]
test_y <- test_data[, ncol(test_data)]

# Train the Naive Bayes model
model <- naiveBayes(train_x, train_y)

# Make predictions on the test set
predictions <- predict(model, test_x)

# Evaluate the model's performance
confusion_matrix <- table(test_y, predictions)
accuracy <- sum(diag(confusion_matrix)) / sum(confusion_matrix)
cat("Confusion Matrix:\n")
print(confusion_matrix)
cat("Accuracy:", accuracy, "\n")
train_y <- as.factor(train_y) 
