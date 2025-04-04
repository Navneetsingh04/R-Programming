# Use a Support Vector Machine (SVM) to classify transactions in the
# "credit-card-fraud.csv" dataset.Experiment with different kernels
# (e.g.. linear, polynomial, RBF) and evaluate their performance 
# using appropriate metrics (e.g., accuracy, precision, recall, F1-score) 
# also Explain performance metrics in details


library(e1071)
library(caTools)
library(caret)


credit_data <- read.csv("C:/Users/Navneet singh/Downloads/credit-card-fraud.csv")
str(credit_data)
View(credit_data)
# Check for missing values
sum(is.na(credit_data))

# Convert 'Class' to a factor
credit_data$Class <- as.factor(credit_data$Class)

# Normalize numeric columns
# normalize <- function(x) {
#         return ((x - min(x)) / (max(x) - min(x)))
# }
# numeric_col <- sapply(credit_data, is.numeric)
# credit_data[numeric_col] <- lapply(credit_data[numeric_col], normalize)

# Alternatively, you can use the scale function for normalization

credit_data[numeric_col] <- scale(credit_data[numeric_col])

# Split the dataset
set.seed(123)
split <- sample.split(credit_data$Class, SplitRatio = 0.8)
training_set <- subset(credit_data, split == TRUE)
test_set <- subset(credit_data, split == FALSE)

# Train SVM models with different kernels
svm_linear <- svm(Class ~ ., data = training_set, kernel = "linear", cost = 1)
svm_poly <- svm(Class ~ ., data = training_set, kernel = "polynomial", cost = 1, degree = 3)
svm_rbf <- svm(Class ~ ., data = training_set, kernel = "radial", cost = 1, gamma = 0.1)

# Make predictions
pred_linear <- predict(svm_linear, newdata = test_set)
pred_poly <- predict(svm_poly, newdata = test_set)
pred_rbf <- predict(svm_rbf, newdata = test_set)

# Convert predictions to factors
pred_linear <- as.factor(pred_linear)
pred_poly <- as.factor(pred_poly)
pred_rbf <- as.factor(pred_rbf)

# Evaluate models
evaluate_model <- function(actual, predicted) {
        actual <- as.factor(actual)  
        predicted <- as.factor(predicted)
        cm <- confusionMatrix(predicted, actual)  
        accuracy <- cm$overall['Accuracy']
        precision <- cm$byClass['Precision']
        recall <- cm$byClass['Recall']
        f1_score <- cm$byClass['F1']
        
        return (c(accuracy, precision, recall, f1_score))
}

# Print evaluation results
evaluate_model(test_set$Class, pred_linear)
evaluate_model(test_set$Class, pred_poly)
evaluate_model(test_set$Class, pred_rbf)
