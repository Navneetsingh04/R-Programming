# Load the USArrests dataset (an in-built dataset) and apply the SVM algorithm to
# classify states based on crime statistics. Implement the SVM model without
# feature scaling and then with feature scaling using the scale() function

library(e1071) 
library(caTools)

data <-  as.data.frame(USArrests)
View(data)
str(data)

# Add a categorical column (crime level) based on Assault rate
# Categorizing into High (Above median) and Low (Below median)
data$Crime_Level <- ifelse(data$Assault > median(data$Assault), "High", "Low")
data$Crime_Level <- factor(data$Crime_Level)  # Convert to factor

# Split dataset into training (70%) and testing (30%) sets
set.seed(123)
split <- sample.split(data$Crime_Level, SplitRatio = 0.7)
train_data <- subset(data, split == TRUE)
test_data <- subset(data, split == FALSE)

# -------------------------- SVM Without Feature Scaling --------------------------
svm_model_unscaled <- svm(Crime_Level ~ ., data = train_data,
                          kernel = "radial")

# Make predictions
predictions_unscaled <- predict(svm_model_unscaled, test_data)

# Evaluate model performance
conf_matrix_unscaled <- table(test_data$Crime_Level, predictions_unscaled)
accuracy_unscaled <- sum(diag(conf_matrix_unscaled)) / sum(conf_matrix_unscaled)
accuracy_unscaled


# -------------------------- SVM With Feature Scaling --------------------------
# Scale only numeric features
data_scaled <- as.data.frame(scale(USArrests[, 1:4]))
data_scaled$Crime_Level <- data$Crime_Level  # Add back Crime_Level column

# Split dataset again
set.seed(123)
split_scaled <- sample.split(data_scaled$Crime_Level, SplitRatio = 0.7)
train_data_scaled <- subset(data_scaled, split_scaled == TRUE)
test_data_scaled <- subset(data_scaled, split_scaled == FALSE)

# Train SVM model with scaled features
svm_model_scaled <- svm(Crime_Level ~ ., data = train_data_scaled, kernel = "radial")

# Make predictions
predictions_scaled <- predict(svm_model_scaled, test_data_scaled)

# Evaluate model performance
conf_matrix_scaled <- table(test_data_scaled$Crime_Level, predictions_scaled)
accuracy_scaled <- sum(diag(conf_matrix_scaled)) / sum(conf_matrix_scaled)
accuracy_scaled
