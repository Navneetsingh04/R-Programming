# Implement a neural network on the "Prostate_cancer.csv" dataset to
# classify patients as heathy or at risk.
# Visualize the neural network architecture and discuss the results

library(caTools)
library(neuralnet)
library(ggplot2)

dataset <- read.csv("C:/Users/Navneet singh/Downloads/Prostate_cancer.csv")
str(dataset)
View(dataset)

sum(is.na(cancer))
# 
# 
# cancer$diagnosis <- ifelse(cancer$diagnosis == "M", 1, 0) 
# cancer$diagnosis <- as.factor(cancer$diagnosis)
# 
# # handling missing values
# 
# cancer[is.na(cancer)] <- lapply(cancer,function(x)
#         ifelse(is.numeric(x),median(x,na.rm = TRUE),x))
# 
# # Normalize the data
# 
# numeric_col <- sapply(cancer, is.numeric)
# cancer[, numeric_col] <- scale(cancer[, numeric_col])
# # Split the dataset into training and testing sets
# 
# set.seed(123)
# split <- sample.split(cancer$diagnosis, SplitRatio = 0.8)
# training_set <- subset(cancer, split == TRUE)
# test_set <- subset(cancer, split == FALSE)
# 
# # Train the neural network model
# 
# nn_model <- neuralnet(diagnosis ~ ., data = training_set, hidden = c(5, 3), linear.output = FALSE)


str(dataset)
summary(dataset)

# Handling missing values (replace NA with median for numerical features)
dataset[is.na(dataset)] <- lapply(dataset, function(x) {
  if (is.numeric(x)) {
    x[is.na(x)] <- median(x, na.rm = TRUE)
  }
  return(x)
})

# Convert categorical target variable to numeric (1 = at risk, 0 = healthy)
dataset$diagnosis <- ifelse(dataset$diagnosis == "M", 1, 0)  # Assuming "M" is at risk

# Normalize numerical features
normalize <- function(x) { return ((x - min(x)) / (max(x) - min(x))) }
numeric_columns <- sapply(dataset, is.numeric)
dataset[numeric_columns] <- lapply(dataset[numeric_columns], normalize)

# Splitting dataset into training (80%) and testing (20%) sets
set.seed(123)
split <- sample.split(dataset$diagnosis, SplitRatio = 0.8)
train_set <- subset(dataset, split == TRUE)
test_set <- subset(dataset, split == FALSE)

# Train Neural Network model
nn_model <- neuralnet(diagnosis ~ ., data = train_set, hidden = c(5, 3), linear.output = FALSE)

# Visualizing the Neural Network Architecture
plot(nn_model)

# Predicting on test data
nn_pred <- compute(nn_model, test_set[, -which(names(test_set) == "diagnosis")])
nn_pred_class <- ifelse(nn_pred$net.result > 0.5, 1, 0)  # Convert probabilities to class labels

# Evaluate model performance
conf_matrix <- table(Predicted = nn_pred_class, Actual = test_set$diagnosis)
print(conf_matrix)

# Calculate accuracy
accuracy <- sum(diag(conf_matrix)) / sum(conf_matrix)
cat("Model Accuracy:", accuracy * 100, "%\n")

# Compute additional evaluation metrics
confusionMatrix(as.factor(nn_pred_class), as.factor(test_set$diagnosis))

