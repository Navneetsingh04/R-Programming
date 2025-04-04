# Implement a K-Nearest Neighbors (KNN) classifier on the "heart_disease_dataset.csv"
# to predict whether the diagnosis is positive (presence of heart disease) or negative 
# (absence of heart disease) Pre-process the data by handling missing values,
# normalizing features, and encoding categorical variables if necessary
# Evaluate the model's accuracy using appropriate metrics

library(class)
library(caTools)

data <- read.csv("C:/Users/Navneet singh/Downloads/heart_disease_dataset.csv")
View(data)
str(data)

sum(is.na(data))

data$target <- as.factor(data$target)

# normalization

numeric_col <- sapply(data,is.numeric)
data[numeric_col] <- scale(data[numeric_col])
# Splitting the dataset into training and testing sets

set.seed(123)
split <- sample.split(data$target, SplitRatio = 0.8)
train_data <- subset(data, split == TRUE)

test_data <- subset(data, split == FALSE)
# Separate features and target variable
train_x <- train_data[, -ncol(train_data)]
train_y <- train_data[, ncol(train_data)]

test_x <- test_data[, -ncol(test_data)]
test_y <- test_data[, ncol(test_data)]
# KNN classifier
k <- 5
predictions <- knn(train_x, test_x, train_y, k)
# Evaluate the model's accuracy
confusion_matrix <- table(test_y, predictions)
accuracy <- sum(diag(confusion_matrix)) / sum(confusion_matrix)
cat("Confusion Matrix:\n")
print(confusion_matrix)
cat("Accuracy:", accuracy, "\n")

# visualization
library(ggplot2)
ggplot()+
  geom_point(aes(x = test_data$age, y = test_data$chol, color = predictions)) +
  labs(title = "KNN Classifier Predictions", x = "Age", y = "Cholesterol") +
  scale_color_manual(values = c("red", "blue"), labels = c("No Heart Disease", "Heart Disease")) +
  theme_minimal()
