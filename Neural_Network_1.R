library(neuralnet)
library(caTools)  # For data splitting
library(ggplot2)

# Load dataset
data <- read.csv("C:/Users/Navneet singh/Downloads/Social_Network_Ads.csv")
data = data[3:5]  # Selecting features

# Convert Purchased column to numeric (required for neuralnet package)
data$Purchased = as.numeric(as.character(data$Purchased))

# Split dataset into Training (75%) and Testing (25%)
set.seed(123)
split = sample.split(data$Purchased, SplitRatio = 0.75)
train_set = subset(data, split == TRUE)
test_set = subset(data, split == FALSE)

# Normalize the dataset (important for neural networks)
normalize <- function(x) { (x - min(x)) / (max(x) - min(x)) }
train_set[, 1:2] <- as.data.frame(lapply(train_set[, 1:2], normalize))
test_set[, 1:2] <- as.data.frame(lapply(test_set[, 1:2], normalize))


# Train neural network with 2 hidden layers (5 and 3 neurons)
nn_model <- neuralnet(Purchased ~ Age + EstimatedSalary, 
                      data = train_set, 
                      hidden = c(5, 3),  # 2 hidden layers
                      linear.output = FALSE) # Sigmoid activation

# Visualize the Neural Network
plot(nn_model)


# Predict on test data
nn_predictions <- compute(nn_model, test_set[, 1:2])
predicted_values <- ifelse(nn_predictions$net.result > 0.5, 1, 0)

# Confusion Matrix
cm <- table(test_set$Purchased, predicted_values)
print(cm)

# Accuracy Calculation
accuracy <- sum(diag(cm)) / sum(cm) * 100
print(paste("Accuracy:", accuracy, "%"))


# Scatter plot of predictions
ggplot(test_set, aes(x = Age, y = EstimatedSalary, color = as.factor(predicted_values))) +
        geom_point(size = 4) +
        ggtitle('Neural Network Predictions') +
        xlab('Age') +
        ylab('Estimated Salary') +
        theme_minimal()
