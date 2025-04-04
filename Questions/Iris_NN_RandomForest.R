# How can a neural network and a random forest model be used to 
# classify the species of iris flowers based on their sepal and 
# petal measurements using the iris dataset in R? Evaluate and
# compare the performance of the models by calculating their 
# accuracy. What conclusions car be drawn regarding the 
# effectiveness of neural networks versus random forests for this
# classification task. 

# Load required libraries
library(caret)
library(neuralnet)
library(randomForest)
library(caTools)

# Load and inspect the iris dataset
data(iris)
summary(iris)
str(iris)

# Check for missing values
sum(is.na(iris))  

# Convert Species to numeric factor (1, 2, 3)
iris$Species <- as.numeric(factor(iris$Species))

# Split data 
set.seed(123)
split <- sample.split(iris$Species, SplitRatio = 0.8)
train_data <- subset(iris, split == TRUE)
test_data <- subset(iris, split == FALSE)

# Scale the numeric features (important for neural networks)
preprocess_params <- preProcess(train_data[,1:4], method = c("center", "scale"))
train_data_scaled <- predict(preprocess_params, train_data)
test_data_scaled <- predict(preprocess_params, test_data)

# Train Neural Network Model
set.seed(123)
nn_model <- neuralnet(Species ~ Sepal.Length + Sepal.Width + Petal.Length + Petal.Width,
                      data = train_data_scaled,
                      hidden = c(5, 3),  # Two hidden layers with 5 and 3 neurons
                      linear.output = FALSE,  # Regression mode to output continuous values
                      act.fct = "logistic")

# Plot the neural network
plot(nn_model)

# Make predictions
nn_pred <- compute(nn_model, test_data_scaled[,1:4])$net.result

# Convert back to factor levels for comparison
nn_pred <- factor(nn_pred, levels = c(1,2,3), labels = levels(factor(iris$Species)))
true_labels <- factor(test_data$Species, levels = c(1,2,3), labels = levels(factor(iris$Species)))

# Evaluate performance
nn_confusion <- confusionMatrix(nn_pred, true_labels)
nn_accuracy <- nn_confusion$overall['Accuracy']
nn_accuracy

# Random Forest Model (no need to scale for RF)
set.seed(123)
rf_model <- randomForest(Species ~ ., 
                         data = train_data,
                         ntree = 500,
                         importance = TRUE)

# Make predictions
rf_pred <- predict(rf_model, test_data)

# Convert test_data$Species to a factor with correct levels
test_data$Species <- factor(test_data$Species, levels = c(1,2,3))

# Convert rf_pred to factor with same levels
rf_pred <- factor(rf_pred, levels = c(1,2,3))

# Compute confusion matrix
rf_confusion <- confusionMatrix(rf_pred, test_data$Species)

# Extract accuracy
rf_accuracy <- rf_confusion$overall['Accuracy']

# Print accuracy
print(rf_accuracy)

# Compare model performance
results <- data.frame(
        Model = c("Neural Network", "Random Forest"),
        Accuracy = c(nn_accuracy, rf_accuracy)
)

# Print comparison
print(results)

# Variable importance for Random Forest
varImpPlot(rf_model, main = "Random Forest - Variable Importance")

# Visualize the performance comparison
ggplot(results, aes(x = Model, y = Accuracy, fill = Model)) +
        geom_bar(stat = "identity") +
        geom_text(aes(label = round(Accuracy, 3)), vjust = -0.5) +
        ylim(0, 1) +
        ggtitle("Model Accuracy Comparison") +
        theme_minimal()

