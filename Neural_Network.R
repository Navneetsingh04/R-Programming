# Neural Network is a type of supervised learning algorithm which is used to
# predict the output of a variable based on input data. It is inspired by the
# structure of the human brain and consists of layers of interconnected nodes
# that process information. In this script, we will use the neuralnet package
# to build a neural network model to predict whether a customer will purchase
# a product based on their age and estimated salary.

library(neuralnet)
library(caTools)  # For data splitting
library(ggplot2)  # For visualization


# Load the data
data <- read.csv("C:/Users/Navneet singh/Downloads/Social_Network_Ads.csv")
View(data)
data = data[3:5]
data$Purchased = factor(data$Purchased, levels = c(0, 1))

# Split the data
set.seed(123)
split = sample.split(data$Purchased, SplitRatio = 0.75)
training_set = subset(data, split == TRUE)


test_set = subset(data, split == FALSE)

# Scale the data

training_set[-3] = scale(training_set[-3])
test_set[-3] = scale(test_set[-3])

# Train the model
classifier = neuralnet(Purchased ~ ., data = training_set, hidden = c(5, 5))

plot(classifier)

# Make predictions
predictions = predict(classifier, test_set[-3])
predictions = ifelse(predictions > 0.5, 1, 0)
predictions
# Create confusion matrix

cm = table(test_set[, 3], predictions)
cm

# Calculate accuracy
accuracy = sum(diag(cm)) / sum(cm) * 100
print(paste("Accuracy:", accuracy, "%"))

# Visualize the results
library(ggplot2)
ggplot(training_set, aes(x = Age, y = EstimatedSalary, color = Purchased)) +
  geom_point(size = 4) +
  ggtitle('Neural Network (Training Set)') +
  xlab('Age') +
  ylab('Estimated Salary') +
  theme_minimal()
