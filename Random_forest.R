# Random forest is a supervised learning algorithm that can be used for both 
# classification and regression tasks.
# It works by constructing a multitude of decision trees during training and 
# outputting the mode of the classes (classification) or mean prediction (regression)

library(randomForest)
library(caTools)

data <- read.csv("C:/Users/Navneet singh/Downloads/Social_Network_Ads.csv")
View(data)

# subsetting

data <- data[3:5]
data$Purchased <- factor(data$Purchased,levels = c(0,1))
# Splitting the dataset into training and testing sets
set.seed(123)

split <- sample.split(data$Purchased, SplitRatio = 0.75)
training_set <- subset(data, split == TRUE)
test_set <- subset(data, split == FALSE)

# Feature scaling

training_set[-3] <- scale(training_set[-3])
test_set[-3] <- scale(test_set[-3])
# Fitting Random Forest to the Training set

classifier <- randomForest(x = training_set[-3],
                            y = training_set$Purchased,
                            ntree = 10)
# Predicting the Test set results
pred <- predict(classifier, newdata = test_set[-3])
pred
# Making the Confusion Matrix

cm <- table(test_set[, 3], pred)
cm

