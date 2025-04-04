# Train a decision tree classifier on the "credit-card-fraud.csv" dataset to detect 
# fraudulent transactions Visualize the decision tree and discuss important features.

library(caTools)
library(rpart)
library(rpart.plot)

data <- read.csv("C:/Users/Navneet singh/Downloads/credit-card-fraud.csv")
View(data)
str(data)

sum(is.na(data))

data$Class <- as.factor(data$Class)
# Normalize the data

numeric_col <- sapply(data,is.numeric)
data[, numeric_col] <- scale(data[, numeric_col])

set.seed(123)
split <- sample.split(data$Class,SplitRatio = 0.8)
train_set <- subset(data, split == TRUE)
test_set <- subset(data, split == FALSE)

# Train a decision tree classifier

classifier <- rpart(Class ~ ., data = train_set, method = "class")

prediction <- predict(classifier,newdata = test_set, 
                      type = "class")
confusion_matrix <- table(test_set$Class, prediction)
confusion_matrix

accuracy <- sum(diag(confusion_matrix)) / sum(confusion_matrix)
accuracy*100

# Visualize the decision tree

rpart.plot(classifier, type = 3, extra = 101, fallen.leaves = TRUE,
           main = "Decision Tree for Credit Card Fraud Detection")
