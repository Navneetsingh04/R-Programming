# Decision Tree is a type of supervised learning algorithm that is mostly used 
# in classification problems. It works for both categorical and continuous
# input and output variables. 
# It is a tree-structured classifier, where internal nodes represent the 
# features of a dataset, branches represent the decision rules and each 
# leaf node represents the outcome.

library(caTools)
library(rpart)

data <- read.csv("C:/Users/Navneet singh/Downloads/Social_Network_Ads.csv")
View(data)
data = data[3:5]
data$Purchased = factor(data$Purchased, levels = c(0, 1))

set.seed(123)

split = sample.split(data$Purchased, SplitRatio = 0.75)
training_set = subset(data, split == TRUE)
test_set = subset(data, split == FALSE)

training_set[-3] = scale(training_set[-3])
test_set[-3] = scale(test_set[-3])

classifier = rpart(formula = Purchased ~ ., data = training_set)

predict = predict(classifier, newdata = test_set[-3],type = 'class')
predict

cm = table(test_set[, 3], predict)
cm

# Visualizing the Decision Tree
library(rpart.plot)
rpart.plot(classifier)

