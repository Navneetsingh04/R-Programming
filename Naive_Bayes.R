# Naive Bayes is a classification technique based on Bayes’ Theorem with 
# an assumption of independence among predictors.
# A Naive Bayes classifier assumes that the presence of a particular feature
# in a class is unrelated to the presence of any other feature.

# Naive bayes is mainly used in text classification and with problems having 
# multiple classes.

data <- read.csv("C:/Users/Navneet singh/Downloads/Social_Network_Ads.csv")
str(data)
View(data)

library(e1071)
library(caTools)

data$Purchased = factor(data$Purchased, levels = c(0, 1))
set.seed(123)
split = sample.split(data$Purchased, SplitRatio = 0.75)
training_set = subset(data, split == TRUE)
test_set = subset(data, split == FALSE)

training_set[-3] = scale(training_set[-3])
test_set[-3] = scale(test_set[-3])

# Fitting Naive Bayes to the Training set
# NaiveBayes function is used to fit the Naive Bayes model it takes 
# the training set and the dependent variable as arguments

classifier = naiveBayes(x = training_set[-3], y = training_set$Purchased)
classifier

# Predicting the Test set results
y_pred = predict(classifier, newdata = test_set[-3])
y_pred

# Making the Confusion Matrix
cm = table(test_set[, 3], y_pred)
cm

# Visualising the Training set results
library(ggplot2)
ggplot()+
  geom_point(aes(x = training_set$Age, y = training_set$EstimatedSalary,
                 color = as.factor(training_set$Purchased)))+
  ggtitle('Training set')+
  xlab('Age')+
  ylab('Estimated Salary')
