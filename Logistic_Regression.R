
# Logistic Regression is a classification algorithm used to assign observations 
# to a discrete set of classes.
# it is used to predict the probability of an event occurring by fitting data
# to a logit function.

data <- read.csv("C:/Users/Navneet singh/Downloads/Social_Network_Ads.csv")
View(data)

# Selecting only relevant columns (Age, EstimatedSalary, Purchased)
data <- data[3:5]

# Splitting dataset into Training and Test sets
library(caTools)
set.seed(123)
split = sample.split(data$Purchased, SplitRatio = 0.75)
training_set = subset(data, split == TRUE)
test_set = subset(data, split == FALSE)

# Feature Scaling 
#scale is used to standardize the data i.e it scales the data to have
# a mean of 0 and a standard deviation of 1
training_set[-3] = scale(training_set[-3]) 
test_set[-3] = scale(test_set[-3])

# Fitting Logistic Regression to the Training set

# glm is used to fit the generalized linear model
# family = binomial is used to specify that we are using logistic regression
# formula = Purchased ~ . is used to specify that Purchased is dependent on all the columns
classifier = glm(formula = Purchased ~ ., family = binomial, data = training_set)
prob_pred = predict(classifier, type = 'response', newdata = test_set[-3])
prob_pred = ifelse(prob_pred > 0.5, 1, 0)
prob_pred

# Making the Confusion Matrix
# confusion matrix is used to evaluate the performance of the model
# table is used to create a contingency table of the factors
cm = table(test_set[, 3], prob_pred > 0.5)
cm
