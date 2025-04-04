# KNN algorithm is a classification algorithm used to assign observations to 
# a discrete set of classes. It is used to predict the class of a new observation 
# by finding the k-nearest neighbours of the new observation and assigning the 
# majority class among these neighbours to the new observation.

data <- read.csv("C:/Users/Navneet singh/Downloads/Social_Network_Ads.csv")
str(data)

data <- data[3:5]

# Splitting the dataset into the Training set and Test set
library(caTools)
set.seed(123)
split = sample.split(data$Purchased,SplitRatio = 0.75)
training_set = subset(data,split == TRUE)
test_set = subset(data,split == FALSE)

#feature scaling

training_set[-3] = scale(training_set[-3])
test_set[-3] = scale(test_set[-3])

# Fitting K-NN to the Training set

library(class)
# knn is used to fit the k-nearest neighbours model
# knn is a function that takes the training set, test set, 
# class of the training set and the value of k as arguments
# here class is the dependent variable

classifier = knn(train = training_set[, -3],
                 test = test_set[,-3],cl = training_set[, 3],k = 5)
classifier
trianing_set

# Making the Confusion Matrix
# confusion matrix is used to evaluate the performance of the model
cm = table(test_set[, 3], classifier)
cm

# Visualising the Training set results

library(ggplot2)
ggplot()+
  geom_point(aes(x = training_set$Age, y = training_set$EstimatedSalary,
                 color = as.factor(training_set$Purchased)))+
  ggtitle('Training set')+
  xlab('Age')+
  ylab('Estimated Salary')

# Visualising the Test set results

ggplot()+
  geom_point(aes(x = test_set$Age, y = test_set$EstimatedSalary,
                 color = as.factor(test_set$Purchased)))+
  ggtitle('Test set')+
  xlab('Age')+
  ylab('Estimated Salary')
