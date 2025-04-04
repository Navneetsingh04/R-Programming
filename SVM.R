# SVM is a supervised machine learning algorithm which can be used for classification
# or regression problems. It uses a technique called the kernel trick to transform
# our data and then based on these transformations it finds an optimal boundary 
# between the possible outputs.


library(caTools)
library(e1071)

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

classifier = svm(formula = Purchased ~ ., data = training_set,
                 type = 'C-classification', kernel = 'linear')
classifier

predict = predict(classifier, newdata = test_set[-3])
predict

cm = table(test_set[, 3], predict)
cm

accuracy = sum(diag(cm)) / sum(cm) * 100
print(paste("Accuracy:", accuracy, "%"))

# Visualising the Training set results
library(ggplot2)
ggplot(training_set, aes(x = Age, y = EstimatedSalary, color = Purchased)) +
        geom_point(size = 4) +
        ggtitle('SVM (Training Set)') +
        xlab('Age') +
        ylab('Estimated Salary') +
        theme_minimal()
