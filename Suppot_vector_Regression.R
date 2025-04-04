# SVR is a type of support vector machine that supports linear and non-linear 
# regression.It is used to predict the continuous dependent variable.


data = read.csv("C:/Users/Navneet singh/Downloads/Position_Salaries (1).csv")
View(data)

data = data[2:3]

# e1071 is a package that provides functions for statistical learning used for
# classification and regression analysis. i.e. SVM

library(e1071)
# Fitting SVR to the dataset
# svm is used to fit the support vector machine model
# type = 'eps-regression' is used to specify that we are using SVR
regressor = svm(formula = Salary ~ ., data = data, type = 'eps-regression')
summary(regressor)

er = predict(regressor, data.frame(Level = 6.5))
er

# Visualising the SVR results
library(ggplot2)
ggplot()+
  geom_point(aes(x = data$Level, y = data$Salary), color = 'red')+
  geom_line(aes(x = data$Level, y = predict(regressor, newdata = data)), color = 'blue')+
  ggtitle('SVR Representation')+
  xlab('Level')+
  ylab('Salary')
