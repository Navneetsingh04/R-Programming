data <- read.csv("C:/Users/Navneet singh/Downloads/Salary_Data.csv",header = TRUE)
head(data)
View(data)

# caTools is a package that contains a function called sample.split() 
# that can be used to split the dataset into training and test set.
# The function takes two arguments: the first argument is the target variable
# and the second argument is the split ratio.
library(caTools)
set.seed(123)
# split the dataset into training and test set in the ratio 2:1
split <- sample.split(data$Salary, SplitRatio = 2/3)

# subset function used to split the dataset into training and test set
training_set <- subset(data, split == TRUE)
test_set <- subset(data, split == FALSE)
training_set
test_set

# lm function is used to create a linear regression model 
# The function takes two arguments: the first argument is the formula is
# used to specify the target variable and the independent variable
regressor <- lm(Salary ~ YearsExperience,
                 data = training_set)
# summary is used to get the summary of the linear regression model and the output

summary(regressor)

# predict function is used to make predictions on the test set
y_pred <- predict(regressor, newdata = test_set)
y_pred
test_set

library(ggplot2)
ggplot() +
  geom_point(aes(x = training_set$YearsExperience, y = training_set$Salary),
             colour = 'red') +
  geom_line(aes(x = training_set$YearsExperience, y = predict(regressor, newdata = training_set)),
            colour = 'blue') +
  ggtitle('Salary vs Experience (Training set)') +
  xlab('Years of experience') +
  ylab('Salary')



#linear regression on iris dataset

iris<-data.frame(iris)
head(iris)

set.seed(123)
split <- sample.split(iris$Sepal.Length,SplitRatio = 0.8)
training_set <- subset(iris,split == TRUE)
test_set <- subset(iris,split == FALSE)
training_set
test_set

regressor <- lm(Sepal.Length ~ Sepal.Width,
                 data = training_set)
summary(regressor)

prediction <- predict(regressor,newdata = test_set)
prediction

plot(iris$Sepal.Width,iris$Sepal.Length,col = 'red',
     main = 'Sepal Width vs Sepal Length',
     xlab = 'Sepal Width',ylab = 'Sepal Length')
# abline is used to draw a regression line
abline(regressor,col = 'blue')

# find the correlation between the Sepal Width and Sepal Length
cor(iris$Sepal.Width,iris$Sepal.Length)

#finding accuracy
accuracy <- mean((prediction - test_set$Sepal.Length)^2)
accuracy
