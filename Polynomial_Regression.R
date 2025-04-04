# Polynomial linear Regression is used when the relationship 
# between the independent variable and dependent variable is not linear

dataset = read.csv("C:/Users/Navneet singh/Downloads/Position_Salaries (1).csv")
View(dataset)
# dataset[2:3] is used to select the columns from 2 to 3 because 
# we want to predict the salary based on the level
dataset = dataset[2:3]
# Fitting Polynomial Regression to the dataset

# lm is used to fit the linear model it takes the formula as the argument
# and the data it takes the formula in the form of y ~ x1 + x2 + x3 + ... + xn
# here the formula is Salary ~ . which means Salary is dependent on all the columns

lr = lm(formula = Salary ~ ., data = dataset)
summary(lr)

# Visualising the Polynomial Regression results
dataset$Level2 = dataset$Level^2
dataset$Level3 = dataset$Level^3

ploy_reg = lm(formula = Salary ~ .,dataset)
summary(ploy_reg)

library(ggplot2)
# plotting the graph for linear regression
ggplot()+geom_point(aes(x = dataset$Level, y = dataset$Salary))
ggplot()+geom_point(aes(x = dataset$Level, y = dataset$Salary),color = 'red')+
geom_line(aes(x = dataset$Level, y = predict(lr, newdata = dataset)), color = 'blue')+
ggtitle('Result of Linear Regression')+
xlab('Level')+
ylab('Salary')

# Plotting the graph for polynomial regression
        
ggplot()+geom_point(aes(x = dataset$Level, y = dataset$Salary),color = 'red')+
geom_line(aes(x = dataset$Level,y = predict(ploy_reg,newdata = dataset)), color = 'blue')+
ggtitle('Result of Ploynomial Regression')+
xlab('Level')+
ylab('Salary')
        
        
dataset$level4 = dataset$Level^4
dataset$level5 = dataset$Level^5
        
        
        