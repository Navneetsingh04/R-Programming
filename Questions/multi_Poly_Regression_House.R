# From the data "house_prices" dataset, Predict the house prises using multiple 
# linear regression model visualize the results

library(caTools)
library(ggplot2)

data <- read.csv("C:/Users/Navneet singh/Downloads/house-prices.csv")
View(data)
str(data)
summary(data)

sum(is.na(data))

# normalization 

numeric_col <- sapply(data, is.numeric)
data[numeric_col] <- scale(data[numeric_col])
# Split the dataset into training and testing sets

set.seed(123)
split <- sample.split(data$Price, SplitRatio = 0.8)
train_set <- subset(data, split == TRUE)
test_set <- subset(data, split == FALSE)

# Train the multiple linear regression model

model <- lm(Price ~ ., data = train_set)
summary(model)
# Predict on the test set

predictions <- predict(model, newdata = test_set)
predictions

#Evalute the model performance


actual <- test_set$Price
mse <- mean((predictions-actual)^2)
rmse <- sqrt(mse)
rmse

