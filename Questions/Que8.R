# Use the groceries.csv dataset to conduct market basket analysis, Identify the 
# top 5 frequent item sets and analyze their support, confidence, and lift values

# Load necessary libraries
library(arules)
library(arulesViz)

# Load the dataset

data <- read.csv("C:/Users/Navneet singh/Downloads/groceries.csv",header = FALSE)
View(data)
# Convert the data to transactions

dataset <- read.transactions("C:/Users/Navneet singh/Downloads/groceries.csv", 
                            format = "basket", sep = ",", rm.duplicates = TRUE)
View(dataset)

# Inspect the dataset
# inspect(dataset[1:5])

summary(dataset)

# Find frequent itemsets using Apriori algorithm

itemFrequencyPlot(dataset, topN = 20, 
        main = "Top 20 Frequent Items", 
        xlab = "Items", ylab = "Frequency")

# Find frequent itemsets with minimum support of 0.01

rules <- apriori(dataset, 
              parameter = list(support = 0.01, confidence = 0.5))
# Inspect the top 5 rules
inspect(rules[1:5])
# Sort rules by lift

inspect(sort(rules, by = "lift")[1:5])
# Visualize the rules using a scatter plot
plot(rules, method = "scatter", 
     measure = c("support", "lift"), 
     shading = "confidence", 
     main = "Market Basket Analysis")
