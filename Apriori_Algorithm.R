# Apriori algorithm is a popular algorithm used for mining frequent itemsets 
# and generating association rules in transactional databases.
# it is widely used in market basket analysis to discover relationships
# between items purchased together.
# The algorithm works by identifying frequent itemsets in the dataset and 
# then generating association rules based on these itemsets.

# Association rules are used to find interesting relationships between
# variables in large datasets.


library(arules)

data <- read.csv("C:/Users/Navneet singh/Downloads/Market_Basket_Optimisation.csv",
                 header = FALSE)
View(data)

# Convert the data to transactions because apriori algorithm
# works on transaction data so we need to convert it into sparse matrix

dataset <- read.transactions("C:/Users/Navneet singh/Downloads/Market_Basket_Optimisation.csv",
                          sep = ",",
                          rm.duplicates = TRUE,
                          format = "basket")

summary(dataset)
itemFrequencyPlot(dataset,topN = 20)

# The apriori algorithm is used to find frequent itemsets and
# generate association rules.

# apriori function is used to find frequent itemsets and generate association rules
# it takes a dataset and a set of parameters as input
# here support and confidence are the two main parameters support is calculated as the
# proportion of transactions that contain the itemset, 
# while confidence is calculated as the proportion of transactions that contain the 
# itemset given the antecedent.

rules = apriori(data = dataset,
                parameter = list(support = 0.004, confidence = 0.2))


# The inspect function is used to view the rules
# The sort function is used to sort the rules by lift
# The lift is a measure of how much more likely the consequent is to occur
# given the antecedent compared to its overall frequency.
inspect(sort(rules, by = "lift")[1:10])


# The plot function is used to visualize the rules

library(arulesViz)
plot(rules[1:10], method = "graph", control = list(type = "items"))

