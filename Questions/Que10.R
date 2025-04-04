# In the "sales-data.csv" dataset, clean the data by handling missing values and 
# removing duplicates.
# Then: explore and visualize the structure of the data.

library(dplyr)
library(ggplot2)
library(VIM)

data <- read.csv("C:/Users/Navneet singh/Downloads/sales-data.csv", stringsAsFactors = TRUE)
View(data)
str(data)
summary(data)

# Remove duplicate rows
data <- distinct(data)

# Check missing values
sum(is.na(data))
aggr(data, col = c("blue", "red"), numbers = TRUE, sortVars = TRUE, 
     cex.axis = 0.7, gap = 2, ylab = c("Missing Data", "Pattern"))

# Select only numeric columns
numeric_col <- sapply(data, is.numeric)
data_numeric <- data[, numeric_col]

# Histograms
for (col in names(data_numeric)) {
        print(ggplot(data, aes_string(x = col)) +
                      geom_histogram(fill = "blue", color = "black", bins = 30) +
                      labs(title = paste("Distribution of", col)) +
                      theme_minimal())
}

# Correlation matrix
cor_matrix <- cor(data_numeric, use = "complete.obs")
print(cor_matrix)

