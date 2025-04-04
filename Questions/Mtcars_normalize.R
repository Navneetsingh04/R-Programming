# Given a dataset mtcars(an in-built dataset) in R, normalize the numeric values
# in the dataset for 'am' attribute, replace 1 and 0 with the labels Y and N 
# respectively Create a boxplot for the normalized hp (horsepower) attribute


data <- mtcars

# Normalize numeric values using Min-Max Scaling
normalize <- function(x) {
        return ((x - min(x)) / (max(x) - min(x)))
}

data_norm <- as.data.frame(lapply(data, normalize))

# Replace 'am' values: 1 -> "Y", 0 -> "N"
data_norm$am <- factor(mtcars$am, levels = c(0, 1), labels = c("N", "Y"))

# Create a boxplot for the normalized 'hp' attribute
boxplot(data_norm$hp, main = "Boxplot of Normalized Horsepower (hp)", 
        ylab = "Normalized hp", col = "skyblue", border = "black")

# Display first few rows to verify changes
head(data_norm)
