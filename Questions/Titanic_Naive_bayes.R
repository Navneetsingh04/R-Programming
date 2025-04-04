# Using the Titanic dataset (an in-built dataset), built a Naive Bayes classifier
# to predict whether a passenger survived the Titanic disaster based on various
# features. Spit the dataset into training je g. 70%) and lasting (30%) sets to 
# evaluate  the model's performance using a confusion matrix

library(e1071)
library(caTools)

# Load Titanic dataset and convert to data frame
data <- as.data.frame(Titanic)

# Expand dataset based on frequency column (each row becomes individual observation)
# rep is a function that replicates the rows of the data frame according to the frequency
expanded_data <- data[rep(1:nrow(data), data$Freq), 1:4]

# Convert categorical variables to factors (ensuring correct types)
expanded_data$Survived <- factor(expanded_data$Survived, levels = c("No", "Yes"))
expanded_data$Class <- factor(expanded_data$Class)
expanded_data$Sex <- factor(expanded_data$Sex)
expanded_data$Age <- factor(expanded_data$Age)

# Drop the frequency column (not needed for modeling)
expanded_data$Freq <- NULL

# Check for missing values
sum(is.na(expanded_data))

# Split dataset into training (70%) and testing (30%) sets
set.seed(123)
split <- sample.split(expanded_data$Survived, SplitRatio = 0.7)
train_data <- subset(expanded_data, split == TRUE)
test_data <- subset(expanded_data, split == FALSE)

# Train the Naive Bayes model
nb_model <- naiveBayes(Survived ~ ., data = train_data)

# Make predictions
predictions <- predict(nb_model, test_data)

# Evaluate model using confusion matrix
conf_matrix <- table(test_data$Survived, predictions)
print(conf_matrix)

# Compute accuracy
accuracy <- sum(diag(conf_matrix)) / sum(conf_matrix)
accuracy*100
