# Load packages
# install.packages("ROCR")
# install.packages("mice")
library(mlbench) # For PimaIndiansDiabetes2
library(tidyverse) # For data transformation & plots
library(caret) # For partitioning & confusion matrix
library(ROCR) # For ROC & AUC calculation
library(mice) # For handling missing values via imputation
library(car) # For VIF and residual plots

# Load the data
data("PimaIndiansDiabetes2")
glimpse(PimaIndiansDiabetes2)

# Count NA values per column
colSums(is.na(PimaIndiansDiabetes2))

# Perform Multiple Imputation by Chained Equations (MICE)
set.seed(123)
imputed_data <- mice(PimaIndiansDiabetes2, m = 1, method = "pmm", printFlag = FALSE)

# Complete the dataset with the imputed values
clean_diabetes <- complete(imputed_data)

# Re-verify that missing values are gone
sum(is.na(clean_diabetes))

# Baseline class distribution
prop.table(table(clean_diabetes$diabetes))

# Visualize Glucose vs. Diabetes
ggplot(clean_diabetes, aes(x = diabetes, y = glucose, fill = diabetes)) +
    geom_boxplot(alpha = 0.7) +
    theme_minimal() +
    labs(title = "Glucose Concentration by Diabetes Status", x = "Diabetes Status", y = "Plasma Glucose")

set.seed(42)

# Stratified split using caret
train_index <- createDataPartition(clean_diabetes$diabetes, p = 0.8, list = FALSE)

train_set <- clean_diabetes[train_index, ]
test_set <- clean_diabetes[-train_index, ]

# Fit generalized linear model
diabetes_model <- glm(diabetes ~ ., data = train_set, family = binomial)

# Review parameters
summary(diabetes_model)

# Check for multicollinearity using VIF
vif(diabetes_model)

#  No VIF values above 5, so multicollinearity is not a concern here.

# Calculate Odds Ratios and 95% Confidence Intervals
exp(cbind(OR = coef(diabetes_model), confint(diabetes_model)))

# Predict probabilities
test_set$prob <- predict(diabetes_model, newdata = test_set, type = "response")

# Convert probabilities to a factor class using a standard 0.5 threshold
test_set$pred_class <- factor(ifelse(test_set$prob > 0.5, "pos", "neg"), levels = c("neg", "pos"))

# Generate the Confusion Matrix
confusionMatrix(test_set$pred_class, test_set$diabetes, positive = "pos")

# Calculate ROC components
pred_obj <- prediction(test_set$prob, test_set$diabetes)
perf_obj <- performance(pred_obj, measure = "tpr", x.measure = "fpr")

# Plot ROC
plot(perf_obj, col = "darkgreen", lwd = 2, main = "ROC Curve: Pima Indians Diabetes Model")
abline(a = 0, b = 1, lty = 2, col = "red") # Random baseline

# Calculate AUC
auc_obj <- performance(pred_obj, measure = "auc")
print(paste("AUC Score:", round(auc_obj@y.values[[1]], 4)))
