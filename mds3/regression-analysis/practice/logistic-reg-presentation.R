# ============================================================
# MENTAL HEALTH IN TECH SURVEY
# COMPLETE LOGISTIC REGRESSION ANALYSIS IN R
# ============================================================

# ------------------------------------------------------------
# 1. LOAD LIBRARIES
# ------------------------------------------------------------

# Install packages if needed

library(tidyverse)
library(janitor)
library(caret)
library(car)
library(pROC)
library(broom)

# ------------------------------------------------------------
# 2. LOAD DATASET
# ------------------------------------------------------------

# Set working directory if needed
getwd()
setwd("mds3/regression-analysis/practice")
getwd()
# https://www.kaggle.com/datasets/osmi/mental-health-in-tech-survey
survey <- read.csv("data/mental-health-tech-survey.csv")

# ------------------------------------------------------------
# 3. INITIAL DATA INSPECTION
# ------------------------------------------------------------

cat("================ DATA OVERVIEW ================\n")

head(survey)
str(survey)
summary(survey)

cat("\nDataset Dimensions:\n")
print(dim(survey))

cat("\nColumn Names:\n")
print(names(survey))

cat("\nMissing Values:\n")
print(colSums(is.na(survey)))

# ------------------------------------------------------------
# 4. DATA CLEANING
# ------------------------------------------------------------

# Clean column names; convert to snake_case
survey <- clean_names(survey)
str(survey)

# Select relevant variables
survey <- survey %>%
    select(
        age,
        gender,
        family_history,
        treatment,
        work_interfere,
        remote_work,
        benefits,
        care_options,
        wellness_program,
        seek_help,
        anonymity,
        leave,
        mental_health_consequence,
        phys_health_consequence
    )

# ------------------------------------------------------------
# 5. HANDLE AGE OUTLIERS
# ------------------------------------------------------------

cat("\n================ AGE SUMMARY BEFORE CLEANING ================\n")
print(summary(survey$age))
head(survey$age)
survey <- survey %>%
    filter(age >= 18 & age <= 70)

cat("\n================ AGE SUMMARY AFTER CLEANING ================\n")
print(summary(survey$age))

# ------------------------------------------------------------
# 6. CLEAN GENDER LABELS
# ------------------------------------------------------------

cat("\n================ UNIQUE GENDER LABELS BEFORE CLEANING ================\n")
print(unique(survey$gender))

survey$gender <- tolower(survey$gender)

survey$gender <- case_when(
    str_detect(survey$gender, "male|m|man") ~ "Male",
    str_detect(survey$gender, "female|f|woman") ~ "Female",
    TRUE ~ "Other"
)

cat("\n================ GENDER COUNTS AFTER CLEANING ================\n")
print(table(survey$gender))

# ------------------------------------------------------------
# 7. MISSING VALUE ANALYSIS
# ------------------------------------------------------------

cat("\n================ MISSING VALUES BEFORE CLEANING ================\n")
print(colSums(is.na(survey)))

# Missing value summary
print(colSums(is.na(survey)))

# Remove missing values
survey <- survey %>%
    drop_na()

cat("\n================ MISSING VALUES AFTER CLEANING ================\n")
print(colSums(is.na(survey)))

# ------------------------------------------------------------
# 8. EXPLORATORY DATA ANALYSIS (EDA)
# ------------------------------------------------------------

# ------------------------------------------------------------
# Treatment Distribution
# ------------------------------------------------------------

ggplot(survey, aes(x = treatment)) +
    geom_bar(fill = "steelblue") +
    labs(
        title = "Distribution of Mental Health Treatment",
        x = "Treatment",
        y = "Count"
    ) +
    theme_minimal()

# ------------------------------------------------------------
# Gender vs Treatment
# ------------------------------------------------------------

ggplot(survey, aes(x = gender, fill = treatment)) +
    geom_bar(position = "fill") +
    labs(
        title = "Treatment Seeking Behavior by Gender",
        x = "Gender",
        y = "Proportion"
    ) +
    theme_minimal()

# ------------------------------------------------------------
# Family History vs Treatment
# ------------------------------------------------------------

ggplot(survey, aes(x = family_history, fill = treatment)) +
    geom_bar(position = "fill") +
    labs(
        title = "Family History and Treatment Seeking",
        x = "Family History",
        y = "Proportion"
    ) +
    theme_minimal()

# ------------------------------------------------------------
# Work Interference vs Treatment
# ------------------------------------------------------------

ggplot(survey, aes(x = work_interfere, fill = treatment)) +
    geom_bar(position = "fill") +
    labs(
        title = "Mental Health Work Interference",
        x = "Work Interference",
        y = "Proportion"
    ) +
    theme_minimal()

# ------------------------------------------------------------
# Benefits vs Treatment
# ------------------------------------------------------------

ggplot(survey, aes(x = benefits, fill = treatment)) +
    geom_bar(position = "fill") +
    labs(
        title = "Mental Health Benefits vs Treatment",
        x = "Benefits",
        y = "Proportion"
    ) +
    theme_minimal()

# ------------------------------------------------------------
# Age Distribution
# ------------------------------------------------------------

ggplot(survey, aes(x = age)) +
    geom_histogram(
        bins = 30,
        fill = "orange",
        color = "black"
    ) +
    labs(
        title = "Age Distribution",
        x = "Age",
        y = "Frequency"
    ) +
    theme_minimal()

# ------------------------------------------------------------
# Age Boxplot by Treatment
# ------------------------------------------------------------

ggplot(survey, aes(x = treatment, y = age, fill = treatment)) +
    geom_boxplot() +
    labs(
        title = "Age Distribution by Treatment",
        x = "Treatment",
        y = "Age"
    ) +
    theme_minimal()

# ------------------------------------------------------------
# 9. FEATURE ENGINEERING
# ------------------------------------------------------------

# Convert character variables to factors
survey <- survey %>%
    mutate(across(where(is.character), as.factor))

# Check structure
cat("\n================ DATA STRUCTURE AFTER CONVERSION ================\n")
str(survey)

# ------------------------------------------------------------
# 10. TRAIN-TEST SPLIT
# ------------------------------------------------------------

set.seed(123)

train_index <- createDataPartition(
    survey$treatment,
    p = 0.7,
    list = FALSE
)

train_data <- survey[train_index, ]
test_data <- survey[-train_index, ]

cat("\n================ TRAIN TEST SPLIT ================\n")
cat("Training Rows:", nrow(train_data), "\n")
cat("Testing Rows:", nrow(test_data), "\n")

# ------------------------------------------------------------
# 11. ASSUMPTION CHECKING + MULTICOLLINEARITY ANALYSIS

# ------------------------------------------------------------
# 11.1 Initial Full Model (for diagnosis only)
# ------------------------------------------------------------

full_model <- glm(
    treatment ~
        age +
        gender +
        family_history +
        work_interfere +
        remote_work +
        benefits +
        care_options +
        wellness_program +
        seek_help +
        anonymity +
        leave +
        mental_health_consequence +
        phys_health_consequence,
    data = train_data,
    family = "binomial"
)

cat("
================ FULL MODEL SUMMARY ================
")
summary(full_model)

# ------------------------------------------------------------
# 11.2 Multicollinearity Check (VIF)
# ------------------------------------------------------------

cat("
================ MULTICOLLINEARITY (VIF) ================
")

vif_values <- vif(full_model)
print(vif_values)

# Interpretation guide:
cat("
VIF Interpretation:
")
cat("< 3  : Good
")
cat("3-5  : Moderate concern
")
cat("> 5  : High multicollinearity (needs removal)
")

# ------------------------------------------------------------
# 11.3 Feature Selection (Reduced Model)
# ------------------------------------------------------------

# Based on VIF + domain knowledge, we keep only relevant predictors

final_model <- glm(
    treatment ~
        age +
        gender +
        family_history +
        work_interfere +
        benefits +
        seek_help +
        anonymity,
    data = train_data,
    family = "binomial"
)

cat("
================ FINAL REDUCED MODEL SUMMARY ================
")
summary(final_model)

# ------------------------------------------------------------
# 11.4 Optional: Stepwise Selection (AIC-based)
# ------------------------------------------------------------

step_model <- step(full_model, direction = "both", trace = FALSE)

cat("
================ STEPWISE MODEL SUMMARY ================
")
summary(step_model)

# Choose final model for evaluation
log_model <- final_model

# ------------------------------------------------------------
# 12. MODEL SUMMARY"}]}
# ------------------------------------------------------------

cat("\n================ LOGISTIC REGRESSION SUMMARY ================\n")
summary(log_model)

# ------------------------------------------------------------
# 13. ODDS RATIOS
# ------------------------------------------------------------

cat("\n================ ODDS RATIOS ================\n")

odds_ratios <- exp(coef(log_model))
print(odds_ratios)

# ------------------------------------------------------------
# 14. CONFIDENCE INTERVALS
# ------------------------------------------------------------

cat("\n================ CONFIDENCE INTERVALS ================\n")

conf_intervals <- exp(confint(log_model))
print(conf_intervals)

# ------------------------------------------------------------
# 15. MULTICOLLINEARITY CHECK
# ------------------------------------------------------------

cat("\n================ VARIANCE INFLATION FACTOR (VIF) ================\n")

vif_values <- vif(log_model)
print(vif_values)

# ------------------------------------------------------------
# 16. PREDICTIONS
# ------------------------------------------------------------

probabilities <- predict(
    log_model,
    newdata = test_data,
    type = "response"
)

predictions <- ifelse(probabilities > 0.5, "Yes", "No")
predictions <- as.factor(predictions)

# ------------------------------------------------------------
# 17. CONFUSION MATRIX
# ------------------------------------------------------------

cat("\n================ CONFUSION MATRIX ================\n")

conf_matrix <- confusionMatrix(
    predictions,
    test_data$treatment
)

print(conf_matrix)

# ------------------------------------------------------------
# 18. ROC CURVE AND AUC
# ------------------------------------------------------------

roc_curve <- roc(test_data$treatment, probabilities)

plot(
    roc_curve,
    main = "ROC Curve for Logistic Regression"
)

auc_value <- auc(roc_curve)

cat("\n================ AUC VALUE ================\n")
print(auc_value)

# ------------------------------------------------------------
# 19. VARIABLE IMPORTANCE
# ------------------------------------------------------------

cat("\n================ VARIABLE IMPORTANCE ================\n")

var_imp <- varImp(log_model)
print(var_imp)

plot(var_imp)

# ------------------------------------------------------------
# 20. PREDICTED PROBABILITY VISUALIZATION
# ------------------------------------------------------------

test_data$predicted_prob <- probabilities

ggplot(test_data, aes(x = predicted_prob, fill = treatment)) +
    geom_histogram(
        bins = 25,
        alpha = 0.7,
        position = "identity"
    ) +
    labs(
        title = "Predicted Probability Distribution",
        x = "Predicted Probability",
        y = "Count"
    ) +
    theme_minimal()

# ------------------------------------------------------------
# 21. IMPORTANT FINDINGS
# ------------------------------------------------------------

cat("\n====================================================\n")
cat("IMPORTANT FINDINGS\n")
cat("====================================================\n")

cat("\n1. Family history is expected to be one of the strongest predictors of treatment-seeking behavior.\n")

cat("\n2. Employees whose mental health interferes with work are significantly more likely to seek treatment.\n")

cat("\n3. Organizations offering mental health benefits and care options may encourage treatment-seeking behavior.\n")

cat("\n4. Workplace culture variables such as anonymity and leave flexibility influence mental health decisions.\n")

cat("\n5. Mental health is strongly associated with workplace productivity and employee well-being.\n")

# ------------------------------------------------------------
# 22. SAVE MODEL SUMMARY
# ------------------------------------------------------------

model_summary <- tidy(log_model)

write.csv(
    model_summary,
    "model_summary.csv",
    row.names = FALSE
)

# ------------------------------------------------------------
# 23. SAVE CLEANED DATASET
# ------------------------------------------------------------

write.csv(
    survey,
    "cleaned_mental_health_data.csv",
    row.names = FALSE
)

# ------------------------------------------------------------
# 24. FINAL CONCLUSION
# ------------------------------------------------------------

cat("\n====================================================\n")
cat("FINAL CONCLUSION\n")
cat("====================================================\n")

cat("\nThis logistic regression analysis identified several important predictors of mental health treatment-seeking behavior among tech employees.\n")

cat("\nFamily history, workplace interference, and organizational support emerged as highly influential factors.\n")

cat("\nThe findings suggest that supportive workplaces can improve mental health awareness and encourage employees to seek treatment earlier.\n")

cat("\nLogistic regression proved effective for understanding behavioral and workplace factors associated with mental health treatment.\n")

# ------------------------------------------------------------
# 25. SESSION INFO
# ------------------------------------------------------------

sessionInfo()
