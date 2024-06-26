# Load dataset
heart_attack_data <- read.csv("heart_attack_data.csv", colClasses = c(
  age = "numeric",           # Age of the patient
  sex = "factor",            # Sex of the patient (0 = female, 1 = male)
  cp = "factor",             # Chest Pain type (1-4)
  trtbps = "numeric",        # Resting blood pressure (in mm Hg)
  chol = "numeric",          # Cholesterol in mg/dl
  fbs = "factor",            # Fasting blood sugar > 120 mg/dl (1 = true, 0 = false)
  restecg = "factor",        # Resting electrocardiographic results (0-2)
  thalachh = "numeric",      # Maximum heart rate achieved
  exng = "factor",           # Exercise induced angina (1 = yes, 0 = no)
  oldpeak = "numeric",       # Oldpeak
  slp = "factor",            # Slope of the peak exercise ST segment (0-2)
  caa = "factor",            # Number of major vessels (0-3)
  thall = "factor",          # Thall (1-3)
  o2_saturation = "numeric", # Oxygen saturation
  output = "factor"          # Target (0 = less chance of heart attack, 1 = more chance of heart attack)
))

# Convert categorical variables to factors with appropriate labels
heart_attack_data <- heart_attack_data %>%
  mutate(
    sex = factor(sex, levels = c(0, 1), labels = c("female", "male")),
    cp = factor(cp, levels = c(1, 2, 3, 4), labels = c("typical angina", "atypical angina", "non-anginal pain", "asymptomatic")),
    fbs = factor(fbs, levels = c(0, 1), labels = c("false", "true")),
    restecg = factor(restecg, levels = c(0, 1, 2), labels = c("normal", "ST-T wave abnormality", "left ventricular hypertrophy")),
    exng = factor(exng, levels = c(0, 1), labels = c("no", "yes")),
    slp = factor(slp, levels = c(0, 1, 2), labels = c("downsloping", "flat", "upsloping")),
    caa = factor(caa, levels = c(0, 1, 2, 3), labels = c("0", "1", "2", "3")),
    thall = factor(thall, levels = c(1, 2, 3), labels = c("normal", "fixed defect", "reversible defect")),
    output = factor(output, levels = c(0, 1), labels = c("less chance of heart attack", "more chance of heart attack"))
  )

# Load necessary library
library(caret)

# Set seed for reproducibility
set.seed(123)

# Split the data into training (70%) and testing (30%) sets
train_index <- createDataPartition(heart_attack_data_clean$output, p = 0.7, list = FALSE)
train_data <- heart_attack_data_clean[train_index, ]
test_data <- heart_attack_data_clean[-train_index, ]

# Set seed for reproducibility
set.seed(123)

# Define cross-validation control
cv <- trainControl(method = "cv", number = 5, verboseIter = TRUE)

# Your modeling code here (e.g., train a model using caret's train function)
# Example: train a logistic regression model using 5-fold cross-validation
model <- train(output ~ ., data = train_data, method = "glm", family = "binomial", trControl = cv)

# Print model performance
print(model)

# Load necessary libraries
library(randomForest)
library(e1071)  # For SVM
library(caret)

# Define training control
ctrl <- trainControl(method = "cv", number = 5, verboseIter = TRUE)

# Train Random Forest model
set.seed(123)
rf_model <- train(output ~ ., data = heart_attack_data_clean, method = "rf", trControl = ctrl)

# Train Support Vector Machine (SVM) model
set.seed(123)
svm_model <- train(output ~ ., data = heart_attack_data_clean, method = "svmRadial", trControl = ctrl)

# Train Logistic Regression model
set.seed(123)
logit_model <- train(output ~ ., data = heart_attack_data_clean, method = "glm", family = "binomial", trControl = ctrl)

# Print model results
print(rf_model)
print(svm_model)
print(logit_model)

# Load necessary libraries
library(caret)

# Define resampling method
ctrl <- trainControl(method = "cv", number = 5, verboseIter = TRUE)

# Compare model performance
models <- list(rf = rf_model, svm = svm_model, logit = logit_model)
model_resamples <- resamples(models)

# Summarize results
summary(model_resamples)

# Set seed for reproducibility
set.seed(123)

# Number of bootstrap iterations
n_bootstraps <- 1000

# Bootstrap function for age variable
bootstrap_age <- replicate(n_bootstraps, {
  # Sample with replacement from the age variable
  bootstrap_sample <- sample(heart_attack_data_clean$age, replace = TRUE)
  
  # Calculate statistic of interest (e.g., mean, median, standard deviation) for each bootstrap sample
  # Mean and standard deviation
  mean_age <- mean(bootstrap_sample)
  sd_age <- sd(bootstrap_sample)
  
  return(c(mean_age, sd_age))
})

# Calculate mean and standard deviation for each bootstrap sample
bootstrap_stats <- t(bootstrap_age)

# Calculate confidence intervals for mean and standard deviation
confidence_intervals <- apply(bootstrap_stats, 2, function(stat) {
  quantile(stat, c(0.025, 0.975))
})

# Print results
cat("Bootstrap Mean Age:", mean(bootstrap_stats[, 1]), "\n")
cat("95% Confidence Interval for Mean Age:", confidence_intervals[1, 1], "-", confidence_intervals[2, 1], "\n")
cat("Bootstrap Standard Deviation Age:", mean(bootstrap_stats[, 2]), "\n")
cat("95% Confidence Interval for Standard Deviation Age:", confidence_intervals[1, 2], "-", confidence_intervals[2, 2], "\n")
