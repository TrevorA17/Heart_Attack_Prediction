# Load necessary libraries
library(tidyverse)

# Load dataset
heart_attack_data <- read.csv("data/heart.csv", colClasses = c(
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

# Display the structure of the dataset
str(heart_attack_data)

# View the first few rows of the dataset
head(heart_attack_data)

# View the dataset in a separate viewer window
View(heart_attack_data)

# Function to calculate and print frequency and relative frequency
calculate_frequencies <- function(data, column) {
  freq <- table(data[[column]])
  rel_freq <- prop.table(freq)
  
  freq_df <- data.frame(
    Category = names(freq),
    Frequency = as.numeric(freq),
    Relative_Frequency = as.numeric(rel_freq)
  )
  
  print(freq_df)
}

# Calculate and print frequencies for each categorical variable
categorical_vars <- c("sex", "cp", "fbs", "restecg", "exng", "slp", "caa", "thall", "output")

for (var in categorical_vars) {
  cat("\nFrequencies for", var, ":\n")
  calculate_frequencies(heart_attack_data, var)
}

# Measures of Central Tendency
central_tendency <- heart_attack_data %>%
  summarize(
    mean_age = mean(age, na.rm = TRUE),
    median_age = median(age, na.rm = TRUE),
    mode_age = as.numeric(names(sort(table(age), decreasing = TRUE)[1])),
    
    mean_trtbps = mean(trtbps, na.rm = TRUE),
    median_trtbps = median(trtbps, na.rm = TRUE),
    mode_trtbps = as.numeric(names(sort(table(trtbps), decreasing = TRUE)[1])),
    
    mean_chol = mean(chol, na.rm = TRUE),
    median_chol = median(chol, na.rm = TRUE),
    mode_chol = as.numeric(names(sort(table(chol), decreasing = TRUE)[1])),
    
    mean_thalachh = mean(thalachh, na.rm = TRUE),
    median_thalachh = median(thalachh, na.rm = TRUE),
    mode_thalachh = as.numeric(names(sort(table(thalachh), decreasing = TRUE)[1])),
    
    mean_oldpeak = mean(oldpeak, na.rm = TRUE),
    median_oldpeak = median(oldpeak, na.rm = TRUE),
    mode_oldpeak = as.numeric(names(sort(table(oldpeak), decreasing = TRUE)[1])),
    
    mean_o2_saturation = mean(o2_saturation, na.rm = TRUE),
    median_o2_saturation = median(o2_saturation, na.rm = TRUE),
    mode_o2_saturation = as.numeric(names(sort(table(o2_saturation), decreasing = TRUE)[1]))
  )

print(central_tendency)

# Measures of Distribution
distribution_measures <- heart_attack_data %>%
  summarize(
    var_age = var(age, na.rm = TRUE),
    sd_age = sd(age, na.rm = TRUE),
    range_age = range(age, na.rm = TRUE),
    
    var_trtbps = var(trtbps, na.rm = TRUE),
    sd_trtbps = sd(trtbps, na.rm = TRUE),
    range_trtbps = range(trtbps, na.rm = TRUE),
    
    var_chol = var(chol, na.rm = TRUE),
    sd_chol = sd(chol, na.rm = TRUE),
    range_chol = range(chol, na.rm = TRUE),
    
    var_thalachh = var(thalachh, na.rm = TRUE),
    sd_thalachh = sd(thalachh, na.rm = TRUE),
    range_thalachh = range(thalachh, na.rm = TRUE),
    
    var_oldpeak = var(oldpeak, na.rm = TRUE),
    sd_oldpeak = sd(oldpeak, na.rm = TRUE),
    range_oldpeak = range(oldpeak, na.rm = TRUE),
    
    var_o2_saturation = var(o2_saturation, na.rm = TRUE),
    sd_o2_saturation = sd(o2_saturation, na.rm = TRUE),
    range_o2_saturation = range(o2_saturation, na.rm = TRUE)
  )

print(distribution_measures)
