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
