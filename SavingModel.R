# Load required libraries
library(caret)
library(randomForest)

# Define training control
ctrl <- trainControl(method = "cv",  # Use cross-validation
                     number = 10)    # Number of folds

# Train the Random Forest model
set.seed(123)  # Set seed for reproducibility
rf_model <- train(output ~ ., data = heart_attack_data_clean, method = "rf", trControl = ctrl)

# Create a directory named "models" if it doesn't exist
if (!file.exists("./models")) {
  dir.create("./models")
}

# Saving the Random Forest model for the heart attack dataset
saveRDS(rf_model, file = "./models/heart_attack_rf_model.rds")

# Load the saved Random Forest model
loaded_heart_attack_rf_model <- readRDS("./models/heart_attack_rf_model.rds")

# Prepare new data for prediction (adjusting for factor conversions)
new_heart_attack_data <- data.frame(
  age = c(63, 37, 41),   # Example age values
  sex = factor(c(1, 1, 0), levels = c(0, 1), labels = c("female", "male")),  # Adjusted sex values
  cp = factor(c(3, 2, 1), levels = c(1, 2, 3, 4), labels = c("typical angina", "atypical angina", "non-anginal pain", "asymptomatic")),  # Adjusted cp values
  trtbps = c(145, 130, 130),  # Example trtbps values
  chol = c(233, 250, 204),    # Example chol values
  fbs = factor(c(1, 0, 0), levels = c(0, 1), labels = c("false", "true")),      # Adjusted fbs values
  restecg = factor(c(0, 1, 0), levels = c(0, 1, 2), labels = c("normal", "ST-T wave abnormality", "left ventricular hypertrophy")),  # Adjusted restecg values
  thalachh = c(150, 187, 172),  # Example thalachh values
  exng = factor(c(0, 0, 0), levels = c(0, 1), labels = c("no", "yes")),     # Adjusted exng values
  oldpeak = c(2.3, 3.5, 1.4),   # Example oldpeak values
  slp = factor(c(0, 0, 2), levels = c(0, 1, 2), labels = c("downsloping", "flat", "upsloping")),      # Adjusted slp values
  caa = factor(c(0, 0, 2), levels = c(0, 1, 2, 3), labels = c("0", "1", "2", "3")),      # Adjusted caa values
  thall = factor(c(1, 2, 2), levels = c(1, 2, 3), labels = c("normal", "fixed defect", "reversible defect")),    # Adjusted thall values
  o2_saturation = c(98.6, 98.6, 98.6)  # Example o2_saturation values
)

# Use the loaded model to make predictions for new heart attack data
predictions_rf_loaded_model <- predict(loaded_heart_attack_rf_model, newdata = new_heart_attack_data)

# Print predictions
print(predictions_rf_loaded_model)

