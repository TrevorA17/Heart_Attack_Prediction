# Load necessary libraries
library(plumber)

# Load the saved Random Forest model for heart attack prediction
loaded_heart_attack_rf_model <- readRDS("./models/heart_attack_rf_model.rds")

#* @apiTitle Heart Attack Prediction Model API
#* @apiDescription Used to predict the likelihood of a heart attack.

#* @post /predict_heart_attack
#* @param age Numeric: Age of the patient
#* @param sex Factor: Sex of the patient (female/male)
#* @param cp Factor: Chest pain type (typical angina/atypical angina/non-anginal pain/asymptomatic)
#* @param trtbps Numeric: Resting blood pressure (mm Hg)
#* @param chol Numeric: Cholesterol level (mg/dl)
#* @param fbs Factor: Fasting blood sugar (> 120 mg/dl) (false/true)
#* @param restecg Factor: Resting electrocardiographic results (normal/ST-T wave abnormality/left ventricular hypertrophy)
#* @param thalachh Numeric: Maximum heart rate achieved
#* @param exng Factor: Exercise induced angina (no/yes)
#* @param oldpeak Numeric: ST depression induced by exercise relative to rest
#* @param slp Factor: Slope of the peak exercise ST segment (downsloping/flat/upsloping)
#* @param caa Factor: Number of major vessels colored by fluoroscopy (0/1/2/3)
#* @param thall Factor: Thalassemia (normal/fixed defect/reversible defect)
#* @param o2_saturation Numeric: Oxygen saturation level
predict_heart_attack <- function(age, sex, cp, trtbps, chol, fbs, restecg, thalachh, exng, oldpeak, slp, caa, thall, o2_saturation) {
  # Prepare input data
  new_heart_attack_data <- data.frame(
    age = as.numeric(age),
    sex = factor(as.character(sex), levels = c("female", "male")),
    cp = factor(as.character(cp), levels = c("typical angina", "atypical angina", "non-anginal pain", "asymptomatic")),
    trtbps = as.numeric(trtbps),
    chol = as.numeric(chol),
    fbs = factor(as.character(fbs), levels = c("false", "true")),
    restecg = factor(as.character(restecg), levels = c("normal", "ST-T wave abnormality", "left ventricular hypertrophy")),
    thalachh = as.numeric(thalachh),
    exng = factor(as.character(exng), levels = c("no", "yes")),
    oldpeak = as.numeric(oldpeak),
    slp = factor(as.character(slp), levels = c("downsloping", "flat", "upsloping")),
    caa = factor(as.character(caa), levels = c("0", "1", "2", "3")),
    thall = factor(as.character(thall), levels = c("normal", "fixed defect", "reversible defect")),
    o2_saturation = as.numeric(o2_saturation)
  )
  
  # Use the loaded model to make predictions
  predictions <- predict(loaded_heart_attack_rf_model, newdata = new_heart_attack_data)
  
  # Return predictions
  return(as.character(predictions))
}

