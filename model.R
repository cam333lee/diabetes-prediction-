# import functions made in helper_functions folder
files <- list.files("helper_functions", pattern = "\\.R$", full.names = TRUE)
sapply(files, source)

data <- read.csv("diabetes.csv")
# 0 = No Diabetes, 1 = Diabetes
head(data)
summary(data)

# Now you can use your function as usual
cleaned_data <- clean_data(data)

X_train <- cleaned_data$X_train
y_train <- cleaned_data$y_train
X_test <- cleaned_data$X_test
y_test <- cleaned_data$y_test

## EDA Visualizations
plot_correlation_heatmap(data)
plot_outcome_distribution(data)
plot_pregnancies_by_outcome(data)
plot_bmi_by_outcome(data)

## Creating a Logistic Regression Model
log_model <- glm(y_train ~ ., data = X_train, family = binomial)

summary(log_model)

## Evaluating the Model
predictions <- predict(log_model, newdata = X_test, type = "response")

predictions <- factor(ifelse(predictions > 0.5, 1, 0), levels = levels(as.factor(y_test)))

confusionMatrix(predictions, as.factor(y_test))


## Making Predictions
predict_diabetes <- function(pregnancies, glucose, bloodpressure, skinthickness, 
                             insulin, bmi, diabetespedigreefunction, age) {
  input_data <- data.frame(
    Pregnancies = pregnancies,
    Glucose = glucose,
    BloodPressure = bloodpressure,
    SkinThickness = skinthickness,
    Insulin = insulin,
    BMI = bmi,
    DiabetesPedigreeFunction = diabetespedigreefunction,
    Age = age
  )
  input <- as.data.frame(input_data)
  prediction <- predict(log_model, newdata = input, type = "response")
  prediction <- factor(ifelse(prediction > 0.5, 1, 0), levels = levels(as.factor(prediction)))
  
  return(prediction)
}

new_patient <- data.frame(
  pregnancies = 6,
  glucose = 148,
  bloodpressure = 72,
  skinthickness = 35,
  insulin = 0,
  bmi = 33.6,
  diabetespedigreefunction = 0.627,
  age = 50
)

# Use do.call to pass new_patient as arguments to the function
prediction <- do.call(predict_diabetes, as.list(new_patient))

if (any(prediction == 1)) {
  cat("Based on the model's prediction, there is a higher chance of diabetes.")
} else {
  cat("Based on the model's prediction, the risk of diabetes appears lower.")
}