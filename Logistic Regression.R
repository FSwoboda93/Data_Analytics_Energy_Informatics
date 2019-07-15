diabetesData <- read.csv2("~/Downloads/diabetes.csv", stringsAsFactors = FALSE)
View(diabetesData)

# Transforming the features
diabetesData$BMI <- as.numeric(diabetesData$BMI)
diabetesData$DiabetesPedigreeFunction <- as.numeric(diabetesData$DiabetesPedigreeFunction)
diabetesData$Outcome <- as.factor(diabetesData$Outcome)

# Learn logistic regression model
model <- glm(Outcome~BMI, data = diabetesData, family = "binomial")
# Print the model
summary(model)


# Now calculate the probability of an individual with BMI 25 not having diabetes
1/(1+exp(-(3.68641-0.09353*25)))

# Check it against the probability of a prediction based on the model
predict(model, data.frame(BMI = 25), type = "response")


# Adding age to the model
model <- glm(Outcome~BMI + Age, data = diabetesData, family = "binomial")
summary(model)

# Now calculate the probability of an 10-years old individual with BMI 25 not having diabetes
1/(1+exp(-(5.40378  -0.09825 *25 +  10*-0.04561)))
# Check it against the probability of a prediction based on the model
predict(model, data.frame(BMI = 25, Age = 10), type = "response")






