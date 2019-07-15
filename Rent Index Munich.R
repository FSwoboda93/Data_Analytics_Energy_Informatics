#this dataset and the examples are taken from Fahrmeir, L., Kneib, T., Lang, S., 2009. Regression. Springer Berlin Heidelberg.
library(dplyr)
library(ggplot2)


#load the dataset and name the columns correctly
mietspiegel <- read.csv2("rentIndex.raw", sep="\t", stringsAsFactors = FALSE)
mietspiegel <- mietspiegel %>% dplyr::mutate(miete = as.numeric(miete),
                                             mieteqm = as.numeric(mieteqm),
                                             bjahr = as.numeric(bjahr),
                                             bad = as.factor(bad),
                                             bezv = as.factor(bezv),
                                             zh = as.factor(zh))
mietspiegel$flage <- factor(mietspiegel$lage, labels = c("Normal", "Good", "Excellent"))
mietspiegel$kueche <- factor(mietspiegel$kueche, labels = c("Normal", "Upper"))

# Influence of bathroom furniture (one dummy variable) -------------------------

model1 <- lm(miete ~ flaeche*bad, data = mietspiegel)
summary(model1)

plot(model1)

# Influence of bathroom and kitchen furniture (multiple dummy variables) -------


model2 <- lm(miete ~ flaeche*(flage + kueche), data = mietspiegel)
summary(model2)


# Plot of the regression line

coefs <- coef(model2)

g <- ggplot(data=mietspiegel) + geom_point(aes(x=flaeche, y=miete)) +
  xlab("Living Area") + ylab("Net rent") +
  geom_abline(aes(intercept = coefs["(Intercept)"],
                  slope = coefs["flaeche"],
                  color="flage==Normal & kueche==Normal"))  +
  geom_abline(aes(intercept = coefs["(Intercept)"] + coefs["flageGood"],
                  slope = coefs["flaeche"] + coefs["flaeche:flageGood"],
                  color="flage==Good & kueche==Normal"))  +
  geom_abline(aes(intercept = coefs["(Intercept)"] +  coefs["kuecheUpper"],
                  slope = coefs["flaeche"] +  coefs["flaeche:kuecheUpper"],
                  color="flage==Normal & kueche==Upper"))  +
  geom_abline(aes(intercept = coefs["(Intercept)"] + coefs["flageExcellent"],
                  slope = coefs["flaeche"] + coefs["flaeche:flageExcellent"],
                  color="flage==Excellent & kueche==Normal"))  +
  geom_abline(aes(intercept = coefs["(Intercept)"] + coefs["kuecheUpper"] + coefs["flageGood"],
                  slope = coefs["flaeche"] + coefs["flaeche:flageGood"] + coefs["flaeche:kuecheUpper"],
                  color="flage==Good & kueche==Upper")) +
  geom_abline(aes(intercept = coefs["(Intercept)"] + coefs["kuecheUpper"] + coefs["flageExcellent"],
                  slope = coefs["flaeche"] + coefs["flaeche:flageExcellent"] + coefs["flaeche:kuecheUpper"],
                  color="flage==Excellent & kueche==Upper"))  +
  scale_color_discrete(name = "Legend")
g 

# Dummy variables for multiple categories ---------------------------------

#manually coded dummy variables, normal exposure is reference category
mietspiegel$lage_g <- ifelse(mietspiegel$flage=="Good",1,0)
mietspiegel$lage_b <- ifelse(mietspiegel$flage=="Excellent",1,0)
model3 <- lm(miete ~ flaeche*(lage_g+lage_b+kueche), data = mietspiegel)
summary(model3)

#this dataset and the examples are taken from Fahrmeir, L., Kneib, T., Lang, S., 2009. Regression. Springer Berlin Heidelberg.
library(dplyr)
library(ggplot2)


#load the dataset and name the columns correctly
mietspiegel <- read.csv2("~/Downloads/rentIndex.raw", sep="\t", stringsAsFactors = FALSE)
mietspiegel <- mietspiegel %>% dplyr::mutate(miete = as.numeric(miete),
                                             mieteqm = as.numeric(mieteqm),
                                             bjahr = as.numeric(bjahr),
                                             lage = as.factor(lage),
                                             bad = as.factor(bad),
                                             kueche = as.factor(kueche),
                                             bezv = as.factor(bezv),
                                             zh = as.factor(zh))

# Find good predictors ----------------------------------------------------

#we exclude all dwellings build after 1995
mietspiegel <- mietspiegel %>% dplyr::filter(bjahr < 1996)

summary(mietspiegel)

#we want to explain the rent by explanatory variables, so we look for strong corelations
cor(mietspiegel)

#we take a look at the variables
g <- ggplot(data = mietspiegel) + geom_point(aes(x=flaeche, y=miete)) + xlab("Living Area") + ylab("Net rent")


# Simple linear model -----------------------------------------------------

#the net rent can be explained by the living area
model <- lm(miete ~ flaeche, data = mietspiegel)
summary(model)

#The results can be interpreted as follows
# * the estimated value of "flaeche" can be interpreted as the amount the rent 
#   increases with each square meter living area
# * the intercept

#This model is estimated with the least squares method. We assume for the residuals:
# 1) they are independent 
# 2) they have the same variance and their expected value is 0
# 3) the residuals are normally distributed

coefs <- coef(model)

# Plotting the regression line, we can see that the variance increases at higher living areas
g + geom_abline(aes(intercept = coefs["(Intercept)"], slope = coefs["flaeche"]), color="red")

plot(model)

# Transform variables -----------------------------------------------------

#Now, we look at the rent per square meter
g2 <- ggplot(data = mietspiegel) + 
  geom_point(aes(x=flaeche, y=mieteqm)) + 
  xlab("Living Area") + ylab("Net rent per square meter")
g2


#It seems to be a non-linear relation between rent and living area
model2 <- lm(mieteqm ~ I(1/flaeche), data = mietspiegel)



# The I() function leads to an interpretation of the / opeator in an aritmetical way
summary(model2)

#we can plot this non-linear relationship while predicting the y-values
coefs2 <- coefficients(model2)


pred <- predict(model2, data.frame(flaeche = min(mietspiegel$flaeche):max(mietspiegel$flaeche)))
predictionDF <- data.frame(flaeche=min(mietspiegel$flaeche):max(mietspiegel$flaeche), mieteqm = pred)

g2 <- ggplot(data = mietspiegel) + geom_point(aes(x=flaeche, y=mieteqm)) + xlab("Living Area") + ylab("Net rent")
g2 <- g2 +  geom_line(data = predictionDF, aes(x=flaeche, y=mieteqm), color="red")
g2

plot(model2)

# Much noise in the data remains (only 15% of the variance can be explained with 
# the model according to R^2)
# -> try to use the exposure of dwellings (lage) in the next model
# 1=normal, 2=good, 3=Excellent

g3 <- ggplot(data = mietspiegel) + 
  geom_point(aes(x=flaeche, y=miete, shape=lage, col = lage)) +
  xlab("Living Area") + ylab("Net rent") 
g3 

g3 + ggplot2::facet_wrap(~lage)
# Add binary variables ----------------------------------------------------

# create a binary variable with 1 = good exposure, 0 = normal exposure
mietspiegel <- mietspiegel %>% dplyr::mutate(glage =  ifelse(as.numeric(mietspiegel$lage)>1, 1, 0))
model3 <- lm(miete ~ flaeche + glage, data = mietspiegel)
summary(model3)

coefs3 <- coefficients(model3)

#plot both cases
g4 <- ggplot(data = mietspiegel) + 
  geom_point(aes(x=flaeche, y=miete)) + 
  xlab("Living Area") + ylab("Net rent") +
  geom_abline(aes(intercept = coefs3["(Intercept)"], slope = coefs3["flaeche"], color="Normal Exposure")) + 
  geom_abline(aes(intercept = coefs3["(Intercept)"] + coefs3["glage"], slope = coefs3["flaeche"] , color="Good Exposure"))
g4


#Create a dummy variable
mietspiegel$flage <- factor(mietspiegel$lage, labels = c("Normal", "Good", "Excellent"))

#Use the flage
model4 <- lm(miete ~ flaeche + flage, data = mietspiegel)
summary(model4)

coefs4 <- coefficients(model4)

g4 <- ggplot(data = mietspiegel) + 
  geom_point(aes(x=flaeche, y=miete)) + 
  xlab("Living Area") + ylab("Net rent") +
  geom_abline(aes(intercept = coefs4["(Intercept)"], slope = coefs4["flaeche"], color="Normal exposure")) + 
  geom_abline(aes(intercept = coefs4["(Intercept)"] + coefs4["flageGood"], slope = coefs4["flaeche"] , color="Good exposure")) +
  geom_abline(aes(intercept = coefs4["(Intercept)"] + coefs4["flageExcellent"], slope = coefs4["flaeche"] , color="Excellent exposure")) +
  scale_color_discrete(name = "Legend")

g4


# Add interactions between variables --------------------------------------

# model the interactions between two regressors
model5 <- lm(miete ~ flaeche + glage + flaeche:glage, data = mietspiegel)
model5 <- lm(miete ~ flaeche*glage, data=mietspiegel)
summary(model5)

coefs5 <- coef(model5) 

# Interpretation
# (Intercept)   290.096789 (intersection with y-axis)
# flaeche         8.531074 (the slope)
# glage         -23.472519 (added to the intercept)
# flaeche:glage   1.325063 (added to the slope)

g5 <- ggplot(data = mietspiegel) + 
  geom_point(aes(x=flaeche, y=miete)) + 
  xlab("Living Area") + ylab("Net rent") +
  geom_abline(aes(intercept = coefs5["(Intercept)"], slope = coefs5["flaeche"],  color="Normal exposure")) +
  geom_abline(aes(intercept = coefs5["(Intercept)"] + coefs5["glage"], slope = coefs5["flaeche"] + coefs5["flaeche:glage"], color="Good exposure"))
g5


#interaction effect are often used, therefore there is a shortcut for the same model
model6 <- lm(miete ~ flaeche*glage, data = mietspiegel)
summary(model6)

#Use again the flage parameter
model7 <- lm(miete ~ flaeche*flage, data = mietspiegel)

coefs7 <- coef(model7)
summary(model7)

g6 <- ggplot(data = mietspiegel) + 
  geom_point(aes(x=flaeche, y=miete)) + 
  xlab("Living Area") + ylab("Net rent")+
  geom_abline(aes(intercept = coefs7["(Intercept)"], slope = coefs7["flaeche"], color="Normal exposure"))  +
  geom_abline(aes(intercept = coefs7["(Intercept)"] + coefs7["flageGood"], slope = coefs7["flaeche"] + coefs7["flaeche:flageGood"], color="Good exposure")) +
  geom_abline(aes(intercept = coefs7["(Intercept)"] + coefs7["flageExcellent"], slope = coefs7["flaeche"] + coefs7["flaeche:flageExcellent"], color="Excellent exposure")) +
  scale_color_discrete(name="Legend")
g6
# Multiple dummy variables ------------------------------------------------

#we can also add multiple dummy variables. Mind that we only want to consider the 
#effect between living area / exposure, and living area / bathroom, but not the 
#effect between bathrom and exposure!
model7 <- lm(miete ~ flaeche * (glage + bad), data = mietspiegel)
summary(model7)

#Model 7 is equal to model 8 (formula in model 8 is just a 'shortcut')
model8 <- lm(miete ~ flaeche + glage + bad + flaeche:glage + flaeche:bad, data = mietspiegel)
summary(model8)

# the coefficients can be interpreted as follows:
#
# (Intercept)   316.9213    23.1021  13.718  < 2e-16 ***
# --> the shift on y-axis
#
# flaeche         7.9512     0.3389  23.465  < 2e-16 ***
# --> the slope (main effect)
#
# glage         -30.7203    33.4406  -0.919  0.35835    
# --> additional y-axis-shift for good exposure
#
# bad1           108.5022    80.5113   1.348  0.17787    
# --> additional y-axis-shift for good bathroom furniture
#
# flaeche:glage   1.4333     0.4652   3.081  0.00208 ** 
# --> additional slope for good exposure
#
# flaeche:bad     1.9884     0.8884   1.113  0.26599    
# --> additional slope for good bathroom furniture

# The coefficients "glage", "bad" and "flaeche:bad" are not significant, but
# they are included in the model. Therefore they must be used while drawing the 
# regression line!

