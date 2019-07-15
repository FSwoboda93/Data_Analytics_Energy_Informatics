library(dplyr)
library(ggplot2)

#Load the data
Shower_data <- read.csv2("~/Downloads/Shower_data.csv")
Survey_data <- read.csv2("~/Downloads/Shower_Survey_data.csv")

#Remove the first shower, create groups
Shower_data <- Shower_data %>% dplyr::filter(Shower != 1) %>%
  dplyr::mutate(isTreatment = ifelse(group %in% c("1","2"), FALSE, TRUE),
                isIntervention = ifelse(Shower <= 10, FALSE, TRUE))

 
#Create the model for the DiD effect 
model2 <- lm( Volume ~ Shower + isTreatment * isIntervention, data = Shower_data)
summary(model2)
#equal to 
#model3 <- lm( Volume ~ Shower + dummy_phase + dummy_exp_group + dummy_phase:dummy_exp_group, data = Shower_data)


# Vizualize the DiD model -------------------------------------------------

g <- ggplot2::ggplot(data=Shower_data) + geom_point(aes(x=Shower, y=Volume), shape=1) + xlim(c(0,150)) + ylim(c(30,50))

ic <- coefficients(model2)["(Intercept)"] 
slope <- coefficients(model2)["Shower"]
d_inter <- coefficients(model2)["isInterventionTRUE"]
d_exp_t <- coefficients(model2)["isTreatmentTRUE"]
d_inter_exp <- coefficients(model2)["isTreatmentTRUE:isInterventionTRUE"]



#control group baseline phase
g <- g + geom_segment(x = 0, y = ic,
         xend = 9, yend = ic + 9*slope, col = "red", lwd=2)
#control group treatment phase
g <- g + geom_segment(x = 10, y = ic + d_inter,
         xend = 150, yend = ic + d_inter + 150 * slope, col = "red", lwd=2)

g <- g + geom_segment(x = 9, y = ic + 9*slope,
         xend = 10, yend = ic + d_inter, col = "red", lwd=2, lty="dotted")


#treatment group baseline phase
g <- g + geom_segment(x = 0, y = ic + d_exp_t,
         xend = 9, yend = ic + d_exp_t + 9*slope, col = "blue", lwd=2)
#treatment group intervention phase
g <- g + geom_segment(x = 10, y = ic + d_inter + d_inter_exp,
         xend = 150, yend = ic + d_inter + d_inter_exp + 150 * slope, col = "blue", lwd=2)

g <- g + geom_segment(x = 9, y = ic + d_exp_t + 9*slope,
         xend = 10, yend = ic + d_inter + d_inter_exp, col = "blue", lwd=2, lty="dotted")

g


# Qualitative Influence -------------------------------------------------
table(Survey_data$sem05_unnuetz)

Shower_data <- dplyr::left_join(Shower_data, 
                                Survey_data,
                                "Hh_ID") %>% 
               dplyr::rename(unnuetz = sem05_unnuetz)


## Reverse scale; A positive value of unnuetz means now 
## that the participants thinks that the device is useless
Shower_data$unnuetz <- Shower_data$unnuetz * -1

#Create a model that considers the baseline and "unnuetz" for the treatment group
model3 <- lm(Volume ~ (Shower + isTreatment * isIntervention) * unnuetz, data = Shower_data)
summary(model3)


# Remarks:
#  * The encoding of 'unnuetz' reaches from -3 - +3. In the regression model, the 
#    effect of unnuetz=3 will be 3 times stronger than the effect of unnuetz=1. 
#    Therefore such an encoding must be handled very carefully.
#  * In this model, the effect of 'unnuetz' has an interaction with the time 
#    dimension ('Shower'). This means that 'unnuetz' affects the intercept and 
#    the slope!
#  * R^2 is very low because in this context we are not trying to explain the 
#    dependant variable but we want to explain a given effect (DiD-model). 
#    With the given variables it is understandable that we cannot explain the 
#    volume of the showers (group assignment and perceived usefulness are no 
#    predictors for that) - we would achieve much higher R^2 if we base our 
#    model on the savings

library(dplyr)
library(ggplot2)

#Load the data
Shower_data <- read.csv2("~/Downloads/Shower_data.csv")
Survey_data <- read.csv2("~/Downloads/Shower_Survey_data.csv")

#Remove the first shower, create groups
Shower_data <- Shower_data %>% dplyr::filter(Shower != 1) %>%
  dplyr::mutate(isTreatment = ifelse(group %in% c("1","2"), FALSE, TRUE),
                isIntervention = ifelse(Shower <= 10, FALSE, TRUE))


#Create the model for the DiD effect 
model2 <- lm( Volume ~ Shower +isTreatment * isIntervention, data = Shower_data)


summary(model2)
#equal to 
#model3 <- lm( Volume ~ Shower + dummy_phase + dummy_exp_group + dummy_phase:dummy_exp_group, data = Shower_data)


# Vizualize the DiD model -------------------------------------------------

g <- ggplot2::ggplot(data=Shower_data) + geom_point(aes(x=Shower, y=Volume), shape=1) + xlim(c(0,150)) + ylim(c(30,50))

ic <- coefficients(model2)["(Intercept)"] 
slope <- coefficients(model2)["Shower"]
d_inter <- coefficients(model2)["isInterventionTRUE"]
d_exp_t <- coefficients(model2)["isTreatmentTRUE"]
d_inter_exp <- coefficients(model2)["isTreatmentTRUE:isInterventionTRUE"]



#control group baseline phase
g <- g + geom_segment(x = 0, y = ic,
                      xend = 9, yend = ic + 9*slope, col = "red", lwd=2)
#control group treatment phase
g <- g + geom_segment(x = 10, y = ic + d_inter,
                      xend = 150, yend = ic + d_inter + 150 * slope, col = "red", lwd=2)

g <- g + geom_segment(x = 9, y = ic + 9*slope,
                      xend = 10, yend = ic + d_inter, col = "red", lwd=2, lty="dotted")


#treatment group baseline phase
g <- g + geom_segment(x = 0, y = ic + d_exp_t,
                      xend = 9, yend = ic + d_exp_t + 9*slope, col = "blue", lwd=2)
#treatment group intervention phase
g <- g + geom_segment(x = 10, y = ic + d_inter + d_inter_exp,
                      xend = 150, yend = ic + d_inter + d_inter_exp + 150 * slope, col = "blue", lwd=2)

g <- g + geom_segment(x = 9, y = ic + d_exp_t + 9*slope,
                      xend = 10, yend = ic + d_inter + d_inter_exp, col = "blue", lwd=2, lty="dotted")

g


# Qualitative Influence -------------------------------------------------
table(Survey_data$sem05_unnuetz)

Shower_data <- dplyr::left_join(Shower_data, Survey_data, "Hh_ID") %>% dplyr::rename(unnuetz = sem05_unnuetz)



####



model6 <- lm(Volume ~ Shower + (isTreatment * isIntervention) * (gesl), data = Shower_data)
summary(model6)



## Reverse scale; A positive value of unnuetz means now 
## that the participants thinks that the device is useless
Shower_data$unnuetz <- Shower_data$unnuetz * -1

#Create a model that considers the baseline and "unnuetz" for the treatment group
model3 <- lm( Volume ~ (Shower + isTreatment * isIntervention) * unnuetz, data = Shower_data)
summary(model3)


# Remarks:
#  * The encoding of 'unnuetz' reaches from -3 - +3. In the regression model, the 
#    effect of unnuetz=3 will be 3 times stronger than the effect of unnuetz=1. 
#    Therefore such an encoding must be handles very carefully.
#  * In this model, the effect of 'unnuetz' has an interaction with the time 
#    dimension ('Shower'). This means that 'unnuetz' affects the intercept and 
#    the slope!
#  * R^2 is very low because in this context we are not trying to explain the 
#    dependant variable but we want to explain a given effect (DiD-model). 
#    With the given variables it is understandable that we cannot explain the 
#    volume of the showers (group assignment and perceived usefulness are no 
#    predictors for that) - we would achieve much higher R^2 if we base our 
#    model on the savings

# Dummy for unnütz --------------------------------------------------------
#
# We try to overcome with the remarks of model 3 by defining dummy variables.
# Mind that the values {-1,0,1} would belong to another dummy variable, but they
# are included as the reference category in the model.
Shower_data$useful_neg <- ifelse(Shower_data$unnuetz>1, 1, 0)
Shower_data$useful_pos <- ifelse(Shower_data$unnuetz<(-1), 1, 0)

#Create a model that considers the baseline and "unnuetz" for the treatment group
model4 <- lm(Volume ~ (Shower + isTreatment * isIntervention) * (useful_pos + useful_neg), data = Shower_data)
summary(model4)

#The affect of usefulness on the slope is hard to explain, therefore we only
#include the affect on the intercept.
model5 <- lm(Volume ~ Shower + (isTreatment * isIntervention) * (useful_pos + useful_neg), data = Shower_data)
summary(model5)

