#################     Task 1     #################
#a. Load and explore the data
ShowerData <- read.csv2("~/Downloads/Shower_data.csv")

nrow(ShowerData)
summary(ShowerData)

#b. Create groups for treatment, control, baseline and intervention

library(magrittr)
ShowerData <- ShowerData %>% dplyr::mutate(isTreatment = ifelse(group %in% c("1","2"), FALSE, TRUE),
                                           isIntervention = ifelse(Shower <= 10, FALSE, TRUE))


require(ggplot2)
ggplot2::ggplot(data = ShowerData %>% dplyr::filter(!isIntervention),
                aes(x=Volume)) +  geom_histogram()

ggplot2::ggplot(data = ShowerData %>% dplyr::filter(!isIntervention),
                aes(x=Volume)) +  geom_histogram(bins=100)

ggplot2::ggplot(data = ShowerData %>% dplyr::filter(!isIntervention),
                aes(x=Volume)) +  geom_histogram(breaks=seq(from=0, to =350, by=10))


#c. Plot the histogram and correct the baseline
ShowerData <- ShowerData %>% dplyr::mutate(isInterventionCorrected = ifelse(Shower <= 10 & Shower > 1, FALSE, TRUE))

ggplot2::ggplot(data = ShowerData %>% dplyr::filter(!isInterventionCorrected),
                aes(x=Volume)) + geom_histogram()


# Plot the densities in the same plot

ggplot2::ggplot(data = ShowerData %>% dplyr::filter(!isInterventionCorrected),
                aes(x=Volume)) +  geom_density() +
                geom_density(data = ShowerData %>% dplyr::filter(!isIntervention), aes(x=Volume),  color = "red") +
                geom_density(data = ShowerData %>% dplyr::filter(Shower == 1), aes(x=Volume), color = "blue")


#d. Plot the densities for the intervention phase

# Remove first shower
ShowerData <- ShowerData %>% dplyr::filter(Shower > 1)

ggplot2::ggplot(data = ShowerData %>% dplyr::filter(isTreatment & isIntervention),
                aes(x=Volume, color=isTreatment)) +  geom_density() +
                geom_density(data =  ShowerData %>% dplyr::filter(!isTreatment & isIntervention), aes(x=Volume, color=isTreatment)) 

#Calculate the mean and the variances
ShowerData %>% dplyr::filter(isTreatment & isIntervention | !isTreatment & isIntervention) %>%
               dplyr::group_by(isTreatment,isIntervention) %>%
               dplyr::summarise(var = var(Volume), mean = mean(Volume))

#e. Plot the density for the treatment group
ggplot2::ggplot(data = ShowerData %>% dplyr::filter(isTreatment & isIntervention),
                aes(x=Volume, color=isIntervention)) +  geom_density() +
                geom_density(data =  ShowerData %>% dplyr::filter(isTreatment & !isIntervention), aes(x=Volume, color=isIntervention)) 


#################     Task 2     #################
#a. Derive new random variables from the given data set. Omit the first data point

calculateMeanPerHh <- function(filteredData){
  retValues <- filteredData %>% dplyr::group_by(Hh_ID) %>%
               dplyr::summarise(avg = mean(Volume))
  return(retValues)
}

BaseCont <- calculateMeanPerHh(ShowerData %>% dplyr::filter(!isTreatment & !isIntervention))
BaseTreat <- calculateMeanPerHh(ShowerData %>% dplyr::filter(isTreatment & !isIntervention))
InterCont <- calculateMeanPerHh(ShowerData %>% dplyr::filter(!isTreatment & isIntervention))
InterTreat <- calculateMeanPerHh(ShowerData %>% dplyr::filter(isTreatment & isIntervention))


#b. Calculate mean, variance, and standard deviation of the new variables
mean(BaseCont$avg)
mean(BaseTreat$avg)
mean(InterCont$avg)
mean(InterTreat$avg)

var(BaseCont$avg)
var(BaseTreat$avg)
var(InterCont$avg)
var(InterTreat$avg)

sd(BaseCont$avg)
sd(BaseTreat$avg)
sd(InterCont$avg)
sd(InterTreat$avg)

#c. Calculate the two-sided 90% confidence intervals around the means for each variable.

calculateConfidenceInterval <- function(values, alpha){
  n <- length(values)
  interval <- c(lb = mean(values) - qt(1 - alpha/2, df = n - 1) * sd(values)/sqrt(n), 
                ub = mean(values) + qt(1 - alpha/2, df = n - 1) * sd(values)/sqrt(n))
  return(interval)
}

calculateConfidenceInterval(BaseCont$avg, alpha = 0.1)
calculateConfidenceInterval(BaseTreat$avg, alpha = 0.1)
calculateConfidenceInterval(InterCont$avg, alpha = 0.1)
calculateConfidenceInterval(InterTreat$avg, alpha = 0.1)

#With the t.test function
t.test(BaseCont$avg, mu=0, conf.level = 0.9)
t.test(BaseTreat$avg, mu=0, conf.level = 0.9)
t.test(InterCont$avg, mu=0, conf.level = 0.9)
t.test(InterTreat$avg, mu=0, conf.level = 0.9)





#################     Task 3     #################
#a.
#Without t.test

controlGroupShowers <- calculateMeanPerHh(ShowerData %>% dplyr::filter(!isTreatment)) %>%
                       dplyr::pull(avg)

#Calculate the t-statistic
tstat <- (mean(controlGroupShowers) - 40)/ (sd(controlGroupShowers)/ sqrt(length(controlGroupShowers)))
(2 - pt(tstat, df = length(controlGroupShowers)-1)*2)

#With t.test
t.test(controlGroupShowers, mu=40)

#b
controlGroupBaselineShowers <- ShowerData %>% dplyr::filter(!isTreatment & !isIntervention) %>%
                                              dplyr::pull(Volume)

treatmentGroupBaselineShowers <- ShowerData %>% dplyr::filter(isTreatment & !isIntervention) %>%
                                                dplyr::pull(Volume)

t.test(controlGroupBaselineShowers, treatmentGroupBaselineShowers,conf.level=.9)

#c
controlGroupInterventionShowers <- ShowerData %>% dplyr::filter(!isTreatment & isIntervention) %>%
                                                  dplyr::pull(Volume)

treatmentGroupInterventionShowers <- ShowerData %>% dplyr::filter(isTreatment & isIntervention) %>%
                                                    dplyr::pull(Volume)

t.test(controlGroupInterventionShowers, treatmentGroupInterventionShowers ,conf.level=.9)

#d
t.test(treatmentGroupBaselineShowers, treatmentGroupInterventionShowers,conf.level=.9)

#e
t.test(BaseTreat$avg - InterTreat$avg, mu=0)
t.test(BaseTreat$avg, InterTreat$avg, paired = TRUE)
