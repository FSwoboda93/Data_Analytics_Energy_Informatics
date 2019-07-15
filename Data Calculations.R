library(dplyr)
#Read the data
Survey_data <- read.csv2("~/Downloads/Shower_survey_data.csv")

#Check the values for the age
Survey_data %>% dplyr::group_by(alter) %>%
  dplyr::summarise(NumberOfParticipants = n())

#Mark persons as young
Survey_data <- Survey_data %>% dplyr::mutate(isYoung = ifelse(alter %in% c("20-29","30-39"), TRUE, FALSE))


#Check the values of att5
Survey_data %>% dplyr::group_by(att5) %>%
  dplyr::summarise(NumberOfSelections = n())

median(as.numeric(Survey_data$att5))

transformValues <- function(x){
  x <- as.character(x)
  x <- ifelse(x=="Stimme nicht zu", -2, x)
  x <- ifelse(x=="Stimme eher nicht zu", -1, x)
  x <- ifelse(x=="Indifferent", 0, x)
  x <- ifelse(x=="Stimme eher zu", 1, x)
  x <- ifelse(x=="Stimme zu", 2, x)
  return(as.numeric(x))
}

Survey_data <- Survey_data  %>% dplyr::mutate_at(vars(starts_with("att")), funs(transformValues))

#Combine all the environmentalism scores into one (Note att1 and att8 are negative because the scale is flipped)
Survey_data <- Survey_data %>% dplyr::mutate(environmentalism = -att1+att2+att3+att4+att5+att6+att7-att8+att9+att10)

#What are the differences in the mean
mean(Survey_data$environmentalism,na.rm=T)
mean(Survey_data %>% dplyr::filter(isYoung) %>% dplyr::pull(environmentalism),na.rm=T)
mean(Survey_data %>% dplyr::filter(!isYoung) %>%  dplyr::pull(environmentalism),na.rm=T)

# RQ1: Do old and young individuals differ regarding their attitude in environmentalism? (we assume that they do differ)
# H0: Old and young individuals do not differ regarding their attitude in environmentalism?
# H1: Old and young individuals differ regarding their attitude in environmentalism?
# Apply the t-test 
t.test(Survey_data %>% dplyr::filter(isYoung) %>% dplyr::pull(environmentalism),
       Survey_data %>% dplyr::filter(!isYoung) %>% dplyr::pull(environmentalism))

#####
# We cannot reject H0 with this measurement scale
##### 

t.test(Survey_data %>% dplyr::filter(isYoung) %>% dplyr::pull(att5), 
       Survey_data %>% dplyr::filter(!isYoung) %>% dplyr::pull(att5))

####
# With att5, we can reject H0 with in favour of H1

library(dplyr)
#Read the data
Shower_data <- read.csv2("~/Downloads/Shower_data.csv")
Survey_data <- read.csv2("~/Downloads/Shower_survey_data.csv")


transformValues <- function(x){
  x <- as.character(x)
  x <- ifelse(x=="Stimme nicht zu", -2, x)
  x <- ifelse(x=="Stimme eher nicht zu", -1, x)
  x <- ifelse(x=="Indifferent", 0, x)
  x <- ifelse(x=="Stimme eher zu", 1, x)
  x <- ifelse(x=="Stimme zu", 2, x)
  return(as.numeric(x))
}

Survey_data <- Survey_data  %>% dplyr::mutate_at(vars(starts_with("att")), 
                                                 funs(transformValues))

#Combine all the environmentalism scores into one (Note att1 and att8 are negative because the scale is flipped)
Survey_data <- Survey_data %>% dplyr::mutate(environmentalism = 
                                               -att1+att2+att3+att4+att5+att6+att7-att8+att9+att10)

#Drop first shower
Shower_data <- Shower_data %>% dplyr::filter(Shower != 1) %>%
  dplyr::mutate(isTreatment = ifelse(group %in% c("1","2"), FALSE, TRUE),
                isIntervention = ifelse(Shower <= 10, FALSE, TRUE))


#Consider only the treatment group
treatmentShowers <- Shower_data %>% dplyr::filter(isTreatment) 

### Way1
#Calculate the water consumption for each phase and each group


treatmentShowersBaseline <- treatmentShowers %>% dplyr::filter(!isIntervention) %>%
  dplyr::group_by(Hh_ID) %>%
  dplyr::summarise(baselineConsumption = mean(Volume))

treatmentShowersIntervention <- treatmentShowers %>% dplyr::filter(isIntervention) %>%
  dplyr::group_by(Hh_ID) %>%
  dplyr::summarise(interventionConsumption = mean(Volume))

#"Connect" both phases, introduce "savings"
consumptionPerPhase <- dplyr::inner_join(treatmentShowersBaseline, 
                                         treatmentShowersIntervention,
                                         by = "Hh_ID")

#Calculate the savings (simple approach, neglecting the control group, but ok for now)
VolumeSavings <- consumptionPerPhase %>% 
  dplyr::mutate(savings = baselineConsumption - interventionConsumption)



#### Way 2
#Calculate the water consumption for each phase and each group
VolumeMeanPerHH <- Shower_data %>% dplyr::group_by(Hh_ID, isIntervention, isTreatment) %>%
  dplyr::summarise(meanVolume = mean(Volume))

VolumeMeanPerHHWideFormat <- VolumeMeanPerHH %>% tidyr::spread(isIntervention, meanVolume)

#Calculate the savings (simple approach, neglecting the control group, but ok for now)
VolumeSavings <- VolumeMeanPerHHWideFormat %>% dplyr::filter(isTreatment) %>%
  dplyr::mutate(savings = `FALSE` - `TRUE`) %>%
  dplyr::select(-c(`TRUE`, `FALSE`))



#Join savings with survey data
VolumeSavingsEnriched <- dplyr::left_join(VolumeSavings, Survey_data, by = "Hh_ID")

#Create the group for the environmentally friendly people
VolumeSavingsEnrichedMedian <- VolumeSavingsEnriched %>% dplyr::mutate(environmentally_friendly = 
                                                                         environmentalism > median(Survey_data$environmentalism, na.rm = TRUE))

VolumeSavingsEnrichedMedian %>% dplyr::group_by(environmentally_friendly) %>%
  dplyr::summarise(meanSavings = mean(savings),
                   sdSavings = sd(savings))


# those who are environmentally friendly save less (according to the sample mean) than
# those who are not environmentally friendly. Strange at first sight.
# Apply the t-test - the result of the t-test is not surprising given the means
t.test(VolumeSavingsEnrichedMedian %>% dplyr::filter(environmentally_friendly) %>% dplyr::pull(savings),
       VolumeSavingsEnrichedMedian %>% dplyr::filter(!environmentally_friendly) %>% dplyr::pull(savings), 
       alternative="greater")

##### 
# Result: No difference in mean, so we cannot say that environmental attitude makes a difference
# Thus, H0 cannot be rejected 
#####
#Only for att5

VolumeSavingsEnrichedMedianAtt5 <- VolumeSavingsEnrichedMedian %>% 
  dplyr::mutate(environmentally_friendly_att5 = att5 > 0, na.rm = TRUE)

VolumeSavingsEnrichedMedianAtt5 %>% dplyr::group_by(environmentally_friendly_att5) %>%
  dplyr::summarise(meanSavings = mean(savings))
# Again, those who are envrionmentally friendly save less (according to the sample mean) 
# than those who are not environmentally friendly. Strange at first sight.

#Apply the t-test 
t.test(VolumeSavingsEnrichedMedianAtt5 %>% dplyr::filter(environmentally_friendly_att5) %>% dplyr::pull(savings), 
       VolumeSavingsEnrichedMedianAtt5 %>% dplyr::filter(!environmentally_friendly_att5) %>% dplyr::pull(savings), 
       alternative="greater",
       conf.level=0.95)

#####
# Result: No difference in mean, so we cannot say that environmental attitude makes a difference
# H0 cannot be rejected 


#Question 3 in the quiz: It's the central limit theorem
library(ggplot2)
library(dplyr)
#here, we sample 50000 values from the gamma distribution
gammaDis <- data.frame(dis = rgamma(50000, shape = 1))

#The shape of the disitribution
ggplot2::ggplot(gammaDis) + ggplot2::geom_histogram(aes(x=dis))

library(caret)

#Now we split the 50000 data points randomly into 500 data sets...
gammaDis$group <- caret::createFolds(gammaDis$dis, k = 500, list = FALSE)
#and calculate the average
gammaDisMean <- gammaDis %>% dplyr::group_by(group) %>% 
                dplyr::summarise(mean = mean(dis))

ggplot2::ggplot(gammaDisMean) + ggplot2::geom_histogram(aes(x=mean), bins = 20)


library(dplyr)
#Read the data
Shower_data <- read.csv2("~/Downloads/Shower_data.csv")
Survey_data <- read.csv2("~/Downloads/Shower_survey_data.csv")

#Mark persons as young
Survey_data <- Survey_data %>% dplyr::mutate(isYoung = 
                                               ifelse(alter %in% c("20-29","30-39"), TRUE, FALSE))

transformValues <- function(x){
  x <- as.character(x)
  x <- ifelse(x=="Stimme nicht zu", -2, x)
  x <- ifelse(x=="Stimme eher nicht zu", -1, x)
  x <- ifelse(x=="Indifferent", 0, x)
  x <- ifelse(x=="Stimme eher zu", 1, x)
  x <- ifelse(x=="Stimme zu", 2, x)
  return(as.numeric(x))
}

Survey_data <- Survey_data  %>% dplyr::mutate_at(vars(starts_with("att")), funs(transformValues))

#Combine all the environmentalism scores into one (Note att1 and att8 are negative because the scale is flipped)
Survey_data <- Survey_data %>% dplyr::mutate(environmentalism = -att1+att2+att3+att4+att5+att6+att7-att8+att9+att10)

#Drop first shower
Shower_data <- Shower_data %>% dplyr::filter(Shower != 1) %>%
  dplyr::mutate(isTreatment = ifelse(group %in% c("1","2"), FALSE, TRUE),
                isIntervention = ifelse(Shower <= 10, FALSE, TRUE))



# ----------------------------------------------------------------------------------------------------------------------
# RQ3: H0: Individuals with a high score on environmentalism 
# do not have a lower average baseline consumption than others.
#
# H1: Individuals with a high score on environmentalism have a 
# lower average baseline consumption than others.
# ----------------------------------------------------------------------------------------------------------------------


Shower_Data_baseline_mean <- Shower_data %>% 
                             dplyr::filter(!isIntervention & isTreatment) %>% 
                             dplyr::group_by(Hh_ID) %>%
                             dplyr::summarise(meanConsumption = mean(Volume))

Shower_Data_baseline_mean_enriched <- dplyr::left_join(Shower_Data_baseline_mean,
                                                       Survey_data,
                                                       by = "Hh_ID")

#Create the group for the environmentally friendly people
Shower_Data_baseline_mean_enriched  <- Shower_Data_baseline_mean_enriched  %>%
                                       dplyr::mutate(environmentally_friendly = 
                                                      environmentalism > median(Survey_data$environmentalism, na.rm = TRUE))

volume_environmentally_friendly_baseline <- Shower_Data_baseline_mean_enriched %>%
                                   dplyr::filter(environmentally_friendly) %>%
                                   dplyr::pull(meanConsumption)

volume_not_environmentally_friendly_baseline <- Shower_Data_baseline_mean_enriched %>%
                                       dplyr::filter(!environmentally_friendly) %>%
                                       dplyr::pull(meanConsumption)

mean(volume_environmentally_friendly_baseline)
mean(volume_not_environmentally_friendly_baseline)

######
# Well, env. friendly participants seem to have a lower baseline 
# before the intervention (significant at alpha=0.05)
t.test(volume_environmentally_friendly_baseline,
       volume_not_environmentally_friendly_baseline,
       alternative="less",
       conf.level=0.95)
######


######
#The H0 hypothesis has to be rejected in favour of H1
######


# Just for curiosity: how does the consumption compare in the intervention period?

Shower_Data_intervention_mean <- Shower_data %>%
                                dplyr::filter(isIntervention & isTreatment) %>% 
                                dplyr::group_by(Hh_ID) %>%
                                dplyr::summarise(meanConsumption = mean(Volume))

Shower_Data_intervention_mean_enriched <- dplyr::left_join(Shower_Data_intervention_mean,
                                                           Survey_data,
                                                           by = "Hh_ID")

#Create the group for the environmentally friendly people
Shower_Data_intervention_mean_enriched  <- Shower_Data_intervention_mean_enriched  %>%
                                           dplyr::mutate(environmentally_friendly = 
                                                         environmentalism > median(Survey_data$environmentalism, na.rm = TRUE))

volume_environmentally_friendly_intervention <-  Shower_Data_intervention_mean_enriched %>% 
                                                 dplyr::filter(environmentally_friendly) %>% 
                                                 dplyr::pull(meanConsumption)

volume_not_environmentally_friendly_intervention <-  Shower_Data_intervention_mean_enriched %>% 
                                                     dplyr::filter(!environmentally_friendly) %>% 
                                                     dplyr::pull(meanConsumption)
mean(volume_environmentally_friendly_intervention)
mean(volume_not_environmentally_friendly_intervention)

#####
t.test(volume_environmentally_friendly_intervention,
       volume_not_environmentally_friendly_intervention,
       alternative="less",
       conf.level=0.95)
#####

# --------------------------------------------------------------------------------------------------------
# RQ4a: 
# H0: Consumers with a high baseline consumption do not show larger saving effects when feedback is presented than other consumers
# H1: Consumers with a high baseline consumption show larger saving effects when feedback is presented than other consumers
# --------------------------------------------------------------------------------------------------------


consumptionPerPhase <- dplyr::inner_join(Shower_Data_baseline_mean,
                                         Shower_Data_intervention_mean,
                                         "Hh_ID")

consumptionPerPhaseRenamed <- consumptionPerPhase %>% 
  dplyr::rename(baselineConsumption = meanConsumption.x,
                interventionConsumption = meanConsumption.y) %>%
  dplyr::mutate(savings = baselineConsumption - interventionConsumption)

baselineConsumptionMedian <- median(consumptionPerPhaseRenamed$baselineConsumption)

consumptionPerPhaseRenamed <- consumptionPerPhaseRenamed %>%
     dplyr::mutate(isLowConsumer = 
     ifelse(baselineConsumption > baselineConsumptionMedian, FALSE, TRUE))


savings_low_baseline_consumer <- consumptionPerPhaseRenamed %>%
                                 dplyr::filter(isLowConsumer) %>%
                                 dplyr::pull(savings)

savings_not_low_baseline_consumer <- consumptionPerPhaseRenamed %>%
                                     dplyr::filter(!isLowConsumer) %>%
                                     dplyr::pull(savings)


mean(consumptionPerPhaseRenamed$savings)
mean(savings_low_baseline_consumer)
mean(savings_not_low_baseline_consumer)

#####
t.test(savings_not_low_baseline_consumer,
       savings_low_baseline_consumer,
       alternative="greater")
#####
# H0 can be rejected in favour of H1 with an extremely high level of confidence

# The expected average saving effect would be 12.3 liters compared to 6.8 liters


#RQ4b: 
# What would be the savings (in liters) that would be achieved 
# if only above median water users would be addressed by such a campaign?

#####
t.test(savings_not_low_baseline_consumer, conf.level=0.90)
#####

# --------------------------------------------------------------------------------------------------------
# RQ5: 
# We want to show that young users save more than old users
# H0: mu_young_saving <= mu_old_saving
# H1: mu_young_saving > mu_old_saving
# H1: Young consumers show larger saving effects when feedback is presented 
# than older consumers.
# --------------------------------------------------------------------------------------------------------

# Check the values for the age
table(Survey_data$alter)
# Let us split the users in groups above and below 40 years
# Create the set of young persons
Survey_data <- Survey_data %>% 
  dplyr::mutate(isYoung = ifelse(alter %in% c("20-29","30-39"), TRUE, FALSE))

consumptionPerPhaseRenamed  <- dplyr::left_join(consumptionPerPhaseRenamed,
                                                Survey_data,
                                                "Hh_ID")

savings_young <- consumptionPerPhaseRenamed %>% 
                 dplyr::filter(isYoung) %>% 
                 dplyr::pull(savings)

savings_not_young <- consumptionPerPhaseRenamed %>% 
                 dplyr::filter(!isYoung) %>% 
                 dplyr::pull(savings)

mean(consumptionPerPhaseRenamed$savings)
mean(savings_young)
mean(savings_not_young)
#####
t.test(savings_young,
       savings_not_young,
       alternative="greater")
#####
# H0 must be rejected in favor for H1 
#####

