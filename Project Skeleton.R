library(CORElearn)
library(dplyr)
library(caret)
library(ggplot2)

# Load and prepare data ---------------------------------------------------
SMD_Readings <- read.csv2("~/Downloads/SMD_Readings.csv")
SMD_Survey <- read.csv2("~/Downloads/SMD_Survey.csv")


# Define household properties to be predicted (dependant variables) ------

SMD_Survey <- SMD_Survey %>% dplyr::mutate(adults = as.integer(Q3),
                                           children = as.integer(Q4))

SMD_Survey <- SMD_Survey %>% dplyr::mutate(pNumResidents = ifelse(is.na(children), adults, adults+children)) %>%
  dplyr::mutate(pNumResidents = ifelse(pNumResidents==0 | is.na(pNumResidents), NA,
                                       ifelse(pNumResidents==1, "1 person", 
                                              ifelse(pNumResidents==2, "2 persons",
                                                     ifelse(pNumResidents<=5, "3-5 persons","more than 5 persons")))))

# Descriptive analysis of load traces -------------------------------------
# Plot some load curves from households to get familiar with the data

household <- 1076


householdData <- SMD_Readings %>% dplyr::filter(ID == household)

#plot the weekly trace of one household 
g <- ggplot2::ggplot(data = householdData) + 
  ggplot2::geom_line(aes(x=Timestamp, y=Consumption)) +
  ggplot2::ggtitle("Weekly load curve") + 
  ggplot2::xlab("Measurement") + ggplot2::ylab("Consumption")

g


#Plot the monday
g <- ggplot2::ggplot(data = householdData %>% dplyr::filter(Weekday == "Monday")) + 
  ggplot2::geom_line(aes(x=DayTimestamp, y=Consumption)) +
  ggplot2::ggtitle("Monday load curve") + 
  ggplot2::xlab("Measurement") + ggplot2::ylab("Consumption")

g

g <- ggplot2::ggplot(data = householdData) + 
  ggplot2::geom_line(aes(x=DayTimestamp, y=Consumption, color=Weekday, group=Weekday)) +
  ggplot2::ggtitle("Load curve") + 
  ggplot2::xlab("Measurement") + ggplot2::ylab("Consumption")

g

g <- ggplot2::ggplot(data = householdData) + 
  ggplot2::geom_line(aes(x=DayTimestamp, y=Consumption, group=Weekday)) +
  ggplot2::ggtitle("Load curve") + 
  ggplot2::xlab("Measurement") + ggplot2::ylab("Consumption") + facet_wrap(~Weekday)

g




# Dimensionality reduction 1: Feature extraction (smart meter data) -------
# Define and implement 10 features from Formatted_Readings (e.g. mean consumption, mean 
# consumption in the evening)

#Calculate consumption features (general, weekday, weekend)
weekday_features <- SMD_Readings %>% dplyr::group_by(ID) %>% 
  dplyr::filter(Timestamp %in% (1:(24*2*5))) %>%
  dplyr::summarise(s_wd_max = max(Consumption, na.rm = TRUE),
                   s_wd_min = min(Consumption, na.rm = TRUE))

weekend_features <- SMD_Readings %>% dplyr::group_by(ID) %>% 
  dplyr::filter(Timestamp %in% ((24*2*5+1):48*7))%>%
  dplyr::summarise(s_we_max = max(Consumption, na.rm = TRUE),
                   s_we_min = min(Consumption, na.rm = TRUE))


# Combine the data --------------------------------------------------------
# Combine all features in one data frame and store the full feature set in a vector






#The following lines allow to drop infinity and na rows
#enrichedFeatures <- enrichedFeatures %>% mutate_all(funs(replace(., is.infinite(.), NA)))
#cleanedEnrichedFeatures <- na.omit(enrichedFeatures)






# Perform a kNN-algorithm on the defined features: --------------------------------------------------------



#Split the data set


#Perform the knn algorithm


#Calculate the accuracy


#Retry with different parameters


#Perform a 5-fold cross validation


#Are the results stable?





# Perform a logistic regression on the defined features: --------------------------------------------------------



#Split the data set


#Perform the logistic regression


#Calculate the accuracy


#Retry with different parameters


#Perform a 5-fold cross validation


#Are the results stable?




