#################     Calculations     #################


2 + 3*5^2


2^3^5


3*exp(1)^5 + 2


1.4e-2


log2(33554432)


sin(pi/2)


################# Objects and Assignment ###############


vec <- c("a", "b")


vec <- c(3, 7, 11, 15)


u <- 1:10


x <- sample(1:99, 10)

##################    Vectors     #######################


x[1:3]


sort(x)


order(x)


x[order(x)]


x[x%%2==0]


v <- x^2


u <- u + 1


v/x


sqrt(v)


x + 0:2



##################    Matrices     #######################
x <- sample(1:99, 10)

#pause("21. ")
matrix(0,nrow=2,ncol=2)

#pause("22. ")
matrix(x,nrow=2)

#pause("23. ")
A <- matrix(x,nrow=2)

#pause("24. ")
A[1,]

#pause("25. ")
sum(A[,-2])


# Load and inspect the Shower data ----
Shower <- read.csv2("~/Downloads/Shower_data.csv")
?read.csv #help pages for format options of the data file

head(Shower)
tail(Shower)
str(Shower)
summary(Shower)
nrow(Shower)
ncol(Shower)


# the group is not meaningful as numeric! Convert it to factor
Shower$group <- as.factor(Shower$group)
summary(Shower)
levels(Shower$group)

#level names of factors can be changed - mind the order of the elements!
levels(Shower$group) <- c("First group", "Second group", "Fourth group", 
                          "Third group", "Fifth group", "Sixth group")
summary(Shower$group)

# Basic statistics for the Shower data ----

mean(Shower$Showertime) #mean
var(Shower$Showertime)  #variance (implements sample variance)
median(Shower$Showertime)
sd(Shower$Volume)       #standard deviation (implements sample formula)

max(Shower$Showertime)
min(Shower$Showertime)
quantile(Shower$Showertime)



#####

library(dplyr)

#Exercise 1

data <- read.csv2("~/Downloads/Shower_data.csv") 

a <- filter(data, Hh_ID == 6395)
b <- arrange(data, Volume)
c <- filter(data, !Hh_ID %in% c("6395", "5307"))

#Exercise 2
library(weathermetrics)
d <- dplyr::summarise(data, minShowerDuration = min(Showertime),
                      minShowerDuration = max(Showertime))

e <- dplyr::mutate(data, avgtemperaturefahrenheit = weathermetrics::celsius.to.fahrenheit(Avgtemperature))


grouped_showers <- group_by(data, Hh_ID)

measures <- summarise(grouped_showers, meanDuration = mean(Showertime),
                      meanTemperature  = mean(Avgtemperature),
                      meanVolume = mean(Volume))


#####

library(dplyr)

#Exercise 1

data <- read.csv2("~/Downloads/Shower_data.csv") 

a <- filter(data, Hh_ID == 6395)
b <- arrange(data, Volume)
c <- filter(data, !Hh_ID %in% c("6395", "5307"))


#Exercise 2

d <- dplyr::summarise(data, minShowerDuration = min(Showertime),
                       maxShowerDuration = max(Showertime))

e <- dplyr::mutate(data, avgtemperaturefahrenheit = weathermetrics::celsius.to.fahrenheit(Avgtemperature))


grouped_showers <- group_by(data, Hh_ID)

measures <- summarise(grouped_showers, meanDuration = mean(Showertime),
                      meanTemperature  = mean(Avgtemperature),
                      meanVolume = mean(Volume))



#Exercise 3

measures <- data %>% dplyr::group_by(Hh_ID) %>%
                     dplyr::summarise(meanDuration = mean(Showertime),
                                      meanTemperature  = mean(Avgtemperature),
                                      meanVolume = mean(Volume))

moreThan50 <- data %>% dplyr::group_by(Hh_ID) %>%
                       dplyr::summarise(n = n()) %>%
                       dplyr::filter(n > 50)

avgNumberOfShowers <- data %>% dplyr::group_by(Hh_ID, group) %>%
                               dplyr::summarise(n = n()) %>%
                               dplyr::group_by(group) %>%
                               dplyr::summarise(grpmean = mean(n)) %>%
                               dplyr::ungroup() %>%
                               dplyr::summarise(mean = mean(grpmean))


## Join


survey <- read.csv2("~/Downloads/Shower_survey_data.csv") 


combined_dataset <- dplyr::inner_join(data, survey)

result <- combined_dataset %>% dplyr::group_by(X03d_longhair, group) %>% 
                               dplyr::summarise(avgVolume = mean(Volume),
                                                avgDuration = mean(Showertime))



##ggplot2
library(ggplot2)
#Exemplary plots
g <- ggplot(data, aes(x=Avgtemperature, y=Volume))
g <- g + geom_point()
g



g <- g + ggtitle("Distribution of average temparature and volume")
g <- g + xlab("Temperature")
g <- g + ylab("Volume in liters")
g <- g + geom_hline(yintercept = mean(data$Volume), 		 			    color="red")
g


g <- ggplot(data, aes(x=Avgtemperature, y=Volume, 				  color=factor(group)))
g <- g + geom_point()
g



g <- ggplot(data, aes(x=Avgtemperature, y=Volume))
g <- g + geom_point()
g <- g + facet_wrap(~group, nrow = 1)
g
            


#Exercise ggplot2
g <- ggplot(data, aes(x=Showertime, y=Volume))
g <- g + geom_point()
g


g2 <- ggplot(data, aes(x=log(Showertime), y=log(Volume)))
g2 <- g2 + geom_point()
g2

g3 <- ggplot(data, aes(x="",y=Showertime))
g3 <- g3 + geom_boxplot()
g3

g4 <- ggplot(survey, aes(x=einkommen))
g4 <- g4 + geom_bar()
g4

g5 <- ggplot(data, aes(x=Volume ))
g5 <- g5 + geom_density()
g5 <- g5 + facet_wrap(~group)
g5


