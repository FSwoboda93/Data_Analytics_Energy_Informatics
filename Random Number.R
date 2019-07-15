library(dplyr)
library(magrittr)
n=10 			 %>%	# define the sample size
x1<-rnorm(n, mean=4, sd=2)	%>% # Draw five random numbers from a normal distribution N(4,2)
Interval_1 <- c(mean(x1) - 2/sqrt(n), mean(x1) + 2/sqrt(n))  %>%	# Representation of the interval
