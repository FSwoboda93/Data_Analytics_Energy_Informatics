# k-means clustering of raw smart meter data ----------------------------------
library(dplyr)
library(ggplot2)
library(tidyr)

#Loading the data
Data2 <- read.csv2("smart_meter_data.csv")

#Smart meter data; 30 min interval; one  -> 24*2*7 = 336 columns
#The View command can't display more than 100 columns
#View(Data2)


#separate the household ids from SMD
IDs <- Data2 %>% dplyr::pull(V1)
Data2 <- Data2 %>% dplyr::select(-V1)

#k-means clustering
set.seed(1)
Cluster1 <- kmeans(Data2, centers=3)


# function for plotting the  load traces of the clusters
plotcl <- function(Data, clusters, toColumn)
{
  #We enquote our toColumn variable to be able to use it in dplyr
  enquotedCol <- dplyr::enquo(toColumn)
  
  #Calculate the consumption for each measurement index
  data <- Data   %>%   dplyr::summarize_at(vars(V2:!!enquotedCol), funs(mean)) %>%
                       tidyr::gather() %>%
                       dplyr::mutate(key = as.numeric(stringr::str_sub(key, 2)))
  
  #Create basic Plot
  p <- ggplot(data) + 
       geom_line(aes(x=key, y=value, group = 1)) +
       ylab("Consumption") +
       xlab("Measurement index")
  
  #Assign clusters to measurements
  Data2WithClusters <- cbind(Data, Cluster = clusters)
  
  #For each cluster
  for(i in unique(clusters))
  {
    #Calculate the means
    clusterData <- Data2WithClusters %>% dplyr::filter(Cluster == i) %>% 
                                         dplyr::summarize_at(vars(V2:!!enquotedCol), funs(mean)) %>%
                                         tidyr::gather() %>%
                                         dplyr::mutate(key = as.numeric(stringr::str_sub(key, 2)))
    #And draw a series
    p <- p + geom_line(data=clusterData, aes(x=key, y=value, group = i + 1), color=i + 1)
  }
  return(p)
}
plotcl(Data2, Cluster1$cluster, V48)
plotcl(Data2, Cluster1$cluster, V337)

# now, try to normalize the measurements with the mean of the household
set.seed(2)   

Datan <- Data2/rowMeans(Data2)
Cluster2 <- kmeans(Datan, centers=3)
plotcl(Data2,Cluster2$cluster,V48)


# and, try to scale the measurements with the square root
Datas <- sqrt(Data2)
Datasn <- Datas/rowMeans(Datas)
Cluster3 <- kmeans(Datasn, centers=3)
plotcl(Data2,Cluster3$cluster,V48)


# Hierarchical clustering -------------------------------------------------
C <- 1-cor(t(Data2))
library(cluster)
Dendogram <- agnes(C, diss=TRUE, method="complete")

#plot the dendogram
plot(Dendogram, which.plots = 2)

#cut the dendogram in 3 parts
Cluster4 <- cutree(Dendogram, k=3)
plotcl(Data2,Cluster4,V48)


# Clustering with features and household classes --------------------------

#load the data
data_el <- read.csv("~/Downloads/ireland_small_sample.csv")

#enable things to be drawn outside the plot region
par("xpd"=F)

#plot the data (Scatterplot Matrices)
pairs(data_el[,c("consumption","relation_evening_noon","t_above_1kw")])

## Two clusters -----------------------------------------------------------
x <- kmeans(data_el[,c("consumption","relation_evening_noon","t_above_1kw")], 2)
pairs(data_el[,c("consumption","relation_evening_noon","t_above_1kw")],col=x$cluster)

# Single-households 
pairs(data_el[,c("consumption","relation_evening_noon","t_above_1kw")],
      col=x$cluster, pch=as.numeric(data_el$single))
legend(x="bottomright",pch=1:2,legend=levels(data_el$single), cex=0.5)

# Employment status of the people in the household
pairs(data_el[,c("consumption","relation_evening_noon","t_above_1kw")],
      col=x$cluster, pch=as.numeric(data_el$employment))
legend(x="bottomright",pch=1:2,legend=levels(data_el$employment), cex=0.5)