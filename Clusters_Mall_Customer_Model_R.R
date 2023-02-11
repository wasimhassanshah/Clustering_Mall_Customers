
library(tidyverse)
library(data.table)

customer_data<-read.csv("Mall_Customers.csv")


names(customer_data)



data.table::data.table(customer_data) 

summary(customer_data)


#Statistical Analysis
sd(customer_data$Age)
sd(customer_data$Annual.Income..k..)
sd(customer_data$Spending.Score..1.100.)
IQR(customer_data$Age)
IQR(customer_data$Annual.Income..k..)
IQR(customer_data$Spending.Score..1.100.)


#plot

gender = table(customer_data$Gender)
barplot(gender, main = "Using Barplot to display Gender Comparison",
        ylab = "Number",
        xlab = "Gender",
        col = rainbow(2),
        legend = rownames(gender))
 

pct = round((gender/sum(gender))*100)
lbs = paste(c("Female","Male"), " ", pct, "%", sep = " ")

library(plotrix)

pie3D(gender, labels = lbs, main = "Ratio of Male and Female")




#Age plot

hist(customer_data$Age,
     col = "red",
     xlab = "Age Class",
     ylab = "Frequency",
     lables = TRUE)



boxplot(customer_data$Age,
        col= rainbow(1),
        main = "Boxplot: Descriptive Analysis of Age",labels = TRUE)


#It shows , media vakue of 30-35 without any outliers



# Annual Income plot

hist(customer_data$Annual.Income..k..,
     col="green",
     xlab="income class",
     ylab = "Frequency",
     labels = TRUE)

 


plot(density(customer_data$Annual.Income..k..),
     col= "yellow",
     main = "Density plot for Annual Income",
     xlab = "Income",
     ylab = "Density")
polygon(density(customer_data$Annual.Income..k..), col = "green") # TO fill density plot


#average 70 (mode) mean 60.56 normal distribution 



# Spending plot


plot(density(customer_data$Spending.Score..1.100.),
     col= "yellow",
     main = "Density plot for Annual Spending",
     xlab = "Income",
     ylab = "Density")
polygon(density(customer_data$Spending.Score..1.100.), col = "green") # TO fill density plot




hist(customer_data$Spending.Score..1.100.,
     col="green",
     xlab="Spending",
     ylab = "Frequency",
     labels = TRUE)


# K-means

#1:Elbow method to specifiy number of clusters
#2:Silhouette method
#3: Gap statistics


#1:Elbow method to specifiy number of clusters
library(purrr)
set.seed(2)


# Function to calculate total intra-cluster sum of square (Eucledian Distance)

ics <- function(k){
  
  kmeans(customer_data[,3:5], k, iter.max = 100, nstart = 100, algorithm = "Lloyd")$tot.withinss
  
  
}


k_values <- 1:12

ics_values <- map_dbl(k_values, ics)



plot(k_values, ics_values,
     type = "b", pch = 19, frame = FALSE,
     xlab = "Number of clusters k",
     ylab = "Total intra-clusters Sum of Squares")



# 4 is optimum number of clusters according to Elbow method



#2: Average Silhouette method

library(cluster)
library(gridExtra)
library(grid)
k2<- kmeans(customer_data[, 3:5], 2, iter.max = 100, nstart = 50, algorithm = "Lloyd")

s2 <- plot(silhouette(k2$cluster, dist(customer_data[, 3:5], "euclidean")))

#Using fviz_nbclust to determine optimal number of clusters
# Upto 10



k3<- kmeans(customer_data[, 3:5], 3, iter.max = 100, nstart = 50, algorithm = "Lloyd")

s3 <- plot(silhouette(k2$cluster, dist(customer_data[, 3:5], "euclidean")))



k4<- kmeans(customer_data[, 3:5], 4, iter.max = 100, nstart = 50, algorithm = "Lloyd")

s4 <- plot(silhouette(k2$cluster, dist(customer_data[, 3:5], "euclidean")))



k5<- kmeans(customer_data[, 3:5], 5, iter.max = 100, nstart = 50, algorithm = "Lloyd")

s5 <- plot(silhouette(k2$cluster, dist(customer_data[, 3:5], "euclidean")))


k6<- kmeans(customer_data[, 3:5], 6, iter.max = 100, nstart = 50, algorithm = "Lloyd")

s6 <- plot(silhouette(k2$cluster, dist(customer_data[, 3:5], "euclidean")))


k7<- kmeans(customer_data[, 3:5], 7, iter.max = 100, nstart = 50, algorithm = "Lloyd")

s7 <- plot(silhouette(k2$cluster, dist(customer_data[, 3:5], "euclidean")))



k8<- kmeans(customer_data[, 3:5], 8, iter.max = 100, nstart = 50, algorithm = "Lloyd")

s8 <- plot(silhouette(k2$cluster, dist(customer_data[, 3:5], "euclidean")))




k9<- kmeans(customer_data[, 3:5], 9, iter.max = 100, nstart = 50, algorithm = "Lloyd")

s9 <- plot(silhouette(k2$cluster, dist(customer_data[, 3:5], "euclidean")))





k10<- kmeans(customer_data[, 3:5], 10, iter.max = 100, nstart = 50, algorithm = "Lloyd")

s10 <- plot(silhouette(k2$cluster, dist(customer_data[, 3:5], "euclidean")))




library(NbClust)

library(factoextra)

fviz_nbclust(customer_data[ , 3:5], kmeans, method = "silhouette")

# This method 6 is optimal number of clusters



# 3: Gap Statistical Method
set.seed(125)

stat_gap <- clusGap(customer_data[, 3:5], FUN = kmeans, nstart = 25,
                     K.max = 10, B= 50)
 
 fviz_gap_stat(stat_gap)


# go for k6
 
 k6
 
 
 # Visualizing the clustering result using the first two principle components
 
 pccluster = prcomp(customer_data[, 3:5], scale = TRUE)

 summary(pccluster)

 pccluster$rotation[, 1:2]

 
 # Visualize Annual Income vs Spending score clusters
set.seed(1)

ggplot(customer_data, aes(x= Annual.Income..k.., y= Spending.Score..1.100.)) +
  geom_point(start = "identity", aes(color = as.factor(k6$cluster))) +
  scale_color_discrete(name = " ",
                       breaks=c("1","2","3","4","5","6"),
                       labels= c("Cluster 1","Cluster 2","Cluster 3","Cluster 4","Cluster 5","Cluster 6")) +
  ggtitle("Segments of Mall Customers", subtitle = "using K-means Clustering")




# Visualize Spending power vs Age clusters
ggplot(customer_data, aes(x= Annual.Income..k.., y= Age)) +
  geom_point(start = "identity", aes(color = as.factor(k6$cluster))) +
  scale_color_discrete(name = " ",
                       breaks=c("1","2","3","4","5","6"),
                       labels= c("Cluster 1","Cluster 2","Cluster 3","Cluster 4","Cluster 5","Cluster 6")) +
  ggtitle("Segments of Mall Customers", subtitle = "using K-means Clustering")



# Finally visualize K-mean values

kCols= function(vec){cols = rainbow (length (unique (vec)))
return (cols[as.numeric(as.factor(vec))])}
digCluster<-k6$cluster;
dignm<-as.character(digCluster); #Kmean clusters
plot(pccluster$x[,1:2], col = kCols(digCluster), pch =19, xlab= "K-means", ylab = "classes")
legend("bottomleft", unique(dignm), fill= unique(kCols(digCluster)))

































































