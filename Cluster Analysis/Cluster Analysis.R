#Author:  Stephanie Besser
#Date:  October 30, 2020

#Lab 4:  Cluster Analysis in R

#Using Wine Dataset

#Note: Run Shortcut:  CTRL+Enter

#Libraries
library(FactoMineR) #Dataset and HCPC
library(Hmisc) #Describe Function
library(psych) #Multiple Functions for Statistics and Multivariate Analysis
library(GGally) #ggpairs Function
library(ggplot2) #ggplot2 Functions
library(vioplot) #Violin Plot Function
library(corrplot) #Plot Correlations
library(REdaS) #Bartlett's Test of Sphericity
library(psych) #PCA/FA functions
library(factoextra) #PCA Visualizations
library("FactoMineR") #PCA functions / Hierarchical Clustering
library(ade4) #PCA Visualizations
library(cluster) #Basic Clustering Algorithms
library(fpc) #DBSCAN
library(dbscan) #DBSCAN
library(mclust) #Model Clustering
library(kernlab) #Spectral Clustering


##############################################################################################

#Read in Data from R base dataset

library(FactoMineR)
data(wine)

#Check Sample Size and Number of Variables
dim(wine)
#21-Sample Size and 31 variables

#Show for first 6 rows of data
head(wine)

names(wine)

str(wine)

#Subset for Numeric Variables
wine2 <-wine[,3:31]
head(wine2)

################################################################################################

#Check for Missing Values (i.e. NAs)

#For All Variables
sum(is.na(wine2))
#0 total missing values


#################################################################################################################

#Different Cluster Methods

library(cluster)
library(factoextra)

# Let's first scale the data:
#Normalizing the Data

wine_scale <- scale(wine2) 


#K-Means Clustering

#Determining Optimal Number of Clusters
fviz_nbclust(wine_scale, kmeans)


#Run K-Means Cluster Analysis

set.seed(123)
wine_k2 <- kmeans(wine_scale, centers=2, iter.max=123, nstart = 15)
wine_k2

# Cluster size
wine_k2$size

# Cluster means
wine_k2$centers


# Visualize
#Note with the large dataset it will take a long time, but is good for small sample size ~1,000 samples
library("factoextra")
fviz_cluster(wine_k2, data = wine_scale, 
             ellipse.type = "convex",
             palette = "jco",
             repel = TRUE,
             ggtheme = theme_minimal())

#Another Way to Choose Optimal Clusters

#Enter name of the data matrix to be clustered here:
my.data.matrix <- wine_scale  

my.k.choices <- 2:5
n <- length(my.data.matrix[,1])
wss1 <- (n-1)*sum(apply(my.data.matrix,2,var))
wss <- numeric(0)
for(i in my.k.choices) {
  W <- sum(kmeans(my.data.matrix,i)$withinss)
  wss <- c(wss,W)
}
wss <- c(wss1,wss)
plot(c(1,my.k.choices),wss,type='l',xlab='Number of clusters', ylab='Within-groups sum-of-squares', lwd=2)


#Run K-Means Cluster Analysis for 4 Clusters

set.seed(123)
wine_k4 <- kmeans(wine_scale, centers=4, iter.max=100, nstart = 25)
wine_k4

# Visualize
library("factoextra")
fviz_cluster(wine_k4, data = wine_scale, 
             ellipse.type = "convex",
             palette = "jco",
             repel = TRUE,
             ggtheme = theme_minimal())

# Cluster size
wine_k4$size

# Cluster means
wine_k4$centers

#Aggregating the Clusters
wine_k4_clust <- lapply(1:4, function(nc) row.names(wine)[wine_k4$cluster==nc])  
wine_k4_clust  # printing the clusters in terms of the row numbers


#Alternative Way of Running K-Means Clustering

library("factoextra")
# K-means clustering
wine_kmeans4 <- eclust(wine_scale, "kmeans", k = 4,
                            nstart = 15, graph = FALSE)

wine_kmeans4

# Visualize the silhouette of clusters
fviz_silhouette(wine_kmeans4)



#K-Medoids Clustering

#Run PAM Algorithm
library(cluster)
# K-medoids directly on the (standardized) data matrix:
wine_kmed_4 <- pam(wine_scale, k=4, diss=F)
wine_kmed_4


# Visualize
fviz_cluster(wine_kmed_4)

wine_kmed_4$clustering  # printing the "clustering vector"

wine_kmed_4$silinfo$avg.width  #printing the average silhouette width

### A little function to calculate the average silhouette width
### for a variety of choices of k:

my.k.choices <- 2:8
avg.sil.width <- rep(0, times=length(my.k.choices))
for (ii in (1:length(my.k.choices)) ){
  avg.sil.width[ii] <- pam(wine_scale, k=my.k.choices[ii])$silinfo$avg.width
}
print( cbind(my.k.choices, avg.sil.width) )


library(cluster)
# K-medoids directly on the (standardized) data matrix:
wine_kmed_3 <- pam(wine_scale, k=3, diss=F)
wine_kmed_3

# Visualize
fviz_cluster(wine_kmed_3)

#Aggregating the Clusters
wine_med_k3_clust <- lapply(1:3, function(nc) row.names(wine)[wine_kmed_3$clustering==nc])  
wine_med_k3_clust   # printing the clusters in terms of the row number


#Using CLARA

wine2_clara <- clara(wine_scale, 2, samples = 50, pamLike = TRUE)

# Print components of wine_clara 
print(wine2_clara)

# Visualize
fviz_cluster(wine2_clara)



#Hierarchical Clustering

#Single Cluster

# Getting distance matrix:

dist_wine <- dist(wine_scale)
dist_wine

# Single linkage:

wine_single_link <- hclust(dist_wine, method='single')
wine_single_link

# Plotting the single linkage dendrogram:

plot(wine_single_link, labels=row.names(wine), ylab="Distance")

# complete linkage:

wine_complete_link <- hclust(dist_wine, method='complete')
wine_complete_link

# Plotting the complete linkage dendrogram:

plot(wine_complete_link, labels=row.names(wine), ylab="Distance")


# Average linkage:

wine_avg_link <- hclust(dist_wine, method='average')
wine_avg_link

# Plotting the average linkage dendrogram:

plot(wine_avg_link, labels=row.names(wine), ylab="Distance")


# Complete Linkage Solution:

cut_4 <- cutree(wine_complete_link, k=4)
cut_4    # printing the "clustering vector"

wine_hk4_clust <- lapply(1:4, function(nc) row.names(wine)[cut_4==nc])  
wine_hk4_clust   # printing the clusters in terms of the row numbers

#Alternative Way of Running Hierarchical Clustering

# Enhanced hierarchical clustering
wine_hc4_clust <- eclust(wine_scale, "hclust", k = 4,
                              method = "ward.D2", graph = FALSE) 
wine_hc4_clust


#Visualize Hierarchical Cluster Dendrogram

# Dendrogram
fviz_dend(wine_hc4_clust, rect = TRUE, show_labels = TRUE, cex = 0.5) 


# Visualize the silhouette of clusters
fviz_silhouette(wine_hc4_clust)


#Fuzzy Clustering
library(cluster)
wine_fanny <- fanny(wine_scale, 3)

#Membership Coefficients
head(wine_fanny, 3) 

# Dunn's partition coefficient
wine_fanny$coeff


#Visualize
library(factoextra)
fviz_cluster(wine_fanny, ellipse.type = "norm", repel = TRUE,
             palette = "jco", ggtheme = theme_minimal(),
             legend = "right")


#Vizualize Silhouette Width
fviz_silhouette(wine_fanny, palette = "jco",
                ggtheme = theme_minimal())


#Model-Based Clustering
library(mclust)
wine_mc <- Mclust(wine_scale)
wine_mc

#Summary of Cluster Results
summary(wine_mc)      

#Optimal Number of Clusters
wine_mc$G    #Optimal Number of Clusters 3

#Visualizing
library(factoextra)
# BIC values used for choosing the number of clusters
fviz_mclust(wine_mc, "BIC", palette = "jco")
# Classification: plot showing the clustering
fviz_mclust(wine_mc, "classification", geom = "point", 
            pointsize = 1.5, palette = "jco")
# Classification uncertainty
fviz_mclust(wine_mc, "uncertainty", palette = "jco")


#DBSCAN
library(fpc)
library(dbscan)

# Compute DBSCAN using fpc package
library("fpc")
set.seed(123)
wine_db <- fpc::dbscan(wine_scale, eps = 1, MinPts = 5)
wine_db

# Plot DBSCAN results
library("factoextra")
fviz_cluster(wine_db, data = wine_scale, stand = FALSE,
             ellipse = FALSE, show.clust.cent = FALSE,
             geom = "point",palette = "jco", ggtheme = theme_classic())

#Summary of Results
print(wine_db)

#Optimal Number of Clusters
dbscan::kNNdistplot(wine_scale, k =  2)
abline(h = 0.55, lty = 2)


#Hierarchical K-Means Clustering

# Compute hierarchical k-means clustering
library(factoextra)
wine_hk <-hkmeans(wine_scale, 2)

#Print the Results
wine_hk

# Elements returned by hkmeans()
names(wine_hk)

# Visualize the tree
fviz_dend(wine_hk, cex = 0.6, palette = "jco", 
          rect = TRUE, rect_border = "jco", rect_fill = TRUE)

# Visualize the hkmeans final clusters
fviz_cluster(wine_hk, palette = "jco", repel = TRUE,
             ggtheme = theme_classic())


#Spectral Clustering

library(kernlab)

wine_sc <- specc(wine_scale, centers=2)

#print the results
wine_sc


wine_sc_clust <- lapply(1:2, function(nc) row.names(wine)[wine_sc==nc])  
wine_sc_clust   # printing the clusters in terms of the row numbers

#Hierarchical Clustering Principal Component
library(FactoMineR)
HCPC(wine_scale, graph = TRUE)