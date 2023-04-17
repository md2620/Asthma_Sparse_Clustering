### MARC RECOS ####
#Clear environment
rm(list=ls())

df_for_KMeans_un <- read.csv("/rds/general/user/md2620/home/asthma/Malo/Dataframes/Final/unsup_df.csv")

#Reproducibility 
set.seed(1222)

#Packages loading
library(sparcl)
library(survival)
library(compHclust)
library(cluster)
#Check classes of variables
View(sapply(df_for_KMeans_un, class))

#Set Subject.ID as index
df_un_KMeans <- df_for_KMeans_un[-1]
row.names(df_un_KMeans) <- df_for_KMeans_un$X
View(df_un_KMeans)

#Create an index of your original categorical variables
index_categorical <- c(181:361)
df_un_KMeans[,index_categorical] <- lapply(df_un_KMeans[ ,index_categorical], as.factor)

#Remove few variables that you don't use in kmeans and preweighted clustering
df_un_KMeans = subset(df_un_KMeans, select = -c(cohort,cluster_K_2,cluster_K_3,cluster_K_4,
                                                cluster_K_5,cluster_K_6,cluster_K_7,
                                                cluster_K_8,cluster_K_9,cluster_K_10,
                                                cluster_K_11,cluster_K_12,cluster_K_13,
                                                cluster_K_14,cluster_K_15,Healthy,Severe,Severe_asthma,
                                                MildModerate,Severe_Smoker))

#Check classes of variables
View(sapply(df_un_KMeans, class))

#Seperate numerical and categorical variables into 2 different dataframes
#Numerical dataframe
num_df_un_KMeans <- df_un_KMeans %>%
  select_if(Negate(is.factor))

#Categorical dataframe
cat_df_un_KMeans <- df_un_KMeans %>%
  select_if(is.factor)

#Check classes of variables
View(sapply(num_df_un_KMeans, class))

num_df_un_KMeans$Omics.Xylose <- as.numeric(num_df_un_KMeans$Omics.Xylose)
num_df_un_KMeans$Omics.Xanthosine <- as.numeric(num_df_un_KMeans$Omics.Xanthosine)
num_df_un_KMeans$Omics.Furoylglycine <- as.numeric(num_df_un_KMeans$Omics.Furoylglycine)
num_df_un_KMeans$Omics.Allantoin <- as.numeric(num_df_un_KMeans$Omics.Allantoin)
num_df_un_KMeans$Omics.Cytosine <- as.numeric(num_df_un_KMeans$Omics.Cytosine)
num_df_un_KMeans$Omics.Glucosamine <- as.numeric(num_df_un_KMeans$Omics.Glucosamine)
num_df_un_KMeans$Omics.Glutamic.acid <- as.numeric(num_df_un_KMeans$Omics.Glutamic.acid)
num_df_un_KMeans$Omics.Isoleucine <- as.numeric(num_df_un_KMeans$Omics.Isoleucine)
num_df_un_KMeans$Omics.Lysine <- as.numeric(num_df_un_KMeans$Omics.Lysine)
num_df_un_KMeans$Omics.Maltose <- as.numeric(num_df_un_KMeans$Omics.Maltose)
num_df_un_KMeans$Omics.N.Acetylglutamic.acid <- as.numeric(num_df_un_KMeans$Omics.N.Acetylglutamic.acid)
num_df_un_KMeans$Omics.N.Methyl.D.aspartic.acid <- as.numeric(num_df_un_KMeans$Omics.N.Methyl.D.aspartic.acid)
num_df_un_KMeans$Omics.O.Acetylserine <- as.numeric(num_df_un_KMeans$Omics.O.Acetylserine)
num_df_un_KMeans$Omics.Phenylalanine <- as.numeric(num_df_un_KMeans$Omics.Phenylalanine)
num_df_un_KMeans$Omics.Sarcosine <- as.numeric(num_df_un_KMeans$Omics.Sarcosine)
num_df_un_KMeans$Omics.N.Acetylputrescine <- as.numeric(num_df_un_KMeans$Omics.N.Acetylputrescine)

# For visualization purposes, we drop all variables that have 0 variance
#Supply names of columns that have 0 variance
names(num_df_un_KMeans[, sapply(num_df_un_KMeans, function(v) var(v, na.rm=TRUE)==0)])

#Drop columns that have 0 variance from dataframe
num_df_un_KMeans <- num_df_un_KMeans[,apply(num_df_un_KMeans, 2, var, na.rm=TRUE) != 0]

#Run KMeans with different values of K
km_1 <- kmeans(num_df_un_KMeans, centers = 1, nstart = 25)
km_2 <- kmeans(num_df_un_KMeans, centers = 2, nstart = 25)
km_3 <- kmeans(num_df_un_KMeans, centers = 3, nstart = 25)
km_4 <- kmeans(num_df_un_KMeans, centers = 4, nstart = 25)
km_5 <- kmeans(num_df_un_KMeans, centers = 5, nstart = 25)
km_6 <- kmeans(num_df_un_KMeans, centers = 6, nstart = 25)
km_7 <- kmeans(num_df_un_KMeans, centers = 7, nstart = 25)
km_8 <- kmeans(num_df_un_KMeans, centers = 8, nstart = 25)
km_9 <- kmeans(num_df_un_KMeans, centers = 9, nstart = 25)
km_10 <- kmeans(num_df_un_KMeans, centers = 10, nstart = 25)
km_11 <- kmeans(num_df_un_KMeans, centers = 11, nstart = 25)
km_12 <- kmeans(num_df_un_KMeans, centers = 12, nstart = 25)
km_13 <- kmeans(num_df_un_KMeans, centers = 13, nstart = 25)
km_14 <- kmeans(num_df_un_KMeans, centers = 14, nstart = 25)
km_15 <- kmeans(num_df_un_KMeans, centers = 15, nstart = 25)


#Get resulting clusters
print(km_4)
aggregate(num_df_un_KMeans, by=list(cluster=km_4$cluster), mean)

#Cluster size
km_4$size

#Cluster centers
km_4$centers

#Visualize cluster
library(factoextra)

#Either use this
fviz_cluster(km_4, data = num_df_un_KMeans, ellipse = TRUE)

#Or this
# plots to compare according to choice of K
plt2 <- fviz_cluster(km_2, geom = "point",  data = num_df_un_KMeans) + ggtitle("KMeans - 610 Asthma patients separated into 2 clusters")
plt3 <- fviz_cluster(km_3, geom = "point",  data = num_df_un_KMeans) + ggtitle("KMeans - 610 Asthma patients separated into 3 clusters")
plt4 <- fviz_cluster(km_4, geom = "point",  data = num_df_un_KMeans) + ggtitle("KMeans - 610 Asthma patients separated into 4 clusters")
plt5 <- fviz_cluster(km_5, geom = "point",  data = num_df_un_KMeans) + ggtitle("KMeans - 610 Asthma patients separated into 5 clusters")
plt6 <- fviz_cluster(km_6, geom = "point",  data = num_df_un_KMeans) + ggtitle("KMeans - 610 Asthma patients separated into 6 clusters")
plt7 <- fviz_cluster(km_7, geom = "point",  data = num_df_un_KMeans) + ggtitle("KMeans - 610 Asthma patients separated into 7 clusters")
plt8 <- fviz_cluster(km_8, geom = "point",  data = num_df_un_KMeans) + ggtitle("KMeans - 610 Asthma patients separated into 8 clusters")
plt9 <- fviz_cluster(km_9, geom = "point",  data = num_df_un_KMeans) + ggtitle("KMeans - 610 Asthma patients separated into 9 clusters")
plt10 <- fviz_cluster(km_10, geom = "point",  data = num_df_un_KMeans) + ggtitle("KMeans - 610 Asthma patients separated into 10 clusters")
plt11 <- fviz_cluster(km_11, geom = "point",  data = num_df_un_KMeans) + ggtitle("KMeans - 610 Asthma patients separated into 11 clusters")
plt12 <- fviz_cluster(km_12, geom = "point",  data = num_df_un_KMeans) + ggtitle("KMeans - 610 Asthma patients separated into 12 clusters")
plt13 <- fviz_cluster(km_13, geom = "point",  data = num_df_un_KMeans) + ggtitle("KMeans - 610 Asthma patients separated into 13 clusters")
plt14 <- fviz_cluster(km_14, geom = "point",  data = num_df_un_KMeans) + ggtitle("KMeans - 610 Asthma patients separated into 14 clusters")
plt15 <- fviz_cluster(km_15, geom = "point",  data = num_df_un_KMeans) + ggtitle("KMeans - 610 Asthma patients separated into 15 clusters")

#### SAVE KMEANS VIZ RESULTS ####
library(patchwork)
dev.size()
plt2 + plt3 + plt4 + plt5
ggsave("/rds/general/user/md2620/home/asthma/Malo/Results/KMeans_Viz/K2_5.jpeg", width = 12, height = 6)

plt6 + plt7 + plt8 + plt9
ggsave("/rds/general/user/md2620/home/asthma/Malo/Results/KMeans_Viz/K6_9.jpeg", width = 12, height = 6)

plt10 + plt11
ggsave("/rds/general/user/md2620/home/asthma/Malo/Results/KMeans_Viz/K10_11.jpeg", width = 12, height = 6)

plt12 + plt13 
ggsave("/rds/general/user/md2620/home/asthma/Malo/Results/KMeans_Viz/K12_13.jpeg", width = 12, height = 6)

plt14 + plt15
ggsave("/rds/general/user/md2620/home/asthma/Malo/Results/KMeans_Viz/K14_15.jpeg", width = 12, height = 6)



######### KMODES IMPLEMENTATION SIMPLE MATCHING AND VIZ ##########
#Check classes of variables
View(sapply(cat_df_un_KMeans, class))

cat_df_un_KMeans <- fastDummies::dummy_cols(cat_df_un_KMeans)

#Remove the original variables as you have created dummies.
cat_df_un_KMeans <- cat_df_un_KMeans %>%
  select_if(Negate(is.factor))

#Drop all observations with NAs in Kmodes --> We don't want them to be imputed in order to avoid making irrelevant assumptions
cat_df_un_KMeans <- drop_na(cat_df_un_KMeans)
cat_df_un_KMeans <- as.data.frame(na.fill(cat_df_un_KMeans, fill=""))
#We therefore end up with 338 observations
#Load packages
#install.packages("klaR")
#install.packages("zoo")
library(zoo)
library(klaR)
kmodes_2 <- kmodes(na.fill(cat_df_un_KMeans, fill=""), 2, weighted = FALSE, fast = TRUE)
kmodes_3 <- kmodes(na.fill(cat_df_un_KMeans, fill=""), 3, weighted = FALSE, fast = TRUE)
kmodes_4 <- kmodes(na.fill(cat_df_un_KMeans, fill=""), 4, weighted = FALSE, fast = TRUE)
kmodes_5 <- kmodes(na.fill(cat_df_un_KMeans, fill=""), 5, weighted = FALSE, fast = TRUE)
kmodes_6 <- kmodes(na.fill(cat_df_un_KMeans, fill=""), 6, weighted = FALSE, fast = TRUE)
kmodes_7 <- kmodes(na.fill(cat_df_un_KMeans, fill=""), 7, weighted = FALSE, fast = TRUE)
kmodes_8 <- kmodes(na.fill(cat_df_un_KMeans, fill=""), 8, weighted = FALSE, fast = TRUE)
kmodes_9 <- kmodes(na.fill(cat_df_un_KMeans, fill=""), 9, weighted = FALSE, fast = TRUE)
kmodes_10 <- kmodes(na.fill(cat_df_un_KMeans, fill=""), 10, weighted = FALSE, fast = TRUE)
kmodes_11 <- kmodes(na.fill(cat_df_un_KMeans, fill=""), 11, weighted = FALSE, fast = TRUE)
kmodes_12 <- kmodes(na.fill(cat_df_un_KMeans, fill=""), 12, weighted = FALSE, fast = TRUE)
kmodes_13 <- kmodes(na.fill(cat_df_un_KMeans, fill=""), 13, weighted = FALSE, fast = TRUE)
kmodes_14 <- kmodes(na.fill(cat_df_un_KMeans, fill=""), 14, weighted = FALSE, fast = TRUE)
kmodes_15 <- kmodes(na.fill(cat_df_un_KMeans, fill=""), 15, weighted = FALSE, fast = TRUE)

#Append cluster results to dataframe
cat_df_un_KMeans$clusters_2 <- factor(kmodes_2$cluster)
cat_df_un_KMeans$clusters_3 <- factor(kmodes_3$cluster)
cat_df_un_KMeans$clusters_4 <- factor(kmodes_4$cluster)
cat_df_un_KMeans$clusters_5 <- factor(kmodes_5$cluster)
cat_df_un_KMeans$clusters_6 <- factor(kmodes_6$cluster)
cat_df_un_KMeans$clusters_7 <- factor(kmodes_7$cluster)
cat_df_un_KMeans$clusters_8 <- factor(kmodes_8$cluster)
cat_df_un_KMeans$clusters_9 <- factor(kmodes_9$cluster)
cat_df_un_KMeans$clusters_10 <- factor(kmodes_10$cluster)
cat_df_un_KMeans$clusters_11 <- factor(kmodes_11$cluster)
cat_df_un_KMeans$clusters_12 <- factor(kmodes_12$cluster)
cat_df_un_KMeans$clusters_13 <- factor(kmodes_13$cluster)
cat_df_un_KMeans$clusters_14 <- factor(kmodes_14$cluster)
cat_df_un_KMeans$clusters_15 <- factor(kmodes_15$cluster)

# Compute data with principal components ------------------
PCA_cat_df <- cat_df_un_KMeans[c(1:610),c(1:754)]

pca_modes <- prcomp(PCA_cat_df) # compute principal components
summary(pca_modes)

#Make sure clusters variables are factor variables
cat_df_un_KMeans$clusters_2 = as.factor(cat_df_un_KMeans$clusters_2)
cat_df_un_KMeans$clusters_3 = as.factor(cat_df_un_KMeans$clusters_3)
cat_df_un_KMeans$clusters_4 = as.factor(cat_df_un_KMeans$clusters_4)
cat_df_un_KMeans$clusters_5 = as.factor(cat_df_un_KMeans$clusters_5)
cat_df_un_KMeans$clusters_6 = as.factor(cat_df_un_KMeans$clusters_6)
cat_df_un_KMeans$clusters_7 = as.factor(cat_df_un_KMeans$clusters_7)
cat_df_un_KMeans$clusters_8 = as.factor(cat_df_un_KMeans$clusters_8)
cat_df_un_KMeans$clusters_9 = as.factor(cat_df_un_KMeans$clusters_9)
cat_df_un_KMeans$clusters_10 = as.factor(cat_df_un_KMeans$clusters_10)
cat_df_un_KMeans$clusters_11 = as.factor(cat_df_un_KMeans$clusters_11)
cat_df_un_KMeans$clusters_12 = as.factor(cat_df_un_KMeans$clusters_12)
cat_df_un_KMeans$clusters_13 = as.factor(cat_df_un_KMeans$clusters_13)
cat_df_un_KMeans$clusters_14 = as.factor(cat_df_un_KMeans$clusters_14)
cat_df_un_KMeans$clusters_15 = as.factor(cat_df_un_KMeans$clusters_15)


#Check levels to make sure clusters variables are recorded as factor variables
levels(cat_df_un_KMeans$clusters_2)
levels(cat_df_un_KMeans$clusters_3)

################### PCA FOR K=2:6 #####################
##### PCA grouped by kmodes cluster K=2 #######
# Data frame of principal components ----------------------
df_modes_K_2 <- data.frame(pca_modes$x, clusters_2=cat_df_un_KMeans$clusters_2)  # dataframe of principal components
df_modes_K_2_cluster1 <- df_modes_K_2[df_modes_K_2$clusters_2 == "1", ]  # df for 'cluster 1'
df_modes_K_2_cluster2 <- df_modes_K_2[df_modes_K_2$clusters_2 == "2", ]  # df for 'cluster 2'

##### PCA grouped by kmodes cluster K=3 #######
# Data frame of principal components ----------------------
df_modes_K_3 <- data.frame(pca_modes$x, clusters_3=cat_df_un_KMeans$clusters_3)  # dataframe of principal components
df_modes_K_3_cluster1 <- df_modes_K_3[df_modes_K_3$clusters_3 == "1", ]  # df for 'cluster 1'
df_modes_K_3_cluster2 <- df_modes_K_3[df_modes_K_3$clusters_3 == "2", ]  # df for 'cluster 2'
df_modes_K_3_cluster3 <- df_modes_K_3[df_modes_K_3$clusters_3 == "3", ]  # df for 'cluster 3'

##### PCA grouped by kmodes cluster K=4 #######
# Data frame of principal components ----------------------
df_modes_K_4 <- data.frame(pca_modes$x, clusters_4=cat_df_un_KMeans$clusters_4)  # dataframe of principal components
df_modes_K_4_cluster1 <- df_modes_K_4[df_modes_K_4$clusters_4 == "1", ]  # df for 'cluster 1'
df_modes_K_4_cluster2 <- df_modes_K_4[df_modes_K_4$clusters_4 == "2", ]  # df for 'cluster 2'
df_modes_K_4_cluster3 <- df_modes_K_4[df_modes_K_4$clusters_4 == "3", ]  # df for 'cluster 3'
df_modes_K_4_cluster4 <- df_modes_K_4[df_modes_K_4$clusters_4 == "4", ]  # df for 'cluster 4'

##### PCA grouped by kmodes cluster K=5 #######
# Data frame of principal components ----------------------
df_modes_K_5 <- data.frame(pca_modes$x, clusters_5=cat_df_un_KMeans$clusters_5)  # dataframe of principal components
df_modes_K_5_cluster1 <- df_modes_K_5[df_modes_K_5$clusters_5 == "1", ]  # df for 'cluster 1'
df_modes_K_5_cluster2 <- df_modes_K_5[df_modes_K_5$clusters_5 == "2", ]  # df for 'cluster 2'
df_modes_K_5_cluster3 <- df_modes_K_5[df_modes_K_5$clusters_5 == "3", ]  # df for 'cluster 3'
df_modes_K_5_cluster4 <- df_modes_K_5[df_modes_K_5$clusters_5 == "4", ]  # df for 'cluster 4'
df_modes_K_5_cluster5 <- df_modes_K_5[df_modes_K_5$clusters_5 == "5", ]  # df for 'cluster 5'

##### PCA grouped by kmodes cluster K=6 #######
# Data frame of principal components ----------------------
df_modes_K_6 <- data.frame(pca_modes$x, clusters_6=cat_df_un_KMeans$clusters_6)  # dataframe of principal components
df_modes_K_6_cluster1 <- df_modes_K_6[df_modes_K_6$clusters_6 == "1", ]  # df for 'cluster 1'
df_modes_K_6_cluster2 <- df_modes_K_6[df_modes_K_6$clusters_6 == "2", ]  # df for 'cluster 2'
df_modes_K_6_cluster3 <- df_modes_K_6[df_modes_K_6$clusters_6 == "3", ]  # df for 'cluster 3'
df_modes_K_6_cluster4 <- df_modes_K_6[df_modes_K_6$clusters_6 == "4", ]  # df for 'cluster 4'
df_modes_K_6_cluster5 <- df_modes_K_6[df_modes_K_6$clusters_6 == "5", ]  # df for 'cluster 5'
df_modes_K_6_cluster6 <- df_modes_K_6[df_modes_K_6$clusters_6 == "6", ]  # df for 'cluster 6'


############################# VIZ AND SAVE KMODES PCA ####################################
library(patchwork)

# Plot ----------------------------------------------------
pmodes2 <- ggplot(df_modes_K_2, aes(PC1, PC2, col= clusters_2, loadings=TRUE)) + 
  geom_point(aes(shape=clusters_2), size=2) +   # draw points
  labs(title="KModes - 610 Observations grouped in 2 clusters", 
       subtitle="With principal components PC1 and PC2 as X and Y axis",
       caption="Source: UBIOPRED") + 
  coord_cartesian(xlim = 1.2 * c(min(df_modes_K_2$PC1), max(df_modes_K_2$PC1)), 
                  ylim = 1.2 * c(min(df_modes_K_2$PC2), max(df_modes_K_2$PC2))) +   # change axis limits
  geom_encircle(data = df_modes_K_2_cluster1, aes(x=PC1, y=PC2)) +   # draw circles
  geom_encircle(data = df_modes_K_2_cluster2, aes(x=PC1, y=PC2)) 


pmodes2_fin <- print(pmodes2 + labs(colour = "Clusters") + labs(shape="Clusters"))

# Plot ----------------------------------------------------
pmodes3 <- ggplot(df_modes_K_3, aes(PC1, PC2, col= clusters_3, loadings=TRUE)) + 
  geom_point(aes(shape=clusters_3), size=2) +   # draw points
  labs(title="KModes - 610 Observations grouped in 3 clusters", 
       subtitle="With principal components PC1 and PC2 as X and Y axis",
       caption="Source: UBIOPRED") + 
  coord_cartesian(xlim = 1.2 * c(min(df_modes_K_3$PC1), max(df_modes_K_3$PC1)), 
                  ylim = 1.2 * c(min(df_modes_K_3$PC2), max(df_modes_K_3$PC2))) +   # change axis limits
  geom_encircle(data = df_modes_K_3_cluster1, aes(x=PC1, y=PC2)) +   # draw circles
  geom_encircle(data = df_modes_K_3_cluster2, aes(x=PC1, y=PC2)) +
  geom_encircle(data = df_modes_K_3_cluster3, aes(x=PC1, y=PC2))

pmodes3_fin <- print(pmodes3 + labs(colour = "Clusters") + labs(shape="Clusters"))

# Plot ----------------------------------------------------
pmodes4 <- ggplot(df_modes_K_4, aes(PC1, PC2, col= clusters_4, loadings=TRUE)) + 
  geom_point(aes(shape=clusters_4), size=2) +   # draw points
  labs(title="KModes - 610 Observations grouped in 4 clusters", 
       subtitle="With principal components PC1 and PC2 as X and Y axis",
       caption="Source: UBIOPRED") + 
  coord_cartesian(xlim = 1.2 * c(min(df_modes_K_4$PC1), max(df_modes_K_4$PC1)), 
                  ylim = 1.2 * c(min(df_modes_K_4$PC2), max(df_modes_K_4$PC2))) +   # change axis limits
  geom_encircle(data = df_modes_K_4_cluster1, aes(x=PC1, y=PC2)) +   # draw circles
  geom_encircle(data = df_modes_K_4_cluster2, aes(x=PC1, y=PC2)) +
  geom_encircle(data = df_modes_K_4_cluster3, aes(x=PC1, y=PC2)) +
  geom_encircle(data = df_modes_K_4_cluster4, aes(x=PC1, y=PC2)) 

pmodes4_fin <- print(pmodes4 + labs(colour = "Clusters") + labs(shape="Clusters"))

pmodes2_fin| pmodes3_fin /
  pmodes4_fin

ggsave("/rds/general/user/md2620/home/asthma/Malo/Results/PCA_Results_KModes/K2_3_4.jpeg", width = 12, height = 6)

# Plot ----------------------------------------------------
pmodes5 <- ggplot(df_modes_K_5, aes(PC1, PC2, col= clusters_5, loadings=TRUE)) + 
  geom_point(aes(shape=clusters_5), size=2) +   # draw points
  labs(title="KModes - 610 Observations grouped in 5 clusters", 
       subtitle="With principal components PC1 and PC2 as X and Y axis",
       caption="Source: UBIOPRED") + 
  coord_cartesian(xlim = 1.2 * c(min(df_modes_K_5$PC1), max(df_modes_K_5$PC1)), 
                  ylim = 1.2 * c(min(df_modes_K_5$PC2), max(df_modes_K_5$PC2))) +   # change axis limits
  geom_encircle(data = df_modes_K_5_cluster1, aes(x=PC1, y=PC2)) +   # draw circles
  geom_encircle(data = df_modes_K_5_cluster2, aes(x=PC1, y=PC2)) +
  geom_encircle(data = df_modes_K_5_cluster3, aes(x=PC1, y=PC2)) +
  geom_encircle(data = df_modes_K_5_cluster4, aes(x=PC1, y=PC2)) +
  geom_encircle(data = df_modes_K_5_cluster5, aes(x=PC1, y=PC2)) 

pmodes5_fin <- print(pmodes5 + labs(colour = "Clusters") + labs(shape="Clusters"))

# Plot ----------------------------------------------------
pmodes6 <- ggplot(df_modes_K_6, aes(PC1, PC2, col= clusters_6, loadings=TRUE)) + 
  geom_point(aes(shape=clusters_6), size=2) +   # draw points
  labs(title="KModes - 610 Observations grouped in 6 clusters", 
       subtitle="With principal components PC1 and PC2 as X and Y axis",
       caption="Source: UBIOPRED") + 
  coord_cartesian(xlim = 1.2 * c(min(df_modes_K_6$PC1), max(df_modes_K_6$PC1)), 
                  ylim = 1.2 * c(min(df_modes_K_6$PC2), max(df_modes_K_6$PC2))) +   # change axis limits
  geom_encircle(data = df_modes_K_6_cluster1, aes(x=PC1, y=PC2)) +   # draw circles
  geom_encircle(data = df_modes_K_6_cluster2, aes(x=PC1, y=PC2)) +
  geom_encircle(data = df_modes_K_6_cluster3, aes(x=PC1, y=PC2)) +
  geom_encircle(data = df_modes_K_6_cluster4, aes(x=PC1, y=PC2)) +
  geom_encircle(data = df_modes_K_6_cluster5, aes(x=PC1, y=PC2)) +
  geom_encircle(data = df_modes_K_6_cluster6, aes(x=PC1, y=PC2)) 

pmodes6_fin <- print(pmodes6 + labs(colour = "Clusters") + labs(shape="Clusters"))

pmodes5_fin+ pmodes6_fin

ggsave("/rds/general/user/md2620/home/asthma/Malo/Results/PCA_Results_KModes/K5_6.jpeg", width = 12, height = 6)
