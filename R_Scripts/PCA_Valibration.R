##### PCA VISUALIZATION #####
#Visualization clusters according to lambda calibration

#Clear environment
rm(list=ls())

#Reading the files
unsup_df_proc = read.csv("/rds/general/user/md2620/home/asthma/Malo/Dataframes/Final/unsup_df_all_processed.csv")
unsup_calib = read.csv("/rds/general/user/md2620/home/asthma/Malo/Dataframes/Final/unsup_df_calib_lambda.csv")
#devtools::install_github("hrbrmstr/ggalt")
library(ggplot2)
library(ggalt)
#install.packages("ggfortify")
library(ggfortify)
theme_set(theme_classic())

#Drop 1st column 
unsup_df_proc = subset(unsup_df_proc, select = -c(X))

# Compute data with principal components ------------------
PCA_calib <- unsup_df_proc[c(1:610),c(1:362)]

#Set Subject.ID as index
PCA_calibration <- PCA_calib[-1]
row.names(PCA_calibration) <- PCA_calib$Unnamed..0
View(PCA_calibration)

pc_calib <- prcomp(PCA_calibration)  # compute principal components

#Make sure clusters variables are factor variables
unsup_calib$K4_lbd_0.1 = as.factor(unsup_calib$K4_lbd_0.1)
unsup_calib$K4_lbd_0.2 = as.factor(unsup_calib$K4_lbd_0.2)
unsup_calib$K4_lbd_0.3 = as.factor(unsup_calib$K4_lbd_0.3)
unsup_calib$K4_lbd_0.4 = as.factor(unsup_calib$K4_lbd_0.4)
unsup_calib$K4_lbd_0.5 = as.factor(unsup_calib$K4_lbd_0.5)
unsup_calib$K4_lbd_0.6 = as.factor(unsup_calib$K4_lbd_0.6)
unsup_calib$K4_lbd_0.7 = as.factor(unsup_calib$K4_lbd_0.7)
unsup_calib$K4_lbd_0.8 = as.factor(unsup_calib$K4_lbd_0.8)
unsup_calib$K4_lbd_0.9 = as.factor(unsup_calib$K4_lbd_0.9)
unsup_calib$K4_lbd_1 = as.factor(unsup_calib$K4_lbd_1)
unsup_calib$K4_lbd_2 = as.factor(unsup_calib$K4_lbd_2)
unsup_calib$K4_lbd_3 = as.factor(unsup_calib$K4_lbd_3)
unsup_calib$K4_lbd_4 = as.factor(unsup_calib$K4_lbd_4)
unsup_calib$K4_lbd_5 = as.factor(unsup_calib$K4_lbd_5)
unsup_calib$K4_lbd_6 = as.factor(unsup_calib$K4_lbd_6)
unsup_calib$K4_lbd_7 = as.factor(unsup_calib$K4_lbd_7)
unsup_calib$K4_lbd_8 = as.factor(unsup_calib$K4_lbd_8)
unsup_calib$K4_lbd_9 = as.factor(unsup_calib$K4_lbd_9)
unsup_calib$K4_lbd_10 = as.factor(unsup_calib$K4_lbd_10)
unsup_calib$K4_lbd_11 = as.factor(unsup_calib$K4_lbd_11)
unsup_calib$K4_lbd_12 = as.factor(unsup_calib$K4_lbd_12)
unsup_calib$K4_lbd_13 = as.factor(unsup_calib$K4_lbd_13)
unsup_calib$K4_lbd_14 = as.factor(unsup_calib$K4_lbd_14)
unsup_calib$K4_lbd_15 = as.factor(unsup_calib$K4_lbd_15)
unsup_calib$K4_lbd_25 = as.factor(unsup_calib$K4_lbd_25)
unsup_calib$K4_default_lbd = as.factor(unsup_calib$K4_default_lbd)
unsup_calib$K4_heuristics_2_lbd = as.factor(unsup_calib$K4_heuristics_2_lbd)

#Check levels of calibrated cluster variables 
levels(unsup_calib$K4_lbd_0.1)

##### PCA grouped by supervised cluster K=4, lambda = 0.1 #######
# Data frame of principal components ----------------------
calib_01 <- data.frame(pc_calib$x, K4_lbd_0.1=unsup_calib$K4_lbd_0.1)  # dataframe of principal components
calib_01_cluster1 <- calib_01[calib_01$K4_lbd_0.1 == "1", ]  # df for 'cluster 1'
calib_01_cluster2 <- calib_01[calib_01$K4_lbd_0.1 == "2", ]  # df for 'cluster 2'
calib_01_cluster3 <- calib_01[calib_01$K4_lbd_0.1 == "3", ]  # df for 'cluster 3'
calib_01_cluster4 <- calib_01[calib_01$K4_lbd_0.1 == "4", ]  # df for 'cluster 4'

##### PCA grouped by supervised cluster K=4, lambda = 0.2 #######
# Data frame of principal components ----------------------
calib_02 <- data.frame(pc_calib$x, K4_lbd_0.2=unsup_calib$K4_lbd_0.2)  # dataframe of principal components
calib_02_cluster1 <- calib_02[calib_02$K4_lbd_0.2 == "1", ]  # df for 'cluster 1'
calib_02_cluster2 <- calib_02[calib_02$K4_lbd_0.2 == "2", ]  # df for 'cluster 2'
calib_02_cluster3 <- calib_02[calib_02$K4_lbd_0.2 == "3", ]  # df for 'cluster 3'
calib_02_cluster4 <- calib_02[calib_02$K4_lbd_0.2 == "4", ]  # df for 'cluster 4'

##### PCA grouped by supervised cluster K=4, lambda = 0.3 #######
# Data frame of principal components ----------------------
calib_03 <- data.frame(pc_calib$x, K4_lbd_0.3=unsup_calib$K4_lbd_0.3)  # dataframe of principal components
calib_03_cluster1 <- calib_03[calib_03$K4_lbd_0.3 == "1", ]  # df for 'cluster 1'
calib_03_cluster2 <- calib_03[calib_03$K4_lbd_0.3 == "2", ]  # df for 'cluster 2'
calib_03_cluster3 <- calib_03[calib_03$K4_lbd_0.3 == "3", ]  # df for 'cluster 3'
calib_03_cluster4 <- calib_03[calib_03$K4_lbd_0.3 == "4", ]  # df for 'cluster 4'

##### PCA grouped by supervised cluster K=4, lambda = 0.4 #######
# Data frame of principal components ----------------------
calib_04 <- data.frame(pc_calib$x, K4_lbd_0.4=unsup_calib$K4_lbd_0.4)  # dataframe of principal components
calib_04_cluster1 <- calib_04[calib_04$K4_lbd_0.4 == "1", ]  # df for 'cluster 1'
calib_04_cluster2 <- calib_04[calib_04$K4_lbd_0.4 == "2", ]  # df for 'cluster 2'
calib_04_cluster3 <- calib_04[calib_04$K4_lbd_0.4 == "3", ]  # df for 'cluster 3'
calib_04_cluster4 <- calib_04[calib_04$K4_lbd_0.4 == "4", ]  # df for 'cluster 4'

##### PCA grouped by supervised cluster K=4, lambda = 0.5 #######
# Data frame of principal components ----------------------
calib_05 <- data.frame(pc_calib$x, K4_lbd_0.5=unsup_calib$K4_lbd_0.5)  # dataframe of principal components
calib_05_cluster1 <- calib_05[calib_05$K4_lbd_0.5 == "1", ]  # df for 'cluster 1'
calib_05_cluster2 <- calib_05[calib_05$K4_lbd_0.5 == "2", ]  # df for 'cluster 2'
calib_05_cluster3 <- calib_05[calib_05$K4_lbd_0.5 == "3", ]  # df for 'cluster 3'
calib_05_cluster4 <- calib_05[calib_05$K4_lbd_0.5 == "4", ]  # df for 'cluster 4'

##### PCA grouped by supervised cluster K=4, lambda = 0.6 #######
# Data frame of principal components ----------------------
calib_06 <- data.frame(pc_calib$x, K4_lbd_0.6=unsup_calib$K4_lbd_0.6)  # dataframe of principal components
calib_06_cluster1 <- calib_06[calib_06$K4_lbd_0.6 == "1", ]  # df for 'cluster 1'
calib_06_cluster2 <- calib_06[calib_06$K4_lbd_0.6 == "2", ]  # df for 'cluster 2'
calib_06_cluster3 <- calib_06[calib_06$K4_lbd_0.6 == "3", ]  # df for 'cluster 3'
calib_06_cluster4 <- calib_06[calib_06$K4_lbd_0.6 == "4", ]  # df for 'cluster 4'

##### PCA grouped by supervised cluster K=4, lambda = 0.7 #######
# Data frame of principal components ----------------------
calib_07 <- data.frame(pc_calib$x, K4_lbd_0.7=unsup_calib$K4_lbd_0.7)  # dataframe of principal components
calib_07_cluster1 <- calib_07[calib_07$K4_lbd_0.7 == "1", ]  # df for 'cluster 1'
calib_07_cluster2 <- calib_07[calib_07$K4_lbd_0.7 == "2", ]  # df for 'cluster 2'
calib_07_cluster3 <- calib_07[calib_07$K4_lbd_0.7 == "3", ]  # df for 'cluster 3'
calib_07_cluster4 <- calib_07[calib_07$K4_lbd_0.7 == "4", ]  # df for 'cluster 4'

##### PCA grouped by supervised cluster K=4, lambda = 0.8 #######
# Data frame of principal components ----------------------
calib_08 <- data.frame(pc_calib$x, K4_lbd_0.8=unsup_calib$K4_lbd_0.8)  # dataframe of principal components
calib_08_cluster1 <- calib_08[calib_08$K4_lbd_0.8 == "1", ]  # df for 'cluster 1'
calib_08_cluster2 <- calib_08[calib_08$K4_lbd_0.8 == "2", ]  # df for 'cluster 2'
calib_08_cluster3 <- calib_08[calib_08$K4_lbd_0.8 == "3", ]  # df for 'cluster 3'
calib_08_cluster4 <- calib_08[calib_08$K4_lbd_0.8 == "4", ]  # df for 'cluster 4'

##### PCA grouped by supervised cluster K=4, lambda = 0.9 #######
# Data frame of principal components ----------------------
calib_09 <- data.frame(pc_calib$x, K4_lbd_0.9=unsup_calib$K4_lbd_0.9)  # dataframe of principal components
calib_09_cluster1 <- calib_09[calib_09$K4_lbd_0.9 == "1", ]  # df for 'cluster 1'
calib_09_cluster2 <- calib_09[calib_09$K4_lbd_0.9 == "2", ]  # df for 'cluster 2'
calib_09_cluster3 <- calib_09[calib_09$K4_lbd_0.9 == "3", ]  # df for 'cluster 3'
calib_09_cluster4 <- calib_09[calib_09$K4_lbd_0.9 == "4", ]  # df for 'cluster 4'

##### PCA grouped by supervised cluster K=4, lambda = 1 #######
# Data frame of principal components ----------------------
calib_1 <- data.frame(pc_calib$x, K4_lbd_1=unsup_calib$K4_lbd_1)  # dataframe of principal components
calib_1_cluster1 <- calib_1[calib_1$K4_lbd_1 == "1", ]  # df for 'cluster 1'
calib_1_cluster2 <- calib_1[calib_1$K4_lbd_1 == "2", ]  # df for 'cluster 2'
calib_1_cluster3 <- calib_1[calib_1$K4_lbd_1 == "3", ]  # df for 'cluster 3'
calib_1_cluster4 <- calib_1[calib_1$K4_lbd_1 == "4", ]  # df for 'cluster 4'

##### PCA grouped by supervised cluster K=4, lambda = 2 #######
# Data frame of principal components ----------------------
calib_2 <- data.frame(pc_calib$x, K4_lbd_2=unsup_calib$K4_lbd_2)  # dataframe of principal components
calib_2_cluster1 <- calib_2[calib_2$K4_lbd_2 == "1", ]  # df for 'cluster 1'
calib_2_cluster2 <- calib_2[calib_2$K4_lbd_2 == "2", ]  # df for 'cluster 2'
calib_2_cluster3 <- calib_2[calib_2$K4_lbd_2 == "3", ]  # df for 'cluster 3'
calib_2_cluster4 <- calib_2[calib_2$K4_lbd_2 == "4", ]  # df for 'cluster 4'

##### PCA grouped by supervised cluster K=4, lambda = 3 #######
# Data frame of principal components ----------------------
calib_3 <- data.frame(pc_calib$x, K4_lbd_3=unsup_calib$K4_lbd_3)  # dataframe of principal components
calib_3_cluster1 <- calib_2[calib_3$K4_lbd_3 == "1", ]  # df for 'cluster 1'
calib_3_cluster2 <- calib_2[calib_3$K4_lbd_3 == "2", ]  # df for 'cluster 2'
calib_3_cluster3 <- calib_2[calib_3$K4_lbd_3 == "3", ]  # df for 'cluster 3'
calib_3_cluster4 <- calib_2[calib_3$K4_lbd_3 == "4", ]  # df for 'cluster 4'

##### PCA grouped by supervised cluster K=4, lambda = 4 #######
# Data frame of principal components ----------------------
calib_4 <- data.frame(pc_calib$x, K4_lbd_4=unsup_calib$K4_lbd_4)  # dataframe of principal components
calib_4_cluster1 <- calib_2[calib_4$K4_lbd_4 == "1", ]  # df for 'cluster 1'
calib_4_cluster2 <- calib_2[calib_4$K4_lbd_4 == "2", ]  # df for 'cluster 2'
calib_4_cluster3 <- calib_2[calib_4$K4_lbd_4 == "3", ]  # df for 'cluster 3'
calib_4_cluster4 <- calib_2[calib_4$K4_lbd_4 == "4", ]  # df for 'cluster 4'

##### PCA grouped by supervised cluster K=4, lambda = 5 #######
# Data frame of principal components ----------------------
calib_5 <- data.frame(pc_calib$x, K4_lbd_5=unsup_calib$K4_lbd_5)  # dataframe of principal components
calib_5_cluster1 <- calib_5[calib_5$K4_lbd_5 == "1", ]  # df for 'cluster 1'
calib_5_cluster2 <- calib_5[calib_5$K4_lbd_5 == "2", ]  # df for 'cluster 2'
calib_5_cluster3 <- calib_5[calib_5$K4_lbd_5 == "3", ]  # df for 'cluster 3'
calib_5_cluster4 <- calib_5[calib_5$K4_lbd_5 == "4", ]  # df for 'cluster 4'

##### PCA grouped by supervised cluster K=4, lambda = 6 #######
# Data frame of principal components ----------------------
calib_6 <- data.frame(pc_calib$x, K4_lbd_6=unsup_calib$K4_lbd_6)  # dataframe of principal components
calib_6_cluster1 <- calib_2[calib_6$K4_lbd_6 == "1", ]  # df for 'cluster 1'
calib_6_cluster2 <- calib_2[calib_6$K4_lbd_6 == "2", ]  # df for 'cluster 2'
calib_6_cluster3 <- calib_2[calib_6$K4_lbd_6 == "3", ]  # df for 'cluster 3'
calib_6_cluster4 <- calib_2[calib_6$K4_lbd_6 == "4", ]  # df for 'cluster 4'

##### PCA grouped by supervised cluster K=4, lambda = 7 #######
# Data frame of principal components ----------------------
calib_7 <- data.frame(pc_calib$x, K4_lbd_7=unsup_calib$K4_lbd_7)  # dataframe of principal components
calib_7_cluster1 <- calib_7[calib_7$K4_lbd_7 == "1", ]  # df for 'cluster 1'
calib_7_cluster2 <- calib_7[calib_7$K4_lbd_7 == "2", ]  # df for 'cluster 2'
calib_7_cluster3 <- calib_7[calib_7$K4_lbd_7 == "3", ]  # df for 'cluster 3'
calib_7_cluster4 <- calib_7[calib_7$K4_lbd_7 == "4", ]  # df for 'cluster 4'

##### PCA grouped by supervised cluster K=4, lambda = 8 #######
# Data frame of principal components ----------------------
calib_8 <- data.frame(pc_calib$x, K4_lbd_8=unsup_calib$K4_lbd_8)  # dataframe of principal components
calib_8_cluster1 <- calib_8[calib_8$K4_lbd_8 == "1", ]  # df for 'cluster 1'
calib_8_cluster2 <- calib_8[calib_8$K4_lbd_8 == "2", ]  # df for 'cluster 2'
calib_8_cluster3 <- calib_8[calib_8$K4_lbd_8 == "3", ]  # df for 'cluster 3'
calib_8_cluster4 <- calib_8[calib_8$K4_lbd_8 == "4", ]  # df for 'cluster 4'

##### PCA grouped by supervised cluster K=4, lambda = 9 #######
# Data frame of principal components ----------------------
calib_9 <- data.frame(pc_calib$x, K4_lbd_9=unsup_calib$K4_lbd_9)  # dataframe of principal components
calib_9_cluster1 <- calib_9[calib_9$K4_lbd_9 == "1", ]  # df for 'cluster 1'
calib_9_cluster2 <- calib_9[calib_9$K4_lbd_9 == "2", ]  # df for 'cluster 2'
calib_9_cluster3 <- calib_9[calib_9$K4_lbd_9 == "3", ]  # df for 'cluster 3'
calib_9_cluster4 <- calib_9[calib_9$K4_lbd_9 == "4", ]  # df for 'cluster 4'

##### PCA grouped by supervised cluster K=4, lambda = 10 #######
# Data frame of principal components ----------------------
calib_10 <- data.frame(pc_calib$x, K4_lbd_10=unsup_calib$K4_lbd_10)  # dataframe of principal components
calib_10_cluster1 <- calib_10[calib_10$K4_lbd_10 == "1", ]  # df for 'cluster 1'
calib_10_cluster2 <- calib_10[calib_10$K4_lbd_10 == "2", ]  # df for 'cluster 2'
calib_10_cluster3 <- calib_10[calib_10$K4_lbd_10 == "3", ]  # df for 'cluster 3'
calib_10_cluster4 <- calib_10[calib_10$K4_lbd_10 == "4", ]  # df for 'cluster 4'

##### PCA grouped by supervised cluster K=4, lambda = 11 #######
# Data frame of principal components ----------------------
calib_11 <- data.frame(pc_calib$x, K4_lbd_11=unsup_calib$K4_lbd_11)  # dataframe of principal components
calib_11_cluster1 <- calib_11[calib_11$K4_lbd_11 == "1", ]  # df for 'cluster 1'
calib_11_cluster2 <- calib_11[calib_11$K4_lbd_11 == "2", ]  # df for 'cluster 2'
calib_11_cluster3 <- calib_11[calib_11$K4_lbd_11 == "3", ]  # df for 'cluster 3'
calib_11_cluster4 <- calib_11[calib_11$K4_lbd_11 == "4", ]  # df for 'cluster 4'

##### PCA grouped by supervised cluster K=4, lambda = 12 #######
# Data frame of principal components ----------------------
calib_12 <- data.frame(pc_calib$x, K4_lbd_12=unsup_calib$K4_lbd_12)  # dataframe of principal components
calib_12_cluster1 <- calib_12[calib_12$K4_lbd_12 == "1", ]  # df for 'cluster 1'
calib_12_cluster2 <- calib_12[calib_12$K4_lbd_12 == "2", ]  # df for 'cluster 2'
calib_12_cluster3 <- calib_12[calib_12$K4_lbd_12 == "3", ]  # df for 'cluster 3'
calib_12_cluster4 <- calib_12[calib_12$K4_lbd_12 == "4", ]  # df for 'cluster 4'

##### PCA grouped by supervised cluster K=4, lambda = 13 #######
# Data frame of principal components ----------------------
calib_13 <- data.frame(pc_calib$x, K4_lbd_13=unsup_calib$K4_lbd_13)  # dataframe of principal components
calib_13_cluster1 <- calib_13[calib_13$K4_lbd_13 == "1", ]  # df for 'cluster 1'
calib_13_cluster2 <- calib_13[calib_13$K4_lbd_13 == "2", ]  # df for 'cluster 2'
calib_13_cluster3 <- calib_13[calib_13$K4_lbd_13 == "3", ]  # df for 'cluster 3'
calib_13_cluster4 <- calib_13[calib_13$K4_lbd_13 == "4", ]  # df for 'cluster 4'

##### PCA grouped by supervised cluster K=4, lambda = 14 #######
# Data frame of principal components ----------------------
calib_14 <- data.frame(pc_calib$x, K4_lbd_14=unsup_calib$K4_lbd_14)  # dataframe of principal components
calib_14_cluster1 <- calib_14[calib_14$K4_lbd_14 == "1", ]  # df for 'cluster 1'
calib_14_cluster2 <- calib_14[calib_14$K4_lbd_14 == "2", ]  # df for 'cluster 2'
calib_14_cluster3 <- calib_14[calib_14$K4_lbd_14 == "3", ]  # df for 'cluster 3'
calib_14_cluster4 <- calib_14[calib_14$K4_lbd_14 == "4", ]  # df for 'cluster 4'

##### PCA grouped by supervised cluster K=4, lambda = 15 #######
# Data frame of principal components ----------------------
calib_15 <- data.frame(pc_calib$x, K4_lbd_15=unsup_calib$K4_lbd_15)  # dataframe of principal components
calib_15_cluster1 <- calib_15[calib_15$K4_lbd_15 == "1", ]  # df for 'cluster 1'
calib_15_cluster2 <- calib_15[calib_15$K4_lbd_15 == "2", ]  # df for 'cluster 2'
calib_15_cluster3 <- calib_15[calib_15$K4_lbd_15 == "3", ]  # df for 'cluster 3'
calib_15_cluster4 <- calib_15[calib_15$K4_lbd_15 == "4", ]  # df for 'cluster 4'

##### PCA grouped by supervised cluster K=4, lambda = 25 #######
# Data frame of principal components ----------------------
calib_25 <- data.frame(pc_calib$x, K4_lbd_25=unsup_calib$K4_lbd_25)  # dataframe of principal components
calib_25_cluster1 <- calib_25[calib_25$K4_lbd_25 == "1", ]  # df for 'cluster 1'
calib_25_cluster2 <- calib_25[calib_25$K4_lbd_25 == "2", ]  # df for 'cluster 2'
calib_25_cluster3 <- calib_25[calib_25$K4_lbd_25 == "3", ]  # df for 'cluster 3'
calib_25_cluster4 <- calib_25[calib_25$K4_lbd_25 == "4", ]  # df for 'cluster 4'

# Plot ----------------------------------------------------
plt_cal01 <- ggplot(calib_01, aes(PC1, PC2, col=K4_lbd_0.1)) + 
  geom_point(aes(shape=K4_lbd_0.1), size=2) +   # draw points
  labs(title="Unsupervised K-proto - 610 Observations grouped in 4 clusters with lambda = 0.1", 
       subtitle="With principal components PC1 and PC2 as X and Y axis",
       caption="Source: UBIOPRED") + 
  coord_cartesian(xlim = 1.2 * c(min(calib_01$PC1), max(calib_01$PC1)), 
                  ylim = 1.2 * c(min(calib_01$PC2), max(calib_01$PC2))) +   # change axis limits
  geom_encircle(data = calib_01_cluster1, aes(x=PC1, y=PC2)) +   # draw circles
  geom_encircle(data = calib_01_cluster2, aes(x=PC1, y=PC2)) + 
  geom_encircle(data = calib_01_cluster3, aes(x=PC1, y=PC2)) +
  geom_encircle(data = calib_01_cluster4, aes(x=PC1, y=PC2))

plt_calib_01 <- print(plt_cal01 + labs(colour = "Clusters") + labs(shape="Clusters"))

# Plot ----------------------------------------------------
plt_cal02 <- ggplot(calib_02, aes(PC1, PC2, col=K4_lbd_0.2)) + 
  geom_point(aes(shape=K4_lbd_0.2), size=2) +   # draw points
  labs(title="Unsupervised K-proto - 610 Observations grouped in 4 clusters with lambda = 0.2", 
       subtitle="With principal components PC1 and PC2 as X and Y axis",
       caption="Source: UBIOPRED") + 
  coord_cartesian(xlim = 1.2 * c(min(calib_02$PC1), max(calib_02$PC1)), 
                  ylim = 1.2 * c(min(calib_02$PC2), max(calib_02$PC2))) +   # change axis limits
  geom_encircle(data = calib_02_cluster1, aes(x=PC1, y=PC2)) +   # draw circles
  geom_encircle(data = calib_02_cluster2, aes(x=PC1, y=PC2)) + 
  geom_encircle(data = calib_02_cluster3, aes(x=PC1, y=PC2)) +
  geom_encircle(data = calib_02_cluster4, aes(x=PC1, y=PC2))

plt_calib_02 <- print(plt_cal02 + labs(colour = "Clusters") + labs(shape="Clusters"))

# Plot ----------------------------------------------------
plt_cal03 <- ggplot(calib_03, aes(PC1, PC2, col=K4_lbd_0.3)) + 
  geom_point(aes(shape=K4_lbd_0.3), size=2) +   # draw points
  labs(title="Unsupervised K-proto - 610 Observations grouped in 4 clusters with lambda = 0.3", 
       subtitle="With principal components PC1 and PC2 as X and Y axis",
       caption="Source: UBIOPRED") + 
  coord_cartesian(xlim = 1.2 * c(min(calib_03$PC1), max(calib_03$PC1)), 
                  ylim = 1.2 * c(min(calib_03$PC2), max(calib_03$PC2))) +   # change axis limits
  geom_encircle(data = calib_03_cluster1, aes(x=PC1, y=PC2)) +   # draw circles
  geom_encircle(data = calib_03_cluster2, aes(x=PC1, y=PC2)) + 
  geom_encircle(data = calib_03_cluster3, aes(x=PC1, y=PC2)) +
  geom_encircle(data = calib_03_cluster4, aes(x=PC1, y=PC2))

plt_calib_03 <- print(plt_cal03 + labs(colour = "Clusters") + labs(shape="Clusters"))

# Plot ----------------------------------------------------
plt_cal04 <- ggplot(calib_04, aes(PC1, PC2, col=K4_lbd_0.4)) + 
  geom_point(aes(shape=K4_lbd_0.4), size=2) +   # draw points
  labs(title="Unsupervised K-proto - 610 Observations grouped in 4 clusters with lambda = 0.4", 
       subtitle="With principal components PC1 and PC2 as X and Y axis",
       caption="Source: UBIOPRED") + 
  coord_cartesian(xlim = 1.2 * c(min(calib_04$PC1), max(calib_04$PC1)), 
                  ylim = 1.2 * c(min(calib_04$PC2), max(calib_04$PC2))) +   # change axis limits
  geom_encircle(data = calib_04_cluster1, aes(x=PC1, y=PC2)) +   # draw circles
  geom_encircle(data = calib_04_cluster2, aes(x=PC1, y=PC2)) + 
  geom_encircle(data = calib_04_cluster3, aes(x=PC1, y=PC2)) +
  geom_encircle(data = calib_04_cluster4, aes(x=PC1, y=PC2))

plt_calib_04 <- print(plt_cal04 + labs(colour = "Clusters") + labs(shape="Clusters"))

# Plot ----------------------------------------------------
plt_cal05 <- ggplot(calib_05, aes(PC1, PC2, col=K4_lbd_0.5)) + 
  geom_point(aes(shape=K4_lbd_0.5), size=2) +   # draw points
  labs(title="Unsupervised K-proto - 610 Observations grouped in 4 clusters with lambda = 0.5", 
       subtitle="With principal components PC1 and PC2 as X and Y axis",
       caption="Source: UBIOPRED") + 
  coord_cartesian(xlim = 1.2 * c(min(calib_05$PC1), max(calib_05$PC1)), 
                  ylim = 1.2 * c(min(calib_05$PC2), max(calib_05$PC2))) +   # change axis limits
  geom_encircle(data = calib_05_cluster1, aes(x=PC1, y=PC2)) +   # draw circles
  geom_encircle(data = calib_05_cluster2, aes(x=PC1, y=PC2)) + 
  geom_encircle(data = calib_05_cluster3, aes(x=PC1, y=PC2)) +
  geom_encircle(data = calib_05_cluster4, aes(x=PC1, y=PC2))

plt_calib_05 <- print(plt_cal05 + labs(colour = "Clusters") + labs(shape="Clusters"))

# Plot ----------------------------------------------------
plt_cal06 <- ggplot(calib_06, aes(PC1, PC2, col=K4_lbd_0.6)) + 
  geom_point(aes(shape=K4_lbd_0.6), size=2) +   # draw points
  labs(title="Unsupervised K-proto - 610 Observations grouped in 4 clusters with lambda = 0.6", 
       subtitle="With principal components PC1 and PC2 as X and Y axis",
       caption="Source: UBIOPRED") + 
  coord_cartesian(xlim = 1.2 * c(min(calib_06$PC1), max(calib_06$PC1)), 
                  ylim = 1.2 * c(min(calib_06$PC2), max(calib_06$PC2))) +   # change axis limits
  geom_encircle(data = calib_06_cluster1, aes(x=PC1, y=PC2)) +   # draw circles
  geom_encircle(data = calib_06_cluster2, aes(x=PC1, y=PC2)) + 
  geom_encircle(data = calib_06_cluster3, aes(x=PC1, y=PC2)) +
  geom_encircle(data = calib_06_cluster4, aes(x=PC1, y=PC2))

plt_calib_06 <- print(plt_cal06 + labs(colour = "Clusters") + labs(shape="Clusters"))

# Plot ----------------------------------------------------
plt_cal07 <- ggplot(calib_07, aes(PC1, PC2, col=K4_lbd_0.7)) + 
  geom_point(aes(shape=K4_lbd_0.7), size=2) +   # draw points
  labs(title="Unsupervised K-proto - 610 Observations grouped in 4 clusters with lambda = 0.7", 
       subtitle="With principal components PC1 and PC2 as X and Y axis",
       caption="Source: UBIOPRED") + 
  coord_cartesian(xlim = 1.2 * c(min(calib_07$PC1), max(calib_07$PC1)), 
                  ylim = 1.2 * c(min(calib_07$PC2), max(calib_07$PC2))) +   # change axis limits
  geom_encircle(data = calib_07_cluster1, aes(x=PC1, y=PC2)) +   # draw circles
  geom_encircle(data = calib_07_cluster2, aes(x=PC1, y=PC2)) + 
  geom_encircle(data = calib_07_cluster3, aes(x=PC1, y=PC2)) +
  geom_encircle(data = calib_07_cluster4, aes(x=PC1, y=PC2))

plt_calib_07 <- print(plt_cal07 + labs(colour = "Clusters") + labs(shape="Clusters"))

# Plot ----------------------------------------------------
plt_cal08 <- ggplot(calib_08, aes(PC1, PC2, col=K4_lbd_0.8)) + 
  geom_point(aes(shape=K4_lbd_0.8), size=2) +   # draw points
  labs(title="Unsupervised K-proto - 610 Observations grouped in 4 clusters with lambda = 0.8", 
       subtitle="With principal components PC1 and PC2 as X and Y axis",
       caption="Source: UBIOPRED") + 
  coord_cartesian(xlim = 1.2 * c(min(calib_08$PC1), max(calib_08$PC1)), 
                  ylim = 1.2 * c(min(calib_08$PC2), max(calib_08$PC2))) +   # change axis limits
  geom_encircle(data = calib_08_cluster1, aes(x=PC1, y=PC2)) +   # draw circles
  geom_encircle(data = calib_08_cluster2, aes(x=PC1, y=PC2)) + 
  geom_encircle(data = calib_08_cluster3, aes(x=PC1, y=PC2)) +
  geom_encircle(data = calib_08_cluster4, aes(x=PC1, y=PC2))

plt_calib_08 <- print(plt_cal08 + labs(colour = "Clusters") + labs(shape="Clusters"))

# Plot ----------------------------------------------------
plt_cal09 <- ggplot(calib_09, aes(PC1, PC2, col=K4_lbd_0.9)) + 
  geom_point(aes(shape=K4_lbd_0.9), size=2) +   # draw points
  labs(title="Unsupervised K-proto - 610 Observations grouped in 4 clusters with lambda = 0.9", 
       subtitle="With principal components PC1 and PC2 as X and Y axis",
       caption="Source: UBIOPRED") + 
  coord_cartesian(xlim = 1.2 * c(min(calib_09$PC1), max(calib_09$PC1)), 
                  ylim = 1.2 * c(min(calib_09$PC2), max(calib_09$PC2))) +   # change axis limits
  geom_encircle(data = calib_09_cluster1, aes(x=PC1, y=PC2)) +   # draw circles
  geom_encircle(data = calib_09_cluster2, aes(x=PC1, y=PC2)) + 
  geom_encircle(data = calib_09_cluster3, aes(x=PC1, y=PC2)) +
  geom_encircle(data = calib_09_cluster4, aes(x=PC1, y=PC2))

plt_calib_09 <- print(plt_cal09 + labs(colour = "Clusters") + labs(shape="Clusters"))

# Plot ----------------------------------------------------
plt_cal1 <- ggplot(calib_1, aes(PC1, PC2, col=K4_lbd_1)) + 
  geom_point(aes(shape=K4_lbd_1), size=2) +   # draw points
  labs(title="Unsupervised K-proto - 610 Observations grouped in 4 clusters with lambda = 1", 
       subtitle="With principal components PC1 and PC2 as X and Y axis",
       caption="Source: UBIOPRED") + 
  coord_cartesian(xlim = 1.2 * c(min(calib_1$PC1), max(calib_1$PC1)), 
                  ylim = 1.2 * c(min(calib_1$PC2), max(calib_1$PC2))) +   # change axis limits
  geom_encircle(data = calib_1_cluster1, aes(x=PC1, y=PC2)) +   # draw circles
  geom_encircle(data = calib_1_cluster2, aes(x=PC1, y=PC2)) + 
  geom_encircle(data = calib_1_cluster3, aes(x=PC1, y=PC2)) +
  geom_encircle(data = calib_1_cluster4, aes(x=PC1, y=PC2))

plt_calib_1 <- print(plt_cal1 + labs(colour = "Clusters") + labs(shape="Clusters"))

# Plot ----------------------------------------------------
plt_cal2 <- ggplot(calib_2, aes(PC1, PC2, col=K4_lbd_2)) + 
  geom_point(aes(shape=K4_lbd_2), size=2) +   # draw points
  labs(title="Unsupervised K-proto - 610 Observations grouped in 4 clusters with lambda = 2", 
       subtitle="With principal components PC1 and PC2 as X and Y axis",
       caption="Source: UBIOPRED") + 
  coord_cartesian(xlim = 1.2 * c(min(calib_2$PC1), max(calib_2$PC1)), 
                  ylim = 1.2 * c(min(calib_2$PC2), max(calib_2$PC2))) +   # change axis limits
  geom_encircle(data = calib_2_cluster1, aes(x=PC1, y=PC2)) +   # draw circles
  geom_encircle(data = calib_2_cluster2, aes(x=PC1, y=PC2)) + 
  geom_encircle(data = calib_2_cluster3, aes(x=PC1, y=PC2)) +
  geom_encircle(data = calib_2_cluster4, aes(x=PC1, y=PC2))

plt_calib_2 <- print(plt_cal2 + labs(colour = "Clusters") + labs(shape="Clusters"))

# Plot ----------------------------------------------------
plt_cal3 <- ggplot(calib_3, aes(PC1, PC2, col=K4_lbd_3)) + 
  geom_point(aes(shape=K4_lbd_3), size=2) +   # draw points
  labs(title="Unsupervised K-proto - 610 Observations grouped in 4 clusters with lambda = 3", 
       subtitle="With principal components PC1 and PC2 as X and Y axis",
       caption="Source: UBIOPRED") + 
  coord_cartesian(xlim = 1.2 * c(min(calib_3$PC1), max(calib_3$PC1)), 
                  ylim = 1.2 * c(min(calib_3$PC2), max(calib_3$PC2))) +   # change axis limits
  geom_encircle(data = calib_3_cluster1, aes(x=PC1, y=PC2)) +   # draw circles
  geom_encircle(data = calib_3_cluster2, aes(x=PC1, y=PC2)) + 
  geom_encircle(data = calib_3_cluster3, aes(x=PC1, y=PC2)) +
  geom_encircle(data = calib_3_cluster4, aes(x=PC1, y=PC2))

plt_calib_3 <- print(plt_cal3 + labs(colour = "Clusters") + labs(shape="Clusters"))

# Plot ----------------------------------------------------
plt_cal4 <- ggplot(calib_4, aes(PC1, PC2, col=K4_lbd_4)) + 
  geom_point(aes(shape=K4_lbd_4), size=2) +   # draw points
  labs(title="Unsupervised K-proto - 610 Observations grouped in 4 clusters with lambda = 4", 
       subtitle="With principal components PC1 and PC2 as X and Y axis",
       caption="Source: UBIOPRED") + 
  coord_cartesian(xlim = 1.2 * c(min(calib_4$PC1), max(calib_4$PC1)), 
                  ylim = 1.2 * c(min(calib_4$PC2), max(calib_4$PC2))) +   # change axis limits
  geom_encircle(data = calib_4_cluster1, aes(x=PC1, y=PC2)) +   # draw circles
  geom_encircle(data = calib_4_cluster2, aes(x=PC1, y=PC2)) + 
  geom_encircle(data = calib_4_cluster3, aes(x=PC1, y=PC2)) +
  geom_encircle(data = calib_4_cluster4, aes(x=PC1, y=PC2))

plt_calib_4 <- print(plt_cal4 + labs(colour = "Clusters") + labs(shape="Clusters"))

# Plot ----------------------------------------------------
plt_cal1 <- ggplot(calib_1, aes(PC1, PC2, col=K4_lbd_1)) + 
  geom_point(aes(shape=K4_lbd_1), size=2) +   # draw points
  labs(title="Unsupervised K-proto - 610 Observations grouped in 4 clusters with lambda = 1", 
       subtitle="With principal components PC1 and PC2 as X and Y axis",
       caption="Source: UBIOPRED") + 
  coord_cartesian(xlim = 1.2 * c(min(calib_1$PC1), max(calib_1$PC1)), 
                  ylim = 1.2 * c(min(calib_1$PC2), max(calib_1$PC2))) +   # change axis limits
  geom_encircle(data = calib_1_cluster1, aes(x=PC1, y=PC2)) +   # draw circles
  geom_encircle(data = calib_1_cluster2, aes(x=PC1, y=PC2)) + 
  geom_encircle(data = calib_1_cluster3, aes(x=PC1, y=PC2)) +
  geom_encircle(data = calib_1_cluster4, aes(x=PC1, y=PC2))

plt_calib_1 <- print(plt_cal1 + labs(colour = "Clusters") + labs(shape="Clusters"))

# Plot ----------------------------------------------------
plt_cal25 <- ggplot(calib_25, aes(PC1, PC2, col=K4_lbd_25)) + 
  geom_point(aes(shape=K4_lbd_25), size=2) +   # draw points
  labs(title="Unsupervised K-proto - 610 Observations grouped in 4 clusters with lambda = 25", 
       subtitle="With principal components PC1 and PC2 as X and Y axis",
       caption="Source: UBIOPRED") + 
  coord_cartesian(xlim = 1.2 * c(min(calib_25$PC1), max(calib_25$PC1)), 
                  ylim = 1.2 * c(min(calib_25$PC2), max(calib_25$PC2))) +   # change axis limits
  geom_encircle(data = calib_25_cluster1, aes(x=PC1, y=PC2)) +   # draw circles
  geom_encircle(data = calib_25_cluster2, aes(x=PC1, y=PC2)) + 
  geom_encircle(data = calib_25_cluster3, aes(x=PC1, y=PC2)) +
  geom_encircle(data = calib_25_cluster4, aes(x=PC1, y=PC2))

plt_calib_25 <- print(plt_cal25 + labs(colour = "Clusters") + labs(shape="Clusters"))


#Test 3D 

