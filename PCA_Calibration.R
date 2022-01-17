##### PCA VISUALIZATION #####
#Visualization clusters according to lambda calibration

#Clear environment
rm(list=ls())

#Reading the files
unsup_df_proc = read.csv("/rds/general/user/md2620/home/asthma/Malo/Dataframes/Final/unsup_df_all_processed.csv")
unsup_calib = read.csv("/rds/general/user/md2620/home/asthma/Malo/Dataframes/Final/unsup_df_calib_lambda_original.csv")
#devtools::install_github("hrbrmstr/ggalt")
library(ggplot2)
library(ggalt)
#install.packages("ggfortify")
library(ggfortify)
theme_set(theme_classic())

#Drop 1st column 
unsup_df_proc = subset(unsup_df_proc, select = -c(X))

#Drop zero variance variables
unsup_df_proc = subset(unsup_df_proc, select = -c(Omics.Xylose,Omics.Xanthosine,Omics.Furoylglycine,Omics.Allantoin,
                                                            Omics.Cytosine,Omics.Glucosamine,Omics.Glutamic.acid,
                                                            Omics.Isoleucine,Omics.Lysine,Omics.Maltose,
                                                            Omics.N.Acetylglutamic.acid,Omics.N.Methyl.D.aspartic.acid,Omics.O.Acetylserine,
                                                            Omics.Phenylalanine,Omics.Sarcosine,Omics.N.Acetylputrescine))

# Compute data with principal components ------------------
PCA_calib <- unsup_df_proc[c(1:610),c(1:346)]

#Set Subject.ID as index
PCA_calibration <- PCA_calib[-1]
row.names(PCA_calibration) <- PCA_calib$Unnamed..0
View(PCA_calibration)

pc_calib <- prcomp(PCA_calibration)  # compute principal components
summary(pc_calib)
#PC1 : 7.617%
#PC2 : 4.317%

#Make sure clusters variables are factor variables
unsup_calib$K3_lbd_0.1 = as.factor(unsup_calib$K3_lbd_0.1)
unsup_calib$K3_lbd_0.2 = as.factor(unsup_calib$K3_lbd_0.2)
unsup_calib$K3_lbd_0.3 = as.factor(unsup_calib$K3_lbd_0.3)
unsup_calib$K3_lbd_0.4 = as.factor(unsup_calib$K3_lbd_0.4)
unsup_calib$K3_lbd_0.5 = as.factor(unsup_calib$K3_lbd_0.5)
unsup_calib$K3_lbd_0.6 = as.factor(unsup_calib$K3_lbd_0.6)
unsup_calib$K3_lbd_0.7 = as.factor(unsup_calib$K3_lbd_0.7)
unsup_calib$K3_lbd_0.8 = as.factor(unsup_calib$K3_lbd_0.8)
unsup_calib$K3_lbd_0.9 = as.factor(unsup_calib$K3_lbd_0.9)
unsup_calib$K3_lbd_1 = as.factor(unsup_calib$K3_lbd_1)
unsup_calib$K3_lbd_2 = as.factor(unsup_calib$K3_lbd_2)
unsup_calib$K3_lbd_3 = as.factor(unsup_calib$K3_lbd_3)
unsup_calib$K3_lbd_3.1 = as.factor(unsup_calib$K3_lbd_3.1)
unsup_calib$K3_lbd_4 = as.factor(unsup_calib$K3_lbd_4)
unsup_calib$K3_lbd_5 = as.factor(unsup_calib$K3_lbd_5)
unsup_calib$K3_lbd_6 = as.factor(unsup_calib$K3_lbd_6)
unsup_calib$K3_lbd_7 = as.factor(unsup_calib$K3_lbd_7)
unsup_calib$K3_lbd_8 = as.factor(unsup_calib$K3_lbd_8)
unsup_calib$K3_lbd_9 = as.factor(unsup_calib$K3_lbd_9)
unsup_calib$K3_lbd_10 = as.factor(unsup_calib$K3_lbd_10)
unsup_calib$K3_lbd_11 = as.factor(unsup_calib$K3_lbd_11)
unsup_calib$K3_lbd_12 = as.factor(unsup_calib$K3_lbd_12)
unsup_calib$K3_lbd_13 = as.factor(unsup_calib$K3_lbd_13)
unsup_calib$K3_lbd_14 = as.factor(unsup_calib$K3_lbd_14)
unsup_calib$K3_lbd_15 = as.factor(unsup_calib$K3_lbd_15)
unsup_calib$K3_lbd_25 = as.factor(unsup_calib$K3_lbd_25)
unsup_calib$K3_default_lbd = as.factor(unsup_calib$K3_default_lbd)
unsup_calib$K3_heuristics_2_lbd = as.factor(unsup_calib$K3_heuristics_2_lbd)

#Check levels of calibrated cluster variables 
levels(unsup_calib$K3_lbd_0.1)

##### PCA grouped by unsupervised cluster K=3, lambda = 0.1 #######
# Data frame of principal components ----------------------
calib_01 <- data.frame(pc_calib$x, K3_lbd_0.1=unsup_calib$K3_lbd_0.1)  # dataframe of principal components
calib_01_cluster1 <- calib_01[calib_01$K3_lbd_0.1 == "1", ]  # df for 'cluster 1'
calib_01_cluster2 <- calib_01[calib_01$K3_lbd_0.1 == "2", ]  # df for 'cluster 2'
calib_01_cluster3 <- calib_01[calib_01$K3_lbd_0.1 == "3", ]  # df for 'cluster 3'


##### PCA grouped by unsupervised cluster K=3, lambda = 0.2 #######
# Data frame of principal components ----------------------
calib_02 <- data.frame(pc_calib$x, K3_lbd_0.2=unsup_calib$K3_lbd_0.2)  # dataframe of principal components
calib_02_cluster1 <- calib_02[calib_02$K3_lbd_0.2 == "1", ]  # df for 'cluster 1'
calib_02_cluster2 <- calib_02[calib_02$K3_lbd_0.2 == "2", ]  # df for 'cluster 2'
calib_02_cluster3 <- calib_02[calib_02$K3_lbd_0.2 == "3", ]  # df for 'cluster 3'


##### PCA grouped by unsupervised cluster K=3, lambda = 0.3 #######
# Data frame of principal components ----------------------
calib_03 <- data.frame(pc_calib$x, K3_lbd_0.3=unsup_calib$K3_lbd_0.3)  # dataframe of principal components
calib_03_cluster1 <- calib_03[calib_03$K3_lbd_0.3 == "1", ]  # df for 'cluster 1'
calib_03_cluster2 <- calib_03[calib_03$K3_lbd_0.3 == "2", ]  # df for 'cluster 2'
calib_03_cluster3 <- calib_03[calib_03$K3_lbd_0.3 == "3", ]  # df for 'cluster 3'


##### PCA grouped by unsupervised cluster K=3, lambda = 0.4 #######
# Data frame of principal components ----------------------
calib_04 <- data.frame(pc_calib$x, K3_lbd_0.4=unsup_calib$K3_lbd_0.4)  # dataframe of principal components
calib_04_cluster1 <- calib_04[calib_04$K3_lbd_0.4 == "1", ]  # df for 'cluster 1'
calib_04_cluster2 <- calib_04[calib_04$K3_lbd_0.4 == "2", ]  # df for 'cluster 2'
calib_04_cluster3 <- calib_04[calib_04$K3_lbd_0.4 == "3", ]  # df for 'cluster 3'


##### PCA grouped by unsupervised cluster K=3, lambda = 0.5 #######
# Data frame of principal components ----------------------
calib_05 <- data.frame(pc_calib$x, K3_lbd_0.5=unsup_calib$K3_lbd_0.5)  # dataframe of principal components
calib_05_cluster1 <- calib_05[calib_05$K3_lbd_0.5 == "1", ]  # df for 'cluster 1'
calib_05_cluster2 <- calib_05[calib_05$K3_lbd_0.5 == "2", ]  # df for 'cluster 2'
calib_05_cluster3 <- calib_05[calib_05$K3_lbd_0.5 == "3", ]  # df for 'cluster 3'


##### PCA grouped by unsupervised cluster K=3, lambda = 0.6 #######
# Data frame of principal components ----------------------
calib_06 <- data.frame(pc_calib$x, K3_lbd_0.6=unsup_calib$K3_lbd_0.6)  # dataframe of principal components
calib_06_cluster1 <- calib_06[calib_06$K3_lbd_0.6 == "1", ]  # df for 'cluster 1'
calib_06_cluster2 <- calib_06[calib_06$K3_lbd_0.6 == "2", ]  # df for 'cluster 2'
calib_06_cluster3 <- calib_06[calib_06$K3_lbd_0.6 == "3", ]  # df for 'cluster 3'


##### PCA grouped by unsupervised cluster K=3, lambda = 0.7 #######
# Data frame of principal components ----------------------
calib_07 <- data.frame(pc_calib$x, K3_lbd_0.7=unsup_calib$K3_lbd_0.7)  # dataframe of principal components
calib_07_cluster1 <- calib_07[calib_07$K3_lbd_0.7 == "1", ]  # df for 'cluster 1'
calib_07_cluster2 <- calib_07[calib_07$K3_lbd_0.7 == "2", ]  # df for 'cluster 2'
calib_07_cluster3 <- calib_07[calib_07$K3_lbd_0.7 == "3", ]  # df for 'cluster 3'

##### PCA grouped by unsupervised cluster K=3, lambda = 0.8 #######
# Data frame of principal components ----------------------
calib_08 <- data.frame(pc_calib$x, K3_lbd_0.8=unsup_calib$K3_lbd_0.8)  # dataframe of principal components
calib_08_cluster1 <- calib_08[calib_08$K3_lbd_0.8 == "1", ]  # df for 'cluster 1'
calib_08_cluster2 <- calib_08[calib_08$K3_lbd_0.8 == "2", ]  # df for 'cluster 2'
calib_08_cluster3 <- calib_08[calib_08$K3_lbd_0.8 == "3", ]  # df for 'cluster 3'


##### PCA grouped by unsupervised cluster K=3, lambda = 0.9 #######
# Data frame of principal components ----------------------
calib_09 <- data.frame(pc_calib$x, K3_lbd_0.9=unsup_calib$K3_lbd_0.9)  # dataframe of principal components
calib_09_cluster1 <- calib_09[calib_09$K3_lbd_0.9 == "1", ]  # df for 'cluster 1'
calib_09_cluster2 <- calib_09[calib_09$K3_lbd_0.9 == "2", ]  # df for 'cluster 2'
calib_09_cluster3 <- calib_09[calib_09$K3_lbd_0.9 == "3", ]  # df for 'cluster 3'


##### PCA grouped by unsupervised cluster K=3, lambda = 1 #######
# Data frame of principal components ----------------------
calib_1 <- data.frame(pc_calib$x, K3_lbd_1=unsup_calib$K3_lbd_1)  # dataframe of principal components
calib_1_cluster1 <- calib_1[calib_1$K3_lbd_1 == "1", ]  # df for 'cluster 1'
calib_1_cluster2 <- calib_1[calib_1$K3_lbd_1 == "2", ]  # df for 'cluster 2'
calib_1_cluster3 <- calib_1[calib_1$K3_lbd_1 == "3", ]  # df for 'cluster 3'


##### PCA grouped by unsupervised cluster K=3, lambda = 2 #######
# Data frame of principal components ----------------------
calib_2 <- data.frame(pc_calib$x, K3_lbd_2=unsup_calib$K3_lbd_2)  # dataframe of principal components
calib_2_cluster1 <- calib_2[calib_2$K3_lbd_2 == "1", ]  # df for 'cluster 1'
calib_2_cluster2 <- calib_2[calib_2$K3_lbd_2 == "2", ]  # df for 'cluster 2'
calib_2_cluster3 <- calib_2[calib_2$K3_lbd_2 == "3", ]  # df for 'cluster 3'


##### PCA grouped by unsupervised cluster K=3, lambda = 3 #######
# Data frame of principal components ----------------------
calib_3 <- data.frame(pc_calib$x, K3_lbd_3=unsup_calib$K3_lbd_3)  # dataframe of principal components
calib_3_cluster1 <- calib_3[calib_3$K3_lbd_3 == "1", ]  # df for 'cluster 1'
calib_3_cluster2 <- calib_3[calib_3$K3_lbd_3 == "2", ]  # df for 'cluster 2'
calib_3_cluster3 <- calib_3[calib_3$K3_lbd_3 == "3", ]  # df for 'cluster 3'

##### PCA grouped by unsupervised cluster K=3, lambda = 3.1 #######
# Data frame of principal components ----------------------
calib_3.1 <- data.frame(pc_calib$x, K3_lbd_3.1=unsup_calib$K3_lbd_3.1)  # dataframe of principal components
calib_3.1_cluster1 <- calib_3.1[calib_3.1$K3_lbd_3.1 == "1", ]  # df for 'cluster 1'
calib_3.1_cluster2 <- calib_3.1[calib_3.1$K3_lbd_3.1 == "2", ]  # df for 'cluster 2'
calib_3.1_cluster3 <- calib_3.1[calib_3.1$K3_lbd_3.1 == "3", ]  # df for 'cluster 3'

##### PCA grouped by unsupervised cluster K=3, lambda = 4 #######
# Data frame of principal components ----------------------
calib_4 <- data.frame(pc_calib$x, K3_lbd_4=unsup_calib$K3_lbd_4)  # dataframe of principal components
calib_4_cluster1 <- calib_4[calib_4$K3_lbd_4 == "1", ]  # df for 'cluster 1'
calib_4_cluster2 <- calib_4[calib_4$K3_lbd_4 == "2", ]  # df for 'cluster 2'
calib_4_cluster3 <- calib_4[calib_4$K3_lbd_4 == "3", ]  # df for 'cluster 3'


##### PCA grouped by unsupervised cluster K=3, lambda = 5 #######
# Data frame of principal components ----------------------
calib_5 <- data.frame(pc_calib$x, K3_lbd_5=unsup_calib$K3_lbd_5)  # dataframe of principal components
calib_5_cluster1 <- calib_5[calib_5$K3_lbd_5 == "1", ]  # df for 'cluster 1'
calib_5_cluster2 <- calib_5[calib_5$K3_lbd_5 == "2", ]  # df for 'cluster 2'
calib_5_cluster3 <- calib_5[calib_5$K3_lbd_5 == "3", ]  # df for 'cluster 3'


##### PCA grouped by unsupervised cluster K=3, lambda = 6 #######
# Data frame of principal components ----------------------
calib_6 <- data.frame(pc_calib$x, K3_lbd_6=unsup_calib$K3_lbd_6)  # dataframe of principal components
calib_6_cluster1 <- calib_6[calib_6$K3_lbd_6 == "1", ]  # df for 'cluster 1'
calib_6_cluster2 <- calib_6[calib_6$K3_lbd_6 == "2", ]  # df for 'cluster 2'
calib_6_cluster3 <- calib_6[calib_6$K3_lbd_6 == "3", ]  # df for 'cluster 3'


##### PCA grouped by unsupervised cluster K=3, lambda = 7 #######
# Data frame of principal components ----------------------
calib_7 <- data.frame(pc_calib$x, K3_lbd_7=unsup_calib$K3_lbd_7)  # dataframe of principal components
calib_7_cluster1 <- calib_7[calib_7$K3_lbd_7 == "1", ]  # df for 'cluster 1'
calib_7_cluster2 <- calib_7[calib_7$K3_lbd_7 == "2", ]  # df for 'cluster 2'
calib_7_cluster3 <- calib_7[calib_7$K3_lbd_7 == "3", ]  # df for 'cluster 3'


##### PCA grouped by unsupervised cluster K=3, lambda = 8 #######
# Data frame of principal components ----------------------
calib_8 <- data.frame(pc_calib$x, K3_lbd_8=unsup_calib$K3_lbd_8)  # dataframe of principal components
calib_8_cluster1 <- calib_8[calib_8$K3_lbd_8 == "1", ]  # df for 'cluster 1'
calib_8_cluster2 <- calib_8[calib_8$K3_lbd_8 == "2", ]  # df for 'cluster 2'
calib_8_cluster3 <- calib_8[calib_8$K3_lbd_8 == "3", ]  # df for 'cluster 3'


##### PCA grouped by unsupervised cluster K=3, lambda = 9 #######
# Data frame of principal components ----------------------
calib_9 <- data.frame(pc_calib$x, K3_lbd_9=unsup_calib$K3_lbd_9)  # dataframe of principal components
calib_9_cluster1 <- calib_9[calib_9$K3_lbd_9 == "1", ]  # df for 'cluster 1'
calib_9_cluster2 <- calib_9[calib_9$K3_lbd_9 == "2", ]  # df for 'cluster 2'
calib_9_cluster3 <- calib_9[calib_9$K3_lbd_9 == "3", ]  # df for 'cluster 3'


##### PCA grouped by unsupervised cluster K=3, lambda = 10 #######
# Data frame of principal components ----------------------
calib_10 <- data.frame(pc_calib$x, K3_lbd_10=unsup_calib$K3_lbd_10)  # dataframe of principal components
calib_10_cluster1 <- calib_10[calib_10$K3_lbd_10 == "1", ]  # df for 'cluster 1'
calib_10_cluster2 <- calib_10[calib_10$K3_lbd_10 == "2", ]  # df for 'cluster 2'
calib_10_cluster3 <- calib_10[calib_10$K3_lbd_10 == "3", ]  # df for 'cluster 3'


##### PCA grouped by unsupervised cluster K=3, lambda = 11 #######
# Data frame of principal components ----------------------
calib_11 <- data.frame(pc_calib$x, K3_lbd_11=unsup_calib$K3_lbd_11)  # dataframe of principal components
calib_11_cluster1 <- calib_11[calib_11$K3_lbd_11 == "1", ]  # df for 'cluster 1'
calib_11_cluster2 <- calib_11[calib_11$K3_lbd_11 == "2", ]  # df for 'cluster 2'
calib_11_cluster3 <- calib_11[calib_11$K3_lbd_11 == "3", ]  # df for 'cluster 3'

##### PCA grouped by unsupervised cluster K=3, lambda = 12 #######
# Data frame of principal components ----------------------
calib_12 <- data.frame(pc_calib$x, K3_lbd_12=unsup_calib$K3_lbd_12)  # dataframe of principal components
calib_12_cluster1 <- calib_12[calib_12$K3_lbd_12 == "1", ]  # df for 'cluster 1'
calib_12_cluster2 <- calib_12[calib_12$K3_lbd_12 == "2", ]  # df for 'cluster 2'
calib_12_cluster3 <- calib_12[calib_12$K3_lbd_12 == "3", ]  # df for 'cluster 3'


##### PCA grouped by unsupervised cluster K=3, lambda = 13 #######
# Data frame of principal components ----------------------
calib_13 <- data.frame(pc_calib$x, K3_lbd_13=unsup_calib$K3_lbd_13)  # dataframe of principal components
calib_13_cluster1 <- calib_13[calib_13$K3_lbd_13 == "1", ]  # df for 'cluster 1'
calib_13_cluster2 <- calib_13[calib_13$K3_lbd_13 == "2", ]  # df for 'cluster 2'
calib_13_cluster3 <- calib_13[calib_13$K3_lbd_13 == "3", ]  # df for 'cluster 3'


##### PCA grouped by unsupervised cluster K=3, lambda = 14 #######
# Data frame of principal components ----------------------
calib_14 <- data.frame(pc_calib$x, K3_lbd_14=unsup_calib$K3_lbd_14)  # dataframe of principal components
calib_14_cluster1 <- calib_14[calib_14$K3_lbd_14 == "1", ]  # df for 'cluster 1'
calib_14_cluster2 <- calib_14[calib_14$K3_lbd_14 == "2", ]  # df for 'cluster 2'
calib_14_cluster3 <- calib_14[calib_14$K3_lbd_14 == "3", ]  # df for 'cluster 3'


##### PCA grouped by unsupervised cluster K=3, lambda = 15 #######
# Data frame of principal components ----------------------
calib_15 <- data.frame(pc_calib$x, K3_lbd_15=unsup_calib$K3_lbd_15)  # dataframe of principal components
calib_15_cluster1 <- calib_15[calib_15$K3_lbd_15 == "1", ]  # df for 'cluster 1'
calib_15_cluster2 <- calib_15[calib_15$K3_lbd_15 == "2", ]  # df for 'cluster 2'
calib_15_cluster3 <- calib_15[calib_15$K3_lbd_15 == "3", ]  # df for 'cluster 3'


##### PCA grouped by unsupervised cluster K=3, lambda = 25 #######
# Data frame of principal components ----------------------
calib_25 <- data.frame(pc_calib$x, K3_lbd_25=unsup_calib$K3_lbd_25)  # dataframe of principal components
calib_25_cluster1 <- calib_25[calib_25$K3_lbd_25 == "1", ]  # df for 'cluster 1'
calib_25_cluster2 <- calib_25[calib_25$K3_lbd_25 == "2", ]  # df for 'cluster 2'
calib_25_cluster3 <- calib_25[calib_25$K3_lbd_25 == "3", ]  # df for 'cluster 3'

##### PCA grouped by unsupervised cluster K=3, lambda = default #######
# Data frame of principal components ----------------------
calib_default <- data.frame(pc_calib$x, K3_default_lbd=unsup_calib$K3_default_lbd)  # dataframe of principal components
calib_default_cluster1 <- calib_default[calib_default$K3_default_lbd == "1", ]  # df for 'cluster 1'
calib_default_cluster2 <- calib_default[calib_default$K3_default_lbd == "2", ]  # df for 'cluster 2'
calib_default_cluster3 <- calib_default[calib_default$K3_default_lbd == "3", ]  # df for 'cluster 3'

##### PCA grouped by unsupervised cluster K=3, lambda = default_2 #######
# Data frame of principal components ----------------------
calib_default_2 <- data.frame(pc_calib$x, K3_heuristics_2_lbd=unsup_calib$K3_heuristics_2_lbd)  # dataframe of principal components
calib_default_2_cluster1 <- calib_default_2[calib_default_2$K3_heuristics_2_lbd == "1", ]  # df for 'cluster 1'
calib_default_2_cluster2 <- calib_default_2[calib_default_2$K3_heuristics_2_lbd == "2", ]  # df for 'cluster 2'
calib_default_2_cluster3 <- calib_default_2[calib_default_2$K3_heuristics_2_lbd == "3", ]  # df for 'cluster 3'

# Plot ----------------------------------------------------
plt_cal01 <- ggplot(calib_01, aes(PC1, PC2, col=K3_lbd_0.1)) + 
  geom_point(aes(shape=K3_lbd_0.1), size=2) +   # draw points
  labs(title="K-proto - 610 Observations - 3 clusters with lambda = 0.1", 
       subtitle="With principal components PC1 and PC2 as X and Y axis",
       caption="Source: UBIOPRED") + 
  coord_cartesian(xlim = 1.2 * c(min(calib_01$PC1), max(calib_01$PC1)), 
                  ylim = 1.2 * c(min(calib_01$PC2), max(calib_01$PC2))) +   # change axis limits
  geom_encircle(data = calib_01_cluster1, aes(x=PC1, y=PC2)) +   # draw circles
  geom_encircle(data = calib_01_cluster2, aes(x=PC1, y=PC2)) + 
  geom_encircle(data = calib_01_cluster3, aes(x=PC1, y=PC2)) +
  xlab("PC 1 (7.617%)") + 
  
  ylab("PC 2 (4.317%)") 

plt_calib_01 <- print(plt_cal01 + labs(colour = "Clusters") + labs(shape="Clusters"))

# Plot ----------------------------------------------------
plt_cal02 <- ggplot(calib_02, aes(PC1, PC2, col=K3_lbd_0.2)) + 
  geom_point(aes(shape=K3_lbd_0.2), size=2) +   # draw points
  labs(title="K-proto - 610 Observations - 3 clusters with lambda = 0.2", 
       subtitle="With principal components PC1 and PC2 as X and Y axis",
       caption="Source: UBIOPRED") + 
  coord_cartesian(xlim = 1.2 * c(min(calib_02$PC1), max(calib_02$PC1)), 
                  ylim = 1.2 * c(min(calib_02$PC2), max(calib_02$PC2))) +   # change axis limits
  geom_encircle(data = calib_02_cluster1, aes(x=PC1, y=PC2)) +   # draw circles
  geom_encircle(data = calib_02_cluster2, aes(x=PC1, y=PC2)) + 
  geom_encircle(data = calib_02_cluster3, aes(x=PC1, y=PC2)) 

plt_calib_02 <- print(plt_cal02 + labs(colour = "Clusters") + labs(shape="Clusters"))

# Plot ----------------------------------------------------
plt_cal03 <- ggplot(calib_03, aes(PC1, PC2, col=K3_lbd_0.3)) + 
  geom_point(aes(shape=K3_lbd_0.3), size=2) +   # draw points
  labs(title="K-proto - 610 Observations - 3 clusters with lambda = 0.3", 
       subtitle="With principal components PC1 and PC2 as X and Y axis",
       caption="Source: UBIOPRED") + 
  coord_cartesian(xlim = 1.2 * c(min(calib_03$PC1), max(calib_03$PC1)), 
                  ylim = 1.2 * c(min(calib_03$PC2), max(calib_03$PC2))) +   # change axis limits
  geom_encircle(data = calib_03_cluster1, aes(x=PC1, y=PC2)) +   # draw circles
  geom_encircle(data = calib_03_cluster2, aes(x=PC1, y=PC2)) + 
  geom_encircle(data = calib_03_cluster3, aes(x=PC1, y=PC2)) 

plt_calib_03 <- print(plt_cal03 + labs(colour = "Clusters") + labs(shape="Clusters"))

# Plot ----------------------------------------------------
plt_cal04 <- ggplot(calib_04, aes(PC1, PC2, col=K3_lbd_0.4)) + 
  geom_point(aes(shape=K3_lbd_0.4), size=2) +   # draw points
  labs(title="K-proto - 610 Observations - clusters with lambda = 0.4", 
       subtitle="With principal components PC1 and PC2 as X and Y axis",
       caption="Source: UBIOPRED") + 
  coord_cartesian(xlim = 1.2 * c(min(calib_04$PC1), max(calib_04$PC1)), 
                  ylim = 1.2 * c(min(calib_04$PC2), max(calib_04$PC2))) +   # change axis limits
  geom_encircle(data = calib_04_cluster1, aes(x=PC1, y=PC2)) +   # draw circles
  geom_encircle(data = calib_04_cluster2, aes(x=PC1, y=PC2)) + 
  geom_encircle(data = calib_04_cluster3, aes(x=PC1, y=PC2)) 

plt_calib_04 <- print(plt_cal04 + labs(colour = "Clusters") + labs(shape="Clusters"))

# Plot ----------------------------------------------------
plt_cal05 <- ggplot(calib_05, aes(PC1, PC2, col=K3_lbd_0.5)) + 
  geom_point(aes(shape=K3_lbd_0.5), size=2) +   # draw points
  labs(title="K-proto - 610 Observations - 3 clusters with lambda = 0.5", 
       subtitle="With principal components PC1 and PC2 as X and Y axis",
       caption="Source: UBIOPRED") + 
  coord_cartesian(xlim = 1.2 * c(min(calib_05$PC1), max(calib_05$PC1)), 
                  ylim = 1.2 * c(min(calib_05$PC2), max(calib_05$PC2))) +   # change axis limits
  geom_encircle(data = calib_05_cluster1, aes(x=PC1, y=PC2)) +   # draw circles
  geom_encircle(data = calib_05_cluster2, aes(x=PC1, y=PC2)) + 
  geom_encircle(data = calib_05_cluster3, aes(x=PC1, y=PC2)) 

plt_calib_05 <- print(plt_cal05 + labs(colour = "Clusters") + labs(shape="Clusters"))

# Plot ----------------------------------------------------
plt_cal06 <- ggplot(calib_06, aes(PC1, PC2, col=K3_lbd_0.6)) + 
  geom_point(aes(shape=K3_lbd_0.6), size=2) +   # draw points
  labs(title="K-proto - 610 Observations - 3 clusters with lambda = 0.6", 
       subtitle="With principal components PC1 and PC2 as X and Y axis",
       caption="Source: UBIOPRED") + 
  coord_cartesian(xlim = 1.2 * c(min(calib_06$PC1), max(calib_06$PC1)), 
                  ylim = 1.2 * c(min(calib_06$PC2), max(calib_06$PC2))) +   # change axis limits
  geom_encircle(data = calib_06_cluster1, aes(x=PC1, y=PC2)) +   # draw circles
  geom_encircle(data = calib_06_cluster2, aes(x=PC1, y=PC2)) + 
  geom_encircle(data = calib_06_cluster3, aes(x=PC1, y=PC2)) 

plt_calib_06 <- print(plt_cal06 + labs(colour = "Clusters") + labs(shape="Clusters"))

# Plot ----------------------------------------------------
plt_cal07 <- ggplot(calib_07, aes(PC1, PC2, col=K3_lbd_0.7)) + 
  geom_point(aes(shape=K3_lbd_0.7), size=2) +   # draw points
  labs(title="K-proto - 610 Observations - 3 clusters with lambda = 0.7", 
       subtitle="With principal components PC1 and PC2 as X and Y axis",
       caption="Source: UBIOPRED") + 
  coord_cartesian(xlim = 1.2 * c(min(calib_07$PC1), max(calib_07$PC1)), 
                  ylim = 1.2 * c(min(calib_07$PC2), max(calib_07$PC2))) +   # change axis limits
  geom_encircle(data = calib_07_cluster1, aes(x=PC1, y=PC2)) +   # draw circles
  geom_encircle(data = calib_07_cluster2, aes(x=PC1, y=PC2)) + 
  geom_encircle(data = calib_07_cluster3, aes(x=PC1, y=PC2)) 

plt_calib_07 <- print(plt_cal07 + labs(colour = "Clusters") + labs(shape="Clusters"))

# Plot ----------------------------------------------------
plt_cal08 <- ggplot(calib_08, aes(PC1, PC2, col=K3_lbd_0.8)) + 
  geom_point(aes(shape=K3_lbd_0.8), size=2) +   # draw points
  labs(title="K-proto - 610 Observations - 3 clusters with lambda = 0.8", 
       subtitle="With principal components PC1 and PC2 as X and Y axis",
       caption="Source: UBIOPRED") + 
  coord_cartesian(xlim = 1.2 * c(min(calib_08$PC1), max(calib_08$PC1)), 
                  ylim = 1.2 * c(min(calib_08$PC2), max(calib_08$PC2))) +   # change axis limits
  geom_encircle(data = calib_08_cluster1, aes(x=PC1, y=PC2)) +   # draw circles
  geom_encircle(data = calib_08_cluster2, aes(x=PC1, y=PC2)) + 
  geom_encircle(data = calib_08_cluster3, aes(x=PC1, y=PC2)) 
plt_calib_08 <- print(plt_cal08 + labs(colour = "Clusters") + labs(shape="Clusters"))

# Plot ----------------------------------------------------
plt_cal09 <- ggplot(calib_09, aes(PC1, PC2, col=K3_lbd_0.9)) + 
  geom_point(aes(shape=K3_lbd_0.9), size=2) +   # draw points
  labs(title="K-proto - 610 Observations - 3 clusters with lambda = 0.9", 
       subtitle="With principal components PC1 and PC2 as X and Y axis",
       caption="Source: UBIOPRED") + 
  coord_cartesian(xlim = 1.2 * c(min(calib_09$PC1), max(calib_09$PC1)), 
                  ylim = 1.2 * c(min(calib_09$PC2), max(calib_09$PC2))) +   # change axis limits
  geom_encircle(data = calib_09_cluster1, aes(x=PC1, y=PC2)) +   # draw circles
  geom_encircle(data = calib_09_cluster2, aes(x=PC1, y=PC2)) + 
  geom_encircle(data = calib_09_cluster3, aes(x=PC1, y=PC2)) 

plt_calib_09 <- print(plt_cal09 + labs(colour = "Clusters") + labs(shape="Clusters"))

# Plot ----------------------------------------------------
plt_cal1 <- ggplot(calib_1, aes(PC1, PC2, col=K3_lbd_1)) + 
  geom_point(aes(shape=K3_lbd_1), size=2) +   # draw points
  labs(title="K-proto - 610 Observations - 3 clusters with lambda = 1", 
       subtitle="With principal components PC1 and PC2 as X and Y axis",
       caption="Source: UBIOPRED") + 
  coord_cartesian(xlim = 1.2 * c(min(calib_1$PC1), max(calib_1$PC1)), 
                  ylim = 1.2 * c(min(calib_1$PC2), max(calib_1$PC2))) +   # change axis limits
  geom_encircle(data = calib_1_cluster1, aes(x=PC1, y=PC2)) +   # draw circles
  geom_encircle(data = calib_1_cluster2, aes(x=PC1, y=PC2)) + 
  geom_encircle(data = calib_1_cluster3, aes(x=PC1, y=PC2)) 

plt_calib_1 <- print(plt_cal1 + labs(colour = "Clusters") + labs(shape="Clusters"))

# Plot ----------------------------------------------------
plt_cal2 <- ggplot(calib_2, aes(PC1, PC2, col=K3_lbd_2)) + 
  geom_point(aes(shape=K3_lbd_2), size=2) +   # draw points
  labs(title="K-proto - 610 Observations - 3 clusters with lambda = 2", 
       subtitle="With principal components PC1 and PC2 as X and Y axis",
       caption="Source: UBIOPRED") + 
  coord_cartesian(xlim = 1.2 * c(min(calib_2$PC1), max(calib_2$PC1)), 
                  ylim = 1.2 * c(min(calib_2$PC2), max(calib_2$PC2))) +   # change axis limits
  geom_encircle(data = calib_2_cluster1, aes(x=PC1, y=PC2)) +   # draw circles
  geom_encircle(data = calib_2_cluster2, aes(x=PC1, y=PC2)) + 
  geom_encircle(data = calib_2_cluster3, aes(x=PC1, y=PC2)) 

plt_calib_2 <- print(plt_cal2 + labs(colour = "Clusters") + labs(shape="Clusters"))

# Plot ----------------------------------------------------
plt_cal3 <- ggplot(calib_3, aes(PC1, PC2, col= K3_lbd_3)) + 
  geom_point(aes(shape= K3_lbd_3), size=2) +   # draw points
  labs(title="K-proto - 610 Observations - 3 clusters with lambda = 3", 
       subtitle="With principal components PC1 and PC2 as X and Y axis",
       caption="Source: UBIOPRED") + 
  coord_cartesian(xlim = 1.2 * c(min(calib_3$PC1), max(calib_3$PC1)), 
                  ylim = 1.2 * c(min(calib_3$PC2), max(calib_3$PC2))) +   # change axis limits
  geom_encircle(data = calib_3_cluster1, aes(x=PC1, y=PC2)) +   # draw circles
  geom_encircle(data = calib_3_cluster2, aes(x=PC1, y=PC2)) + 
  geom_encircle(data = calib_3_cluster3, aes(x=PC1, y=PC2)) 

plt_calib_3 <- print(plt_cal3 + labs(colour = "Clusters") + labs(shape="Clusters"))

# Plot ----------------------------------------------------
plt_cal4 <- ggplot(calib_4, aes(PC1, PC2, col=K3_lbd_4)) + 
  geom_point(aes(shape=K3_lbd_4), size=2) +   # draw points
  labs(title="K-proto - 610 Observations - 3 clusters with lambda = 4", 
       subtitle="With principal components PC1 and PC2 as X and Y axis",
       caption="Source: UBIOPRED") + 
  coord_cartesian(xlim = 1.2 * c(min(calib_4$PC1), max(calib_4$PC1)), 
                  ylim = 1.2 * c(min(calib_4$PC2), max(calib_4$PC2))) +   # change axis limits
  geom_encircle(data = calib_4_cluster1, aes(x=PC1, y=PC2)) +   # draw circles
  geom_encircle(data = calib_4_cluster2, aes(x=PC1, y=PC2)) + 
  geom_encircle(data = calib_4_cluster3, aes(x=PC1, y=PC2)) 

plt_calib_4 <- print(plt_cal4 + labs(colour = "Clusters") + labs(shape="Clusters"))

# Plot ----------------------------------------------------
plt_cal5 <- ggplot(calib_5, aes(PC1, PC2, col=K3_lbd_5)) + 
  geom_point(aes(shape=K3_lbd_5), size=2) +   # draw points
  labs(title="K-proto - 610 Observations - 3 clusters with lambda = 5", 
       subtitle="With principal components PC1 and PC2 as X and Y axis",
       caption="Source: UBIOPRED") + 
  coord_cartesian(xlim = 1.2 * c(min(calib_5$PC1), max(calib_5$PC1)), 
                  ylim = 1.2 * c(min(calib_5$PC2), max(calib_5$PC2))) +   # change axis limits
  geom_encircle(data = calib_5_cluster1, aes(x=PC1, y=PC2)) +   # draw circles
  geom_encircle(data = calib_5_cluster2, aes(x=PC1, y=PC2)) + 
  geom_encircle(data = calib_5_cluster3, aes(x=PC1, y=PC2)) 

plt_calib_5 <- print(plt_cal5 + labs(colour = "Clusters") + labs(shape="Clusters"))

# Plot ----------------------------------------------------
plt_cal5 <- ggplot(calib_5, aes(PC1, PC2, col=K3_lbd_5)) + 
  geom_point(aes(shape=K3_lbd_5), size=2) +   # draw points
  labs(title="K-proto - 610 Observations - 3 clusters with lambda = 5", 
       subtitle="With principal components PC1 and PC2 as X and Y axis",
       caption="Source: UBIOPRED") + 
  coord_cartesian(xlim = 1.2 * c(min(calib_5$PC1), max(calib_5$PC1)), 
                  ylim = 1.2 * c(min(calib_5$PC2), max(calib_5$PC2))) +   # change axis limits
  geom_encircle(data = calib_5_cluster1, aes(x=PC1, y=PC2)) +   # draw circles
  geom_encircle(data = calib_5_cluster2, aes(x=PC1, y=PC2)) + 
  geom_encircle(data = calib_5_cluster3, aes(x=PC1, y=PC2)) 

plt_calib_5 <- print(plt_cal5 + labs(colour = "Clusters") + labs(shape="Clusters"))

# Plot ----------------------------------------------------
plt_cal6 <- ggplot(calib_6, aes(PC1, PC2, col=K3_lbd_6)) + 
  geom_point(aes(shape=K3_lbd_6), size=2) +   # draw points
  labs(title="K-proto - 610 Observations - 3 clusters with lambda = 6", 
       subtitle="With principal components PC1 and PC2 as X and Y axis",
       caption="Source: UBIOPRED") + 
  coord_cartesian(xlim = 1.2 * c(min(calib_6$PC1), max(calib_6$PC1)), 
                  ylim = 1.2 * c(min(calib_6$PC2), max(calib_6$PC2))) +   # change axis limits
  geom_encircle(data = calib_6_cluster1, aes(x=PC1, y=PC2)) +   # draw circles
  geom_encircle(data = calib_6_cluster2, aes(x=PC1, y=PC2)) + 
  geom_encircle(data = calib_6_cluster3, aes(x=PC1, y=PC2)) 

plt_calib_6 <- print(plt_cal6 + labs(colour = "Clusters") + labs(shape="Clusters"))

# Plot ----------------------------------------------------
plt_cal7 <- ggplot(calib_7, aes(PC1, PC2, col=K3_lbd_7)) + 
  geom_point(aes(shape=K3_lbd_7), size=2) +   # draw points
  labs(title="K-proto - 610 Observations - 3 clusters with lambda = 7", 
       subtitle="With principal components PC1 and PC2 as X and Y axis",
       caption="Source: UBIOPRED") + 
  coord_cartesian(xlim = 1.2 * c(min(calib_7$PC1), max(calib_7$PC1)), 
                  ylim = 1.2 * c(min(calib_7$PC2), max(calib_7$PC2))) +   # change axis limits
  geom_encircle(data = calib_7_cluster1, aes(x=PC1, y=PC2)) +   # draw circles
  geom_encircle(data = calib_7_cluster2, aes(x=PC1, y=PC2)) + 
  geom_encircle(data = calib_7_cluster3, aes(x=PC1, y=PC2)) 

plt_calib_7 <- print(plt_cal7 + labs(colour = "Clusters") + labs(shape="Clusters"))

# Plot ----------------------------------------------------
plt_cal8 <- ggplot(calib_8, aes(PC1, PC2, col=K3_lbd_8)) + 
  geom_point(aes(shape=K3_lbd_8), size=2) +   # draw points
  labs(title="K-proto - 610 Observations - 3 clusters with lambda = 8", 
       subtitle="With principal components PC1 and PC2 as X and Y axis",
       caption="Source: UBIOPRED") + 
  coord_cartesian(xlim = 1.2 * c(min(calib_8$PC1), max(calib_8$PC1)), 
                  ylim = 1.2 * c(min(calib_8$PC2), max(calib_8$PC2))) +   # change axis limits
  geom_encircle(data = calib_8_cluster1, aes(x=PC1, y=PC2)) +   # draw circles
  geom_encircle(data = calib_8_cluster2, aes(x=PC1, y=PC2)) + 
  geom_encircle(data = calib_8_cluster3, aes(x=PC1, y=PC2)) 

plt_calib_8 <- print(plt_cal8 + labs(colour = "Clusters") + labs(shape="Clusters"))

# Plot ----------------------------------------------------
plt_cal9 <- ggplot(calib_9, aes(PC1, PC2, col=K3_lbd_9)) + 
  geom_point(aes(shape=K3_lbd_9), size=2) +   # draw points
  labs(title="K-proto - 610 Observations - 3 clusters with lambda = 9", 
       subtitle="With principal components PC1 and PC2 as X and Y axis",
       caption="Source: UBIOPRED") + 
  coord_cartesian(xlim = 1.2 * c(min(calib_9$PC1), max(calib_9$PC1)), 
                  ylim = 1.2 * c(min(calib_9$PC2), max(calib_9$PC2))) +   # change axis limits
  geom_encircle(data = calib_9_cluster1, aes(x=PC1, y=PC2)) +   # draw circles
  geom_encircle(data = calib_9_cluster2, aes(x=PC1, y=PC2)) + 
  geom_encircle(data = calib_9_cluster3, aes(x=PC1, y=PC2)) 

plt_calib_9 <- print(plt_cal9 + labs(colour = "Clusters") + labs(shape="Clusters"))

# Plot ----------------------------------------------------
plt_cal10 <- ggplot(calib_10, aes(PC1, PC2, col=K3_lbd_10)) + 
  geom_point(aes(shape=K3_lbd_10), size=2) +   # draw points
  labs(title="K-proto - 610 Observations - 3 clusters with lambda = 10", 
       subtitle="With principal components PC1 and PC2 as X and Y axis",
       caption="Source: UBIOPRED") + 
  coord_cartesian(xlim = 1.2 * c(min(calib_10$PC1), max(calib_10$PC1)), 
                  ylim = 1.2 * c(min(calib_10$PC2), max(calib_10$PC2))) +   # change axis limits
  geom_encircle(data = calib_10_cluster1, aes(x=PC1, y=PC2)) +   # draw circles
  geom_encircle(data = calib_10_cluster2, aes(x=PC1, y=PC2)) + 
  geom_encircle(data = calib_10_cluster3, aes(x=PC1, y=PC2)) 

plt_calib_10 <- print(plt_cal10 + labs(colour = "Clusters") + labs(shape="Clusters"))

# Plot ----------------------------------------------------
plt_cal11 <- ggplot(calib_11, aes(PC1, PC2, col=K3_lbd_11)) + 
  geom_point(aes(shape=K3_lbd_11), size=2) +   # draw points
  labs(title="K-proto - 610 Observations - 3 clusters with lambda = 11", 
       subtitle="With principal components PC1 and PC2 as X and Y axis",
       caption="Source: UBIOPRED") + 
  coord_cartesian(xlim = 1.2 * c(min(calib_11$PC1), max(calib_11$PC1)), 
                  ylim = 1.2 * c(min(calib_11$PC2), max(calib_11$PC2))) +   # change axis limits
  geom_encircle(data = calib_11_cluster1, aes(x=PC1, y=PC2)) +   # draw circles
  geom_encircle(data = calib_11_cluster2, aes(x=PC1, y=PC2)) + 
  geom_encircle(data = calib_11_cluster3, aes(x=PC1, y=PC2)) 

plt_calib_11 <- print(plt_cal11 + labs(colour = "Clusters") + labs(shape="Clusters"))

# Plot ----------------------------------------------------
plt_cal12 <- ggplot(calib_12, aes(PC1, PC2, col=K3_lbd_12)) + 
  geom_point(aes(shape=K3_lbd_12), size=2) +   # draw points
  labs(title="K-proto - 610 Observations - 3 clusters with lambda = 12", 
       subtitle="With principal components PC1 and PC2 as X and Y axis",
       caption="Source: UBIOPRED") + 
  coord_cartesian(xlim = 1.2 * c(min(calib_12$PC1), max(calib_12$PC1)), 
                  ylim = 1.2 * c(min(calib_12$PC2), max(calib_12$PC2))) +   # change axis limits
  geom_encircle(data = calib_12_cluster1, aes(x=PC1, y=PC2)) +   # draw circles
  geom_encircle(data = calib_12_cluster2, aes(x=PC1, y=PC2)) + 
  geom_encircle(data = calib_12_cluster3, aes(x=PC1, y=PC2)) 

plt_calib_12 <- print(plt_cal12 + labs(colour = "Clusters") + labs(shape="Clusters"))

# Plot ----------------------------------------------------
plt_cal13 <- ggplot(calib_13, aes(PC1, PC2, col=K3_lbd_13)) + 
  geom_point(aes(shape=K3_lbd_13), size=2) +   # draw points
  labs(title="K-proto - 610 Observations - 3 clusters with lambda = 13", 
       subtitle="With principal components PC1 and PC2 as X and Y axis",
       caption="Source: UBIOPRED") + 
  coord_cartesian(xlim = 1.2 * c(min(calib_13$PC1), max(calib_13$PC1)), 
                  ylim = 1.2 * c(min(calib_13$PC2), max(calib_13$PC2))) +   # change axis limits
  geom_encircle(data = calib_13_cluster1, aes(x=PC1, y=PC2)) +   # draw circles
  geom_encircle(data = calib_13_cluster2, aes(x=PC1, y=PC2)) + 
  geom_encircle(data = calib_13_cluster3, aes(x=PC1, y=PC2)) 

plt_calib_13 <- print(plt_cal13 + labs(colour = "Clusters") + labs(shape="Clusters"))

# Plot ----------------------------------------------------
plt_cal14 <- ggplot(calib_14, aes(PC1, PC2, col=K3_lbd_14)) + 
  geom_point(aes(shape=K3_lbd_14), size=2) +   # draw points
  labs(title="K-proto - 610 Observations - 3 clusters with lambda = 14", 
       subtitle="With principal components PC1 and PC2 as X and Y axis",
       caption="Source: UBIOPRED") + 
  coord_cartesian(xlim = 1.2 * c(min(calib_14$PC1), max(calib_14$PC1)), 
                  ylim = 1.2 * c(min(calib_14$PC2), max(calib_14$PC2))) +   # change axis limits
  geom_encircle(data = calib_14_cluster1, aes(x=PC1, y=PC2)) +   # draw circles
  geom_encircle(data = calib_14_cluster2, aes(x=PC1, y=PC2)) + 
  geom_encircle(data = calib_14_cluster3, aes(x=PC1, y=PC2)) 

plt_calib_14 <- print(plt_cal14 + labs(colour = "Clusters") + labs(shape="Clusters"))

# Plot ----------------------------------------------------
plt_cal15 <- ggplot(calib_15, aes(PC1, PC2, col=K3_lbd_15)) + 
  geom_point(aes(shape=K3_lbd_15), size=2) +   # draw points
  labs(title="K-proto - 610 Observations - 3 clusters with lambda = 15", 
       subtitle="With principal components PC1 and PC2 as X and Y axis",
       caption="Source: UBIOPRED") + 
  coord_cartesian(xlim = 1.2 * c(min(calib_15$PC1), max(calib_15$PC1)), 
                  ylim = 1.2 * c(min(calib_15$PC2), max(calib_15$PC2))) +   # change axis limits
  geom_encircle(data = calib_15_cluster1, aes(x=PC1, y=PC2)) +   # draw circles
  geom_encircle(data = calib_15_cluster2, aes(x=PC1, y=PC2)) + 
  geom_encircle(data = calib_15_cluster3, aes(x=PC1, y=PC2)) +
  xlab("PC 1 (7.617%)") + 
  
  ylab("PC 2 (4.317%)") 

plt_calib_15 <- print(plt_cal15 + labs(colour = "Clusters") + labs(shape="Clusters"))

# Plot ----------------------------------------------------
plt_cal25 <- ggplot(calib_25, aes(PC1, PC2, col=K3_lbd_25)) + 
  geom_point(aes(shape=K3_lbd_25), size=2) +   # draw points
  labs(title="K-proto - 610 Observations - 3 clusters with lambda = 25", 
       subtitle="With principal components PC1 and PC2 as X and Y axis",
       caption="Source: UBIOPRED") + 
  coord_cartesian(xlim = 1.2 * c(min(calib_25$PC1), max(calib_25$PC1)), 
                  ylim = 1.2 * c(min(calib_25$PC2), max(calib_25$PC2))) +   # change axis limits
  geom_encircle(data = calib_25_cluster1, aes(x=PC1, y=PC2)) +   # draw circles
  geom_encircle(data = calib_25_cluster2, aes(x=PC1, y=PC2)) + 
  geom_encircle(data = calib_25_cluster3, aes(x=PC1, y=PC2)) 

plt_calib_25 <- print(plt_cal25 + labs(colour = "Clusters") + labs(shape="Clusters"))

#Plot with default heuristics and therefore lambdas
plt_cal_default <- ggplot(calib_default, aes(PC1, PC2, col=K3_default_lbd)) + 
  geom_point(aes(shape=K3_default_lbd), size=2) +   # draw points
  labs(title="K-proto - 610 Observations - 3 clusters with default lambda", 
       subtitle="With principal components PC1 and PC2 as X and Y axis",
       caption="Source: UBIOPRED") + 
  coord_cartesian(xlim = 1.2 * c(min(calib_default$PC1), max(calib_default$PC1)), 
                  ylim = 1.2 * c(min(calib_default$PC2), max(calib_default$PC2))) +   # change axis limits
  geom_encircle(data = calib_default_cluster1, aes(x=PC1, y=PC2)) +   # draw circles
  geom_encircle(data = calib_default_cluster2, aes(x=PC1, y=PC2)) + 
  geom_encircle(data = calib_default_cluster3, aes(x=PC1, y=PC2)) +
  xlab("PC 1 (7.617%)") + 
  
  ylab("PC 2 (4.317%)") 

plt_calib_default <- print(plt_cal_default + labs(colour = "Clusters") + labs(shape="Clusters"))

#Plot with changed heuristics and therefore lambdas
plt_cal_default_2 <- ggplot(calib_default_2, aes(PC1, PC2, col=K3_heuristics_2_lbd)) + 
  geom_point(aes(shape=K3_heuristics_2_lbd), size=2) +   # draw points
  labs(title="K-proto - 610 Observations - 3 clusters with second default heuristic for lambda", 
       subtitle="With principal components PC1 and PC2 as X and Y axis",
       caption="Source: UBIOPRED") + 
  coord_cartesian(xlim = 1.2 * c(min(calib_default_2$PC1), max(calib_default_2$PC1)), 
                  ylim = 1.2 * c(min(calib_default_2$PC2), max(calib_default_2$PC2))) +   # change axis limits
  geom_encircle(data = calib_default_2_cluster1, aes(x=PC1, y=PC2)) +   # draw circles
  geom_encircle(data = calib_default_2_cluster2, aes(x=PC1, y=PC2)) + 
  geom_encircle(data = calib_default_2_cluster3, aes(x=PC1, y=PC2)) +
  xlab("PC 1 (7.617%)") + 
  
  ylab("PC 2 (4.317%)") 

plt_calib_default_2 <- print(plt_cal_default_2 + labs(colour = "Clusters") + labs(shape="Clusters"))

plt_cal3.1 <- ggplot(calib_3.1, aes(PC1, PC2, col=K3_lbd_3.1)) + 
  geom_point(aes(shape=K3_lbd_3.1), size=2) +   # draw points
  labs(title="K-proto - 610 Observations - 3 clusters with lambda = 3.1", 
       subtitle="With principal components PC1 and PC2 as X and Y axis",
       caption="Source: UBIOPRED") + 
  coord_cartesian(xlim = 1.2 * c(min(calib_3.1$PC1), max(calib_3.1$PC1)), 
                  ylim = 1.2 * c(min(calib_3.1$PC2), max(calib_3.1$PC2))) +   # change axis limits
  geom_encircle(data = calib_3.1_cluster1, aes(x=PC1, y=PC2)) +   # draw circles
  geom_encircle(data = calib_3.1_cluster2, aes(x=PC1, y=PC2)) + 
  geom_encircle(data = calib_3.1_cluster3, aes(x=PC1, y=PC2)) +
  xlab("PC1 (7.617%)") + 
  
  ylab("PC2 (4.317%)") 

plt_calib_3.1 <- print(plt_cal3.1 + labs(colour = "Clusters") + labs(shape="Clusters"))


#Resume the plots available and patchwork them
#plt_calib_01
#plt_calib_02
#plt_calib_03
#plt_calib_04
#plt_calib_05
#plt_calib_06
#plt_calib_07
#plt_calib_08
#plt_calib_09
#plt_calib_1
#plt_calib_2
#plt_calib_3
#plt_calib_4
#plt_calib_5
#plt_calib_6
#plt_calib_7
#plt_calib_8
#plt_calib_9
#plt_calib_10
#plt_calib_11
#plt_calib_12
#plt_calib_13
#plt_calib_14
#plt_calib_15
#plt_calib_25
#plt_calib_default
#plt_calib_default_2


## Save 5 most important plots (optimal and default + extremes) into 2 documents
library(patchwork)
plt_calib_01 + plt_calib_3.1 / plt_calib_15
ggsave("/rds/general/user/md2620/home/asthma/Malo/Results/PCA_Calibration/optimal_extremes.jpeg", width = 22, height = 12)

library(patchwork)
plt_calib_default + plt_calib_default_2
ggsave("/rds/general/user/md2620/home/asthma/Malo/Results/PCA_Calibration/defaults.jpeg", width = 22, height = 12)

#Total of 27 plots
#Save the plots and make yor final choice
library(patchwork)
plt_calib_01 + plt_calib_02 + plt_calib_03 + plt_calib_04
ggsave("/rds/general/user/md2620/home/asthma/Malo/Results/PCA_Calibration/lbd_01_04.jpeg", width = 18, height = 12)

library(patchwork)
plt_calib_05 + plt_calib_06 + plt_calib_07 + plt_calib_08
ggsave("/rds/general/user/md2620/home/asthma/Malo/Results/PCA_Calibration/lbd_05_08.jpeg", width = 18, height = 12)

library(patchwork)
plt_calib_09 + plt_calib_1 + plt_calib_2 + plt_calib_3
ggsave("/rds/general/user/md2620/home/asthma/Malo/Results/PCA_Calibration/lbd_09_3.jpeg", width = 18, height = 12)

library(patchwork)
plt_calib_4 + plt_calib_5 + plt_calib_6 + plt_calib_7
ggsave("/rds/general/user/md2620/home/asthma/Malo/Results/PCA_Calibration/lbd_4_7.jpeg", width = 18, height = 12)

library(patchwork)
plt_calib_8 + plt_calib_9 + plt_calib_10 + plt_calib_11
ggsave("/rds/general/user/md2620/home/asthma/Malo/Results/PCA_Calibration/lbd_8_11.jpeg", width = 18, height = 12)

library(patchwork)
plt_calib_12 + plt_calib_13 + plt_calib_14 + plt_calib_15
ggsave("/rds/general/user/md2620/home/asthma/Malo/Results/PCA_Calibration/lbd_12_15.jpeg", width = 18, height = 12)

library(patchwork)
plt_calib_default + plt_calib_default_2
ggsave("/rds/general/user/md2620/home/asthma/Malo/Results/PCA_Calibration/lbd_defaults.jpeg", width = 18, height = 8)

library(patchwork)
plt_calib_25
ggsave("/rds/general/user/md2620/home/asthma/Malo/Results/PCA_Calibration/lbd_25.jpeg", width = 8, height = 8)

#Final choice of usupervised k proto is K=3 and lambda = 3

