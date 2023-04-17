##### PCA VISUALIZATION #####
#Visualization clusters characteristics

#Reading the files
viz_unsup = read.csv("/rds/general/user/md2620/home/asthma/Malo/Dataframes/Final/unsup_df_all_processed.csv")

#devtools::install_github("hrbrmstr/ggalt")
library(ggplot2)
library(ggalt)
#install.packages("ggfortify")
library(ggfortify)
theme_set(theme_classic())

# Compute data with principal components ------------------
PCA_unsup_df <- viz_unsup[c(1:610),c(2:363)]

#Set Subject.ID as index
PCA_unsup_df_final <- PCA_unsup_df[-1]
row.names(PCA_unsup_df_final) <- PCA_unsup_df$Unnamed..0

#Drop zero variance variables
PCA_unsup_df_final = subset(PCA_unsup_df_final, select = -c(Omics.Xylose,Omics.Xanthosine,Omics.Furoylglycine,Omics.Allantoin,
Omics.Cytosine,Omics.Glucosamine,Omics.Glutamic.acid,
Omics.Isoleucine,Omics.Lysine,Omics.Maltose,
Omics.N.Acetylglutamic.acid,Omics.N.Methyl.D.aspartic.acid,Omics.O.Acetylserine,
Omics.Phenylalanine,Omics.Sarcosine,Omics.N.Acetylputrescine))


View(PCA_unsup_df_final)

pca_mod_un <- prcomp(PCA_unsup_df_final)  # compute principal components
summary(pca_mod_un)
#PC1: 7.617% 
#PC2: 4.317% 

##### PCA grouped by cohort #######
# Data frame of principal components ----------------------
df_pc_un <- data.frame(pca_mod_un$x, cohort=viz_unsup$cohort)  # dataframe of principal components
df_pc_un_severe <- df_pc_un[df_pc_un$cohort == "Severe", ]  # df for 'severe'
df_pc_un_sev_smoker <- df_pc_un[df_pc_un$cohort == "Severe_Smoker", ]  # df for 'severe smoker'
df_pc_un_mild <- df_pc_un[df_pc_un$cohort == "Mild/Moderate", ]  # df for 'mild moderate'
df_pc_un_healthy <- df_pc_un[df_pc_un$cohort == "Healthy", ]  # df for 'healthy

# Plot ----------------------------------------------------
ggplot(df_pc_un, aes(PC1, PC2, col=cohort)) + 
  geom_point(aes(shape=cohort), size=2) +   # draw points
  labs(title="Observations grouped per severity form of asthma - Unsupervised Kproto", 
       subtitle="With principal components PC1 and PC2 as X and Y axis",
       caption="Source: UBIOPRED") + 
  coord_cartesian(xlim = 1.2 * c(min(df_pc_un$PC1), max(df_pc_un$PC1)), 
                  ylim = 1.2 * c(min(df_pc_un$PC2), max(df_pc_un$PC2))) +   # change axis limits
  geom_encircle(data = df_pc_un_severe, aes(x=PC1, y=PC2)) +   # draw circles
  geom_encircle(data = df_pc_un_sev_smoker, aes(x=PC1, y=PC2)) + 
  geom_encircle(data = df_pc_un_mild, aes(x=PC1, y=PC2)) +
  geom_encircle(data = df_pc_un_healthy, aes(x=PC1, y=PC2)) +
  xlab("PC 1 (7.617%)") + 
  
  ylab("PC 2 (4.317%)") 

ggsave("/rds/general/user/md2620/home/asthma/Malo/Results/PCA_Results_Unsup/PCA_by_cohort.jpeg", width = 12, height = 6)

#The plot above shows that observations are not seperated according to asthma severity
#Let's check by categorizing the 5 most important numerical variables driving the unsup Kproto clusters found via features importance plots
#Categorize the following variables Omics.Nitrotyrosine,Omics.Methionine,Age,Questionnaires.ACQ.FEV1.Precentage,Omics.S.Adenosylhomocysteine
#Summary stats of these 5 variables

summary(PCA_unsup_df_final$Omics.Nitrotyrosine)
#Min.  1st Qu.   Median     Mean  3rd Qu.     Max. 
#-4.10285 -0.65463 -0.05469  0.00000  0.63970  5.14527 

#Min - 1st quartile = low to very low levels :
#1st to 3rd quartile = medium levels
#3rd quartile to max = high to very high levels
df_pc_un$Nitrotyrosinegroup <- cut(PCA_unsup_df_final$Omics.Nitrotyrosine, breaks = c(-Inf,-0.65463,0.63970,Inf), labels = c("Low to very low ","Medium","High to very high"))

ggplot(df_pc_un, aes(PC1, PC2, col=Nitrotyrosinegroup)) + 
  geom_point(aes(shape=Nitrotyrosinegroup), size=2) +   # draw points
  labs(title="Observations grouped according to Nitrotyrosine levels - Unsupervised Kproto", 
       subtitle="With principal components PC1 and PC2 as X and Y axis",
       col = "Nitrotyrosine level",
       shape = "Nitrotyrosine level",
       caption="Source: UBIOPRED") + 
  coord_cartesian(xlim = 1.2 * c(min(df_pc_un$PC1), max(df_pc_un$PC1)), 
                  ylim = 1.2 * c(min(df_pc_un$PC2), max(df_pc_un$PC2))) +   # change axis limits
  #geom_encircle(data = df_pc_un$Nitrotyrosinegroup, aes(x=PC1, y=PC2)) +   # draw circles
  #geom_encircle(data = df_pc_un_sev_smoker, aes(x=PC1, y=PC2)) + 
  #geom_encircle(data = df_pc_un_mild, aes(x=PC1, y=PC2)) +
  #geom_encircle(data = df_pc_un_healthy, aes(x=PC1, y=PC2)) +
  xlab("PC 1 (7.617%)") + 
  
  ylab("PC 2 (4.317%)") 

ggsave("/rds/general/user/md2620/home/asthma/Malo/Results/PCA_Results_Unsup/PCA_by_nitrotyrosine.jpeg", width = 12, height = 6)

summary(PCA_unsup_df_final$Age)
#Min.  1st Qu.   Median     Mean  3rd Qu.     Max. 
#-1.93133 -0.83887  0.08058  0.00000  0.78909  2.21142

df_pc_un$Agegroup <- cut(PCA_unsup_df_final$Age, breaks = c(-Inf,-0.83887,0.78909,Inf), labels = c("Young","Mid-Age","Old"))

ggplot(df_pc_un, aes(PC1, PC2, col=Agegroup)) + 
  geom_point(aes(shape=Agegroup), size=2) +   # draw points
  labs(title="Observations grouped per age group - Unsupervised Kproto", 
       subtitle="With principal components PC1 and PC2 as X and Y axis",
       col = "Age group",
       shape = "Age group",
       caption="Source: UBIOPRED") + 
  coord_cartesian(xlim = 1.2 * c(min(df_pc_un$PC1), max(df_pc_un$PC1)), 
                  ylim = 1.2 * c(min(df_pc_un$PC2), max(df_pc_un$PC2))) +   # change axis limits
  #geom_encircle(data = df_pc_un$Nitrotyrosinegroup, aes(x=PC1, y=PC2)) +   # draw circles
  #geom_encircle(data = df_pc_un_sev_smoker, aes(x=PC1, y=PC2)) + 
  #geom_encircle(data = df_pc_un_mild, aes(x=PC1, y=PC2)) +
  #geom_encircle(data = df_pc_un_healthy, aes(x=PC1, y=PC2)) +
  xlab("PC 1 (7.617%)") + 
  
  ylab("PC 2 (4.317%)") 
ggsave("/rds/general/user/md2620/home/asthma/Malo/Results/PCA_Results_Unsup/PCA_by_agegroup.jpeg", width = 12, height = 6)


summary(PCA_unsup_df_final$Omics.Methionine)
#Min.  1st Qu.   Median     Mean  3rd Qu.     Max. 
#-3.92458 -0.56561  0.01362  0.00000  0.60963  5.51982 

df_pc_un$methioninegroup <- cut(PCA_unsup_df_final$Omics.Methionine, breaks = c(-Inf,-0.56561,0.60963,Inf), labels = c("Low to very low ","Medium","High to very high"))

ggplot(df_pc_un, aes(PC1, PC2, col=methioninegroup)) + 
  geom_point(aes(shape=methioninegroup), size=2) +   # draw points
  labs(title="Observations grouped according to Methionine levels - Unsupervised Kproto", 
       subtitle="With principal components PC1 and PC2 as X and Y axis",
       col = "Methionine level",
       shape = "Methionine level",
       caption="Source: UBIOPRED") + 
  coord_cartesian(xlim = 1.2 * c(min(df_pc_un$PC1), max(df_pc_un$PC1)), 
                  ylim = 1.2 * c(min(df_pc_un$PC2), max(df_pc_un$PC2))) +   # change axis limits
  #geom_encircle(data = df_pc_un$Nitrotyrosinegroup, aes(x=PC1, y=PC2)) +   # draw circles
  #geom_encircle(data = df_pc_un_sev_smoker, aes(x=PC1, y=PC2)) + 
  #geom_encircle(data = df_pc_un_mild, aes(x=PC1, y=PC2)) +
  #geom_encircle(data = df_pc_un_healthy, aes(x=PC1, y=PC2)) +
  xlab("PC 1 (7.617%)") + 
  
  ylab("PC 2 (4.317%)") 
ggsave("/rds/general/user/md2620/home/asthma/Malo/Results/PCA_Results_Unsup/PCA_by_methionine.jpeg", width = 12, height = 6)

summary(PCA_unsup_df_final$Questionnaires.ACQ.FEV1.Precentage)
#Min.  1st Qu.   Median     Mean  3rd Qu.     Max. 
#-2.39872 -0.78404  0.02002  0.00000  0.83138  3.29729

df_pc_un$FEV1group <- cut(PCA_unsup_df_final$Questionnaires.ACQ.FEV1.Precentage, breaks = c(-Inf,-0.78404,0.83138,Inf), labels = c("Low","Medium","High"))

ggplot(df_pc_un, aes(PC1, PC2, col=FEV1group)) + 
  geom_point(aes(shape=FEV1group), size=2) +   # draw points
  labs(title="Observations grouped according to FEV1 % - Unsupervised Kproto", 
       subtitle="With principal components PC1 and PC2 as X and Y axis",
       col = "FEV1 %",
       shape = "FEV1 %",
       caption="Source: UBIOPRED") + 
  coord_cartesian(xlim = 1.2 * c(min(df_pc_un$PC1), max(df_pc_un$PC1)), 
                  ylim = 1.2 * c(min(df_pc_un$PC2), max(df_pc_un$PC2))) +   # change axis limits
  #geom_encircle(data = df_pc_un$Nitrotyrosinegroup, aes(x=PC1, y=PC2)) +   # draw circles
  #geom_encircle(data = df_pc_un_sev_smoker, aes(x=PC1, y=PC2)) + 
  #geom_encircle(data = df_pc_un_mild, aes(x=PC1, y=PC2)) +
  #geom_encircle(data = df_pc_un_healthy, aes(x=PC1, y=PC2)) +
  xlab("PC 1 (7.617%)") + 
  
  ylab("PC 2 (4.317%)") 
ggsave("/rds/general/user/md2620/home/asthma/Malo/Results/PCA_Results_Unsup/PCA_by_FEV1%.jpeg", width = 12, height = 6)


summary(PCA_unsup_df_final$Omics.S.Adenosylhomocysteine)
#Min.  1st Qu.   Median     Mean  3rd Qu.     Max. 
#-3.35106 -0.68115 -0.00179  0.00000  0.69539  3.51655 
df_pc_un$Adenosylhomocysteinegroup <- cut(PCA_unsup_df_final$Omics.S.Adenosylhomocysteine, breaks = c(-Inf,-0.68115,0.69539,Inf), labels = c("Low to very low","Medium","High to very high"))

ggplot(df_pc_un, aes(PC1, PC2, col=Adenosylhomocysteinegroup)) + 
  geom_point(aes(shape=Adenosylhomocysteinegroup), size=2) +   # draw points
  labs(title="Observations grouped according to S.Adenosylhomocysteine levels - Unsupervised Kproto", 
       subtitle="With principal components PC1 and PC2 as X and Y axis",
       col = "S.Adenosylhomocysteine level",
       shape = "S.Adenosylhomocysteine level",
       caption="Source: UBIOPRED") + 
  coord_cartesian(xlim = 1.2 * c(min(df_pc_un$PC1), max(df_pc_un$PC1)), 
                  ylim = 1.2 * c(min(df_pc_un$PC2), max(df_pc_un$PC2))) +   # change axis limits
  #geom_encircle(data = df_pc_un$Nitrotyrosinegroup, aes(x=PC1, y=PC2)) +   # draw circles
  #geom_encircle(data = df_pc_un_sev_smoker, aes(x=PC1, y=PC2)) + 
  #geom_encircle(data = df_pc_un_mild, aes(x=PC1, y=PC2)) +
  #geom_encircle(data = df_pc_un_healthy, aes(x=PC1, y=PC2)) +
  xlab("PC 1 (7.617%)") + 
  
  ylab("PC 2 (4.317%)") 
ggsave("/rds/general/user/md2620/home/asthma/Malo/Results/PCA_Results_Unsup/PCA_by_S.Adenosylhomocysteine.jpeg", width = 12, height = 6)

#Make sure clusters variables are factor variables
viz_unsup$cluster_K_2 = as.factor(viz_unsup$cluster_K_2)
viz_unsup$cluster_K_3 = as.factor(viz_unsup$cluster_K_3)
viz_unsup$cluster_K_4 = as.factor(viz_unsup$cluster_K_4)
viz_unsup$cluster_K_5 = as.factor(viz_unsup$cluster_K_5)
viz_unsup$cluster_K_6 = as.factor(viz_unsup$cluster_K_6)
viz_unsup$cluster_K_7 = as.factor(viz_unsup$cluster_K_7)
viz_unsup$cluster_K_8 = as.factor(viz_unsup$cluster_K_8)
viz_unsup$cluster_K_9 = as.factor(viz_unsup$cluster_K_9)
viz_unsup$cluster_K_10 = as.factor(viz_unsup$cluster_K_10)
viz_unsup$cluster_K_11 = as.factor(viz_unsup$cluster_K_11)
viz_unsup$cluster_K_12 = as.factor(viz_unsup$cluster_K_12)
viz_unsup$cluster_K_13 = as.factor(viz_unsup$cluster_K_13)
viz_unsup$cluster_K_14 = as.factor(viz_unsup$cluster_K_14)
viz_unsup$cluster_K_15 = as.factor(viz_unsup$cluster_K_15)

#Check levels
levels(viz_unsup$cluster_K_11)

################## PCA K = 12 Impossible to run -- too many shapes ###################
##### PCA grouped by unsupervised cluster K=12 #######
# Data frame of principal components ----------------------
df_pc_un_K_12 <- data.frame(pca_mod_un$x, cluster_K_12=viz_sup$cluster_K_12)  # dataframe of principal components
df_pc_un_K_12_cluster1 <- df_pc_un_K_12[df_pc_un_K_12$cluster_K_12 == "1", ]  # df for 'cluster 1'
df_pc_un_K_12_cluster2 <- df_pc_un_K_12[df_pc_un_K_11$cluster_K_12 == "2", ]  # df for 'cluster 2'
df_pc_un_K_12_cluster3 <- df_pc_un_K_12[df_pc_un_K_12$cluster_K_12 == "3", ]  # df for 'cluster 3'
df_pc_un_K_12_cluster4 <- df_pc_un_K_12[df_pc_un_K_12$cluster_K_12 == "4", ]  # df for 'cluster 4'
df_pc_un_K_12_cluster5 <- df_pc_un_K_12[df_pc_un_K_12$cluster_K_12 == "5", ]  # df for 'cluster 5'
df_pc_un_K_12_cluster6 <- df_pc_un_K_12[df_pc_un_K_12$cluster_K_12 == "6", ]  # df for 'cluster 6'
df_pc_un_K_12_cluster7 <- df_pc_un_K_12[df_pc_un_K_12$cluster_K_12 == "7", ]  # df for 'cluster 7'
df_pc_un_K_12_cluster8 <- df_pc_un_K_12[df_pc_un_K_12$cluster_K_12 == "8", ]  # df for 'cluster 8'
df_pc_un_K_12_cluster9 <- df_pc_un_K_12[df_pc_un_K_12$cluster_K_12 == "9", ]  # df for 'cluster 9'
df_pc_un_K_12_cluster10 <- df_pc_un_K_12[df_pc_un_K_12$cluster_K_12 == "10", ]  # df for 'cluster 10'
df_pc_un_K_12_cluster11 <- df_pc_un_K_12[df_pc_un_K_12$cluster_K_12 == "11", ]  # df for 'cluster 11'
df_pc_un_K_12_cluster12 <- df_pc_un_K_12[df_pc_un_K_12$cluster_K_12 == "12", ]  # df for 'cluster 11'

# Plot ----------------------------------------------------
ggplot(df_pc_un_K_12, aes(PC1, PC2, col=cluster_K_12)) + 
  geom_point(aes(shape=cluster_K_12), size=2) +   # draw points
  labs(title="Observations grouped in 12 clusters", 
       subtitle="With principal components PC1 and PC2 as X and Y axis",
       caption="Source: UBIOPRED") + 
  coord_cartesian(xlim = 1.2 * c(min(df_pc_un_K_12$PC1), max(df_pc_un_K_12$PC1)), 
                  ylim = 1.2 * c(min(df_pc_un_K_12$PC2), max(df_pc_un_K_12$PC2))) +   # change axis limits
  geom_encircle(data = df_pc_un_K_12_cluster1, aes(x=PC1, y=PC2)) +   # draw circles
  geom_encircle(data = df_pc_un_K_12_cluster2, aes(x=PC1, y=PC2)) + 
  geom_encircle(data = df_pc_un_K_12_cluster3, aes(x=PC1, y=PC2)) +
  geom_encircle(data = df_pc_un_K_12_cluster4, aes(x=PC1, y=PC2)) +
  geom_encircle(data = df_pc_un_K_12_cluster5, aes(x=PC1, y=PC2)) +
  geom_encircle(data = df_pc_un_K_12_cluster6, aes(x=PC1, y=PC2)) +
  geom_encircle(data = df_pc_un_K_12_cluster7, aes(x=PC1, y=PC2)) +
  geom_encircle(data = df_pc_un_K_12_cluster8, aes(x=PC1, y=PC2)) +
  geom_encircle(data = df_pc_un_K_12_cluster9, aes(x=PC1, y=PC2)) +
  geom_encircle(data = df_pc_un_K_12_cluster10, aes(x=PC1, y=PC2)) +
  geom_encircle(data = df_pc_un_K_12_cluster11, aes(x=PC1, y=PC2)) +
  geom_encircle(data = df_pc_un_K_12_cluster12, aes(x=PC1, y=PC2))


################### PCA FOR K=2:6 #####################
##### PCA grouped by supervised cluster K=2 #######
# Data frame of principal components ----------------------
df_pc_un_K_2 <- data.frame(pca_mod_un$x, cluster_K_2=viz_unsup$cluster_K_2)  # dataframe of principal components
df_pc_un_K_2_cluster1 <- df_pc_un_K_2[df_pc_un_K_2$cluster_K_2 == "1", ]  # df for 'cluster 1'
df_pc_un_K_2_cluster2 <- df_pc_un_K_2[df_pc_un_K_2$cluster_K_2 == "2", ]  # df for 'cluster 2'

##### PCA grouped by supervised cluster K=3 #######
# Data frame of principal components ----------------------
df_pc_un_K_3 <- data.frame(pca_mod_un$x, cluster_K_3=viz_unsup$cluster_K_3)  # dataframe of principal components
df_pc_un_K_3_cluster1 <- df_pc_un_K_3[df_pc_un_K_3$cluster_K_3 == "1", ]  # df for 'cluster 1'
df_pc_un_K_3_cluster2 <- df_pc_un_K_3[df_pc_un_K_3$cluster_K_3 == "2", ]  # df for 'cluster 2'
df_pc_un_K_3_cluster3 <- df_pc_un_K_3[df_pc_un_K_3$cluster_K_3 == "3", ]  # df for 'cluster 3'

##### PCA grouped by supervised cluster K=4 #######
# Data frame of principal components ----------------------
df_pc_un_K_4 <- data.frame(pca_mod_un$x, cluster_K_4=viz_unsup$cluster_K_4)  # dataframe of principal components
df_pc_un_K_4_cluster1 <- df_pc_un_K_4[df_pc_un_K_4$cluster_K_4 == "1", ]  # df for 'cluster 1'
df_pc_un_K_4_cluster2 <- df_pc_un_K_4[df_pc_un_K_4$cluster_K_4 == "2", ]  # df for 'cluster 2'
df_pc_un_K_4_cluster3 <- df_pc_un_K_4[df_pc_un_K_4$cluster_K_4 == "3", ]  # df for 'cluster 3'
df_pc_un_K_4_cluster4 <- df_pc_un_K_4[df_pc_un_K_4$cluster_K_4 == "4", ]  # df for 'cluster 4'

##### PCA grouped by supervised cluster K=5 #######
# Data frame of principal components ----------------------
df_pc_un_K_5 <- data.frame(pca_mod_un$x, cluster_K_5=viz_unsup$cluster_K_5)  # dataframe of principal components
df_pc_un_K_5_cluster1 <- df_pc_un_K_5[df_pc_un_K_5$cluster_K_5 == "1", ]  # df for 'cluster 1'
df_pc_un_K_5_cluster2 <- df_pc_un_K_5[df_pc_un_K_5$cluster_K_5 == "2", ]  # df for 'cluster 2'
df_pc_un_K_5_cluster3 <- df_pc_un_K_5[df_pc_un_K_5$cluster_K_5 == "3", ]  # df for 'cluster 3'
df_pc_un_K_5_cluster4 <- df_pc_un_K_5[df_pc_un_K_5$cluster_K_5 == "4", ]  # df for 'cluster 4'
df_pc_un_K_5_cluster5 <- df_pc_un_K_5[df_pc_un_K_5$cluster_K_5 == "5", ]  # df for 'cluster 5'

##### PCA grouped by supervised cluster K=6 #######
# Data frame of principal components ----------------------
df_pc_un_K_6 <- data.frame(pca_mod_un$x, cluster_K_6=viz_unsup$cluster_K_6)  # dataframe of principal components
df_pc_un_K_6_cluster1 <- df_pc_un_K_6[df_pc_un_K_6$cluster_K_6 == "1", ]  # df for 'cluster 1'
df_pc_un_K_6_cluster2 <- df_pc_un_K_6[df_pc_un_K_6$cluster_K_6 == "2", ]  # df for 'cluster 2'
df_pc_un_K_6_cluster3 <- df_pc_un_K_6[df_pc_un_K_6$cluster_K_6 == "3", ]  # df for 'cluster 3'
df_pc_un_K_6_cluster4 <- df_pc_un_K_6[df_pc_un_K_6$cluster_K_6 == "4", ]  # df for 'cluster 4'
df_pc_un_K_6_cluster5 <- df_pc_un_K_6[df_pc_un_K_6$cluster_K_6 == "5", ]  # df for 'cluster 5'
df_pc_un_K_6_cluster6 <- df_pc_un_K_6[df_pc_un_K_6$cluster_K_6 == "6", ]  # df for 'cluster 6'


#install.packages("patchwork")
library(patchwork)

# Plot ----------------------------------------------------
p1_bis <- ggplot(df_pc_un_K_2, aes(PC1, PC2, col= cluster_K_2, loadings=TRUE)) + 
  geom_point(aes(shape=cluster_K_2), size=2) +   # draw points
  labs(title="Unsupervised K-proto - 610 Observations grouped in 2 clusters", 
       subtitle="With principal components PC1 and PC2 as X and Y axis",
       caption="Source: UBIOPRED") + 
  coord_cartesian(xlim = 1.2 * c(min(df_pc_un_K_2$PC1), max(df_pc_un_K_2$PC1)), 
                  ylim = 1.2 * c(min(df_pc_un_K_2$PC2), max(df_pc_un_K_2$PC2))) +   # change axis limits
  geom_encircle(data = df_pc_un_K_2_cluster1, aes(x=PC1, y=PC2)) +   # draw circles
  geom_encircle(data = df_pc_un_K_2_cluster2, aes(x=PC1, y=PC2)) +
  xlab("PC 1 (7.617%)") + 
  
  ylab("PC 2 (4.317%)") 

p1_unsup <- print(p1_bis + labs(colour = "Clusters") + labs(shape="Clusters"))

# Plot ----------------------------------------------------
p2_bis <- ggplot(df_pc_un_K_3, aes(PC1, PC2, col=cluster_K_3)) + 
  geom_point(aes(shape=cluster_K_3), size=2) +   # draw points
  labs(title="Unsupervised K-proto - 610 Observations grouped in 3 clusters", 
       subtitle="With principal components PC1 and PC2 as X and Y axis",
       caption="Source: UBIOPRED") + 
  coord_cartesian(xlim = 1.2 * c(min(df_pc_un_K_3$PC1), max(df_pc_un_K_3$PC1)), 
                  ylim = 1.2 * c(min(df_pc_un_K_3$PC2), max(df_pc_un_K_3$PC2))) +   # change axis limits
  geom_encircle(data = df_pc_un_K_3_cluster1, aes(x=PC1, y=PC2)) +   # draw circles
  geom_encircle(data = df_pc_un_K_3_cluster2, aes(x=PC1, y=PC2)) + 
  geom_encircle(data = df_pc_un_K_3_cluster3, aes(x=PC1, y=PC2)) +
  xlab("PC 1 (7.617%)") + 
  
  ylab("PC 2 (4.317%)") 

p2_unsup <- print(p2_bis + labs(colour = "Clusters") + labs(shape="Clusters"))

# Plot ----------------------------------------------------
p3_bis <- ggplot(df_pc_un_K_4, aes(PC1, PC2, col=cluster_K_4)) + 
  geom_point(aes(shape=cluster_K_4), size=2) +   # draw points
  labs(title="Unsupervised K-proto - 610 Observations grouped in 4 clusters", 
       subtitle="With principal components PC1 and PC2 as X and Y axis",
       caption="Source: UBIOPRED") + 
  coord_cartesian(xlim = 1.2 * c(min(df_pc_un_K_4$PC1), max(df_pc_un_K_4$PC1)), 
                  ylim = 1.2 * c(min(df_pc_un_K_4$PC2), max(df_pc_un_K_4$PC2))) +   # change axis limits
  geom_encircle(data = df_pc_un_K_4_cluster1, aes(x=PC1, y=PC2)) +   # draw circles
  geom_encircle(data = df_pc_un_K_4_cluster2, aes(x=PC1, y=PC2)) + 
  geom_encircle(data = df_pc_un_K_4_cluster3, aes(x=PC1, y=PC2)) +
  geom_encircle(data = df_pc_un_K_4_cluster4, aes(x=PC1, y=PC2))+
  xlab("PC 1 (7.617%)") + 
  
  ylab("PC 2 (4.317%)") 

p3_unsup <- print(p3_bis + labs(colour = "Clusters") + labs(shape="Clusters"))

p1_unsup | p2_unsup /
  p3_unsup

ggsave("/rds/general/user/md2620/home/asthma/Malo/Results/PCA_Results_Unsup/K2_3_4.jpeg", width = 12, height = 6)

###### Silhouette scores graphs ###########
autoplot(silhouette(pam(df_pc_un_K_2, 2L)))
ggsave("/rds/general/user/md2620/home/asthma/Malo/Results/Silhouette_scores/K_2_unsup.jpeg", width = 12, height = 8)
autoplot(silhouette(pam(df_pc_un_K_3, 3L)))
ggsave("/rds/general/user/md2620/home/asthma/Malo/Results/Silhouette_scores/K_3_unsup.jpeg", width = 12, height = 8)
autoplot(silhouette(pam(df_pc_un_K_4, 4L)))
ggsave("/rds/general/user/md2620/home/asthma/Malo/Results/Silhouette_scores/K_4_unsup.jpeg", width = 12, height = 8)
autoplot(silhouette(pam(df_pc_un_K_5, 5L)))
ggsave("/rds/general/user/md2620/home/asthma/Malo/Results/Silhouette_scores/K_5_unsup.jpeg", width = 12, height = 8)
autoplot(silhouette(pam(df_pc_un_K_6, 6L)))
ggsave("/rds/general/user/md2620/home/asthma/Malo/Results/Silhouette_scores/K_6_unsup.jpeg", width = 12, height = 8)
autoplot(silhouette(pam(df_pc_un_K_12,12L)))
ggsave("/rds/general/user/md2620/home/asthma/Malo/Results/Silhouette_scores/K_12_unsup.jpeg", width = 12, height = 8)
### not run
#autoplot(pam(df_pc_un_K_2,2), frame = TRUE, frame.type = 'norm', label.size = 3,
#loadings = TRUE, loadings.label = TRUE, loadings.label.size  = 3)
###

library(patchwork)
dev.size()
# Plot ----------------------------------------------------
p4_bis <- ggplot(df_pc_un_K_5, aes(PC1, PC2, col=cluster_K_5)) + 
  geom_point(aes(shape=cluster_K_5), size=2) +   # draw points
  labs(title="Unsupervised K-proto - 610 Observations grouped in 5 clusters", 
       subtitle="With principal components PC1 and PC2 as X and Y axis",
       caption="Source: UBIOPRED") + 
  coord_cartesian(xlim = 1.2 * c(min(df_pc_un_K_5$PC1), max(df_pc_un_K_5$PC1)), 
                  ylim = 1.2 * c(min(df_pc_un_K_5$PC2), max(df_pc_un_K_5$PC2)))+
  xlab("PC 1 (7.617%)") + 
  
  ylab("PC 2 (4.317%)")    # change axis limits
#geom_encircle(data = df_pc_un_K_5_cluster1, aes(x=PC1, y=PC2)) +   # draw circles
#geom_encircle(data = df_pc_un_K_5_cluster2, aes(x=PC1, y=PC2)) + 
#geom_encircle(data = df_pc_un_K_5_cluster3, aes(x=PC1, y=PC2)) +
#geom_encircle(data = df_pc_un_K_5_cluster4, aes(x=PC1, y=PC2)) +
#geom_encircle(data = df_pc_un_K_5_cluster5, aes(x=PC1, y=PC2)) 

p4_unsup <- print(p4_bis + labs(colour = "Clusters") + labs(shape="Clusters"))

# Plot ----------------------------------------------------
p5_bis <- ggplot(df_pc_un_K_6, aes(PC1, PC2, col=cluster_K_6)) + 
  geom_point(aes(shape=cluster_K_6), size=2) +   # draw points
  labs(title="Unsupervised K-proto - 610 Observations grouped in 6 clusters", 
       subtitle="With principal components PC1 and PC2 as X and Y axis",
       caption="Source: UBIOPRED") + 
  coord_cartesian(xlim = 1.2 * c(min(df_pc_un_K_6$PC1), max(df_pc_un_K_6$PC1)), 
                  ylim = 1.2 * c(min(df_pc_un_K_6$PC2), max(df_pc_un_K_6$PC2)))+
  xlab("PC 1 (7.617%)") + 
  
  ylab("PC 2 (4.317%)")     # change axis limits
#geom_encircle(data = df_pc_un_K_6_cluster1, aes(x=PC1, y=PC2)) +   # draw circles
#geom_encircle(data = df_pc_un_K_6_cluster2, aes(x=PC1, y=PC2)) + 
#geom_encircle(data = df_pc_un_K_6_cluster3, aes(x=PC1, y=PC2)) +
#geom_encircle(data = df_pc_un_K_6_cluster4, aes(x=PC1, y=PC2)) +
#geom_encircle(data = df_pc_un_K_6_cluster5, aes(x=PC1, y=PC2)) +
#geom_encircle(data = df_pc_un_K_6_cluster6, aes(x=PC1, y=PC2))
p5_unsup <- print(p5_bis + labs(colour = "Clusters") + labs(shape="Clusters"))

p4_unsup + p5_unsup

ggsave("/rds/general/user/md2620/home/asthma/Malo/Results/PCA_Results_Unsup/K5_6.jpeg", width = 12, height = 6)

