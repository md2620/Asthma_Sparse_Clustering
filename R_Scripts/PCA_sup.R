##### PCA VISUALIZATION #####
#Visualization clusters characteristics

#Reading the files
viz_sup = read.csv("/rds/general/user/md2620/home/asthma/Malo/Dataframes/Final/sup_df_all_processed.csv")

#devtools::install_github("hrbrmstr/ggalt")
library(ggplot2)
library(ggalt)
#install.packages("ggfortify")
library(ggfortify)
theme_set(theme_classic())

# Compute data with principal components ------------------
PCA_sup_df <- viz_sup[c(1:610),c(2:115)]

#Set Subject.ID as index
PCA_sup_df_final <- PCA_sup_df[-1]
row.names(PCA_sup_df_final) <- PCA_sup_df$Unnamed..0
View(PCA_sup_df_final)

pca_mod <- prcomp(PCA_sup_df_final)  # compute principal components


##### PCA grouped by cohort #######
# Data frame of principal components ----------------------
df_pc <- data.frame(pca_mod$x, cohort=viz_sup$cohort)  # dataframe of principal components
df_pc_severe <- df_pc[df_pc$cohort == "Severe", ]  # df for 'severe'
df_pc_sev_smoker <- df_pc[df_pc$cohort == "Severe_Smoker", ]  # df for 'severe smoker'
df_pc_mild <- df_pc[df_pc$cohort == "Mild/Moderate", ]  # df for 'mild moderate'
df_pc_healthy <- df_pc[df_pc$cohort == "Healthy", ]  # df for 'healthy

# Plot ----------------------------------------------------
ggplot(df_pc, aes(PC1, PC2, col=cohort)) + 
  geom_point(aes(shape=cohort), size=2) +   # draw points
  labs(title="Observations grouped per severity form of asthma", 
       subtitle="With principal components PC1 and PC2 as X and Y axis",
       caption="Source: UBIOPRED") + 
  coord_cartesian(xlim = 1.2 * c(min(df_pc$PC1), max(df_pc$PC1)), 
                  ylim = 1.2 * c(min(df_pc$PC2), max(df_pc$PC2))) +   # change axis limits
  geom_encircle(data = df_pc_severe, aes(x=PC1, y=PC2)) +   # draw circles
  geom_encircle(data = df_pc_sev_smoker, aes(x=PC1, y=PC2)) + 
  geom_encircle(data = df_pc_mild, aes(x=PC1, y=PC2)) +
  geom_encircle(data = df_pc_healthy, aes(x=PC1, y=PC2))

ggsave("/rds/general/user/md2620/home/asthma/Malo/Results/PCA_Results_Sup/PCA_by_cohort.jpeg", width = 12, height = 6)


#Make sure clusters variables are factor variables
viz_sup$cluster_K_2 = as.factor(viz_sup$cluster_K_2)
viz_sup$cluster_K_3 = as.factor(viz_sup$cluster_K_3)
viz_sup$cluster_K_4 = as.factor(viz_sup$cluster_K_4)
viz_sup$cluster_K_5 = as.factor(viz_sup$cluster_K_5)
viz_sup$cluster_K_6 = as.factor(viz_sup$cluster_K_6)
viz_sup$cluster_K_7 = as.factor(viz_sup$cluster_K_7)
viz_sup$cluster_K_8 = as.factor(viz_sup$cluster_K_8)
viz_sup$cluster_K_9 = as.factor(viz_sup$cluster_K_9)
viz_sup$cluster_K_10 = as.factor(viz_sup$cluster_K_10)
viz_sup$cluster_K_11 = as.factor(viz_sup$cluster_K_11)
viz_sup$cluster_K_12 = as.factor(viz_sup$cluster_K_12)
viz_sup$cluster_K_13 = as.factor(viz_sup$cluster_K_13)
viz_sup$cluster_K_14 = as.factor(viz_sup$cluster_K_14)
viz_sup$cluster_K_15 = as.factor(viz_sup$cluster_K_15)

#Check levels
levels(viz_sup$cluster_K_11)

################## PCA K = 11 Impossible to run -- too many shapes ###################
##### PCA grouped by supervised cluster K=11 #######
# Data frame of principal components ----------------------
df_pc_K_11 <- data.frame(pca_mod$x, cluster_K_11=viz_sup$cluster_K_11)  # dataframe of principal components
df_pc_K_11_cluster1 <- df_pc_K_11[df_pc_K_11$cluster_K_11 == "1", ]  # df for 'cluster 1'
df_pc_K_11_cluster2 <- df_pc_K_11[df_pc_K_11$cluster_K_11 == "2", ]  # df for 'cluster 2'
df_pc_K_11_cluster3 <- df_pc_K_11[df_pc_K_11$cluster_K_11 == "3", ]  # df for 'cluster 3'
df_pc_K_11_cluster4 <- df_pc_K_11[df_pc_K_11$cluster_K_11 == "4", ]  # df for 'cluster 4'
df_pc_K_11_cluster5 <- df_pc_K_11[df_pc_K_11$cluster_K_11 == "5", ]  # df for 'cluster 5'
df_pc_K_11_cluster6 <- df_pc_K_11[df_pc_K_11$cluster_K_11 == "6", ]  # df for 'cluster 6'
df_pc_K_11_cluster7 <- df_pc_K_11[df_pc_K_11$cluster_K_11 == "7", ]  # df for 'cluster 7'
df_pc_K_11_cluster8 <- df_pc_K_11[df_pc_K_11$cluster_K_11 == "8", ]  # df for 'cluster 8'
df_pc_K_11_cluster9 <- df_pc_K_11[df_pc_K_11$cluster_K_11 == "9", ]  # df for 'cluster 9'
df_pc_K_11_cluster10 <- df_pc_K_11[df_pc_K_11$cluster_K_11 == "10", ]  # df for 'cluster 10'
df_pc_K_11_cluster11 <- df_pc_K_11[df_pc_K_11$cluster_K_11 == "11", ]  # df for 'cluster 11'

# Plot ----------------------------------------------------
ggplot(df_pc_K_11, aes(PC1, PC2, col=cluster_K_11)) + 
  geom_point(aes(shape=cluster_K_11), size=2) +   # draw points
  labs(title="Observations grouped in 11 clusters", 
       subtitle="With principal components PC1 and PC2 as X and Y axis",
       caption="Source: UBIOPRED") + 
  coord_cartesian(xlim = 1.2 * c(min(df_pc_K_11$PC1), max(df_pc_K_11$PC1)), 
                  ylim = 1.2 * c(min(df_pc_K_11$PC2), max(df_pc_K_11$PC2))) +   # change axis limits
  geom_encircle(data = df_pc_K_11_cluster1, aes(x=PC1, y=PC2)) +   # draw circles
  geom_encircle(data = df_pc_K_11_cluster2, aes(x=PC1, y=PC2)) + 
  geom_encircle(data = df_pc_K_11_cluster3, aes(x=PC1, y=PC2)) +
  geom_encircle(data = df_pc_K_11_cluster4, aes(x=PC1, y=PC2)) +
  geom_encircle(data = df_pc_K_11_cluster5, aes(x=PC1, y=PC2)) +
  geom_encircle(data = df_pc_K_11_cluster6, aes(x=PC1, y=PC2)) +
  geom_encircle(data = df_pc_K_11_cluster7, aes(x=PC1, y=PC2)) +
  geom_encircle(data = df_pc_K_11_cluster8, aes(x=PC1, y=PC2)) +
  geom_encircle(data = df_pc_K_11_cluster9, aes(x=PC1, y=PC2)) +
  geom_encircle(data = df_pc_K_11_cluster10, aes(x=PC1, y=PC2)) +
  geom_encircle(data = df_pc_K_11_cluster11, aes(x=PC1, y=PC2)) 


################### PCA FOR K=2:6 #####################
##### PCA grouped by supervised cluster K=2 #######
# Data frame of principal components ----------------------
df_pc_K_2 <- data.frame(pca_mod$x, cluster_K_2=viz_sup$cluster_K_2)  # dataframe of principal components
df_pc_K_2_cluster1 <- df_pc_K_2[df_pc_K_2$cluster_K_2 == "1", ]  # df for 'cluster 1'
df_pc_K_2_cluster2 <- df_pc_K_2[df_pc_K_2$cluster_K_2 == "2", ]  # df for 'cluster 2'

##### PCA grouped by supervised cluster K=3 #######
# Data frame of principal components ----------------------
df_pc_K_3 <- data.frame(pca_mod$x, cluster_K_3=viz_sup$cluster_K_3)  # dataframe of principal components
df_pc_K_3_cluster1 <- df_pc_K_3[df_pc_K_3$cluster_K_3 == "1", ]  # df for 'cluster 1'
df_pc_K_3_cluster2 <- df_pc_K_3[df_pc_K_3$cluster_K_3 == "2", ]  # df for 'cluster 2'
df_pc_K_3_cluster3 <- df_pc_K_3[df_pc_K_3$cluster_K_3 == "3", ]  # df for 'cluster 3'

##### PCA grouped by supervised cluster K=4 #######
# Data frame of principal components ----------------------
df_pc_K_4 <- data.frame(pca_mod$x, cluster_K_4=viz_sup$cluster_K_4)  # dataframe of principal components
df_pc_K_4_cluster1 <- df_pc_K_4[df_pc_K_4$cluster_K_4 == "1", ]  # df for 'cluster 1'
df_pc_K_4_cluster2 <- df_pc_K_4[df_pc_K_4$cluster_K_4 == "2", ]  # df for 'cluster 2'
df_pc_K_4_cluster3 <- df_pc_K_4[df_pc_K_4$cluster_K_4 == "3", ]  # df for 'cluster 3'
df_pc_K_4_cluster4 <- df_pc_K_4[df_pc_K_4$cluster_K_4 == "4", ]  # df for 'cluster 4'

##### PCA grouped by supervised cluster K=5 #######
# Data frame of principal components ----------------------
df_pc_K_5 <- data.frame(pca_mod$x, cluster_K_5=viz_sup$cluster_K_5)  # dataframe of principal components
df_pc_K_5_cluster1 <- df_pc_K_5[df_pc_K_5$cluster_K_5 == "1", ]  # df for 'cluster 1'
df_pc_K_5_cluster2 <- df_pc_K_5[df_pc_K_5$cluster_K_5 == "2", ]  # df for 'cluster 2'
df_pc_K_5_cluster3 <- df_pc_K_5[df_pc_K_5$cluster_K_5 == "3", ]  # df for 'cluster 3'
df_pc_K_5_cluster4 <- df_pc_K_5[df_pc_K_5$cluster_K_5 == "4", ]  # df for 'cluster 4'
df_pc_K_5_cluster5 <- df_pc_K_5[df_pc_K_5$cluster_K_5 == "5", ]  # df for 'cluster 5'

##### PCA grouped by supervised cluster K=5 #######
# Data frame of principal components ----------------------
df_pc_K_6 <- data.frame(pca_mod$x, cluster_K_6=viz_sup$cluster_K_6)  # dataframe of principal components
df_pc_K_6_cluster1 <- df_pc_K_6[df_pc_K_6$cluster_K_6 == "1", ]  # df for 'cluster 1'
df_pc_K_6_cluster2 <- df_pc_K_6[df_pc_K_6$cluster_K_6 == "2", ]  # df for 'cluster 2'
df_pc_K_6_cluster3 <- df_pc_K_6[df_pc_K_6$cluster_K_6 == "3", ]  # df for 'cluster 3'
df_pc_K_6_cluster4 <- df_pc_K_6[df_pc_K_6$cluster_K_6 == "4", ]  # df for 'cluster 4'
df_pc_K_6_cluster5 <- df_pc_K_6[df_pc_K_6$cluster_K_6 == "5", ]  # df for 'cluster 5'
df_pc_K_6_cluster6 <- df_pc_K_6[df_pc_K_6$cluster_K_6 == "6", ]  # df for 'cluster 5'


#install.packages("patchwork")
library(patchwork)

# Plot ----------------------------------------------------
p1 <- ggplot(df_pc_K_2, aes(PC1, PC2, col= cluster_K_2, loadings=TRUE)) + 
  geom_point(aes(shape=cluster_K_2), size=2) +   # draw points
  labs(title="Supervised K-proto - 610 Observations grouped in 2 clusters", 
       subtitle="With principal components PC1 and PC2 as X and Y axis",
       caption="Source: UBIOPRED") + 
  coord_cartesian(xlim = 1.2 * c(min(df_pc_K_2$PC1), max(df_pc_K_2$PC1)), 
                  ylim = 1.2 * c(min(df_pc_K_2$PC2), max(df_pc_K_2$PC2))) +   # change axis limits
  geom_encircle(data = df_pc_K_2_cluster1, aes(x=PC1, y=PC2)) +   # draw circles
  geom_encircle(data = df_pc_K_2_cluster2, aes(x=PC1, y=PC2)) 

p1_sup <- print(p1 + labs(colour = "Clusters") + labs(shape="Clusters"))

# Plot ----------------------------------------------------
p2 <- ggplot(df_pc_K_3, aes(PC1, PC2, col=cluster_K_3)) + 
  geom_point(aes(shape=cluster_K_3), size=2) +   # draw points
  labs(title="Supervised K-proto - 610 Observations grouped in 3 clusters", 
       subtitle="With principal components PC1 and PC2 as X and Y axis",
       caption="Source: UBIOPRED") + 
  coord_cartesian(xlim = 1.2 * c(min(df_pc_K_3$PC1), max(df_pc_K_3$PC1)), 
                  ylim = 1.2 * c(min(df_pc_K_3$PC2), max(df_pc_K_3$PC2))) +   # change axis limits
  geom_encircle(data = df_pc_K_3_cluster1, aes(x=PC1, y=PC2)) +   # draw circles
  geom_encircle(data = df_pc_K_3_cluster2, aes(x=PC1, y=PC2)) + 
  geom_encircle(data = df_pc_K_3_cluster3, aes(x=PC1, y=PC2)) 

p2_sup <- print(p2 + labs(colour = "Clusters") + labs(shape="Clusters"))

# Plot ----------------------------------------------------
p3 <- ggplot(df_pc_K_4, aes(PC1, PC2, col=cluster_K_4)) + 
  geom_point(aes(shape=cluster_K_4), size=2) +   # draw points
  labs(title="Supervised K-proto - 610 Observations grouped in 4 clusters", 
       subtitle="With principal components PC1 and PC2 as X and Y axis",
       caption="Source: UBIOPRED") + 
  coord_cartesian(xlim = 1.2 * c(min(df_pc_K_4$PC1), max(df_pc_K_4$PC1)), 
                  ylim = 1.2 * c(min(df_pc_K_4$PC2), max(df_pc_K_4$PC2))) +   # change axis limits
  geom_encircle(data = df_pc_K_4_cluster1, aes(x=PC1, y=PC2)) +   # draw circles
  geom_encircle(data = df_pc_K_4_cluster2, aes(x=PC1, y=PC2)) + 
  geom_encircle(data = df_pc_K_4_cluster3, aes(x=PC1, y=PC2)) +
  geom_encircle(data = df_pc_K_4_cluster4, aes(x=PC1, y=PC2))

p3_sup <- print(p3 + labs(colour = "Clusters") + labs(shape="Clusters"))

p1_sup | p2_sup /
  p3_sup

ggsave("/rds/general/user/md2620/home/asthma/Malo/Results/PCA_Results_Sup/K2_3_4.jpeg", width = 12, height = 6)

###### Silhouette scores graphs ###########
autoplot(silhouette(pam(df_pc_K_2, 2L)))
ggsave("/rds/general/user/md2620/home/asthma/Malo/Results/Silhouette_scores/K_2_sup.jpeg", width = 12, height = 8)
autoplot(silhouette(pam(df_pc_K_3, 3L)))
ggsave("/rds/general/user/md2620/home/asthma/Malo/Results/Silhouette_scores/K_3_sup.jpeg", width = 12, height = 8)
autoplot(silhouette(pam(df_pc_K_4, 4L)))
ggsave("/rds/general/user/md2620/home/asthma/Malo/Results/Silhouette_scores/K_4_sup.jpeg", width = 12, height = 8)
autoplot(silhouette(pam(df_pc_K_5, 5L)))
ggsave("/rds/general/user/md2620/home/asthma/Malo/Results/Silhouette_scores/K_5_sup.jpeg", width = 12, height = 8)
autoplot(silhouette(pam(df_pc_K_6, 6L)))
ggsave("/rds/general/user/md2620/home/asthma/Malo/Results/Silhouette_scores/K_6_sup.jpeg", width = 12, height = 8)
autoplot(silhouette(pam(df_pc_K_11,11L)))
ggsave("/rds/general/user/md2620/home/asthma/Malo/Results/Silhouette_scores/K_11_sup.jpeg", width = 12, height = 8)
### not run
#autoplot(pam(df_pc_K_2,2), frame = TRUE, frame.type = 'norm', label.size = 3,
         #loadings = TRUE, loadings.label = TRUE, loadings.label.size  = 3)
###

library(patchwork)
dev.size()
# Plot ----------------------------------------------------
p4 <- ggplot(df_pc_K_5, aes(PC1, PC2, col=cluster_K_5)) + 
  geom_point(aes(shape=cluster_K_5), size=2) +   # draw points
  labs(title="Supervised K-proto - 610 Observations grouped in 5 clusters", 
       subtitle="With principal components PC1 and PC2 as X and Y axis",
       caption="Source: UBIOPRED") + 
  coord_cartesian(xlim = 1.2 * c(min(df_pc_K_5$PC1), max(df_pc_K_5$PC1)), 
                  ylim = 1.2 * c(min(df_pc_K_5$PC2), max(df_pc_K_5$PC2)))   # change axis limits
#geom_encircle(data = df_pc_K_5_cluster1, aes(x=PC1, y=PC2)) +   # draw circles
#geom_encircle(data = df_pc_K_5_cluster2, aes(x=PC1, y=PC2)) + 
#geom_encircle(data = df_pc_K_5_cluster3, aes(x=PC1, y=PC2)) +
#geom_encircle(data = df_pc_K_5_cluster4, aes(x=PC1, y=PC2)) +
#geom_encircle(data = df_pc_K_5_cluster5, aes(x=PC1, y=PC2)) 

p4_sup <- print(p4 + labs(colour = "Clusters") + labs(shape="Clusters"))

# Plot ----------------------------------------------------
p5 <- ggplot(df_pc_K_6, aes(PC1, PC2, col=cluster_K_6)) + 
  geom_point(aes(shape=cluster_K_6), size=2) +   # draw points
  labs(title="Supervised K-proto - 610 Observations grouped in 6 clusters", 
       subtitle="With principal components PC1 and PC2 as X and Y axis",
       caption="Source: UBIOPRED") + 
  coord_cartesian(xlim = 1.2 * c(min(df_pc_K_6$PC1), max(df_pc_K_6$PC1)), 
                  ylim = 1.2 * c(min(df_pc_K_6$PC2), max(df_pc_K_6$PC2)))    # change axis limits
#geom_encircle(data = df_pc_K_6_cluster1, aes(x=PC1, y=PC2)) +   # draw circles
#geom_encircle(data = df_pc_K_6_cluster2, aes(x=PC1, y=PC2)) + 
#geom_encircle(data = df_pc_K_6_cluster3, aes(x=PC1, y=PC2)) +
#geom_encircle(data = df_pc_K_6_cluster4, aes(x=PC1, y=PC2)) +
#geom_encircle(data = df_pc_K_6_cluster5, aes(x=PC1, y=PC2)) +
#geom_encircle(data = df_pc_K_6_cluster6, aes(x=PC1, y=PC2))
p5_sup <- print(p5 + labs(colour = "Clusters") + labs(shape="Clusters"))

p4_sup + p5_sup

ggsave("/rds/general/user/md2620/home/asthma/Malo/Results/PCA_Results_Sup/K5_6.jpeg", width = 12, height = 6)



######################## DRAFT ###########################
#Load ggplot2
library(ggplot2)

p <- ggplot(viz_sup, aes(y = Height.cm , x = d, xmin = ci.low, xmax = ci.high, shape=cluster_K_4)) +
  geom_point() +
  geom_errorbarh(height = .1) +
  scale_x_continuous(limits=c(-2,2),breaks=c(-2,-1.5,-1,-0.5,0,.5,1,1.5,2))+
  geom_vline(xintercept=0, color="grey60",linetype="dashed")+
  facet_grid(cohort ~ ., scales = "free", space = "free") +
  theme_bw() +
  theme(strip.text.y = element_text(angle = 0))
p


ggplot(viz_sup, aes(x = cohort , y= Omics.Xylose)) + geom_boxplot(outlier.colour='red')

viz_sup$cluster_K_4 <- as.factor(viz_sup$cluster_K_4)

ggplot(viz_sup) +
  geom_point(aes(x = cohort , y= Omics.Xylose)) + geom_boxplot()


, color = cluster_K_4,
shape=cluster_K_4))

ggplot(viz_sup) +
  geom_point(aes(x = cluster_K_4 , y= Omics.Xylose, color = cohort,
                 shape=cohort)) + 
  geom_boxplot()

ggplot(viz_sup, aes(x=reorder(cluster_K_6,Height.cm,  FUN=mean, y=Height.cm)))
