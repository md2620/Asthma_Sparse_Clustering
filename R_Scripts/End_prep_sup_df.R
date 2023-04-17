###FINAL ADJUSTMENTS FOR VISUALIZING SUPERVISED CLUSTERS###

#Import datasets
df_sup_viz = read.csv("/rds/general/user/md2620/home/asthma/Malo/Dataframes/Final/df_sup_clusters_attached_viz.csv")

#Transform cluster numbers to 1,2,3,4 instead of 0,1,2,3
#Rename levels for clusters_K_4 in df_sup_viz
df_sup_viz$cluster_K_4 <- as.factor(df_sup_viz$cluster_K_4)
levels(df_sup_viz$cluster_K_4)
library(dplyr)
levels(df_sup_viz$cluster_K_4) <- c("1", "2", "3","4")

## SAVE DATASET UNDER ORIGINAL NAME ##
#Write to a csv
write.csv(df_sup_viz,"/rds/general/user/md2620/home/asthma/Malo/Dataframes/Final/df_sup_clusters_attached_viz.csv")
