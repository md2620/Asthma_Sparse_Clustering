#Preparation of unsupervised df for visualization in UMAP

#Import datasets
df_unsup = read.csv("/rds/general/user/md2620/home/asthma/Malo/Dataframes/Final/df_unsup_clusters_attached.csv")
#Merge dataset with clinical.csv to have your outcome variable / severity measure
outcome_df = read.csv("/rds/general/user/md2620/home/asthma/Data/UBIOPRED_CLINICAL.csv")

###### Rename X to patient in df_HTML_not_processed ######
names(df_unsup)[names(df_unsup) == "X"] <- "Patient"

####### NOW MERGE df_HTML_not_processed and df2 ########
df_unsup_final = merge(df_unsup,outcome_df, by="Patient")

#Set Subject.ID as index
df_unsup_final_2 <- df_unsup_final[-1]
row.names(df_unsup_final_2) <- df_unsup_final$Patient
View(df_unsup_final_2)

#Remove observations that belong to column cohort_v:
df_unsup_final_2 <- subset(df_unsup_final_2, cohort!="cohort_v")

#Create dummy variables for outcome in order to run logistic regressions (clusters against binary outcome)
df_unsup_final_2$Healthy <- ifelse(df_unsup_final_2$cohort == 'Healthy', "Yes", "No")
df_unsup_final_2$MildModerate <- ifelse(df_unsup_final_2$cohort == 'Mild/Moderate', "Yes", "No")
df_unsup_final_2$Severe <- ifelse(df_unsup_final_2$cohort == 'Severe', "Yes", "No")
df_unsup_final_2$Severe_Smoker <- ifelse(df_unsup_final_2$cohort == 'Severe_Smoker', "Yes", "No")
df_unsup_final_2$Severe_asthma <- ifelse(grepl('Severe', df_unsup_final_2$cohort),"Yes", "No")

#Drop the columns you don't want
df_unsup_final_2 = subset(df_unsup_final_2, select = -c(Subject,Sex,Sputum._Neutrophils,Age,Samples,Race,Blood.Eosinophils_.,Subset,Sputum.._Eosinophils,Trial,Blood.Neutrophils_.))

#Transform cluster numbers to 1,2,3 instead of 0,1,2
#Rename levels for clusters in df
df_unsup_final_2$cluster <- as.factor(df_unsup_final_2$cluster)
levels(df_unsup_final_2$cluster)
library(dplyr)
levels(df_unsup_final_2$cluster) <- c("1", "2", "3")

#Save dataframe for UMAP viz:
#Write to a csv
write.csv(df_unsup_final_2,"/rds/general/user/md2620/home/asthma/Malo/Dataframes/Final/df_unsup_clust_attached_viz.csv")
