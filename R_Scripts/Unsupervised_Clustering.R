##### RUN KPROTO SUPERVISED WITH NUM VARIABLES PROCESSED ######
#Clear environment
rm(list=ls())

final_df_unsup_import = read.csv("/rds/general/user/md2620/home/asthma/Malo/Dataframes/Final/df_unsup_num_processed.csv")

#Set Subject.ID as index
final_df_un <- final_df_unsup_import[-1]
row.names(final_df_un) <- final_df_unsup_import$Unnamed..0
View(final_df_un)

#Remove outcome variable cohort before running k-prototypes algo
final_df_un = subset(final_df_un, select = -c(cohort))

# drop all variables that have 0 variance
final_df_un = subset(final_df_un, select = -c(Omics.Furoylglycine,Omics.Glutamic.acid,Omics.N.Acetylglutamic.acid,
                                                      Omics.Phenylalanine,Omics.Allantoin,Omics.Isoleucine,Omics.N.Acetylputrescine,
                                                      Omics.Sarcosine,Omics.Cytosine,Omics.Lysine,Omics.N.Methyl.D.aspartic.acid,
                                                      Omics.Xanthosine,Omics.Glucosamine,Omics.Maltose,Omics.O.Acetylserine,
                                                      Omics.Xylose))

# apply k prototypes on final_df_un - use 2 to 15 clusters
# Add nstart = 5 to change initialization
kprot_unsup2 <- kproto(final_df_un, 2, keep.data=TRUE, na.rm = TRUE)
kprot_unsup3 <- kproto(final_df_un, 3, keep.data=TRUE, na.rm = TRUE)
kprot_unsup3.1 <- kproto(final_df_un, 3, keep.data=TRUE, lambda = 3.1, na.rm = FALSE)
kprot_unsup4 <- kproto(final_df_un, 4, keep.data=TRUE, na.rm = TRUE)
kprot_unsup5 <- kproto(final_df_un, 5, keep.data=TRUE, na.rm = TRUE)
kprot_unsup6 <- kproto(final_df_un, 6, keep.data=TRUE, na.rm = TRUE)
kprot_unsup7 <- kproto(final_df_un, 7, keep.data=TRUE, na.rm = TRUE)
kprot_unsup8 <- kproto(final_df_un, 8, keep.data=TRUE, na.rm = TRUE)
kprot_unsup9 <- kproto(final_df_un, 9, keep.data=TRUE, na.rm = TRUE)
kprot_unsup10 <- kproto(final_df_un, 10, keep.data=TRUE, na.rm = TRUE)
kprot_unsup11 <- kproto(final_df_un, 11, keep.data=TRUE, na.rm = TRUE)
kprot_unsup12 <- kproto(final_df_un, 12, keep.data=TRUE, na.rm = TRUE)
kprot_unsup13 <- kproto(final_df_un, 13, keep.data=TRUE, na.rm = TRUE)
kprot_unsup14 <- kproto(final_df_un, 14, keep.data=TRUE, na.rm = TRUE)
kprot_unsup15 <- kproto(final_df_un, 15, keep.data=TRUE, na.rm = TRUE)

#Transform to matrix the dataframe, also it can be handled by intCriteria function
final_df_un_matrix = data.matrix(final_df_un)

results_kprot_unsup2 = intCriteria(final_df_un_matrix,kprot_unsup2$cluster,"all")
df_results_kprot_unsup2 = as.data.frame(results_kprot_unsup2,row.names = "2")
df_results_kprot_unsup2$K <- row.names(df_results_kprot_unsup2)

results_kprot_unsup3 = intCriteria(final_df_un_matrix,kprot_unsup3$cluster,"all")
df_results_kprot_unsup3 = as.data.frame(results_kprot_unsup3,row.names = "3")
df_results_kprot_unsup3$K <- row.names(df_results_kprot_unsup3)

#Append together df_results_kpres_2 and df_results_kpres_3
df_results_kprot_2_3_un = dplyr::bind_rows(df_results_kprot_unsup2,df_results_kprot_unsup3)

results_kprot_unsup4 = intCriteria(final_df_un_matrix,kprot_unsup4$cluster,"all")
df_results_kprot_unsup4 = as.data.frame(results_kprot_unsup4,row.names = "4")
df_results_kprot_unsup4$K <- row.names(df_results_kprot_unsup4)

#Append together df_results_kprot_2_3 and df_results_kprot_unsup4
df_results_kprot_2_3_4_un = dplyr::bind_rows(df_results_kprot_2_3_un,df_results_kprot_unsup4)

results_kprot_unsup5 = intCriteria(final_df_un_matrix,kprot_unsup5$cluster,"all")
df_results_kprot_unsup5 = as.data.frame(results_kprot_unsup5,row.names = "5")
df_results_kprot_unsup5$K <- row.names(df_results_kprot_unsup5)

#Append together df_results_kprot_2_3_4 and df_results_kprot_5
df_results_kprot_2_to_5_un = dplyr::bind_rows(df_results_kprot_2_3_4_un,df_results_kprot_unsup5)

results_kprot_unsup6 = intCriteria(final_df_un_matrix,kprot_unsup6$cluster,"all")
df_results_kprot_unsup6 = as.data.frame(results_kprot_unsup6,row.names = "6")
df_results_kprot_unsup6$K <- row.names(df_results_kprot_unsup6)

#Append together df_results_kprot_2_to_5 and df_results_kprot_unsup6
df_results_kprot_2_to_6_un = dplyr::bind_rows(df_results_kprot_2_to_5_un,df_results_kprot_unsup6)

results_kprot_unsup7 = intCriteria(final_df_un_matrix,kprot_unsup7$cluster,"all")
df_results_kprot_unsup7 = as.data.frame(results_kprot_unsup7,row.names = "7")
df_results_kprot_unsup7$K <- row.names(df_results_kprot_unsup7)

#Append together df_results_kprot_2_to_6 and df_results_kprot_unsup7
df_results_kprot_2_to_7_un = dplyr::bind_rows(df_results_kprot_2_to_6_un,df_results_kprot_unsup7)

results_kprot_unsup8 = intCriteria(final_df_un_matrix,kprot_unsup8$cluster,"all")
df_results_kprot_unsup8 = as.data.frame(results_kprot_unsup8,row.names = "8")
df_results_kprot_unsup8$K <- row.names(df_results_kprot_unsup8)

#Append together df_results_kprot_2_to_7 and df_results_kprot_unsup8
df_results_kprot_2_to_8_un = dplyr::bind_rows(df_results_kprot_2_to_7_un,df_results_kprot_unsup8)

results_kprot_unsup9 = intCriteria(final_df_un_matrix,kprot_unsup9$cluster,"all")
df_results_kprot_unsup9 = as.data.frame(results_kprot_unsup9,row.names = "9")
df_results_kprot_unsup9$K <- row.names(df_results_kprot_unsup9)

#Append together df_results_kprot_2_to_8 and df_results_kprot_unsup9
df_results_kprot_2_to_9_un = dplyr::bind_rows(df_results_kprot_2_to_8_un,df_results_kprot_unsup9)

results_kprot_unsup10 = intCriteria(final_df_un_matrix,kprot_unsup10$cluster,"all")
df_results_kprot_unsup10 = as.data.frame(results_kprot_unsup10,row.names = "10")
df_results_kprot_unsup10$K <- row.names(df_results_kprot_unsup10)

#Append together df_results_kprot_2_to_9 and df_results_kprot_unsup10
df_results_kprot_2_to_10_un = dplyr::bind_rows(df_results_kprot_2_to_9_un,df_results_kprot_unsup10)

results_kprot_unsup11 = intCriteria(final_df_un_matrix,kprot_unsup11$cluster,"all")
df_results_kprot_unsup11 = as.data.frame(results_kprot_unsup11,row.names = "11")
df_results_kprot_unsup11$K <- row.names(df_results_kprot_unsup11)

#Append together df_results_kprot_2_to_10 and df_results_kprot_unsup11
df_results_kprot_2_to_11_un = dplyr::bind_rows(df_results_kprot_2_to_10_un,df_results_kprot_unsup11)

results_kprot_unsup12 = intCriteria(final_df_un_matrix,kprot_unsup12$cluster,"all")
df_results_kprot_unsup12 = as.data.frame(results_kprot_unsup12,row.names = "12")
df_results_kprot_unsup12$K <- row.names(df_results_kprot_unsup12)

#Append together df_results_kprot_2_to_11 and df_results_kprot_unsup12
df_results_kprot_2_to_12_un = dplyr::bind_rows(df_results_kprot_2_to_11_un,df_results_kprot_unsup12)

results_kprot_unsup13 = intCriteria(final_df_un_matrix,kprot_unsup13$cluster,"all")
df_results_kprot_unsup13 = as.data.frame(results_kprot_unsup13,row.names = "13")
df_results_kprot_unsup13$K <- row.names(df_results_kprot_unsup13)

#Append together df_results_kprot_2_to_12 and df_results_kprot_unsup13
df_results_kprot_2_to_13_un = dplyr::bind_rows(df_results_kprot_2_to_12_un,df_results_kprot_unsup13)

results_kprot_unsup14 = intCriteria(final_df_un_matrix,kprot_unsup14$cluster,"all")
df_results_kprot_unsup14 = as.data.frame(results_kprot_unsup14,row.names = "14")
df_results_kprot_unsup14$K <- row.names(df_results_kprot_unsup14)

#Append together df_results_kprot_2_to_13 and df_results_kprot_unsup14
df_results_kprot_2_to_14_un = dplyr::bind_rows(df_results_kprot_2_to_13_un,df_results_kprot_unsup14)

results_kprot_unsup15 = intCriteria(final_df_un_matrix,kprot_unsup15$cluster,"all")
df_results_kprot_unsup15 = as.data.frame(results_kprot_unsup15,row.names = "15")
df_results_kprot_unsup15$K <- row.names(df_results_kprot_unsup15)

#Append together df_results_kprot_2_to_14 and df_results_kprot_unsup15
df_results_kprot_2_to_15_un = dplyr::bind_rows(df_results_kprot_2_to_14_un,df_results_kprot_unsup15)

## Dataframe is ready to be analyzed --> Choose optimal number of K ##
#################### Plot C_Index #####################
pdf(file = "/rds/general/user/md2620/home/asthma/Malo/Results/Optimal_K_unsup/Cindex.pdf",   # The directory you want to save the file in
    width = 8, # The width of the plot in inches
    height = 8)
plot(df_results_kprot_2_to_15_un$K,df_results_kprot_2_to_15_un$c_index, frame = TRUE, 
     col = "black",
     type = "o",
     pch = 21,
     bg = ifelse(df_results_kprot_2_to_15_un$c_index == min(df_results_kprot_2_to_15_un$c_index), "red", "black"),
     xlab="Number of clusters K",
     ylab="C Index")
legend("topright", legend = "optimum criterion : min")
title(main = "C Index for unsupervised K-prototypes")
dev.off()
#######################################################

#################### Plot Calinski-Harabasz score #####################
pdf(file = "/rds/general/user/md2620/home/asthma/Malo/Results/Optimal_K_unsup/Calinski_Harabasz.pdf",   # The directory you want to save the file in
    width = 10, # The width of the plot in inches
    height = 8)
plot(df_results_kprot_2_to_15_un$K,df_results_kprot_2_to_15_un$calinski_harabasz, frame = TRUE, 
     col = "black",
     type = "o",
     pch = 21,
     bg = ifelse(df_results_kprot_2_to_15_un$calinski_harabasz == max(df_results_kprot_2_to_15_un$calinski_harabasz), "red", "black"),
     xlab="Number of clusters K",
     ylab="Calinski-Harabasz score")
legend("topright", legend = "optimum criterion : max")
title(main = "Finding optimal number of K using Calinski-Harabasz score - Huang initialization method")
dev.off()
#######################################################

#################### Plot Dunn score #####################
pdf(file = "/rds/general/user/md2620/home/asthma/Malo/Results/Optimal_K_unsup/Dunn.pdf",   # The directory you want to save the file in
    width = 8, # The width of the plot in inches
    height = 8)

plot(df_results_kprot_2_to_15_un$K,df_results_kprot_2_to_15_un$dunn, frame = TRUE, 
     col = "black",
     type = "o",
     pch = 21,
     bg = ifelse(df_results_kprot_2_to_15_un$dunn == max(df_results_kprot_2_to_15_un$dunn), "red", "black"),
     xlab="Number of clusters K",
     ylab="Dunn score")
legend("topleft", legend = "optimum criterion : max")
title(main = "Dunn score for unsupervised K-prototypes")

dev.off()
#######################################################

#################### Plot Gamma score #####################
pdf(file = "/rds/general/user/md2620/home/asthma/Malo/Results/Optimal_K_unsup/gamma.pdf",   # The directory you want to save the file in
    width = 8, # The width of the plot in inches
    height = 8)

plot(df_results_kprot_2_to_15_un$K,df_results_kprot_2_to_15_un$gamma, frame = TRUE, 
     col = "black",
     type = "o",
     pch = 21,
     bg = ifelse(df_results_kprot_2_to_15_un$gamma == max(df_results_kprot_2_to_15_un$gamma), "red", "black"),
     xlab="Number of clusters K",
     ylab="Gamma score")
legend("topleft", legend = "optimum criterion : max")
title(main = "Gamma score for unsupervised K-prototypes")

dev.off()
#######################################################


#################### Plot Gplus score #####################
pdf(file = "/rds/general/user/md2620/home/asthma/Malo/Results/Optimal_K_unsup/gplus.pdf",   # The directory you want to save the file in
    width = 8, # The width of the plot in inches
    height = 8)

plot(df_results_kprot_2_to_15_un$K,df_results_kprot_2_to_15_un$g_plus, frame = TRUE, 
     col = "black",
     type = "o",
     pch = 21,
     bg = ifelse(df_results_kprot_2_to_15_un$g_plus == max(df_results_kprot_2_to_15_un$g_plus), "red", "black"),
     xlab="Number of clusters K",
     ylab="Gplus score")
legend("topright", legend = "optimum criterion : max")
title(main = "Gplus score for unsupervised K-prototypes")

dev.off()
#######################################################

#################### Plot mcclain score #####################
pdf(file = "/rds/general/user/md2620/home/asthma/Malo/Results/Optimal_K_unsup/mcclain.pdf",   # The directory you want to save the file in
    width = 8, # The width of the plot in inches
    height = 8)

plot(df_results_kprot_2_to_15_un$K,df_results_kprot_2_to_15_un$mcclain_rao, frame = TRUE, 
     col = "black",
     type = "o",
     pch = 21,
     bg = ifelse(df_results_kprot_2_to_15_un$mcclain_rao == min(df_results_kprot_2_to_15_un$mcclain_rao), "red", "black"),
     xlab="Number of clusters K",
     ylab="Mcclain score")
legend("topright", legend = "optimum criterion : min")
title(main = "Mcclain score for unsupervised K-prototypes")

dev.off()
#######################################################

#################### Plot point biserial score #####################
pdf(file = "/rds/general/user/md2620/home/asthma/Malo/Results/Optimal_K_unsup/ptbiserial.pdf",   # The directory you want to save the file in
    width = 8, # The width of the plot in inches
    height = 8)

plot(df_results_kprot_2_to_15_un$K,df_results_kprot_2_to_15_un$point_biserial, frame = TRUE, 
     col = "black",
     type = "o",
     pch = 21,
     bg = ifelse(df_results_kprot_2_to_15_un$point_biserial == max(df_results_kprot_2_to_15_un$point_biserial), "red", "black"),
     xlab="Number of clusters K",
     ylab="Point Biserial score")
legend("topright", legend = "optimum criterion : max")
title(main = "Point Biserial score for unsupervised K-prototypes")

dev.off()
#######################################################

#################### Plot tau score #####################
pdf(file = "/rds/general/user/md2620/home/asthma/Malo/Results/Optimal_K_unsup/tau.pdf",   # The directory you want to save the file in
    width = 8, # The width of the plot in inches
    height = 8)

plot(df_results_kprot_2_to_15_un$K,df_results_kprot_2_to_15_un$tau, frame = TRUE, 
     col = "black",
     type = "o",
     pch = 21,
     bg = ifelse(df_results_kprot_2_to_15_un$tau == max(df_results_kprot_2_to_15_un$tau), "red", "black"),
     xlab="Number of clusters K",
     ylab="Tau score")
legend("topright", legend = "optimum criterion : max")
title(main = "Tau score for unsupervised K-prototypes")

dev.off()
#######################################################

#################### Plot silhouette score #####################
pdf(file = "/rds/general/user/md2620/home/asthma/Malo/Results/Optimal_K_unsup/silhouette.pdf",   # The directory you want to save the file in
    width = 8, # The width of the plot in inches
    height = 8)

plot(df_results_kprot_2_to_15_un$K,df_results_kprot_2_to_15_un$silhouette, frame = TRUE, 
     col = "black",
     type = "o",
     pch = 21,
     bg = ifelse(df_results_kprot_2_to_15_un$silhouette == max(df_results_kprot_2_to_15_un$silhouette), "red", "black"),
     xlab="Number of clusters K",
     ylab="Silhouette score")
legend("topright", legend = "optimum criterion : max")
title(main = "Silhouette score for unsupervised K-prototypes")

dev.off()
#######################################################

#Go back to final_df_un before you removed cohort variable and re-run the lines of code
final_df_un <- final_df_unsup_import[-1]
row.names(final_df_un) <- final_df_unsup_import$Unnamed..0
View(final_df_un)

# drop all variables that have 0 variance
final_df_un = subset(final_df_un, select = -c(Omics.Furoylglycine,Omics.Glutamic.acid,Omics.N.Acetylglutamic.acid,
                                              Omics.Phenylalanine,Omics.Allantoin,Omics.Isoleucine,Omics.N.Acetylputrescine,
                                              Omics.Sarcosine,Omics.Cytosine,Omics.Lysine,Omics.N.Methyl.D.aspartic.acid,
                                              Omics.Xanthosine,Omics.Glucosamine,Omics.Maltose,Omics.O.Acetylserine,
                                              Omics.Xylose))

####### OPTIMAL NUMBER OF CLUSTERS SEEMS TO BE 2 or 11 ######
####### Attach them all

#Append clusters number to final_df_un --> K=2
final_df_un$cluster_K_2 <- factor(kprot_unsup2$cluster)

#Append clusters number to final_df_un --> K=3
final_df_un$cluster_K_3 <- factor(kprot_unsup3$cluster)

#Append clusters number to final_df_un --> K=3 - OPTIMAL
final_df_un$cluster_optimal <- factor(kprot_unsup3.1$cluster)

#Append clusters number to final_df_un --> K=4
final_df_un$cluster_K_4 <- factor(kprot_unsup4$cluster)

#Append clusters number to final_df_un --> K=5
final_df_un$cluster_K_5 <- factor(kprot_unsup5$cluster)

#Append clusters number to final_df_un --> K=6
final_df_un$cluster_K_6 <- factor(kprot_unsup6$cluster)

#Append clusters number to final_df_un --> K=7
final_df_un$cluster_K_7 <- factor(kprot_unsup7$cluster)

#Append clusters number to final_df_un --> K=8
final_df_un$cluster_K_8 <- factor(kprot_unsup8$cluster)

#Append clusters number to final_df_un --> K=9
final_df_un$cluster_K_9 <- factor(kprot_unsup9$cluster)

#Append clusters number to final_df_un --> K=10
final_df_un$cluster_K_10 <- factor(kprot_unsup10$cluster)

#Append clusters number to final_df_un --> K=11
final_df_un$cluster_K_11 <- factor(kprot_unsup11$cluster)

#Append clusters number to final_df_un --> K=12
final_df_un$cluster_K_12 <- factor(kprot_unsup12$cluster)

#Append clusters number to final_df_un --> K=13
final_df_un$cluster_K_13 <- factor(kprot_unsup13$cluster)

#Append clusters number to final_df_un --> K=14
final_df_un$cluster_K_14 <- factor(kprot_unsup14$cluster)

#Append clusters number to final_df_un --> K=15
final_df_un$cluster_K_15 <- factor(kprot_unsup15$cluster)

#Create dummy variables for outcome in order to run logistic regressions (clusters against binary outcome)
final_df_un$Healthy <- ifelse(final_df_un$cohort == 'Healthy', "Yes", "No")
final_df_un$MildModerate <- ifelse(final_df_un$cohort == 'Mild/Moderate', "Yes", "No")
final_df_un$Severe <- ifelse(final_df_un$cohort == 'Severe', "Yes", "No")
final_df_un$Severe_Smoker <- ifelse(final_df_un$cohort == 'Severe_Smoker', "Yes", "No")
final_df_un$Severe_asthma <- ifelse(grepl('Severe', final_df_un$cohort),"Yes", "No")

## Make sure clusters variables are set as factor variables
final_df_un$cluster_K_2 = as.factor(final_df_un$cluster_K_2)
final_df_un$cluster_K_3 = as.factor(final_df_un$cluster_K_3)
final_df_un$cluster_optimal = as.factor(final_df_un$cluster_optimal)
final_df_un$cluster_K_4 = as.factor(final_df_un$cluster_K_4)
final_df_un$cluster_K_5 = as.factor(final_df_un$cluster_K_5)
final_df_un$cluster_K_6 = as.factor(final_df_un$cluster_K_6)
final_df_un$cluster_K_7 = as.factor(final_df_un$cluster_K_7)
final_df_un$cluster_K_8 = as.factor(final_df_un$cluster_K_8)
final_df_un$cluster_K_9 = as.factor(final_df_un$cluster_K_9)
final_df_un$cluster_K_10 = as.factor(final_df_un$cluster_K_10)
final_df_un$cluster_K_11 = as.factor(final_df_un$cluster_K_11)
final_df_un$cluster_K_12 = as.factor(final_df_un$cluster_K_12)
final_df_un$cluster_K_13 = as.factor(final_df_un$cluster_K_13)
final_df_un$cluster_K_14 = as.factor(final_df_un$cluster_K_14)
final_df_un$cluster_K_15 = as.factor(final_df_un$cluster_K_15)
final_df_un$Healthy = as.factor(final_df_un$Healthy)
final_df_un$MildModerate = as.factor(final_df_un$MildModerate)
final_df_un$Severe = as.factor(final_df_un$Severe)
final_df_un$Severe_Smoker = as.factor(final_df_un$Severe_Smoker)
final_df_un$Severe_asthma = as.factor(final_df_un$Severe_asthma)

#Check class of each variable before saving dataframe
sapply(final_df_un,class)

###################### SAVE final_df for supervision results ###################
#Write to a csv
write.csv(final_df_un,"/rds/general/user/md2620/home/asthma/Malo/Dataframes/Final/unsup_df.csv")

########### POSSIBLE TO HAVE ONLY TAU SCORE,GPLUS,GAMMA AND DUNN INDICES WHEN CAT VARIABLES HAVE NAS #####
########### POSSIBLE TO RE-RUN WITH FINAL UNSUPERVISED DATASET LABELED AND SCALED IF YOU WANT TO HAVE REMAINING INDICES####

########################### RESULTS PART #########################
#Optimal K seems to be either 2 or 12.
#Visualize results
library(clusterCrit)
library(patchwork)
source(clprofiles2)
#K = 2
par(mfrow=c(2,3))
clprofiles(kprot_unsup2,final_df_un)

#K = 3 and lambda = 3.1
par(mfrow=c(2,3))
clprofiles(kprot_unsup3.1,final_df_un)
clprofiles2(kprot_unsup3.1,final_df_un)
#K = 5
par(mfrow=c(1,3))
clprofiles(kprot_unsup5,final_df_un)

#K = 12
par(mfrow=c(1,1))
source("clprofiles2.R")
clprofiles2(kprot_unsup11,final_df_un)


#Based on results from shap plots, print the 20 variables that differ the most across clusters from optimal unsup Kproto using boxplots

#Load dataset
df_calib = read.csv("/rds/general/user/md2620/home/asthma/Malo/Dataframes/Final/df_unsup_not_processed.csv")

#Set Subject.ID as index
df_calibration <- df_calib[-1]
row.names(df_calibration) <- df_calib$X
View(df_calibration)

#Append clusters number to df_calibration --> K=3 - OPTIMAL
df_calibration$cluster_optimal <- factor(kprot_unsup3.1$cluster)

###################### SAVE final_df for supervision results ###################
#Write to a csv
write.csv(df_calibration,"/rds/general/user/md2620/home/asthma/Malo/Dataframes/Final/df_for_results_unsup_Oct24.csv")


#Load df_calibration
df_calibration = read.csv("/rds/general/user/md2620/home/asthma/Malo/Dataframes/Final/df_for_results_unsup_Oct24.csv")

#Set Subject.ID as index
final_df_un <- final_df_unsup_import[-1]
row.names(final_df_un) <- final_df_unsup_import$Unnamed..0
View(final_df_un)

#Plot the 20 most important variables as boxplot
p1 <- ggplot(df_calibration, aes(cluster_optimal, Emphysema.Or.Copd, fill=cluster_optimal)) +      
    geom_boxplot() +
    theme_gray()+
    theme(title = element_text(size=12, face='bold'),
          axis.title.y = element_text(color='black', face='plain'), 
          axis.title.x = element_text(color='black', face='plain'))+
    labs(title = "Emphysema or COPD per cluster - Optimal unsupervised Kprototype",fill = "cluster") +
    xlab("cluster") + 
    ylab("Emphysema or COPD") 
p1_done <- p1 + geom_boxplot(outlier.colour = "red", outlier.shape = 1)
p1_done

ggplot(data = df_calibration, aes(x = Emphysema.Or.Copd, fill = cluster_optimal)) +
    geom_bar(position = "fill") + ylab("proportion") +
    stat_count(geom = "text", 
               aes(label = stat(count)),
               position="fill", colour="white")

p1_cat <- ggplot(data = df_calibration, aes(x = cluster_optimal, fill = Emphysema.Or.Copd)) +
    geom_bar(position = "dodge")
p1_cat
p2 <- ggplot(df_calibration, aes(cluster_optimal, Omics.Methionine, fill=cluster_optimal)) +      
    geom_boxplot() +
    theme_gray()+
    theme(title = element_text(size=12, face='bold'),
          axis.title.y = element_text(color='black', face='plain'), 
          axis.title.x = element_text(color='black', face='plain'))+
    labs(title = "Methionine levels per cluster - Optimal unsupervised Kprototype",fill = "cluster") +
    xlab("cluster") + 
    ylab("Methionine") 
p2_done <- p2 + geom_boxplot(outlier.colour = "red", outlier.shape = 1)
p2_done

ggplot(data = df_calibration, aes(x = Questionnaires.AQLQ.Question.26 , fill = cluster_optimal)) +
    geom_bar(position = "dodge")
p2_cat

p3 <- ggplot(df_calibration, aes(cluster_optimal, Age, fill=cluster_optimal)) +      
    geom_boxplot() +
    theme_gray()+
    theme(title = element_text(size=12, face='bold'),
          axis.title.y = element_text(color='black', face='plain'), 
          axis.title.x = element_text(color='black', face='plain'))+
    labs(title = "Age distribution across clusters - Optimal unsupervised Kprototype",fill = "cluster") +
    xlab("cluster") + 
    ylab("Age") 
p3_done <- p3 + geom_boxplot(outlier.colour = "red", outlier.shape = 1)
p3_done

p4 <- ggplot(df_calibration, aes(cluster_optimal, Questionnaires.ACQ.FEV1.Precentage, fill=cluster_optimal)) +      
    geom_boxplot() +
    theme_gray()+
    theme(title = element_text(size=12, face='bold'),
          axis.title.y = element_text(color='black', face='plain'), 
          axis.title.x = element_text(color='black', face='plain'))+
    labs(title = "FEV1% across clusters - Optimal unsupervised Kprototype",fill = "cluster") +
    xlab("cluster") + 
    ylab("FEV1%") 
p4_done <- p4 + geom_boxplot(outlier.colour = "red", outlier.shape = 1)
p4_done

p5 <- ggplot(df_calibration, aes(cluster_optimal, Omics.S.Adenosylhomocysteine, fill=cluster_optimal)) +      
    geom_boxplot() +
    theme_gray()+
    theme(title = element_text(size=12, face='bold'),
          axis.title.y = element_text(color='black', face='plain'), 
          axis.title.x = element_text(color='black', face='plain'))+
    labs(title = "S.Adenosylhomocysteine levels per cluster - Optimal unsupervised Kprototype",fill = "cluster") +
    xlab("cluster") + 
    ylab("S.Adenosylhomocysteine") 
p5_done <- p5 + geom_boxplot(outlier.colour = "red", outlier.shape = 1)
p5_done

p6 <- ggplot(df_calibration, aes(cluster_optimal, Biomarker.Genentech.CCL18.pg.ml.IMPACT.BL.serum, fill=cluster_optimal)) +      
    geom_boxplot() +
    theme_gray()+
    theme(title = element_text(size=12, face='bold'),
          axis.title.y = element_text(color='black', face='plain'), 
          axis.title.x = element_text(color='black', face='plain'))+
    labs(title = "Biomarker CCL18 levels (pg/ml) per cluster - Optimal unsupervised Kprototype",fill = "cluster") +
    xlab("cluster") + 
    ylab("Biomarker CCL18 (pg/ml)") 
p6_done <- p6 + geom_boxplot(outlier.colour = "red", outlier.shape = 1)
p6_done

p7 <- ggplot(df_calibration, aes(cluster_optimal, Omics.Histidine, fill=cluster_optimal)) +      
    geom_boxplot() +
    theme_gray()+
    theme(title = element_text(size=12, face='bold'),
          axis.title.y = element_text(color='black', face='plain'), 
          axis.title.x = element_text(color='black', face='plain'))+
    labs(title = "Histidine levels per cluster - Optimal unsupervised Kprototype",fill = "cluster") +
    xlab("cluster") + 
    ylab("Histidine") 
p7_done <- p7 + geom_boxplot(outlier.colour = "red", outlier.shape = 1)
p7_done

p8 <- ggplot(df_calibration, aes(cluster_optimal, Omics.N.Acetylcarnosine, fill=cluster_optimal)) +      
    geom_boxplot() +
    theme_gray()+
    theme(title = element_text(size=12, face='bold'),
          axis.title.y = element_text(color='black', face='plain'), 
          axis.title.x = element_text(color='black', face='plain'))+
    labs(title = "N.Acetylcarnosine levels per cluster - Optimal unsupervised Kprototype",fill = "cluster") +
    xlab("cluster") + 
    ylab("N.Acetylcarnosine") 
p8_done <- p8 + geom_boxplot(outlier.colour = "red", outlier.shape = 1)
p8_done

p9 <- ggplot(df_calibration, aes(cluster_optimal, Questionnaires.SNOT.Total.Imputed, fill=cluster_optimal)) +      
    geom_boxplot() +
    theme_gray()+
    theme(title = element_text(size=12, face='bold'),
          axis.title.y = element_text(color='black', face='plain'), 
          axis.title.x = element_text(color='black', face='plain'))+
    labs(title = "SNOT questionnaires scores per cluster - Optimal unsupervised Kprototype",fill = "cluster") +
    xlab("cluster") + 
    ylab("SNOT score") 
p9_done <- p9 + geom_boxplot(outlier.colour = "red", outlier.shape = 1)
p9_done

p10 <- ggplot(df_calibration, aes(cluster_optimal, Omics.Guanosine, fill=cluster_optimal)) +      
    geom_boxplot() +
    theme_gray()+
    theme(title = element_text(size=12, face='bold'),
          axis.title.y = element_text(color='black', face='plain'), 
          axis.title.x = element_text(color='black', face='plain'))+
    labs(title = "Guanosine levels per cluster - Optimal unsupervised Kprototype",fill = "cluster") +
    xlab("cluster") + 
    ylab("Guanosine") 
p10_done <- p10 + geom_boxplot(outlier.colour = "red", outlier.shape = 1)
p10_done

p11 <- ggplot(df_calibration, aes(cluster_optimal, Omics.Biopterin, fill=cluster_optimal)) +      
    geom_boxplot() +
    theme_gray()+
    theme(title = element_text(size=12, face='bold'),
          axis.title.y = element_text(color='black', face='plain'), 
          axis.title.x = element_text(color='black', face='plain'))+
    labs(title = "Biopterin levels per cluster - Optimal unsupervised Kprototype",fill = "cluster") +
    xlab("cluster") + 
    ylab("Biopterin") 
p11_done <- p11 + geom_boxplot(outlier.colour = "red", outlier.shape = 1)
p11_done

p12 <- ggplot(df_calibration, aes(cluster_optimal, Omics.Propionylcarnitine, fill=cluster_optimal)) +      
    geom_boxplot() +
    theme_gray()+
    theme(title = element_text(size=12, face='bold'),
          axis.title.y = element_text(color='black', face='plain'), 
          axis.title.x = element_text(color='black', face='plain'))+
    labs(title = "Propionylcarnitine levels per cluster - Optimal unsupervised Kprototype",fill = "cluster") +
    xlab("cluster") + 
    ylab("Propionylcarnitine") 
p12_done <- p12 + geom_boxplot(outlier.colour = "red", outlier.shape = 1)
p12_done

p13 <- ggplot(df_calibration, aes(cluster_optimal, Clinical.Screening.Wbcs.x10.3.uL, fill=cluster_optimal)) +      
    geom_boxplot() +
    theme_gray()+
    theme(title = element_text(size=12, face='bold'),
          axis.title.y = element_text(color='black', face='plain'), 
          axis.title.x = element_text(color='black', face='plain'))+
    labs(title = "White blood cell levels per cluster - Optimal unsupervised Kprototype",fill = "cluster") +
    xlab("cluster") + 
    ylab("White blood cell (x10.3.uL)") 
p13_done <- p13 + geom_boxplot(outlier.colour = "red", outlier.shape = 1)
p13_done

p14 <- ggplot(df_calibration, aes(cluster_optimal, Biomarker.Galectin.3.pg.ml.serum, fill=cluster_optimal)) +      
    geom_boxplot() +
    theme_gray()+
    theme(title = element_text(size=12, face='bold'),
          axis.title.y = element_text(color='black', face='plain'), 
          axis.title.x = element_text(color='black', face='plain'))+
    labs(title = "Galectin-3 levels per cluster - Optimal unsupervised Kprototype",fill = "cluster") +
    xlab("cluster") + 
    ylab("Galectin-3 (pg/ml)") 
p14_done <- p14 + geom_boxplot(outlier.colour = "red", outlier.shape = 1)
p14_done

p15 <- ggplot(df_calibration, aes(cluster_optimal, Omics.Tyrosine, fill=cluster_optimal)) +      
    geom_boxplot() +
    theme_gray()+
    theme(title = element_text(size=12, face='bold'),
          axis.title.y = element_text(color='black', face='plain'), 
          axis.title.x = element_text(color='black', face='plain'))+
    labs(title = "Tyrosine levels per cluster - Optimal unsupervised Kprototype",fill = "cluster") +
    xlab("cluster") + 
    ylab("Tyrosine") 
p15_done <- p15 + geom_boxplot(outlier.colour = "red", outlier.shape = 1)
p15_done

p16 <- ggplot(df_calibration, aes(cluster_optimal, Omics.Pyroglutamylglycine, fill=cluster_optimal)) +      
    geom_boxplot() +
    theme_gray()+
    theme(title = element_text(size=12, face='bold'),
          axis.title.y = element_text(color='black', face='plain'), 
          axis.title.x = element_text(color='black', face='plain'))+
    labs(title = "Pyroglutamylglycine levels per cluster - Optimal unsupervised Kprototype",fill = "cluster") +
    xlab("cluster") + 
    ylab("Pyroglutamylglycine") 
p16_done <- p16 + geom_boxplot(outlier.colour = "red", outlier.shape = 1)
p16_done

p17 <- ggplot(df_calibration, aes(cluster_optimal, Biomarker.MCP1.pg.ml.MSD.BL.plasma, fill=cluster_optimal)) +      
    geom_boxplot() +
    theme_gray()+
    theme(title = element_text(size=12, face='bold'),
          axis.title.y = element_text(color='black', face='plain'), 
          axis.title.x = element_text(color='black', face='plain'))+
    labs(title = "Monocyte-Chemoattractant protein-1 levels per cluster - Optimal unsupervised Kprototype",fill = "cluster") +
    xlab("cluster") + 
    ylab("MCP1 (pg/ml)") 
p17_done <- p17 + geom_boxplot(outlier.colour = "red", outlier.shape = 1)
p17_done

p18 <- ggplot(df_calibration, aes(cluster_optimal, Omics.Caffeine, fill=cluster_optimal)) +      
    geom_boxplot() +
    theme_gray()+
    theme(title = element_text(size=12, face='bold'),
          axis.title.y = element_text(color='black', face='plain'), 
          axis.title.x = element_text(color='black', face='plain'))+
    labs(title = "Caffeine levels per cluster - Optimal unsupervised Kprototype",fill = "cluster") +
    xlab("cluster") + 
    ylab("Caffeine") 
p18_done <- p18 + geom_boxplot(outlier.colour = "red", outlier.shape = 1)
p18_done

p19 <- ggplot(df_calibration, aes(cluster_optimal, Clinical.Baseline.PEF, fill=cluster_optimal)) +      
    geom_boxplot() +
    theme_gray()+
    theme(title = element_text(size=12, face='bold'),
          axis.title.y = element_text(color='black', face='plain'), 
          axis.title.x = element_text(color='black', face='plain'))+
    labs(title = "Peak Expiratory Flow levels across clusters - Optimal unsupervised Kprototype",fill = "cluster") +
    xlab("cluster") + 
    ylab("PEF") 
p19_done <- p19 + geom_boxplot(outlier.colour = "red", outlier.shape = 1)
p19_done

p20 <- ggplot(df_calibration, aes(cluster_optimal, Omics.Hydroxykynurenine, fill=cluster_optimal)) +      
    geom_boxplot() +
    theme_gray()+
    theme(title = element_text(size=12, face='bold'),
          axis.title.y = element_text(color='black', face='plain'), 
          axis.title.x = element_text(color='black', face='plain'))+
    labs(title = "Hydroxykynurenine levels per cluster - Optimal unsupervised Kprototype",fill = "cluster") +
    xlab("cluster") + 
    ylab("Hydroxykynurenine") 
p20_done <- p20 + geom_boxplot(outlier.colour = "red", outlier.shape = 1)
p20_done

############ BOXPLOTS RESULTS #############
#Save the boxplot results of the variables driving the most the clustering
library(patchwork)
p1_done + p2_done + p3_done + p4_done
ggsave("/rds/general/user/md2620/home/asthma/Malo/Results/unsup_boxplots_oct24/p1_p4.pdf", width = 20, height = 12)

library(patchwork)
p5_done + p6_done + p7_done + p8_done
ggsave("/rds/general/user/md2620/home/asthma/Malo/Results/unsup_boxplots_oct24/p5_p8.pdf", width = 20, height = 12)

library(patchwork)
p9_done + p10_done + p11_done + p12_done
ggsave("/rds/general/user/md2620/home/asthma/Malo/Results/unsup_boxplots_oct24/p9_p12.pdf", width = 20, height = 12)

library(patchwork)
p13_done + p14_done + p15_done + p16_done
ggsave("/rds/general/user/md2620/home/asthma/Malo/Results/unsup_boxplots_oct24/p13_p16.pdf", width = 20, height = 12)

library(patchwork)
p17_done + p18_done + p19_done + p20_done
ggsave("/rds/general/user/md2620/home/asthma/Malo/Results/unsup_boxplots_oct24/p17_p20.pdf", width = 20, height = 12)

########### END BOXPLOTS RESULTS #############
citation("stats")
rm(list=ls())

df_calibration_imp = read.csv("/rds/general/user/md2620/home/asthma/Malo/Dataframes/Final/df_for_results_unsup_Oct24.csv")

#Set Subject.ID as index
df_calibration <- df_calibration_imp[-1]
row.names(df_calibration) <- df_calibration_impt$Unnamed..0
View(df_calibration)

########## ALL VARIABLES DIFFERENCES ACROSS THE 3 OPTIMAL CLUSTERS - KPROTO UNSUP - USE OF TABLES ###########
#Define your controls (tests used and measures we look at)
mycontrols  <- tableby.control(test=FALSE, total=FALSE,
                               numeric.test="anova", cat.test="chisq",
                               numeric.stats=c("Nmiss","mean","median", "q1q3"),
                               cat.stats=c("countpct"),
                               digits = 0L, digits.count = 0L, 
                               digits.pct = 0L, digits.p = 0L,
                               stats.labels=list(Nmiss="missing", mean='Mean', median='Median', q1q3='Q1,Q3'))


#Summary table for biomarkers per cluster - only numerical variables
tab_biomarkers <- summary(tableby(cluster_optimal ~ Biomarker.C5a.pg.ml.serum + Biomarker.CD30.pg.ml.serum
                                  + Biomarker.CD40L.pg.ml.serum + Biomarker.DPPIV.pg.ml.serum
                                  + Biomarker.Galectin.3.pg.ml.serum + Biomarker.IL.18.pg.ml.serum
                                  + Biomarker.IL.1alpha.pg.ml.serum + Biomarker.IL.6Ralpha.pg.ml.serum
                                  + Biomarker.LBP.pg.ml.serum + Biomarker.Lumican.pg.ml.serum
                                  + Biomarker.MCP.4.pg.ml.serum + Biomarker.MMP.3.pg.ml.serum
                                  + Biomarker.RAGE.pg.ml.serum + Biomarker.SHBG.pg.ml.serum
                                  + Biomarker.Serpin.E1.pg.ml.serum + Biomarker.alpha1.microglobulin.pg.ml.serum
                                  + Biomarker.CCL17.pg.ml.MSD.BL.plasma + Biomarker.CCL22.pg.ml.MSD.BL.plasma
                                  + Biomarker.EOTAXIN.pg.ml.MSD.BL.plasma + Biomarker.EOTAXIN3.pg.ml.MSD.BL.plasma
                                  + Biomarker.Baseline.BI.Cytokines.Chemokines.IFNg.pg.ml.MSD.BL.plasma 
                                  + Biomarker.Baseline.BI.Cytokines.Chemokines.IL6.pg.ml.MSD.BL.plasma 
                                  + Biomarker.IL8.pg.ml.MSD.BL.plasma + Biomarker.IP10.pg.ml.MSD.BL.plasma
                                  + Biomarker.MCP1.pg.ml.MSD.BL.plasma + Biomarker.MIP1b.pg.ml.MSD.BL.plasma
                                  + Biomarker.TNFa.pg.ml.MSD.BL.plasma + Biomarker.Genentech.CCL18.pg.ml.IMPACT.BL.serum
                                  + Biomarker.Genentech.IL13.pg.ml.IMPACT.BL.serum
                                  + Biomarker.Genentech.Periostin.ng.ml.ELECSYS.BL.serum
                                  + Biomarker.Karolinska.hsCRP.hCRP.mg.L,data=df_calibration, control = mycontrols),title="Individual biomarker's Mean, Median and 25th-75th Quantiles for each cluster ")

write2word(tab_biomarkers,"/rds/general/user/md2620/home/asthma/Malo/Results/unsup_pdf_tables_oct24/biomarkers.doc")
########################## CALIBRATION OF LAMBDA NO PREPROCESS OF NUMERIC VARIABLES##########################
#Clear environment
rm(list=ls())

#Load dataset
df_calib = read.csv("/rds/general/user/md2620/home/asthma/Malo/Dataframes/Final/df_unsup_not_processed.csv")

#Set Subject.ID as index
df_calibration <- df_calib[-1]
row.names(df_calibration) <- df_calib$X
View(df_calibration)

df_calibration = subset(df_calibration, select = -c(Omics.Furoylglycine,Omics.Glutamic.acid,Omics.N.Acetylglutamic.acid,
                                                            Omics.Phenylalanine,Omics.Allantoin,Omics.Isoleucine,Omics.N.Acetylputrescine,
                                                            Omics.Sarcosine,Omics.Cytosine,Omics.Lysine,Omics.N.Methyl.D.aspartic.acid,
                                                            Omics.Xanthosine,Omics.Glucosamine,Omics.Maltose,Omics.O.Acetylserine,
                                                            Omics.Xylose))


#Remove outcome variable cohort before running k-prototypes algo
df_calibration = subset(df_calibration, select = -c(cohort))

#Create an index of your original categorical variables
index_cat <- c(181:361)
df_calibration[,index_cat] <- lapply(df_calibration[ ,index_cat], as.factor)

num_cat <- c(1:180)
df_calibration[,num_cat] <- lapply(df_calibration[ ,num_cat], as.numeric)
View(sapply(df_calibration,class))

#Select numerical variables
num_df_calibration <- df_calibration %>%
    select_if(Negate(is.factor))

#Select categorical variables
cat_df_calibration<- df_calibration %>%
    select_if(Negate(is.numeric))

# For visualization purposes, we drop all variables that have 0 variance when data is scaled
num_df_calibration = subset(num_df_calibration, select = -c(Omics.Furoylglycine,Omics.Glutamic.acid,Omics.N.Acetylglutamic.acid,
                                                    Omics.Phenylalanine,Omics.Allantoin,Omics.Isoleucine,Omics.N.Acetylputrescine,
                                                    Omics.Sarcosine,Omics.Cytosine,Omics.Lysine,Omics.N.Methyl.D.aspartic.acid,
                                                    Omics.Xanthosine,Omics.Glucosamine,Omics.Maltose,Omics.O.Acetylserine,
                                                    Omics.Xylose))


#Combine cat and num dataframes before running kproto
df_calibration <- cbind(num_df_calibration, cat_df_calibration)

#Check that class of variables is respected
sapply(df_calibration,class)
# Using optimal number of clusters K = 3, calibrate lambda by re-running K-prototypes (unstandardized variables all the way)
############## ############## 
kprot_unsup3_lbd_0.1 <- kproto(df_calibration, 3, keep.data=TRUE, na.rm = FALSE, lambda = 0.1) #close to KMeans
kprot_unsup3_lbd_0.2 <- kproto(df_calibration, 3, keep.data=TRUE, na.rm = FALSE, lambda = 0.2)
kprot_unsup3_lbd_0.3 <- kproto(df_calibration, 3, keep.data=TRUE, na.rm = FALSE, lambda = 0.3)
kprot_unsup3_lbd_0.4 <- kproto(df_calibration, 3, keep.data=TRUE, na.rm = FALSE, lambda = 0.4)
kprot_unsup3_lbd_0.5 <- kproto(df_calibration, 3, keep.data=TRUE, na.rm = FALSE, lambda = 0.5)
kprot_unsup3_lbd_0.6 <- kproto(df_calibration, 3, keep.data=TRUE, na.rm = FALSE, lambda = 0.6)
kprot_unsup3_lbd_0.7 <- kproto(df_calibration, 3, keep.data=TRUE, na.rm = FALSE, lambda = 0.7)
kprot_unsup3_lbd_0.8 <- kproto(df_calibration, 3, keep.data=TRUE, na.rm = FALSE, lambda = 0.8)
kprot_unsup3_lbd_0.9 <- kproto(df_calibration, 3, keep.data=TRUE, na.rm = FALSE, lambda = 0.9)
kprot_unsup3_lbd_1 <- kproto(df_calibration, 3, keep.data=TRUE, na.rm = FALSE, lambda = 1)
kprot_unsup3_lbd_2 <- kproto(df_calibration, 3, keep.data=TRUE, na.rm = FALSE, lambda = 2)
kprot_unsup3_lbd_3 <- kproto(df_calibration, 3, keep.data=TRUE, na.rm = FALSE, lambda = 3)
kprot_unsup3_lbd_4 <- kproto(df_calibration, 3, keep.data=TRUE, na.rm = FALSE, lambda = 4)
kprot_unsup3_lbd_5 <- kproto(df_calibration, 3, keep.data=TRUE, na.rm = FALSE, lambda = 5)
kprot_unsup3_lbd_6 <- kproto(df_calibration, 3, keep.data=TRUE, na.rm = FALSE, lambda = 6)
kprot_unsup3_lbd_7 <- kproto(df_calibration, 3, keep.data=TRUE, na.rm = FALSE, lambda = 7)
kprot_unsup3_lbd_8 <- kproto(df_calibration, 3, keep.data=TRUE, na.rm = FALSE, lambda = 8)
kprot_unsup3_lbd_9 <- kproto(df_calibration, 3, keep.data=TRUE, na.rm = FALSE, lambda = 9)
kprot_unsup3_lbd_10 <- kproto(df_calibration, 3, keep.data=TRUE, na.rm = FALSE, lambda = 10)
kprot_unsup3_lbd_11 <- kproto(df_calibration, 3, keep.data=TRUE, na.rm = FALSE, lambda = 11)
kprot_unsup3_lbd_12 <- kproto(df_calibration, 3, keep.data=TRUE, na.rm = FALSE, lambda = 12)
kprot_unsup3_lbd_13 <- kproto(df_calibration, 3, keep.data=TRUE, na.rm = FALSE, lambda = 13)
kprot_unsup3_lbd_14 <- kproto(df_calibration, 3, keep.data=TRUE, na.rm = FALSE, lambda = 14)
kprot_unsup3_lbd_15 <- kproto(df_calibration, 3, keep.data=TRUE, na.rm = FALSE, lambda = 15)
kprot_unsup3_lbd_25 <- kproto(df_calibration, 3, keep.data=TRUE, na.rm = FALSE, lambda = 25) #emphasize a lot the weight given to categorical variables

#Estimated lambda by default (does not advantage one type of variables over the other:
x <- lambdaest(df_calibration, num.method = 1, fac.method = 1, outtype = "numeric")

#Lambda with respective individual weight given to each variable allows to find average numeric and categorical variation respectively:
x_ind_vector <- lambdaest(df_calibration, num.method = 1, fac.method = 1, outtype = "vector")

# Setting lambda = Average numeric variation / Average categorical variation = 1 (up to max of 2) allows to not favor one type of variables over the other when clustering.
lambda_actual <- 1.067969/0.3503353 #Variation in the data by default. =3.04 here (emphasized towards numerical variables here)

#Change heuristic (sd instead of variance for numerical variables and absolute value instead of square root for categorical variables too)
x_2 <- lambdaest(df_calibration, num.method = 2, fac.method = 2, outtype = "numeric") #Default lambda with these heuristics = 3.804
x_ind_vector_2 <- lambdaest(df_calibration, num.method = 2, fac.method = 2, outtype = "vector") #Average numeric variation = 0.9579188 ; Average categorical variation = 0.2517807  

#Try running 2 proto algorithms with these lambdas:
kprot_unsup3_lbd_x <-  kproto(df_calibration, 3, keep.data=TRUE, na.rm = FALSE, lambda = x)
kprot_unsup3_lbd_x_2 <-  kproto(df_calibration, 3, keep.data=TRUE, na.rm = FALSE, lambda = x_2)


#Append calibrated clusters with K=4 to final_df_unsup_import for viz -see which is best 
df_calibration$K3_lbd_0.1 <- factor(kprot_unsup3_lbd_0.1$cluster)
df_calibration$K3_lbd_0.2 <- factor(kprot_unsup3_lbd_0.2$cluster)
df_calibration$K3_lbd_0.3 <- factor(kprot_unsup3_lbd_0.3$cluster)
df_calibration$K3_lbd_0.4 <- factor(kprot_unsup3_lbd_0.4$cluster)
df_calibration$K3_lbd_0.5 <- factor(kprot_unsup3_lbd_0.5$cluster)
df_calibration$K3_lbd_0.6 <- factor(kprot_unsup3_lbd_0.6$cluster)
df_calibration$K3_lbd_0.7 <- factor(kprot_unsup3_lbd_0.7$cluster)
df_calibration$K3_lbd_0.8 <- factor(kprot_unsup3_lbd_0.8$cluster)
df_calibration$K3_lbd_0.9 <- factor(kprot_unsup3_lbd_0.9$cluster)
df_calibration$K3_lbd_1 <- factor(kprot_unsup3_lbd_1$cluster)

df_calibration$K3_lbd_2 <- factor(kprot_unsup3_lbd_2$cluster)
df_calibration$K3_lbd_3 <- factor(kprot_unsup3_lbd_3$cluster)
df_calibration$K3_lbd_4 <- factor(kprot_unsup3_lbd_4$cluster)
df_calibration$K3_lbd_5 <- factor(kprot_unsup3_lbd_5$cluster)
df_calibration$K3_lbd_6 <- factor(kprot_unsup3_lbd_6$cluster)
df_calibration$K3_lbd_7 <- factor(kprot_unsup3_lbd_7$cluster)
df_calibration$K3_lbd_8 <- factor(kprot_unsup3_lbd_8$cluster)
df_calibration$K3_lbd_9 <- factor(kprot_unsup3_lbd_9$cluster)
df_calibration$K3_lbd_10 <- factor(kprot_unsup3_lbd_10$cluster)
df_calibration$K3_lbd_11 <- factor(kprot_unsup3_lbd_11$cluster)
df_calibration$K3_lbd_12 <- factor(kprot_unsup3_lbd_12$cluster)
df_calibration$K3_lbd_13 <- factor(kprot_unsup3_lbd_13$cluster)
df_calibration$K3_lbd_14 <- factor(kprot_unsup3_lbd_14$cluster)
df_calibration$K3_lbd_15 <- factor(kprot_unsup3_lbd_15$cluster)
df_calibration$K3_lbd_25 <- factor(kprot_unsup3_lbd_25$cluster)
df_calibration$K3_default_lbd <- factor(kprot_unsup3_lbd_x$cluster)
df_calibration$K3_heuristics_2_lbd <- factor(kprot_unsup3_lbd_x_2$cluster)
df_calibration$cohort <- factor(df_calib$cohort)
#Save dataset to visualize which lambda best separates the observations into the 3 clusters.
setwd("/rds/general/user/md2620/home/asthma/Malo/Dataframes/Final")
write.csv(df_calibration,"unsup_df_calib_lambda_no_proc.csv")
