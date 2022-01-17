##### RUN KPROTO SUPERVISED WITH NUM VARIABLES PROCESSED AS WELL AS CATERGORICAL TO CHECK OPTIMAL NUMBER OF K ######
#Clear environment
rm(list=ls())

df_unsup_scaled = read.csv("/rds/general/user/md2620/home/asthma/Malo/Dataframes/Final/unsup_df_all_processed.csv")

#Drop X variable
df_unsup_scaled = subset(df_unsup_scaled, select = -c(X))
#Set Subject.ID as index
df_un_scaled <- df_unsup_scaled[-1]
row.names(df_un_scaled) <- df_unsup_scaled$Unnamed..0
View(df_un_scaled)

#Remove outcome variable cohort before running k-prototypes algo
df_un_scaled = subset(df_un_scaled, select = -c(cohort))

#Remove other variables that you don't use in kproto
df_un_scaled = subset(df_un_scaled, select = -c(cluster_K_2,cluster_K_3,cluster_K_4,
                                                cluster_K_5,cluster_K_6,cluster_K_7,
                                                cluster_K_8,cluster_K_9,cluster_K_10,
                                                cluster_K_11,cluster_K_12,cluster_K_13,
                                                cluster_K_14,cluster_K_15,Healthy,Severe,Severe_asthma,
                                                MildModerate,Severe_Smoker))


#Create an index of your original categorical variables
index_cat <- c(181:361)
df_un_scaled[,index_cat] <- lapply(df_un_scaled[ ,index_cat], as.factor)

num_cat <- c(1:180)
df_un_scaled[,num_cat] <- lapply(df_un_scaled[ ,num_cat], as.numeric)
View(sapply(df_un_scaled,class))

#Select numerical variables
num_df_un_scaled <- df_un_scaled %>%
    select_if(Negate(is.factor))

#Select categorical variables
cat_df_un_scaled <- df_un_scaled %>%
    select_if(Negate(is.numeric))

# For visualization purposes, we drop all variables that have 0 variance
#Supply names of columns that have 0 variance
names(num_df_un_scaled[, sapply(num_df_un_scaled, function(v) var(v, na.rm=TRUE)==0)])

#Drop columns that have 0 variance from dataframe
num_df_un_scaled <- num_df_un_scaled[,apply(num_df_un_scaled, 2, var, na.rm=TRUE) != 0]


#Combine cat and num dataframes before running kproto
df_un_scaled <- cbind(num_df_un_scaled, cat_df_un_scaled)

#Check that class of variables is respected
sapply(df_un_scaled,class)

#Check variance of variables --> shall be equal to 1 for numerical variables
x_ind_vector <- lambdaest(df_un_scaled, num.method = 1, fac.method = 1, outtype = "vector")

#Default lambda for kproto below - one to use if you don't want to advantage one type of variables over the other
x <- lambdaest(df_un_scaled, num.method = 1, fac.method = 1, outtype = "numeric")

# apply k prototypes on df_un_scaled - use 2 to 15 clusters
proto_scaled2 <- kproto(df_un_scaled, 2, keep.data=TRUE, na.rm = FALSE)
proto_scaled3 <- kproto(df_un_scaled, 3, keep.data=TRUE, na.rm = FALSE)
proto_scaled4 <- kproto(df_un_scaled, 4, keep.data=TRUE, na.rm = FALSE)
proto_scaled5 <- kproto(df_un_scaled, 5, keep.data=TRUE, na.rm = FALSE)
proto_scaled6 <- kproto(df_un_scaled, 6, keep.data=TRUE, na.rm = FALSE)
proto_scaled7 <- kproto(df_un_scaled, 7, keep.data=TRUE, na.rm = FALSE)
proto_scaled8 <- kproto(df_un_scaled, 8, keep.data=TRUE, na.rm = FALSE)
proto_scaled9 <- kproto(df_un_scaled, 9, keep.data=TRUE, na.rm = FALSE)
proto_scaled10 <- kproto(df_un_scaled, 10, keep.data=TRUE, na.rm = FALSE)
proto_scaled11 <- kproto(df_un_scaled, 11, keep.data=TRUE, na.rm = FALSE)
proto_scaled12 <- kproto(df_un_scaled, 12, keep.data=TRUE, na.rm = FALSE)
proto_scaled13 <- kproto(df_un_scaled, 13, keep.data=TRUE, na.rm = FALSE)
proto_scaled14 <- kproto(df_un_scaled, 14, keep.data=TRUE, na.rm = FALSE)
proto_scaled15 <- kproto(df_un_scaled, 15, keep.data=TRUE, na.rm = FALSE)

#Transform to matrix the dataframe, also it can be handled by intCriteria function
df_un_scaled_matrix = data.matrix(df_un_scaled)

results_proto_scaled2 = intCriteria(df_un_scaled_matrix,proto_scaled2$cluster,"all")
df_results_proto_scaled2 = as.data.frame(results_proto_scaled2,row.names = "2")
df_results_proto_scaled2$K <- row.names(df_results_proto_scaled2)

results_proto_scaled3 = intCriteria(df_un_scaled_matrix,proto_scaled3$cluster,"all")
df_results_proto_scaled3 = as.data.frame(results_proto_scaled3,row.names = "3")
df_results_proto_scaled3$K <- row.names(df_results_proto_scaled3)

#Append together df_results_kpres_2 and df_results_kpres_3
df_results_kprot_2_3_un_scaled = dplyr::bind_rows(df_results_proto_scaled2,df_results_proto_scaled3)

results_proto_scaled4 = intCriteria(df_un_scaled_matrix,proto_scaled4$cluster,"all")
df_results_proto_scaled4 = as.data.frame(results_proto_scaled4,row.names = "4")
df_results_proto_scaled4$K <- row.names(df_results_proto_scaled4)

#Append together df_results_kprot_2_3 and df_results_proto_scaled4
df_results_kprot_2_3_4_un_scaled = dplyr::bind_rows(df_results_kprot_2_3_un_scaled,df_results_proto_scaled4)

results_proto_scaled5 = intCriteria(df_un_scaled_matrix,proto_scaled5$cluster,"all")
df_results_proto_scaled5 = as.data.frame(results_proto_scaled5,row.names = "5")
df_results_proto_scaled5$K <- row.names(df_results_proto_scaled5)

#Append together df_results_kprot_2_3_4 and df_results_kprot_5
df_results_kprot_2_to_5_un_scaled = dplyr::bind_rows(df_results_kprot_2_3_4_un_scaled,df_results_proto_scaled5)

results_proto_scaled6 = intCriteria(df_un_scaled_matrix,proto_scaled6$cluster,"all")
df_results_proto_scaled6 = as.data.frame(results_proto_scaled6,row.names = "6")
df_results_proto_scaled6$K <- row.names(df_results_proto_scaled6)

#Append together df_results_kprot_2_to_5 and df_results_proto_scaled6
df_results_kprot_2_to_6_un_scaled = dplyr::bind_rows(df_results_kprot_2_to_5_un_scaled,df_results_proto_scaled6)

results_proto_scaled7 = intCriteria(df_un_scaled_matrix,proto_scaled7$cluster,"all")
df_results_proto_scaled7 = as.data.frame(results_proto_scaled7,row.names = "7")
df_results_proto_scaled7$K <- row.names(df_results_proto_scaled7)

#Append together df_results_kprot_2_to_6 and df_results_proto_scaled7
df_results_kprot_2_to_7_un_scaled = dplyr::bind_rows(df_results_kprot_2_to_6_un_scaled,df_results_proto_scaled7)

results_proto_scaled8 = intCriteria(df_un_scaled_matrix,proto_scaled8$cluster,"all")
df_results_proto_scaled8 = as.data.frame(results_proto_scaled8,row.names = "8")
df_results_proto_scaled8$K <- row.names(df_results_proto_scaled8)

#Append together df_results_kprot_2_to_7 and df_results_proto_scaled8
df_results_kprot_2_to_8_un_scaled = dplyr::bind_rows(df_results_kprot_2_to_7_un_scaled,df_results_proto_scaled8)

results_proto_scaled9 = intCriteria(df_un_scaled_matrix,proto_scaled9$cluster,"all")
df_results_proto_scaled9 = as.data.frame(results_proto_scaled9,row.names = "9")
df_results_proto_scaled9$K <- row.names(df_results_proto_scaled9)

#Append together df_results_kprot_2_to_8 and df_results_proto_scaled9
df_results_kprot_2_to_9_un_scaled = dplyr::bind_rows(df_results_kprot_2_to_8_un_scaled,df_results_proto_scaled9)

results_proto_scaled10 = intCriteria(df_un_scaled_matrix,proto_scaled10$cluster,"all")
df_results_proto_scaled10 = as.data.frame(results_proto_scaled10,row.names = "10")
df_results_proto_scaled10$K <- row.names(df_results_proto_scaled10)

#Append together df_results_kprot_2_to_9 and df_results_proto_scaled10
df_results_kprot_2_to_10_un_scaled = dplyr::bind_rows(df_results_kprot_2_to_9_un_scaled,df_results_proto_scaled10)

results_proto_scaled11 = intCriteria(df_un_scaled_matrix,proto_scaled11$cluster,"all")
df_results_proto_scaled11 = as.data.frame(results_proto_scaled11,row.names = "11")
df_results_proto_scaled11$K <- row.names(df_results_proto_scaled11)

#Append together df_results_kprot_2_to_10 and df_results_proto_scaled11
df_results_kprot_2_to_11_un_scaled = dplyr::bind_rows(df_results_kprot_2_to_10_un_scaled,df_results_proto_scaled11)

results_proto_scaled12 = intCriteria(df_un_scaled_matrix,proto_scaled12$cluster,"all")
df_results_proto_scaled12 = as.data.frame(results_proto_scaled12,row.names = "12")
df_results_proto_scaled12$K <- row.names(df_results_proto_scaled12)

#Append together df_results_kprot_2_to_11 and df_results_proto_scaled12
df_results_kprot_2_to_12_un_scaled = dplyr::bind_rows(df_results_kprot_2_to_11_un_scaled,df_results_proto_scaled12)

results_proto_scaled13 = intCriteria(df_un_scaled_matrix,proto_scaled13$cluster,"all")
df_results_proto_scaled13 = as.data.frame(results_proto_scaled13,row.names = "13")
df_results_proto_scaled13$K <- row.names(df_results_proto_scaled13)

#Append together df_results_kprot_2_to_12 and df_results_proto_scaled13
df_results_kprot_2_to_13_un_scaled = dplyr::bind_rows(df_results_kprot_2_to_12_un_scaled,df_results_proto_scaled13)

results_proto_scaled14 = intCriteria(df_un_scaled_matrix,proto_scaled14$cluster,"all")
df_results_proto_scaled14 = as.data.frame(results_proto_scaled14,row.names = "14")
df_results_proto_scaled14$K <- row.names(df_results_proto_scaled14)

#Append together df_results_kprot_2_to_13 and df_results_proto_scaled14
df_results_kprot_2_to_14_un_scaled = dplyr::bind_rows(df_results_kprot_2_to_13_un_scaled,df_results_proto_scaled14)

results_proto_scaled15 = intCriteria(df_un_scaled_matrix,proto_scaled15$cluster,"all")
df_results_proto_scaled15 = as.data.frame(results_proto_scaled15,row.names = "15")
df_results_proto_scaled15$K <- row.names(df_results_proto_scaled15)

#Append together df_results_kprot_2_to_14 and df_results_proto_scaled15
df_results_kprot_2_to_15_un_scaled = dplyr::bind_rows(df_results_kprot_2_to_14_un_scaled,df_results_proto_scaled15)

## Dataframe is ready to be analyzed --> Choose optimal number of K ##
#################### Plot C_Index #####################
pdf(file = "/rds/general/user/md2620/home/asthma/Malo/Results/Optimal_K_unsup_scaled_retest/Cindex.pdf",   # The directory you want to save the file in
    width = 8, # The width of the plot in inches
    height = 8)
plot(df_results_kprot_2_to_15_un_scaled$K,df_results_kprot_2_to_15_un_scaled$c_index, frame = TRUE, 
     col = "black",
     type = "o",
     pch = 21,
     bg = ifelse(df_results_kprot_2_to_15_un_scaled$c_index == min(df_results_kprot_2_to_15_un_scaled$c_index), "red", "black"),
     xlab="Number of clusters K",
     ylab="C Index")
legend("topright", legend = "optimum criterion : min")
title(main = "C Index for unsupervised K-prototypes")
dev.off()
#######################################################

#################### Plot Calinski-Harabasz score #####################
pdf(file = "/rds/general/user/md2620/home/asthma/Malo/Results/Optimal_K_unsup_scaled_retest/Calinski_Harabasz.pdf",   # The directory you want to save the file in
    width = 8, # The width of the plot in inches
    height = 8)
plot(df_results_kprot_2_to_15_un_scaled$K,df_results_kprot_2_to_15_un_scaled$calinski_harabasz, frame = TRUE, 
     col = "black",
     type = "o",
     pch = 21,
     bg = ifelse(df_results_kprot_2_to_15_un_scaled$calinski_harabasz == max(df_results_kprot_2_to_15_un_scaled$calinski_harabasz), "red", "black"),
     xlab="Number of clusters K",
     ylab="Calinski-Harabasz score")
legend("topright", legend = "optimum criterion : max")
title(main = "Calinski-Harabasz score for unsupervised K-prototypes - Huang Initialization method")
dev.off()
#######################################################

#################### Plot Dunn score #####################
pdf(file = "/rds/general/user/md2620/home/asthma/Malo/Results/Optimal_K_unsup_scaled_retest/Dunn.pdf",   # The directory you want to save the file in
    width = 8, # The width of the plot in inches
    height = 8)

plot(df_results_kprot_2_to_15_un_scaled$K,df_results_kprot_2_to_15_un_scaled$dunn, frame = TRUE, 
     col = "black",
     type = "o",
     pch = 21,
     bg = ifelse(df_results_kprot_2_to_15_un_scaled$dunn == max(df_results_kprot_2_to_15_un_scaled$dunn), "red", "black"),
     xlab="Number of clusters K",
     ylab="Dunn score")
legend("topright", legend = "optimum criterion : max")
title(main = "Dunn score for unsupervised K-prototypes")

dev.off()
#######################################################

#################### Plot Gamma score #####################
pdf(file = "/rds/general/user/md2620/home/asthma/Malo/Results/Optimal_K_unsup_scaled_retest/gamma.pdf",   # The directory you want to save the file in
    width = 8, # The width of the plot in inches
    height = 8)

plot(df_results_kprot_2_to_15_un_scaled$K,df_results_kprot_2_to_15_un_scaled$gamma, frame = TRUE, 
     col = "black",
     type = "o",
     pch = 21,
     bg = ifelse(df_results_kprot_2_to_15_un_scaled$gamma == max(df_results_kprot_2_to_15_un_scaled$gamma), "red", "black"),
     xlab="Number of clusters K",
     ylab="Gamma score")
legend("bottomright", legend = "optimum criterion : max")
title(main = "Gamma score for unsupervised K-prototypes")

dev.off()
#######################################################


#################### Plot Gplus score #####################
pdf(file = "/rds/general/user/md2620/home/asthma/Malo/Results/Optimal_K_unsup_scaled_retest/gplus.pdf",   # The directory you want to save the file in
    width = 8, # The width of the plot in inches
    height = 8)

plot(df_results_kprot_2_to_15_un_scaled$K,df_results_kprot_2_to_15_un_scaled$g_plus, frame = TRUE, 
     col = "black",
     type = "o",
     pch = 21,
     bg = ifelse(df_results_kprot_2_to_15_un_scaled$g_plus == max(df_results_kprot_2_to_15_un_scaled$g_plus), "red", "black"),
     xlab="Number of clusters K",
     ylab="Gplus score")
legend("topright", legend = "optimum criterion : max")
title(main = "Gplus score for unsupervised K-prototypes")

dev.off()
#######################################################

#################### Plot mcclain score #####################
pdf(file = "/rds/general/user/md2620/home/asthma/Malo/Results/Optimal_K_unsup_scaled_retest/mcclain.pdf",   # The directory you want to save the file in
    width = 8, # The width of the plot in inches
    height = 8)

plot(df_results_kprot_2_to_15_un_scaled$K,df_results_kprot_2_to_15_un_scaled$mcclain_rao, frame = TRUE, 
     col = "black",
     type = "o",
     pch = 21,
     bg = ifelse(df_results_kprot_2_to_15_un_scaled$mcclain_rao == min(df_results_kprot_2_to_15_un_scaled$mcclain_rao), "red", "black"),
     xlab="Number of clusters K",
     ylab="Mcclain score")
legend("topright", legend = "optimum criterion : min")
title(main = "Mcclain score for unsupervised K-prototypes")

dev.off()
#######################################################

#################### Plot point biserial score #####################
pdf(file = "/rds/general/user/md2620/home/asthma/Malo/Results/Optimal_K_unsup_scaled_retest/ptbiserial.pdf",   # The directory you want to save the file in
    width = 8, # The width of the plot in inches
    height = 8)

plot(df_results_kprot_2_to_15_un_scaled$K,df_results_kprot_2_to_15_un_scaled$point_biserial, frame = TRUE, 
     col = "black",
     type = "o",
     pch = 21,
     bg = ifelse(df_results_kprot_2_to_15_un_scaled$point_biserial == max(df_results_kprot_2_to_15_un_scaled$point_biserial), "red", "black"),
     xlab="Number of clusters K",
     ylab="Point Biserial score")
legend("topright", legend = "optimum criterion : max")
title(main = "Point Biserial score for unsupervised K-prototypes")

dev.off()
#######################################################

#################### Plot tau score #####################
pdf(file = "/rds/general/user/md2620/home/asthma/Malo/Results/Optimal_K_unsup_scaled_retest/tau.pdf",   # The directory you want to save the file in
    width = 8, # The width of the plot in inches
    height = 8)

plot(df_results_kprot_2_to_15_un_scaled$K,df_results_kprot_2_to_15_un_scaled$tau, frame = TRUE, 
     col = "black",
     type = "o",
     pch = 21,
     bg = ifelse(df_results_kprot_2_to_15_un_scaled$tau == max(df_results_kprot_2_to_15_un_scaled$tau), "red", "black"),
     xlab="Number of clusters K",
     ylab="Tau score")
legend("topright", legend = "optimum criterion : max")
title(main = "Tau score for unsupervised K-prototypes")

dev.off()
#######################################################

#################### Plot silhouette score #####################
pdf(file = "/rds/general/user/md2620/home/asthma/Malo/Results/Optimal_K_unsup_scaled_retest/silhouette.pdf",   # The directory you want to save the file in
    width = 8, # The width of the plot in inches
    height = 8)

plot(df_results_kprot_2_to_15_un_scaled$K,df_results_kprot_2_to_15_un_scaled$silhouette, frame = TRUE, 
     col = "black",
     type = "o",
     pch = 21,
     bg = ifelse(df_results_kprot_2_to_15_un_scaled$silhouette == max(df_results_kprot_2_to_15_un_scaled$silhouette), "red", "black"),
     xlab="Number of clusters K",
     ylab="Silhouette score")
legend("topright", legend = "optimum criterion : max")
title(main = "Silhouette score for unsupervised K-prototypes")

dev.off()
#######################################################

##### According to viz via PCA, optimal unsupervised K = 2 or K = 4.
##### Internal indices confirm this with potentially K=12
##### Calinski-Harabasz score is maximized for K = 4. 