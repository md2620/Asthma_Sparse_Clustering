### MALO DIROU ###
### Sept 16th - 2021 ###
### Unsupervised K-prototypes with standardized numerical and categorical variables

#Read File - Scaled categorical variables after having labeled-encoded them.
df_HTML_processed = read.csv("/rds/general/user/md2620/home/asthma/Malo/Dataframes/df_for_HTML_tables_processed.csv")

#Set Subject.ID as index
df_HTML_processed_2 <- df_HTML_processed[-1]
row.names(df_HTML_processed_2) <- df_HTML_processed$X
View(df_HTML_processed_2)

#Drop cluster variable
df_HTML_processed_2 = subset(df_HTML_processed_2, select = -c(cluster))

#Create an index of your original categorical variables
index <- c(176:362)
df_HTML_processed_2[,index] <- lapply(df_HTML_processed_2[ ,index], as.factor)

# apply k prototypes - use 2 to 10 clusters
kscale_2 <- kproto(df_HTML_processed_2, 2, keep.data=TRUE)
kscale_3 <- kproto(df_HTML_processed_2, 3, keep.data=TRUE)
kscale_4 <- kproto(df_HTML_processed_2, 4, keep.data=TRUE)
kscale_5 <- kproto(df_HTML_processed_2, 5, keep.data=TRUE)
kscale_6 <- kproto(df_HTML_processed_2, 6, keep.data=TRUE)
kscale_7 <- kproto(df_HTML_processed_2, 7, keep.data=TRUE)
kscale_8 <- kproto(df_HTML_processed_2, 8, keep.data=TRUE)
kscale_9 <- kproto(df_HTML_processed_2, 9, keep.data=TRUE)
kscale_10 <- kproto(df_HTML_processed_2, 10, keep.data=TRUE)

#If you want to see where differences come from among the different clusters (viz)
#clprofiles(kscale_2,df_HTML_processed_2)
#clprofiles(kscale_3,df_HTML_processed_2)
#clprofiles(kscale,df_HTML_processed_2)
#clprofiles(kscale_5,df_HTML_processed_2)
#clprofiles(kscale_6,df_HTML_processed_2)
#clprofiles(kscale_7,df_HTML_processed_2)
#clprofiles(kscale_8,df_HTML_processed_2)
#clprofiles(kscale_9,df_HTML_processed_2)
#clprofiles(kscale_10,df_HTML_processed_2)

#Get summary for each observation according to cluster member
#summary(kscale_2)
#summary(kscale_3)
#summary(kscale)
#summary(kscale_5)
#summary(kscale_6)
#summary(kscale_7)
#summary(kscale_8)
#summary(kscale_9)
#summary(kscale_10)

#Visualize cluster
#fviz_cluster(kscale_3)

#Compute internal validation indices to find optimal number of clusters K
#install.packages("clusterCrit")
library(clusterCrit)
# Compute all the internal indices for the k_proto algorithms you just ran
#### NOTE APPART #####
kscale_2$cluster #each datapoint appended to its respective cluster.
######################

#Transform to matrix the dataframe, also it can be handled by intCriteria function
df_HTML_processed_2_matrix = data.matrix(df_HTML_processed_2)

results_kscale_2 = intCriteria(df_HTML_processed_2_matrix,kscale_2$cluster,"all")
df_results_kscale_2 = as.data.frame(results_kscale_2,row.names = "2")
df_results_kscale_2$K <- row.names(df_results_kscale_2)

results_kscale_3 = intCriteria(df_HTML_processed_2_matrix,kscale_3$cluster,"all")
df_results_kscale_3 = as.data.frame(results_kscale_3,row.names = "3")
df_results_kscale_3$K <- row.names(df_results_kscale_3)

#Append together df_results_kscale_2 and df_results_kscale_3
df_results_kscale_2_3 = dplyr::bind_rows(df_results_kscale_2,df_results_kscale_3)

results_kscale_4 = intCriteria(df_HTML_processed_2_matrix,kscale_4$cluster,"all")
df_results_kscale_4 = as.data.frame(results_kscale_4,row.names = "4")
df_results_kscale_4$K <- row.names(df_results_kscale_4)

#Append together df_results_kscale_2_3 and df_results_kscale
df_results_kscale_2_3_4 = dplyr::bind_rows(df_results_kscale_2_3,df_results_kscale_4)

results_kscale_5 = intCriteria(df_HTML_processed_2_matrix,kscale_5$cluster,"all")
df_results_kscale_5 = as.data.frame(results_kscale_5,row.names = "5")
df_results_kscale_5$K <- row.names(df_results_kscale_5)

#Append together df_results_kscale_2_3_4 and df_results_kscale_5
df_results_kscale_2_to_5 = dplyr::bind_rows(df_results_kscale_2_3_4,df_results_kscale_5)

results_kscale_6 = intCriteria(df_HTML_processed_2_matrix,kscale_6$cluster,"all")
df_results_kscale_6 = as.data.frame(results_kscale_6,row.names = "6")
df_results_kscale_6$K <- row.names(df_results_kscale_6)

#Append together df_results_kscale_2_to_5 and df_results_kscale_6
df_results_kscale_2_to_6 = dplyr::bind_rows(df_results_kscale_2_to_5,df_results_kscale_6)

results_kscale_7 = intCriteria(df_HTML_processed_2_matrix,kscale_7$cluster,"all")
df_results_kscale_7 = as.data.frame(results_kscale_7,row.names = "7")
df_results_kscale_7$K <- row.names(df_results_kscale_7)

#Append together df_results_kscale_2_to_6 and df_results_kscale_7
df_results_kscale_2_to_7 = dplyr::bind_rows(df_results_kscale_2_to_6,df_results_kscale_7)

results_kscale_8 = intCriteria(df_HTML_processed_2_matrix,kscale_8$cluster,"all")
df_results_kscale_8 = as.data.frame(results_kscale_8,row.names = "8")
df_results_kscale_8$K <- row.names(df_results_kscale_8)

#Append together df_results_kscale_2_to_7 and df_results_kscale_8
df_results_kscale_2_to_8 = dplyr::bind_rows(df_results_kscale_2_to_7,df_results_kscale_8)

results_kscale_9 = intCriteria(df_HTML_processed_2_matrix,kscale_9$cluster,"all")
df_results_kscale_9 = as.data.frame(results_kscale_9,row.names = "9")
df_results_kscale_9$K <- row.names(df_results_kscale_9)

#Append together df_results_kscale_2_to_8 and df_results_kscale_9
df_results_kscale_2_to_9 = dplyr::bind_rows(df_results_kscale_2_to_8,df_results_kscale_9)

results_kscale_10 = intCriteria(df_HTML_processed_2_matrix,kscale_10$cluster,"all")
df_results_kscale_10 = as.data.frame(results_kscale_10,row.names = "10")
df_results_kscale_10$K <- row.names(df_results_kscale_10)

#Append together df_results_kscale_2_to_9 and df_results_kscale_10
df_results_kscale_2_to_10 = dplyr::bind_rows(df_results_kscale_2_to_9,df_results_kscale_10)

########### IF YOU WANTED TO Compute some of the indexes ONLY #############
#intCriteria(x,cl$cluster,c("C_index","Calinski_Harabasz","Dunn"))
# The names are case insensitive and can be abbreviated
#intCriteria(x,cl$cluster,c("det","cal","dav"))
####################################################################

#################### Plot C_Index #####################
pdf(file = "/rds/general/user/md2620/home/asthma/Malo/Results/Optimal_K_cat_scaled/Cindex.pdf",   # The directory you want to save the file in
    width = 8, # The width of the plot in inches
    height = 8)
plot(df_results_kscale_2_to_10$K,df_results_kscale_2_to_10$c_index, frame = TRUE, 
     col = "black",
     type = "o",
     pch = 21,
     bg = ifelse(df_results_kscale_2_to_10$c_index == min(df_results_kscale_2_to_10$c_index), "red", "black"),
     xlab="Number of clusters K",
     ylab="C Index")
legend("topright", legend = "optimum criterion : min")
title(main = "C Index for K-prototypes")
dev.off()
#######################################################

#################### Plot Calinski-Harabasz score #####################
pdf(file = "/rds/general/user/md2620/home/asthma/Malo/Results/Optimal_K_cat_scaled/Calinski_Harabasz.pdf",   # The directory you want to save the file in
    width = 8, # The width of the plot in inches
    height = 8)
plot(df_results_kscale_2_to_10$K,df_results_kscale_2_to_10$calinski_harabasz, frame = TRUE, 
     col = "black",
     type = "o",
     pch = 21,
     bg = ifelse(df_results_kscale_2_to_10$calinski_harabasz == max(df_results_kscale_2_to_10$calinski_harabasz), "red", "black"),
     xlab="Number of clusters K",
     ylab="Calinski-Harabasz score")
legend("topright", legend = "optimum criterion : max")
title(main = "Calinski-Harabasz score for K-prototypes")
dev.off()
#######################################################

#################### Plot Dunn score #####################
pdf(file = "/rds/general/user/md2620/home/asthma/Malo/Results/Optimal_K_cat_scaled/Dunn.pdf",   # The directory you want to save the file in
    width = 8, # The width of the plot in inches
    height = 8)

plot(df_results_kscale_2_to_10$K,df_results_kscale_2_to_10$dunn, frame = TRUE, 
     col = "black",
     type = "o",
     pch = 21,
     bg = ifelse(df_results_kscale_2_to_10$dunn == max(df_results_kscale_2_to_10$dunn), "red", "black"),
     xlab="Number of clusters K",
     ylab="Dunn score")
legend("topleft", legend = "optimum criterion : max")
title(main = "Dunn score for K-prototypes")

dev.off()
#######################################################

#################### Plot Gamma score #####################
pdf(file = "/rds/general/user/md2620/home/asthma/Malo/Results/Optimal_K_cat_scaled/gamma.pdf",   # The directory you want to save the file in
    width = 8, # The width of the plot in inches
    height = 8)

plot(df_results_kscale_2_to_10$K,df_results_kscale_2_to_10$gamma, frame = TRUE, 
     col = "black",
     type = "o",
     pch = 21,
     bg = ifelse(df_results_kscale_2_to_10$gamma == max(df_results_kscale_2_to_10$gamma), "red", "black"),
     xlab="Number of clusters K",
     ylab="Gamma score")
legend("topright", legend = "optimum criterion : max")
title(main = "Gamma score for K-prototypes")

dev.off()
#######################################################


#################### Plot Gplus score #####################
pdf(file = "/rds/general/user/md2620/home/asthma/Malo/Results/Optimal_K_cat_scaled/gplus.pdf",   # The directory you want to save the file in
    width = 8, # The width of the plot in inches
    height = 8)

plot(df_results_kscale_2_to_10$K,df_results_kscale_2_to_10$g_plus, frame = TRUE, 
     col = "black",
     type = "o",
     pch = 21,
     bg = ifelse(df_results_kscale_2_to_10$g_plus == max(df_results_kscale_2_to_10$g_plus), "red", "black"),
     xlab="Number of clusters K",
     ylab="Gplus score")
legend("topright", legend = "optimum criterion : max")
title(main = "Gplus score for K-prototypes")

dev.off()
#######################################################

#################### Plot mcclain score #####################
pdf(file = "/rds/general/user/md2620/home/asthma/Malo/Results/Optimal_K_cat_scaled/mcclain.pdf",   # The directory you want to save the file in
    width = 8, # The width of the plot in inches
    height = 8)

plot(df_results_kscale_2_to_10$K,df_results_kscale_2_to_10$mcclain_rao, frame = TRUE, 
     col = "black",
     type = "o",
     pch = 21,
     bg = ifelse(df_results_kscale_2_to_10$mcclain_rao == min(df_results_kscale_2_to_10$mcclain_rao), "red", "black"),
     xlab="Number of clusters K",
     ylab="Mcclain score")
legend("topright", legend = "optimum criterion : min")
title(main = "Mcclain score for K-prototypes")

dev.off()
#######################################################

#################### Plot point biserial score #####################
pdf(file = "/rds/general/user/md2620/home/asthma/Malo/Results/Optimal_K_cat_scaled/ptbiserial.pdf",   # The directory you want to save the file in
    width = 8, # The width of the plot in inches
    height = 8)

plot(df_results_kscale_2_to_10$K,df_results_kscale_2_to_10$point_biserial, frame = TRUE, 
     col = "black",
     type = "o",
     pch = 21,
     bg = ifelse(df_results_kscale_2_to_10$point_biserial == max(df_results_kscale_2_to_10$point_biserial), "red", "black"),
     xlab="Number of clusters K",
     ylab="Point Biserial score")
legend("topright", legend = "optimum criterion : max")
title(main = "Point Biserial score for K-prototypes")

dev.off()
#######################################################

#################### Plot tau score #####################
pdf(file = "/rds/general/user/md2620/home/asthma/Malo/Results/Optimal_K_cat_scaled/tau.pdf",   # The directory you want to save the file in
    width = 8, # The width of the plot in inches
    height = 8)

plot(df_results_kscale_2_to_10$K,df_results_kscale_2_to_10$tau, frame = TRUE, 
     col = "black",
     type = "o",
     pch = 21,
     bg = ifelse(df_results_kscale_2_to_10$tau == max(df_results_kscale_2_to_10$tau), "red", "black"),
     xlab="Number of clusters K",
     ylab="Tau score")
legend("topright", legend = "optimum criterion : max")
title(main = "Tau score for K-prototypes")

dev.off()
#######################################################

############ calculate optimal number of cluster, index values and cluster partition with Silhouette-index ############
k_max <- 10 
val_sil <- validation_kproto(method = "silhouette", data = df_HTML_processed_2, k = 2:k_max, nstart = 5)

#Save results for silhouette score
setwd("/rds/general/user/md2620/home/asthma/Malo/Results/Optimal_K_cat_scaled")
#install.packages("rlist")
require(rlist)
list.save(val_sil, 'silhouette.rds')

#Plot results from silhouette index
pdf(file = "/rds/general/user/md2620/home/asthma/Malo/Results/Optimal_K_cat_scaled/Silhouette.pdf",   # The directory you want to save the file in
    width = 8, # The width of the plot in inches
    height = 8)

plot(2:k_max,val_sil$indices, frame = TRUE, 
     col = "black",
     type = "o",
     pch = 21,
     bg = ifelse(val_sil$indices == max(val_sil$indices), "red", "black"),
     xlab="Number of clusters K",
     ylab="Silhouette score")
legend("topright", legend = "optimum criterion : max")
title(main = "Silhouette score for K-prototypes")

dev.off()
