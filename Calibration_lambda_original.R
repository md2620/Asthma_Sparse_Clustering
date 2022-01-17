########################## CALIBRATION OF LAMBDA FULL PREPROCESS OF NUMERICAL VARIABLES##########################
#Clear environment
rm(list=ls())

#Load dataset
final_df_unsup_import = read.csv("/rds/general/user/md2620/home/asthma/Malo/Dataframes/Final/df_unsup_num_processed.csv")

#Set Subject.ID as index
final_df_un <- final_df_unsup_import[-1]
row.names(final_df_un) <- final_df_unsup_import$Unnamed..0
View(final_df_un)

#Remove outcome variable cohort before running k-prototypes algo
final_df_un = subset(final_df_un, select = -c(cohort))

#Create an index of your original categorical variables
index_cat <- c(181:361)
final_df_un[,index_cat] <- lapply(final_df_un[ ,index_cat], as.factor)

num_cat <- c(1:180)
final_df_un[,num_cat] <- lapply(final_df_un[ ,num_cat], as.numeric)
View(sapply(final_df_un,class))

#Select numerical variables
num_final_df_un <- final_df_un %>%
  select_if(Negate(is.factor))

#Select categorical variables
cat_final_df_un<- final_df_un %>%
  select_if(Negate(is.numeric))

# For visualization purposes, we drop all variables that have 0 variance when data is scaled
num_final_df_un = subset(num_final_df_un, select = -c(Omics.Furoylglycine,Omics.Glutamic.acid,Omics.N.Acetylglutamic.acid,
                                                            Omics.Phenylalanine,Omics.Allantoin,Omics.Isoleucine,Omics.N.Acetylputrescine,
                                                            Omics.Sarcosine,Omics.Cytosine,Omics.Lysine,Omics.N.Methyl.D.aspartic.acid,
                                                            Omics.Xanthosine,Omics.Glucosamine,Omics.Maltose,Omics.O.Acetylserine,
                                                            Omics.Xylose))


#Combine cat and num dataframes before running kproto
final_df_un <- cbind(num_final_df_un, cat_final_df_un)

#Load kproto functions
source("/rds/general/user/md2620/home/asthma/Malo/R_Scripts/validation_k_proto_functions.R")

################################################ DO NOT RUN ###########################################
################################################ FOR PCA CALIBRATION ##################################
#Check that class of variables is respected
View(sapply(final_df_un,class))
# Using optimal number of clusters K = 3, calibrate lambda by re-running K-prototypes (unstandardized variables all the way)
############## ############## 
#Test for stability analysis function
kprot_test_C <- do.call(clustMixType::kproto,args = c(list(x=final_df_un,k=4,na.rm = FALSE,lambda=0.1)))
mygroups <- kprot_test_C$cluster

stats::kmeans
#End stability test

kprot_unsup3_proc_lbd_0.1 <- kproto(final_df_un, 3, keep.data=TRUE, na.rm = FALSE, lambda = 0.1) #close to KMeans
kprot_unsup3_proc_lbd_0.2 <- kproto(final_df_un, 3, keep.data=TRUE, na.rm = FALSE, lambda = 0.2)
kprot_unsup3_proc_lbd_0.3 <- kproto(final_df_un, 3, keep.data=TRUE, na.rm = FALSE, lambda = 0.3)
kprot_unsup3_proc_lbd_0.4 <- kproto(final_df_un, 3, keep.data=TRUE, na.rm = FALSE, lambda = 0.4)
kprot_unsup3_proc_lbd_0.5 <- kproto(final_df_un, 3, keep.data=TRUE, na.rm = FALSE, lambda = 0.5)
kprot_unsup3_proc_lbd_0.6 <- kproto(final_df_un, 3, keep.data=TRUE, na.rm = FALSE, lambda = 0.6)
kprot_unsup3_proc_lbd_0.7 <- kproto(final_df_un, 3, keep.data=TRUE, na.rm = FALSE, lambda = 0.7)
kprot_unsup3_proc_lbd_0.8 <- kproto(final_df_un, 3, keep.data=TRUE, na.rm = FALSE, lambda = 0.8)
kprot_unsup3_proc_lbd_0.9 <- kproto(final_df_un, 3, keep.data=TRUE, na.rm = FALSE, lambda = 0.9)
kprot_unsup3_proc_lbd_1 <- kproto(final_df_un, 3, keep.data=TRUE, na.rm = FALSE, lambda = 1)
kprot_unsup3_proc_lbd_2 <- kproto(final_df_un, 3, keep.data=TRUE, na.rm = FALSE, lambda = 2)
kprot_unsup3_proc_lbd_3 <- kproto(final_df_un, 3, keep.data=TRUE, na.rm = FALSE, lambda = 3)
#Optimal one below
kprot_unsup3_proc_lbd_3.1 <- kproto(final_df_un, 3, keep.data=TRUE, na.rm = FALSE, lambda = 3.1)
kprot_unsup3_proc_lbd_4 <- kproto(final_df_un, 3, keep.data=TRUE, na.rm = FALSE, lambda = 4)
kprot_unsup3_proc_lbd_5 <- kproto(final_df_un, 3, keep.data=TRUE, na.rm = FALSE, lambda = 5)
kprot_unsup3_proc_lbd_6 <- kproto(final_df_un, 3, keep.data=TRUE, na.rm = FALSE, lambda = 6)
kprot_unsup3_proc_lbd_7 <- kproto(final_df_un, 3, keep.data=TRUE, na.rm = FALSE, lambda = 7)
kprot_unsup3_proc_lbd_8 <- kproto(final_df_un, 3, keep.data=TRUE, na.rm = FALSE, lambda = 8)
kprot_unsup3_proc_lbd_9 <- kproto(final_df_un, 3, keep.data=TRUE, na.rm = FALSE, lambda = 9)
kprot_unsup3_proc_lbd_10 <- kproto(final_df_un, 3, keep.data=TRUE, na.rm = FALSE, lambda = 10)
kprot_unsup3_proc_lbd_11 <- kproto(final_df_un, 3, keep.data=TRUE, na.rm = FALSE, lambda = 11)
kprot_unsup3_proc_lbd_12 <- kproto(final_df_un, 3, keep.data=TRUE, na.rm = FALSE, lambda = 12)
kprot_unsup3_proc_lbd_13 <- kproto(final_df_un, 3, keep.data=TRUE, na.rm = FALSE, lambda = 13)
kprot_unsup3_proc_lbd_14 <- kproto(final_df_un, 3, keep.data=TRUE, na.rm = FALSE, lambda = 14)
kprot_unsup3_proc_lbd_15 <- kproto(final_df_un, 3, keep.data=TRUE, na.rm = FALSE, lambda = 15)
kprot_unsup3_proc_lbd_25 <- kproto(final_df_un, 3, keep.data=TRUE, na.rm = FALSE, lambda = 25) #emphasize a lot the weight given to categorical variables

#Estimated lambda by default (does not advantage one type of variables over the other:
x <- lambdaest(final_df_un, num.method = 1, fac.method = 1, outtype = "numeric")

#Lambda with respective individual weight given to each variable allows to find average numeric and categorical variation respectively:
x_ind_vector <- lambdaest(final_df_un, num.method = 1, fac.method = 1, outtype = "vector") ### Lambda normal = 2.306257

# Setting lambda = Average numeric variation / Average categorical variation = 1 (up to max of 2) allows to not favor one type of variables over the other when clustering.
lambda_actual <- 1.001642/0.4343151 #Variation in the data by default. =2.306257 here (emphasized towards numerical variables here)

#Change heuristic (sd instead of variance for numerical variables and absolute value instead of square root for categorical variables too)
x_2 <- lambdaest(final_df_un, num.method = 2, fac.method = 2, outtype = "numeric") #Default lambda with these heuristics = 3.021185
x_ind_vector_2 <- lambdaest(final_df_un, num.method = 2, fac.method = 2, outtype = "vector") #Average numeric variation = 1.000821 ; Average categorical variation = 0.3312676  

#Try running 2 proto algorithms with these lambdas:
kprot_unsup3_proc_lbd_x <-  kproto(final_df_un, 3, keep.data=TRUE, na.rm = FALSE, lambda = x)
kprot_unsup3_proc_lbd_x_2 <-  kproto(final_df_un, 3, keep.data=TRUE, na.rm = FALSE, lambda = x_2)


#Append calibrated clusters with K=4 to final_df_unsup_import for viz -see which is best 
final_df_un$K3_lbd_0.1 <- factor(kprot_unsup3_proc_lbd_0.1$cluster)
final_df_un$K3_lbd_0.2 <- factor(kprot_unsup3_proc_lbd_0.2$cluster)
final_df_un$K3_lbd_0.3 <- factor(kprot_unsup3_proc_lbd_0.3$cluster)
final_df_un$K3_lbd_0.4 <- factor(kprot_unsup3_proc_lbd_0.4$cluster)
final_df_un$K3_lbd_0.5 <- factor(kprot_unsup3_proc_lbd_0.5$cluster)
final_df_un$K3_lbd_0.6 <- factor(kprot_unsup3_proc_lbd_0.6$cluster)
final_df_un$K3_lbd_0.7 <- factor(kprot_unsup3_proc_lbd_0.7$cluster)
final_df_un$K3_lbd_0.8 <- factor(kprot_unsup3_proc_lbd_0.8$cluster)
final_df_un$K3_lbd_0.9 <- factor(kprot_unsup3_proc_lbd_0.9$cluster)
final_df_un$K3_lbd_1 <- factor(kprot_unsup3_proc_lbd_1$cluster)
final_df_un$K3_lbd_2 <- factor(kprot_unsup3_proc_lbd_2$cluster)
final_df_un$K3_lbd_3 <- factor(kprot_unsup3_proc_lbd_3$cluster)
#Optimal one below
final_df_un$K3_lbd_3.1 <- factor(kprot_unsup3_proc_lbd_3.1$cluster)
final_df_un$K3_lbd_4 <- factor(kprot_unsup3_proc_lbd_4$cluster)
final_df_un$K3_lbd_5 <- factor(kprot_unsup3_proc_lbd_5$cluster)
final_df_un$K3_lbd_6 <- factor(kprot_unsup3_proc_lbd_6$cluster)
final_df_un$K3_lbd_7 <- factor(kprot_unsup3_proc_lbd_7$cluster)
final_df_un$K3_lbd_8 <- factor(kprot_unsup3_proc_lbd_8$cluster)
final_df_un$K3_lbd_9 <- factor(kprot_unsup3_proc_lbd_9$cluster)
final_df_un$K3_lbd_10 <- factor(kprot_unsup3_proc_lbd_10$cluster)
final_df_un$K3_lbd_11 <- factor(kprot_unsup3_proc_lbd_11$cluster)
final_df_un$K3_lbd_12 <- factor(kprot_unsup3_proc_lbd_12$cluster)
final_df_un$K3_lbd_13 <- factor(kprot_unsup3_proc_lbd_13$cluster)
final_df_un$K3_lbd_14 <- factor(kprot_unsup3_proc_lbd_14$cluster)
final_df_un$K3_lbd_15 <- factor(kprot_unsup3_proc_lbd_15$cluster)
final_df_un$K3_lbd_25 <- factor(kprot_unsup3_proc_lbd_25$cluster)
final_df_un$K3_default_lbd <- factor(kprot_unsup3_proc_lbd_x$cluster)
final_df_un$K3_heuristics_2_lbd <- factor(kprot_unsup3_proc_lbd_x_2$cluster)
final_df_un$cohort <- factor(final_df_unsup_import$cohort)
#Save dataset to visualize which lambda best separates the observations into the 3 clusters.
setwd("/rds/general/user/md2620/home/asthma/Malo/Dataframes/Final")
write.csv(final_df_un,"unsup_df_calib_lambda_original.csv")
################################################ END FOR PCA CALIBRATION ##################################

#################################### DO NOT RUN NEITHER ###################################
##### PAULA ##############
k_max <- 4
steps_lambda <- seq(from = 1, to = 5, by = 0.1)


df_total <- data.frame()
for(k in 3 : k_max){
  
  for (lambda in steps_lambda){
    mcclain_2 <- mcclain_kproto(data = final_df_un, k = k, lambda = lambda, na.rm=FALSE)
    row <- nrow(df_total)+1
    df_total[row,1] <- k
    df_total[row,2] <- mcclain_2$indices[1]
    df_total[row,3] <- lambda
    
    }
}

colnames(df_total) <- c("k","Mcclain_Score","lambda")

df_total_wide <- reshape(df_total, idvar = "k", timevar = "lambda", direction = "wide")

########## END PAULA ###########

############ GOOD ONE TO RUN FOR EVERY K = 1 to 15 and lambda = 0 to 5 with step size of 0.1 #########
k_max <- 4
steps_lambda_2 <- seq(from = 1, to = 1.5, by = 0.1)

df_total_2 <- data.frame()
for(k in 2 : k_max){
  
  for (lambda in steps_lambda_2){
    mcclain_test <- mcclain_kproto(data = final_df_un, k = k, lambda = lambda, na.rm=FALSE)
    row_test <- nrow(df_total_2)+1
    df_total_2[row_test,1] <- k
    df_total_2[row_test,2] <- lambda
    df_total_2[row_test,3] <- mcclain_test$indices[1]
    
  }
}

colnames(df_total_2) <- c("K","Lambda","Mcclain_Score")

#Save dataframe for lists
write.csv(df_total_2,"/rds/general/user/md2620/home/asthma/Malo/Results/Calibration/df_mcclain_example.csv")

#Run matrix from there 
# Link : 
library(ggplot2)
plt_mcclain <- ggplot(df_total_2,aes(Lambda,K))+
  geom_tile(aes(fill=Mcclain_Score))+
  ggtitle("Calibration of lambda coefficient and K to minimize McClain score")

ggsave("/rds/general/user/md2620/home/asthma/Malo/Results/Calibration/mcclain_matrix_example.jpeg", width = 8, height = 6)
########################## END EXAMPLE WITH MATRIX DONE #############################################
#################################### END DO NOT RUN NEITHER ###################################

########################### SERIOUS #############################################################
#### MCCLAIN K-LAMBDA CALIBRATION UNSUPERVISED K-PROTOTYPE ############
k_max <- 4
steps_lambda_mcclain <- seq(from = 0.1, to = 5, by = 0.1)

df_total_mcclain <- data.frame()
for(k in 2 : k_max){
  
  for (lambda in steps_lambda_mcclain){
    mcclain <- mcclain_kproto(data = final_df_un, k = k, lambda = lambda, na.rm=FALSE)
    row_mcclain <- nrow(df_total_mcclain)+1
    df_total_mcclain[row_mcclain,1] <- k
    df_total_mcclain[row_mcclain,2] <- lambda
    df_total_mcclain[row_mcclain,3] <- mcclain$indices[1]
    
  }
}

colnames(df_total_mcclain) <- c("K","Lambda","Mcclain_Score")

#Save dataframe for lists
write.csv(df_total_mcclain,"/rds/general/user/md2620/home/asthma/Malo/Results/Calibration/df_mcclain.csv")

#Run matrix from there 
# Link : 
library(ggplot2)
plt_mcclain_final <- ggplot(df_total_mcclain,aes(Lambda,K))+
  geom_tile(aes(fill=Mcclain_Score))+
  ggtitle("Calibration of lambda coefficient and K to minimize McClain score")

plt_mcclain_final
ggsave("/rds/general/user/md2620/home/asthma/Malo/Results/Calibration/mcclain_matrix.jpeg", width = 14, height = 10)
########################## END + MATRIX DONE #############################################


#### DUNN K-LAMBDA CALIBRATION UNSUPERVISED K-PROTOTYPE ############
#k_max <- 15
rm(lambda)
steps_lambda_dunn <- seq(from = 0.1, to = 0.3, by = 0.1)

df_total_dunn <- data.frame()
for(k in 2 : k_max){
  
  for (lambda in steps_lambda_dunn){
    dunn <- dunn_kproto(data = final_df_un, k = k, lambda = lambda, na.rm=FALSE)
    row_dunn <- nrow(df_total_dunn)+1
    df_total_dunn[row_dunn,1] <- k
    df_total_dunn[row_dunn,2] <- lambda
    df_total_dunn[row_dunn,3] <- dunn$indices[1]
    
  }
}

colnames(df_total_dunn) <- c("K","Lambda","Dunn_Score")

#Save dataframe for lists
write.csv(df_total_dunn,"/rds/general/user/md2620/home/asthma/Malo/Results/Calibration/df_dunn.csv")

#Run matrix from there 
# Link : 
library(ggplot2)
plt_dunn <- ggplot(df_total_dunn,aes(Lambda,K))+
  geom_tile(aes(fill=Dunn_Score))+
  ggtitle("Calibration of lambda coefficient and K to maximize Dunn score")

plt_dunn
ggsave("/rds/general/user/md2620/home/asthma/Malo/Results/Calibration/dunn_matrix.jpeg", width = 14, height = 10)
########################## END DUNN + MATRIX DONE #############################################

#### Point-Biserial K-LAMBDA CALIBRATION UNSUPERVISED K-PROTOTYPE ############
#k_max <- 15
rm(lambda)
steps_lambda_ptbiserial <- seq(from = 0.1, to = 5, by = 0.1)

df_total_ptbiserial <- data.frame()
for(k in 2 : k_max){
  
  for (lambda in steps_lambda_ptbiserial){
    ptbiserial <- ptbiserial_kproto(data = final_df_un, k = k, lambda = lambda, na.rm=FALSE)
    row_ptbiserial <- nrow(df_total_ptbiserial)+1
    df_total_ptbiserial[row_ptbiserial,1] <- k
    df_total_ptbiserial[row_ptbiserial,2] <- lambda
    df_total_ptbiserial[row_ptbiserial,3] <- ptbiserial$indices[1]
    
  }
}

colnames(df_total_ptbiserial) <- c("K","Lambda","Pt_Biserial_Score")

#Save dataframe for lists
write.csv(df_total_ptbiserial,"/rds/general/user/md2620/home/asthma/Malo/Results/Calibration/df_ptbiserial.csv")

#Run matrix from there 
# Link : 
library(ggplot2)
plt_ptbiserial <- ggplot(df_total_ptbiserial,aes(Lambda,K))+
  geom_tile(aes(fill=Pt_Biserial_Score))+
  ggtitle("Calibration of lambda coefficient and K to maximize Point-Biserial score")

plt_ptbiserial
ggsave("/rds/general/user/md2620/home/asthma/Malo/Results/Calibration/ptbiserial_matrix.jpeg", width = 14, height = 10)

########################## END PTBISERIAL + MATRIX DONE #############################################


#### GAMMA K-LAMBDA CALIBRATION UNSUPERVISED K-PROTOTYPE ############
k_max <- 15
rm(lambda)
steps_lambda_gamma <- seq(from = 0.1, to = 5, by = 0.2)

test <- gamma_kproto(data = final_df_un, k = 2, lambda = 0.1, na.rm=FALSE)
test_2 <- ptbiserial_kproto(data = final_df_un, k = 2, lambda = 0.1, na.rm=FALSE)
df_total_gamma <- data.frame()
for(k in 2 : k_max){
  
  for (lambda in steps_lambda_gamma){
    gamma <- gamma_kproto(data = final_df_un, k = k, lambda = lambda, na.rm=FALSE) 
    row_gamma <- nrow(df_total_gamma)+1
    df_total_gamma[row_gamma,1] <- k
    df_total_gamma[row_gamma,2] <- lambda
    df_total_gamma[row_gamma,3] <- gamma$indices[1]
    
  }
}

colnames(df_total_gamma) <- c("K","Lambda","Gamma_Score")

#Save dataframe for lists
write.csv(df_total_gamma,"/rds/general/user/md2620/home/asthma/Malo/Results/Calibration/df_gamma.csv")

#Run matrix from there 
# Link : 
library(ggplot2)
plt_gamma <- ggplot(df_total_gamma,aes(Lambda,K))+
  geom_tile(aes(fill=Gamma_Score))+
  ggtitle("Calibration of lambda coefficient and K to maximize Gamma score")

plt_gamma
ggsave("/rds/general/user/md2620/home/asthma/Malo/Results/Calibration/gamma_matrix.jpeg", width = 14, height = 10)

########################## END GAMMA + MATRIX DONE #############################################


#### GPLUS K-LAMBDA CALIBRATION UNSUPERVISED K-PROTOTYPE ############
#k_max <- 15
rm(lambda)
steps_lambda_gplus <- seq(from = 0.1, to = 5, by = 0.1)

df_total_gplus <- data.frame()
for(k in 2 : k_max){
  
  for (lambda in steps_lambda_gplus){
    gplus <- gplus_kproto(data = final_df_un, k = k, lambda = lambda, na.rm=FALSE)
    row_gplus <- nrow(df_total_gplus)+1
    df_total_gplus[row_gplus,1] <- k
    df_total_gplus[row_gplus,2] <- lambda
    df_total_gplus[row_gplus,3] <- gplus$indices[1]
    
  }
}

colnames(df_total_gplus) <- c("K","Lambda","Gplus_Score")

#Save dataframe for lists
write.csv(df_total_gplus,"/rds/general/user/md2620/home/asthma/Malo/Results/Calibration/df_gplus.csv")

#Run matrix from there 
# Link : 
library(ggplot2)
plt_gplus <- ggplot(df_total_gplus,aes(Lambda,K))+
  geom_tile(aes(fill=Gplus_Score))+
  ggtitle("Calibration of lambda coefficient and K to minimize Gplus score")

plt_gplus
ggsave("/rds/general/user/md2620/home/asthma/Malo/Results/Calibration/gplus_matrix.jpeg", width = 14, height = 10)

########################## END GPLUS + MATRIX DONE #############################################


#### SILHOUETTE K-LAMBDA CALIBRATION UNSUPERVISED K-PROTOTYPE ############
k_max <- 15
rm(lambda)
steps_lambda_silhouette <- seq(from = 0.1, to = 5, by = 0.2)

df_total_silhouette <- data.frame()
for(k in 2 : k_max){
  
  for (lambda in steps_lambda_silhouette){
    sil <- silhouette_kproto(data = final_df_un, k = k, lambda = lambda, na.rm=FALSE)
    row_silhouette <- nrow(df_total_silhouette)+1
    df_total_silhouette[row_silhouette,1] <- k
    df_total_silhouette[row_silhouette,2] <- lambda
    df_total_silhouette[row_silhouette,3] <- sil$indices[1]
  }
}


colnames(df_total_silhouette) <- c("K","Lambda","Silhouette_Score")

#Save dataframe for lists
write.csv(df_total_silhouette,"/rds/general/user/md2620/home/asthma/Malo/Results/Calibration/df_silhouette.csv")

#Run matrix from there 
# Link : 
library(ggplot2)
plt_silhouette <- ggplot(df_total_silhouette,aes(Lambda,K))+
  geom_tile(aes(fill=Silhouette_Score))+
  ggtitle("Calibration of lambda coefficient and K to maximize Silhouette score")

plt_silhouette
ggsave("/rds/general/user/md2620/home/asthma/Malo/Results/Calibration/silhouette_matrix.jpeg", width = 14, height = 10)

########################## END SILHOUETTE + MATRIX DONE #############################################


#### GPLUS K-LAMBDA CALIBRATION UNSUPERVISED K-PROTOTYPE ############
#k_max <- 15
rm(lambda)
steps_lambda_tau <- seq(from = 0.1, to = 5, by = 0.1)

df_total_tau <- data.frame()
for(k in 2 : k_max){
  
  for (lambda in steps_lambda_tau){
    tau <- tau_kproto(data = final_df_un, k = k, lambda = lambda, na.rm=FALSE)
    row_tau <- nrow(df_total_tau)+1
    df_total_tau[row_tau,1] <- k
    df_total_tau[row_tau,2] <- lambda
    df_total_tau[row_tau,3] <- tau$indices[1]
    
  }
}

colnames(df_total_tau) <- c("K","Lambda","Tau_Score")

#Save dataframe for lists
write.csv(df_total_tau,"/rds/general/user/md2620/home/asthma/Malo/Results/Calibration/df_tau.csv")

#Run matrix from there 
# Link : 
library(ggplot2)
plt_tau <- ggplot(df_total_tau,aes(Lambda,K))+
  geom_tile(aes(fill=Tau_Score))+
  ggtitle("Calibration of lambda coefficient and K to maximize Tau score")

plt_tau
ggsave("/rds/general/user/md2620/home/asthma/Malo/Results/Calibration/tau_matrix.jpeg", width = 14, height = 10)

########################## END TAU + MATRIX DONE #############################################


#### CINDEX K-LAMBDA CALIBRATION UNSUPERVISED K-PROTOTYPE ############
#k_max <- 15
rm(lambda)
steps_lambda_cindex <- seq(from = 0.1, to = 5, by = 0.1)

df_total_cindex <- data.frame()
for(k in 2 : k_max){
  
  for (lambda in steps_lambda_cindex){
    cindex <- cindex_kproto(data = final_df_un, k = k, lambda = lambda, na.rm=FALSE)
    row_cindex <- nrow(df_total_cindex)+1
    df_total_cindex[row_cindex,1] <- k
    df_total_cindex[row_cindex,2] <- lambda
    df_total_cindex[row_cindex,3] <- cindex$indices[1]
    
  }
}

colnames(df_total_cindex) <- c("K","Lambda","Cindex_Score")

#Save dataframe for lists
write.csv(df_total_cindex,"/rds/general/user/md2620/home/asthma/Malo/Results/Calibration/df_cindex.csv")

#Run matrix from there 
# Link : 
library(ggplot2)
plt_cindex <- ggplot(df_total_cindex,aes(Lambda,K))+
  geom_tile(aes(fill=Cindex_Score))+
  ggtitle("Calibration of lambda coefficient and K to minimize Cindex score")

plt_cindex
ggsave("/rds/general/user/md2620/home/asthma/Malo/Results/Calibration/cindex_matrix.jpeg", width = 14, height = 10)

########################## END CINDEX + MATRIX DONE #############################################


## DRAFT ##

# calculate optimal number of cluster, index values and clusterpartition with Silhouette-index
val <- validation_kproto(method = "silhouette", data = final_df_un, k = 3:5, lambda = 1)
val$indices

#install.packages("githubinstall") # if you have not installed "githubinstall" package
#githubinstall::githubinstall("magicfor")
library(magicfor)

magic_for(progress = TRUE)
lambda_max <- 3 
for (lambda in 1:lambda_max){
  validation_kproto(method = "silhouette", data = final_df_un, k = 2:3, lambda = lambda, nstart=2, na.rm=FALSE)
}
magic_result_as_dataframe()

#For some reason, function does not work as lamb is not define
#search <- optilambda(final_df_un,k=3:5,iter.max=30,nstart=2,keep.data = TRUE)
val_dunn <- validation_kproto(method = "ptbiserial", data = final_df_un, object= k = 2:3, nstart=2, na.rm=FALSE)
val_2 <- validation_kproto(method = "silhouette", data = final_df_un, k = 2:3, nstart=2, na.rm=FALSE)

#Optimal number of K according to mcclain score
mcclain <- mcclain_kproto(data = final_df_un, k = 2:3, nstart=2, na.rm=FALSE)
mcclain$k_opt

#Optimal number of K according to dunn
dunn_kproto()

#Optimal number of K according to cindex
cindex_kproto()

lambda_max <- 2

result <- data.frame(matrix(nrow = 3, ncol = 3))
colnames(result) <- c("lambda","k","Mcclain_Score")

#Check results from first k-proto
kprot_unsup3_proc_lbd_0.1$iter
kprot_unsup3_proc_lbd_0.1$withinss
kprot_unsup3_proc_lbd_0.1$tot.withinss
kprot_unsup3_proc_lbd_3$tot.withinss
kprot_unsup3_proc_lbd_3$withinss
kprot_unsup3_proc_lbd_9$tot.withinss
kprot_unsup3_proc_lbd_9$withinss
kprot_unsup3_proc_lbd_9$lambda
kprot_unsup3_proc_lbd_2$tot.withinss



library(ggplot2)
ggplot(df_total,aes(lambda,k))+
  geom_tile(aes(fill=Mcclain_Score))

ggplot(data=df_total_wide,aes(x=df_total_wide[,2],y=df_total_wide[,1],fill=df_total_wide[,3]))+
  geom_tile()


df <- data.frame(matrix(unlist(mcclain_2)))
df <- data.frame(mcclain_2)
df_total <- rbind(df_total,df)

lambda_max <- 3
k_max <- 5
result <- data.frame(matrix(nrow = 3, ncol = 3))
colnames(result) <- c("lambda","k","Mcclain_Score")
for (lambda in 1:lambda_max){
  for(k in 3 : k_max){
    mcclain_2 <- mcclain_kproto(data = final_df_un, k = k, lambda = lambda, nstart=2, na.rm=FALSE)
    result[lambda, 1] <- lambda
    result[lambda, 2] <- k
    result[lambda, 3] <- mcclain_2$indices
  }
}


lambda_max <- 3
k_max <- 5
result <- data.frame(matrix(nrow = 9, ncol = 3))
colnames(result) <- c("k","lambda","Mcclain_Score")
for(k in 3:k_max){
  for (lambda in 1:lambda_max){
    mcclain_2 <- mcclain_kproto(data = final_df_un, k = k, lambda = lambda, nstart=2, na.rm=FALSE)
    result[k, 1] <- k
    result[k, 2] <- lambda
    result[k, 3] <- mcclain_2$indices
  }
}


lambda_max <- 3
k_max <- 5
result <- data.frame(matrix(nrow = 9, ncol = 2))
colnames(result) <- c("k","lambda","Mcclain_Score")
for(k in 3:k_max){
  for (lambda in 1:lambda_max){
    mcclain_3 <- mcclain_kproto(data = final_df_un, k = k, lambda = lambda, nstart=2, na.rm=FALSE)
    result[k, 1] <- k
    result[k, 2] <- lambda
    result[k, 3] <- mcclain_3$indices
  }
}

#Try again

result <- data.frame(matrix(nrow = 9, ncol = 3))
colnames(result) <- c("k","lambda","Mcclain_Score")
for(k in 4:k_max){
  for (lambda in 2:lambda_max){
    mcclain_3 <- mcclain_kproto(data = final_df_un, k = k, lambda = lambda, na.rm=FALSE)
    result[,1] <- k
    result[,2] <- lambda
    result[,3] <- mcclain_3$indices
  }
}


mcclain_3$indices
mcclain_2$indices

for (lambda in 1:lambda_max)
  {
    mcclain_2 <- mcclain_kproto(data = final_df_un, k = k, lambda = lambda, nstart=2, na.rm=FALSE)
    result[lambda, 1] <- lambda
    result[lambda, 2] <- k
    result[lambda, 3] <- mcclain_2$indices
  }
}


lambda_max <- 3
k_max <- 5
result <- data.frame(matrix(nrow = 9, ncol = 3))
colnames(result) <- c("lambda","k","Mcclain_Score")
for (lambda in 2:lambda_max){
  for(k in 4 : k_max){
    mcclain_again <- mcclain_kproto(data = final_df_un, k = k, lambda = lambda, nstart=2, na.rm=FALSE)
    result[lambda, 1] <- lambda
    result[lambda, 2] <- k
    result[lambda, 3] <- mcclain_again$indices
  }
}

result <- data.frame(matrix(nrow = 9, ncol = 3))
colnames(result) <- c("lambda","k","Mcclain_Score")

mcclain_df <- matrix(nrow=3, ncol=3)
for (lambda in 2:lambda_max){
  for(k in 4 : k_max){
    mcclain_df[lambda,k]$indices = mcclain_kproto(data = final_df_un, k = k, lambda = lambda, nstart=2, na.rm=FALSE)
  }
}

#Mcclain K=2

result_3 <- data.frame(matrix(nrow = 5, ncol = 3))
colnames(result_2) <- c("Lambda","Mcclain_Score","K")
for (lambda in seq(from=1 , to=6, by=1)){
  for (k in seq(from=2, to=4, by=1)){
    mcclain_K3 <- mcclain_kproto(data = final_df_un, k = k, lambda = lambda, na.rm=FALSE)
    result_3[lambda, 1] <- lambda
    result_3[lambda, 2] <- mcclain_K3$indices
    result_3[lambda, 3] <- k
  }
}


kpres <- kproto(final_df_un, 2, keep.data=TRUE, na.rm = FALSE, lambda = 3)
mcclain_kproto(object = kpres)

k=2
#number_sequence = seq(2,3,0.1)
seq_def <- seq(1, 25, 1)
result_4 <- data.frame(matrix(nrow = 25, ncol = 4))
colnames(result_4) <- c("Lambda","K","Mcclain_Score","Dunn_score")
for (lambda in seq_def){
  mcclain_K5 <- mcclain_kproto(data = final_df_un, k = k, lambda = lambda, na.rm=FALSE)
  dunn_K5 <- dunn_kproto(data = final_df_un, k = k, lambda = lambda, na.rm=FALSE)
  result_4[lambda, 1] <- lambda
  result_4[lambda, 2] <- k
  result_4[lambda, 3] <- mcclain_K5$indices
  result_4[lambda, 4] <- dunn_K5$indices
}


seq <- seq(1, 6, 1)
for (lambda in seq_def){
  for k in 
  mcclain_K5 <- mcclain_kproto(data = final_df_un, k = k, lambda = lambda, na.rm=FALSE)


#Check consistency 
kpres <- 


result_2[1,] <- k
kpres <- kproto(final_df_un,)
mcclain$indices

#################### Plot silhouette score #####################
pdf(file = "/rds/general/user/md2620/home/asthma/Malo/Results/test_calib/silhouette.pdf",   # The directory you want to save the file in
    width = 8, # The width of the plot in inches
    height = 8)

plot(df_results_kprot_2_to_15_un$K,val$indices, frame = TRUE, 
     col = "black",
     type = "o",
     pch = 21,
     bg = val$index_opt, "red", "black"),
     xlab="Number of clusters K",
     ylab="Silhouette score")
legend("topright", legend = "optimum criterion : max")
title(main = "Silhouette score for unsupervised K-prototypes")

dev.off()


optilambda <- function(x, k, upper = 3*lambdaest(x), iter.max = 100, nstart=2, keep.data = FALSE){
  foo <- function(lamb, x = x, k = k, iter.max = iter.max, nstart=nstart, keep.data = keep.data){
    res <- kproto(x = x, k = k, lambda = lamb, iter.max = iter.max, nstart=nstart, keep.data = keep.data)  
    numvars <- sapply(x, is.numeric)
    catvars <- sapply(x, is.factor)
    nrows <- nrow(x)
    protos <- res$centers
    
    dists <- ndists <- cdists <- matrix(NA, nrow=nrows, ncol = nrow(res$centers)) #dists <- rep(NA, nrows)
    for(i in 1:nrow(res$centers)){
      d1 <- (x[,numvars, drop = FALSE] - matrix(rep(as.numeric(protos[i,numvars, drop = FALSE]), nrows), nrow=nrows, byrow=T))^2
      d2 <- sapply(which(catvars), function(j) return(x[,j] != rep(protos[i,j], nrows)) )
      ndists[,i] <- rowSums(d1)
      cdists[,i] <- lamb * rowSums(d2)
      dists[,i]  <- ndists[,i] + cdists[,i]
      }
 
    min.dists     <- apply(cbind(res$cluster, dists), 1, function(z) z[z[1]+1])
    min.ndists     <- apply(cbind(res$cluster, ndists), 1, function(z) z[z[1]+1])
    min.cdists     <- apply(cbind(res$cluster, cdists), 1, function(z) z[z[1]+1])
    crit           <- abs(sum(min.ndists) - sum(min.cdists))
    return(crit)
    }

    foo(lamb, x = x, k = k, iter.max = iter.max, nstart=nstart, keep.data = keep.data)
    lambda <- optimize(foo, lower = 0, upper = upper, x = x, k = k, iter.max = iter.max, nstart=nstart, keep.data = keep.data)$minimum
    return(lambda)}


optilambda(x=final_df_un, k = 4, nstart = 5)
res <- kproto(x = final_df_un, k = k, lambda = 9.56, iter.max = iter.max, nstart=5, keep.data = keep.data)  
