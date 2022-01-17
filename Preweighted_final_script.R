########################### PREWEIGHTED SPARSE KMEANS CLUSTERING #####################################
#Clear environment
rm(list=ls())
#Load functions 
# 1st step is to load functions by clicking under Home/asthma/Malo/data_functions/sspcl_funcs.RData
# Then add script for KMeansSparseCluster.permute2 and sig.features (il me semble)
# At the end you need 5 functions before trying to run anything: 
# KMeansSparseCluster.permute2, KMeansSparseCluster2, myUpdateCs, pred.strength.ws, sig.features
#If something does not run correctly, refers to this link and copy/paste the R functions available : https://github.com/sheilagaynor/prewtcl/blob/master/prewtcl/man/KMeansSparseCluster.permute2.Rd

#Function 1 manually ran
KMeansSparseCluster.permute2 <- function (x, y, K = 2, nperms = 25,
                                          wbounds = NULL, silent = FALSE,
                                          nvals = 10,
                                          ws0=rep(1/sqrt(ncol(x)), ncol(x)))
{
  if (is.null(wbounds))
    wbounds <- exp(seq(log(1.2), log(sqrt(ncol(x)) * 0.9),
                       len = nvals))
  permx <- list()
  nnonzerows <- NULL
  nws <- sum(ws0!=0)
  for (i in 1:nperms) {
    permx[[i]] <- matrix(NA, nrow = nrow(x), ncol = ncol(x))
    for (j in 1:ncol(x)) permx[[i]][, j] <- sample(x[, j])
  }
  tots <- NULL
  out <- KMeansSparseCluster2(x, K, wbounds = wbounds, silent = silent,
                              ws0=ws0)
  for (i in 1:length(out)) {
    nnonzerows <- c(nnonzerows, sum(out[[i]]$ws != 0))
    bcss <- sparcl:::GetWCSS(x, out[[i]]$Cs)$bcss.perfeature
    tots <- c(tots, sum(out[[i]]$ws * bcss))
  }
  permtots <- matrix(NA, nrow = length(wbounds), ncol = nperms)
  for (k in 1:nperms) {
    if (!silent)
      cat("Permutation ", k, "of ", nperms, fill = TRUE)
    perm.out <- KMeansSparseCluster2(permx[[k]], K, wbounds = wbounds,
                                     silent = silent, ws0=ws0)
    for (i in 1:length(perm.out)) {
      perm.bcss <- sparcl:::GetWCSS(permx[[k]], perm.out[[i]]$Cs)$bcss.perfeature
      permtots[i, k] <- sum(perm.out[[i]]$ws * perm.bcss)
    }
  }
  gaps <- (log(tots) - apply(log(permtots), 1, mean))
  out <- list(tots = tots, permtots = permtots, nnonzerows = nnonzerows,
              gaps = gaps, sdgaps = apply(log(permtots), 1, sd), wbounds = wbounds,
              bestw = wbounds[which.max(gaps)])
  if (!silent)
    cat(fill = TRUE)
  class(out) <- "kmeanssparseperm"
  return(out)
}

#Function 2 manually ran
sig.features <- function(x, Cs, perm=FALSE, B=1000)
{ library(sparcl)
  
  if (!perm) {
    n.Cs <- length(unique(Cs))
    junk <- sparcl:::GetWCSS(x, Cs)
    pvals <- 1-pf((junk$bcss.perfeature/(n.Cs-1))/
                    (junk$wcss.perfeature/(nrow(x)-n.Cs)), n.Cs-1,
                  nrow(x)-n.Cs)}
  else {
    x <- scale(x)
    x.bcss <- sparcl:::GetWCSS(x, Cs)$bcss.perfeature
    
    if (TRUE) {
      nf <- ncol(x)
      xstar.bcss <- matrix(nrow=B, ncol=nf)
      for (i in 1:B) {
        x.star <- matrix(nrow=nrow(x), ncol=ncol(x))
        for (j in 1:ncol(x)) {
          x.star[,j] <- sample(x[,j])
        }
        xstar.bcss[i,] <- sparcl:::GetWCSS(x.star, Cs)$bcss.perfeature
      }
      pvals <- rep(NA, nf)
      for (i in 1:nf) {
        pvals[i] <- sum(x.bcss[i]<=xstar.bcss)/(B*nf)
      }    }  }
  return(pvals)
}

#Read unsupervised dataset (num variables yeo-johnson scaled and categorical as original)
df_for_KMeans_un <- read.csv("/rds/general/user/md2620/home/asthma/Malo/Dataframes/Final/unsup_df.csv")

#Reproducibility 
set.seed(1222)

#Packages loading
library(sparcl)
library(survival)
library(compHclust)
library(cluster)

#Check classes of variables
View(sapply(df_for_KMeans_un, class))

#Set Subject.ID as index
df_un_KMeans <- df_for_KMeans_un[-1]
row.names(df_un_KMeans) <- df_for_KMeans_un$X
View(df_un_KMeans)

#Create an index of your original categorical variables
index_categorical <- c(181:361)
df_un_KMeans[,index_categorical] <- lapply(df_un_KMeans[ ,index_categorical], as.factor)

#Remove few variables that you don't use in kmeans and preweighted clustering
df_un_KMeans = subset(df_un_KMeans, select = -c(cohort,cluster_K_2,cluster_K_3,cluster_K_4,
                                                cluster_K_5,cluster_K_6,cluster_K_7,
                                                cluster_K_8,cluster_K_9,cluster_K_10,
                                                cluster_K_11,cluster_K_12,cluster_K_13,
                                                cluster_K_14,cluster_K_15,Healthy,Severe,Severe_asthma,
                                                MildModerate,Severe_Smoker))

#Check classes of variables
View(sapply(df_un_KMeans, class))

#Seperate numerical and categorical variables into 2 different dataframes
#Numerical dataframe
num_df_un_KMeans <- df_un_KMeans %>%
  select_if(Negate(is.factor))

#Categorical dataframe
cat_df_un_KMeans <- df_un_KMeans %>%
  select_if(is.factor)

#Check classes of variables
View(sapply(num_df_un_KMeans, class))

#Change integer variables to numeric for preweighted algorithm to work
num_df_un_KMeans$Omics.Xylose <- as.numeric(num_df_un_KMeans$Omics.Xylose)
num_df_un_KMeans$Omics.Xanthosine <- as.numeric(num_df_un_KMeans$Omics.Xanthosine)
num_df_un_KMeans$Omics.Furoylglycine <- as.numeric(num_df_un_KMeans$Omics.Furoylglycine)
num_df_un_KMeans$Omics.Allantoin <- as.numeric(num_df_un_KMeans$Omics.Allantoin)
num_df_un_KMeans$Omics.Cytosine <- as.numeric(num_df_un_KMeans$Omics.Cytosine)
num_df_un_KMeans$Omics.Glucosamine <- as.numeric(num_df_un_KMeans$Omics.Glucosamine)
num_df_un_KMeans$Omics.Glutamic.acid <- as.numeric(num_df_un_KMeans$Omics.Glutamic.acid)
num_df_un_KMeans$Omics.Isoleucine <- as.numeric(num_df_un_KMeans$Omics.Isoleucine)
num_df_un_KMeans$Omics.Lysine <- as.numeric(num_df_un_KMeans$Omics.Lysine)
num_df_un_KMeans$Omics.Maltose <- as.numeric(num_df_un_KMeans$Omics.Maltose)
num_df_un_KMeans$Omics.N.Acetylglutamic.acid <- as.numeric(num_df_un_KMeans$Omics.N.Acetylglutamic.acid)
num_df_un_KMeans$Omics.N.Methyl.D.aspartic.acid <- as.numeric(num_df_un_KMeans$Omics.N.Methyl.D.aspartic.acid)
num_df_un_KMeans$Omics.O.Acetylserine <- as.numeric(num_df_un_KMeans$Omics.O.Acetylserine)
num_df_un_KMeans$Omics.Phenylalanine <- as.numeric(num_df_un_KMeans$Omics.Phenylalanine)
num_df_un_KMeans$Omics.Sarcosine <- as.numeric(num_df_un_KMeans$Omics.Sarcosine)
num_df_un_KMeans$Omics.N.Acetylputrescine <- as.numeric(num_df_un_KMeans$Omics.N.Acetylputrescine)

# For visualization purposes, we drop all variables that have 0 variance
#Supply names of columns that have 0 variance
names(num_df_un_KMeans[, sapply(num_df_un_KMeans, function(v) var(v, na.rm=TRUE)==0)])

#Drop columns that have 0 variance from dataframe
num_df_un_KMeans <- num_df_un_KMeans[,apply(num_df_un_KMeans, 2, var, na.rm=TRUE) != 0]
names(num_df_un_KMeans)
#Code below is the same as the line above but manual removing of it.
#num_df_un_KMeans = subset(num_df_un_KMeans, select = -c(Omics.Xylose,Omics.Xanthosine,Omics.Furoylglycine,Omics.Allantoin,
                                                        #Omics.Cytosine,Omics.Glucosamine,Omics.Glutamic.acid,
                                                        #Omics.Isoleucine,Omics.Lysine,Omics.Maltose,
                                                        #Omics.N.Acetylglutamic.acid,Omics.N.Methyl.D.aspartic.acid,Omics.O.Acetylserine,
                                                        #Omics.Phenylalanine,Omics.Sarcosine,Omics.N.Acetylputrescine))


#Cluster the data
##################
##################
permout_proto <- KMeansSparseCluster.permute(num_df_un_KMeans, K = 2, nperm=5)
sparse_proto <- KMeansSparseCluster(num_df_un_KMeans, K = 2, wbounds=permout_proto$bestw)[[1]]
weightsPRIMARY_proto <- sparse_proto$ws

weightsPRIMARY_proto
##################
#Determine weights
##################
##################


##Determine input weights for second round
signif_proto <- sig.features(num_df_un_KMeans,sparse_proto$Cs)
threshold1_proto = 1*(signif_proto > (0.05/164))
weights_proto = threshold1_proto / sqrt(sum(threshold1_proto))

#Check NaNs in df_scaled
colSums(is.na(num_df_un_KMeans))
#Cluster
##################
##################
permout2_proto <- KMeansSparseCluster.permute2(num_df_un_KMeans,yval, K=2, ws0=weights_proto, nperms=5)
index_proto <- which(permout2_proto$gaps == max(permout2_proto$gaps))
tuning_proto <- permout2_proto$wbound[index_proto]
sspcl.out2_proto <- KMeansSparseCluster2(num_df_un_KMeans, K=2, ws0=weights_proto, wbounds=tuning_proto)
weightsSECONDARY_proto <- sspcl.out2_proto$ws
weightsSECONDARY_proto

##Determine input weights for third round
signif <- sig.features(num_df_un_KMeans,sspcl.out2_proto$Cs)
threshold2 = 1*(signif > (0.05/164))

threshold = ((unname(threshold2) + unname(threshold1_proto)) ==2) + 0

weights_3rd = threshold / sqrt(sum(threshold))


##################
#Cluster
##################
##################
perm_3rd <- KMeansSparseCluster.permute2(num_df_un_KMeans, yval, K=2, ws0=weights_3rd, nperms=5)
index_3rd <- which(perm_3rd$gaps == max(perm_3rd$gaps))
tuning_3rd <- perm_3rd$wbound[index_3rd]
sspcl.out_3rd <- KMeansSparseCluster2(num_df_un_KMeans, K=2, ws0=weights_3rd, wbounds=tuning_3rd)
weightsTERTIARY_proto <- sspcl.out_3rd$ws
weightsTERTIARY_proto
setEPS()
postscript("/rds/general/user/md2620/home/asthma/Malo/Results/Preweighted/num_variables_K_2.eps")
par(mfrow=c(3,1))
plot(weightsPRIMARY_proto, ylab="Weight", xlab="Feature", main="Primary Cluster Weights")
plot(weightsSECONDARY_proto, ylab="Weight", xlab="Feature", main="Secondary Cluster Weights")
plot(weightsTERTIARY_proto, ylab="Weight", xlab="Feature", main="Tertiary Cluster Weights")
dev.off()

############ TRY REDOING OPERATION WITH K=3 #############

#Cluster the data
##################
##################
permout_proto_K3 <- KMeansSparseCluster.permute(num_df_un_KMeans, K = 3, nperm=5)
sparse_proto_K3 <- KMeansSparseCluster(num_df_un_KMeans, K = 3, wbounds=permout_proto_K3$bestw)[[1]]
weightsPRIMARY_proto_K3 <- sparse_proto_K3$ws

weightsPRIMARY_proto_K3
##################
#Determine weights
##################
##################


##Determine input weights for second round
signif_proto_K3 <- sig.features(num_df_un_KMeans,sparse_proto_K3$Cs)
threshold1_proto_K3 = 1*(signif_proto > (0.05/164))
weights_proto_K3 = threshold1_proto_K3 / sqrt(sum(threshold1_proto_K3))

#Check NaNs in df_scaled
colSums(is.na(num_df_un_KMeans))
#Cluster
##################
##################
permout2_proto_K3 <- KMeansSparseCluster.permute2(num_df_un_KMeans,yval, K=3, ws0=weights_proto_K3, nperms=5)
index_proto_K3 <- which(permout2_proto_K3$gaps == max(permout2_proto_K3$gaps))
tuning_proto_K3 <- permout2_proto_K3$wbound[index_proto_K3]
sspcl.out2_proto_K3 <- KMeansSparseCluster2(num_df_un_KMeans, K=3, ws0=weights_proto_K3, wbounds=tuning_proto_K3)
weightsSECONDARY_proto_K3 <- sspcl.out2_proto_K3$ws
weightsSECONDARY_proto_K3

##Determine input weights for third round
signif_K3 <- sig.features(num_df_un_KMeans,sspcl.out2_proto_K3$Cs)
threshold2_K3 = 1*(signif_K3 > (0.05/164))

threshold_K3 = ((unname(threshold2_K3) + unname(threshold1_proto_K3)) ==2) + 0

weights_3rd_K3 = threshold_K3 / sqrt(sum(threshold_K3))


##################
#Cluster
##################
##################
perm_3rd_K3 <- KMeansSparseCluster.permute2(num_df_un_KMeans, yval, K=3, ws0=weights_3rd_K3, nperms=5)
index_3rd_K3 <- which(perm_3rd_K3$gaps == max(perm_3rd_K3$gaps))
tuning_3rd_K3 <- perm_3rd_K3$wbound[index_3rd_K3]
sspcl.out_3rd_K3 <- KMeansSparseCluster2(num_df_un_KMeans, K=3, ws0=weights_3rd_K3, wbounds=tuning_3rd_K3)
weightsTERTIARY_proto_K3 <- sspcl.out_3rd_K3$ws
weightsTERTIARY_proto_K3

setEPS()
postscript("/rds/general/user/md2620/home/asthma/Malo/Results/Preweighted/num_variables_K_3.eps")
par(mfrow=c(3,1))
plot(weightsPRIMARY_proto_K3, ylab="Weight", xlab="Feature", main="Primary Cluster Weights")
plot(weightsSECONDARY_proto_K3, ylab="Weight", xlab="Feature", main="Secondary Cluster Weights")
plot(weightsTERTIARY_proto_K3, ylab="Weight", xlab="Feature", main="Tertiary Cluster Weights")
dev.off()


############ TRY REDOING OPERATION WITH OPTIMAL NUMBER OF CLUSTERS K=4 #############

#Cluster the data
##################
##################
permout_proto_K4 <- KMeansSparseCluster.permute(num_df_un_KMeans, K = 4, nperm=5)
sparse_proto_K4 <- KMeansSparseCluster(num_df_un_KMeans, K = 4, wbounds=permout_proto_K4$bestw)[[1]]
weightsPRIMARY_proto_K4 <- sparse_proto_K4$ws

weightsPRIMARY_proto_K4
##################
#Determine weights
##################
##################


##Determine input weights for second round
signif_proto_K4 <- sig.features(num_df_un_KMeans,sparse_proto_K4$Cs)
threshold1_proto_K4 = 1*(signif_proto > (0.05/164))
weights_proto_K4 = threshold1_proto_K4 / sqrt(sum(threshold1_proto_K4))

#Check NaNs in df_scaled
colSums(is.na(num_df_un_KMeans))
#Cluster
##################
##################
permout2_proto_K4 <- KMeansSparseCluster.permute2(num_df_un_KMeans,yval, K=4, ws0=weights_proto_K4, nperms=5)
index_proto_K4 <- which(permout2_proto_K4$gaps == max(permout2_proto_K4$gaps))
tuning_proto_K4 <- permout2_proto_K4$wbound[index_proto_K4]
sspcl.out2_proto_K4 <- KMeansSparseCluster2(num_df_un_KMeans, K=4, ws0=weights_proto_K4, wbounds=tuning_proto_K4)
weightsSECONDARY_proto_K4 <- sspcl.out2_proto_K4$ws
weightsSECONDARY_proto_K4

##Determine input weights for third round
signif_K4 <- sig.features(num_df_un_KMeans,sspcl.out2_proto_K4$Cs)
threshold2_K4 = 1*(signif_K4 > (0.05/164))

threshold_K4 = ((unname(threshold2_K4) + unname(threshold1_proto_K4)) ==2) + 0

weights_3rd_K4 = threshold_K4 / sqrt(sum(threshold_K4))


##################
#Cluster
##################
##################
perm_3rd_K4 <- KMeansSparseCluster.permute2(num_df_un_KMeans, yval, K=4, ws0=weights_3rd_K4, nperms=5)
index_3rd_K4 <- which(perm_3rd_K4$gaps == max(perm_3rd_K4$gaps))
tuning_3rd_K4 <- perm_3rd_K4$wbound[index_3rd_K4]
sspcl.out_3rd_K4 <- KMeansSparseCluster2(num_df_un_KMeans, K=4, ws0=weights_3rd_K4, wbounds=tuning_3rd_K4)
weightsTERTIARY_proto_K4 <- sspcl.out_3rd_K4$ws
weightsTERTIARY_proto_K4

setEPS()
postscript("/rds/general/user/md2620/home/asthma/Malo/Results/Preweighted/num_variables_K_4.eps")
par(mfrow=c(3,1))
plot(weightsPRIMARY_proto_K4, ylab="Weight", xlab="Feature", main="Primary Cluster Weights")
plot(weightsSECONDARY_proto_K4, ylab="Weight", xlab="Feature", main="Secondary Cluster Weights")
plot(weightsTERTIARY_proto_K4, ylab="Weight", xlab="Feature", main="Tertiary Cluster Weights")
dev.off()

########## PREWEIGHTED KMEANS K=2 #########
#Primary clusters K=2
prim_K2 <- sparse_proto$Cs

#Secondary clusters K=2
sec_K2 <- sspcl.out2_proto$Cs

#Tertiary clusters K=2
tert_K2 <- sspcl.out_3rd$Cs

#Append primary, secondary and tertiary KMeans clusters for K=2 to num_df_un_KMeans
num_df_un_KMeans$prim_K2 <- factor(sparse_proto$Cs)
num_df_un_KMeans$sec_K2 <- factor(sspcl.out2_proto$Cs)
num_df_un_KMeans$tert_K2 <- factor(sspcl.out_3rd$Cs)

########## PREWEIGHTED KMEANS K=3 #########
#Primary clusters K=3
prim_K3 <- sparse_proto_K3$Cs

#Secondary clusters K=3
sec_K3 <- sspcl.out2_proto_K3$Cs

#Tertiary clusters K=3
tert_K3 <- sspcl.out_3rd_K3$Cs

#Append primary, secondary and tertiary KMeans clusters for K=3 to num_df_un_KMeans
num_df_un_KMeans$prim_K3 <- factor(sparse_proto_K3$Cs)
num_df_un_KMeans$sec_K3 <- factor(sspcl.out2_proto_K3$Cs)
num_df_un_KMeans$tert_K3 <- factor(sspcl.out_3rd_K3$Cs)

########## PREWEIGHTED KMEANS K=4 OPTIMAL NUMBER OF CLUSTERS #########
#Primary clusters K=4
prim_K4 <- sparse_proto_K4$Cs

#Secondary clusters K=4
sec_K4 <- sspcl.out2_proto_K4$Cs

#Tertiary clusters K=4
tert_K4 <- sspcl.out_3rd_K4$Cs

#Append primary, secondary and tertiary KMeans clusters for K=4 to num_df_un_KMeans
num_df_un_KMeans$prim_K4 <- factor(sparse_proto_K4$Cs)
num_df_un_KMeans$sec_K4 <- factor(sspcl.out2_proto_K4$Cs)
num_df_un_KMeans$tert_K4 <- factor(sspcl.out_3rd_K4$Cs)

#Append cohort variable (outcome) from df_for_KMeans_un
num_df_un_KMeans$cohort <- factor(df_for_KMeans_un$cohort)

#Save dataset with appended preweighted clusters to look at associations with outcome later
#Write csv
write.csv(num_df_un_KMeans,"/rds/general/user/md2620/home/asthma/Malo/Dataframes/Final/df_preweighted_KMeans_with_results.csv")