##Clustering

#Clear environment
rm(list=ls())

#Reading the files
df_pre = read.csv("/rds/general/user/md2620/home/asthma/Malo/Dataframes/asthma_preprocessed.csv")

#Reproducibility 
set.seed(1222)

#Install packages
#install.packages("sparcl")
#install.packages("compHclust")

#Packages loading
library(sparcl)
library(survival)
library(compHclust)

##Load functions##
KMeansSparseCluster.permute2 <- function (x, y, K = 2, nperms = 25,
                                          wbounds = NULL, silent = FALSE,
                                          nvals = 10,
                                          ws0=rep(1/sqrt(ncol(x)), ncol(x)),
                                          ...)
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
                              ws0=ws0, ...)
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
                                     silent = silent, ws0=ws0, ...)
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

#Check for NAs,NaN, Inf in dataset

sum(sapply(df_pre, is.na))

sum(sapply(df_pre, is.infinite))

sum(sapply(df_pre, is.nan))

#Check for variables class in dataframe - KMeans require variables to be either numeric or integer.
View(sapply(df_pre, class))

#Set X (Patient ID) as index
df_2_pre <- df_pre[-1]
row.names(df_2_pre) <- df_pre$X
View(df_2_pre)

###################Cluster the data#####################
permout <- KMeansSparseCluster.permute(df_2_pre, K = 2, nperm=5)
sparse <- KMeansSparseCluster(df_2_pre, K = 2, wbounds=permout$bestw)[[1]]
weightsPRIMARY <- sparse$ws


##################Run classic K-Means #################
set.seed(123)
km_4 <- kmeans(df_2, 4, nstart = 25)

#Get resulting clusters
print(km_4)
aggregate(df_2, by=list(cluster=km_4$cluster), mean)

#Cluster size
km_4$size

#Cluster centers
km_4$centers

#Visualize cluster
fviz_cluster(km_4, data = df_2)

#Try different k.
km_1 <- kmeans(df_2, centers = 1, nstart = 25)
km_2 <- kmeans(df_2, centers = 2, nstart = 25)
km_3 <- kmeans(df_2, centers = 3, nstart = 25)
km_5 <- kmeans(df_2, centers = 5, nstart = 25)
km_6 <- kmeans(df_2, centers = 6, nstart = 25)
km_7 <- kmeans(df_2, centers = 7, nstart = 25)
km_8 <- kmeans(df_2, centers = 8, nstart = 25)
km_9 <- kmeans(df_2, centers = 9, nstart = 25)
km_10 <- kmeans(df_2, centers = 10, nstart = 25)

# plots to compare according to choice of K
p1 <- fviz_cluster(km_1, geom = "point", data = df_2) + ggtitle("k = 1")
p2 <- fviz_cluster(km_2, geom = "point",  data = df_2) + ggtitle("k = 2")
p3 <- fviz_cluster(km_3, geom = "point",  data = df_2) + ggtitle("k = 3")
p4 <- fviz_cluster(km_4, geom = "point",  data = df_2) + ggtitle("k = 4")
p5 <- fviz_cluster(km_5, geom = "point",  data = df_2) + ggtitle("k = 5")
p6 <- fviz_cluster(km_6, geom = "point",  data = df_2) + ggtitle("k = 6")
p7 <- fviz_cluster(km_7, geom = "point",  data = df_2) + ggtitle("k = 7")
p8 <- fviz_cluster(km_8, geom = "point",  data = df_2) + ggtitle("k = 8")
p9 <- fviz_cluster(km_9, geom = "point",  data = df_2) + ggtitle("k = 9")
p10 <- fviz_cluster(km_10, geom = "point",  data = df_2) + ggtitle("k = 10")

library(gridExtra)
grid.arrange(p1, p2, p3, p4, p5, p6, nrow = 3)
grid.arrange(p7,p8,p9,p10, nrow=2)

##### compute gap statistic #######
library(cluster)
set.seed(123)
gap_stat <- clusGap(df_2, FUN = kmeans, nstart = 25,
                    K.max = 10, B = 5)

gap_stat_simple <- clusGap(df_2, FUN = kmeans, nstart = 25,
                           K.max = 5, B = 1)

#### Visualize gap statistic to define optimal number of clusters
fviz_gap_stat(gap_stat)
fviz_gap_stat(gap_stat_simple)

############### Elbow method #################
set.seed(123)

# function to compute total within-cluster sum of square 
wss <- function(k) {
  kmeans(df_2, k, nstart = 10 )$tot.withinss
}

# Compute and plot wss for k = 1 to k = 15
k.values <- 1:15

# extract wss for 2-15 clusters
wss_values <- map_dbl(k.values, wss)

plot(k.values, wss_values,
     type="b", pch = 19, frame = FALSE, 
     xlab="Number of clusters K",
     ylab="Total within-clusters sum of squares")

#################################################################

################ Silhouette Score ###############################
# function to compute average silhouette for k clusters
avg_sil <- function(k) {
  km.res <- kmeans(df_2, centers = k, nstart = 25)
  ss <- silhouette(km.res$cluster, dist(df_2))
  mean(ss[, 3])
}

# Compute and plot wss for k = 2 to k = 15
k.values <- 2:15

# extract avg silhouette for 2-15 clusters
avg_sil_values <- map_dbl(k.values, avg_sil)

plot(k.values, avg_sil_values,
     type = "b", pch = 19, frame = FALSE, 
     xlab = "Number of clusters K",
     ylab = "Average Silhouettes")
#################################################################

########## Once you found optimal K #############################
# Rerun kmeans algorithm with optimal K
set.seed(123)
final <- kmeans(df_2, 4, nstart = 25) #4 here is your optimal K derived from gapstat result

#Visualize cluster results
print(final)

#Visualize the clusters 
fviz_cluster(final, data = df_2)

##### Extract cluster results + some descriptive stats at cluster level ########
df_2 %>%
  mutate(Cluster = final$cluster) %>%
  group_by(Cluster) %>%
  summarise_all("mean")
#################################################################

########### Try using K-Means only on numerical variables, removing dummies#######
# Check datatype
str(df_2)
num_df_2 <- df_2 %>%
  select_if(Negate(is.integer))


#Try different k.
km_n1 <- kmeans(num_df_2, centers = 1, nstart = 25)
km_n2 <- kmeans(num_df_2, centers = 2, nstart = 25)
km_n3 <- kmeans(num_df_2, centers = 3, nstart = 25)
km_n4 <- kmeans(num_df_2, centers = 4, nstart = 25)
km_n5 <- kmeans(num_df_2, centers = 5, nstart = 25)
km_n6 <- kmeans(num_df_2, centers = 6, nstart = 25)
km_n7 <- kmeans(num_df_2, centers = 7, nstart = 25)
km_n8 <- kmeans(num_df_2, centers = 8, nstart = 25)
km_n9 <- kmeans(num_df_2, centers = 9, nstart = 25)
km_n10 <- kmeans(num_df_2, centers = 10, nstart = 25)

# plots to compare according to choice of K
p1n <- fviz_cluster(km_n1, geom = "point", data = num_df_2) + ggtitle("k = 1")
p2n <- fviz_cluster(km_n2, geom = "point",  data = num_df_2) + ggtitle("k = 2")
p3n <- fviz_cluster(km_n3, geom = "point",  data = num_df_2) + ggtitle("k = 3")
p4n <- fviz_cluster(km_n4, geom = "point",  data = num_df_2) + ggtitle("k = 4")
p5n <- fviz_cluster(km_n5, geom = "point",  data = num_df_2) + ggtitle("k = 5")
p6n <- fviz_cluster(km_n6, geom = "point",  data = num_df_2) + ggtitle("k = 6")
p7n <- fviz_cluster(km_n7, geom = "point",  data = num_df_2) + ggtitle("k = 7")
p8n <- fviz_cluster(km_n8, geom = "point",  data = num_df_2) + ggtitle("k = 8")
p9n <- fviz_cluster(km_n9, geom = "point",  data = num_df_2) + ggtitle("k = 9")
p10n <- fviz_cluster(km_n10, geom = "point",  data = num_df_2) + ggtitle("k = 10")

#library(gridExtra)
grid.arrange(p1n, p2n, p3n, p4n, p5n, p6n, nrow = 3)
dev.off()

setEPS()
postscript("Figure2.eps")
grid.arrange(p7n,p8n,p9n,p10n, nrow=2)
dev.off()


###################Determine weights#############################

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
    
    if (true) {
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

#########Determine input weights for second round###############################
signif <- sig.features(df_2,sparse$Cs)
threshold1 = 1*(signif > (0.05/987)) #116 is the number of variables
weights = threshold1 / sqrt(sum(threshold1))


###################Preweighted Sparse Clustering################################

#Load function
load("/rds/general/user/md2620/home/asthma/Malo/data_functions/sspcl_funcs.RData")

perm <- KMeansSparseCluster.permute2(df_2, yval, K=2, ws0=weights, nperms=5)
sspcl.out <- KMeansSparseCluster2(df_2, K=2, ws0=weights, wbounds=perm$bestw)

df_2$psclust1 <- sparse$Cs
df_2$psclust2 <- sspcl.out$Cs


################################################################################

###############Complementary Hierarchical Clustering############################
df_2.hc <- hclust(as.dist(1-cor(t(df_2))))
df_2.comp <- compHclust(t(df_2), df_2.hc)
df_2.chc <- hclust(as.dist(1-cor(df_2.comp$x.prime)))

df_2$chclust1 <- cutree(df_2.hc, k=2)
df_2$chclust2 <- cutree(df_2.chc, k=2)

################################################################################



##################
#Cluster the data
##################
##################
permout <- KMeansSparseCluster.permute(df_2_pre, K = 2, nperm=5)
sparse <- KMeansSparseCluster(df_2_pre, K = 2, wbounds=permout$bestw)[[1]]
weightsPRIMARY <- sparse$ws


##################
#Determine weights
##################
##################
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
    
    if (true) {
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


##Determine input weights for second round
signif <- sig.features(df_2_pre,sparse$Cs)
threshold1 = 1*(signif > (0.05/987))
weights = threshold1 / sqrt(sum(threshold1))

##################
#Cluster
##################
##################
permout2 <- KMeansSparseCluster.permute2(df_2_pre, yval, K=2, ws0=weights, nperms=5)
index <- which(permout2$gaps == max(permout2$gaps))
tuning <- permout2$wbound[index]
sspcl.out2 <- KMeansSparseCluster2(df_2_pre, K=2, ws0=weights, wbounds=tuning)
weightsSECONDARY <- sspcl.out2$ws
weightsSECONDARY

##Determine input weights for third round
signif <- sig.features(df_2_pre,sspcl.out$Cs)
threshold2 = 1*(signif > (0.05/987))

threshold = ((unname(threshold2) + unname(threshold1)) ==2) + 0

weights = threshold / sqrt(sum(threshold))


##################
#Cluster
##################
##################
perm <- KMeansSparseCluster.permute2(df_2_pre, yval, K=2, ws0=weights, nperms=5)
index <- which(perm$gaps == max(perm$gaps))
tuning <- perm$wbound[index]
sspcl.out <- KMeansSparseCluster2(df_2_pre, K=2, ws0=weights, wbounds=tuning)
weightsTERTIARY <- sspcl.out[[1]]$ws

setEPS()
postscript("/rds/general/user/md2620/home/asthma/Malo/Figure1.eps")
par(mfrow=c(3,1))
plot(weightsPRIMARY, ylab="Weight", xlab="Feature", main="Primary Cluster Weights")
plot(weightsSECONDARY, ylab="Weight", xlab="Feature", main="Secondary Cluster Weights")
plot(weightsTERTIARY, ylab="Weight", xlab="Feature", main="Tertiary Cluster Weights")
dev.off()


colnames(df_2)

###########Outcome variable - Severity Measure - Eosinophils %##################
df_2$X.UBIOPRED.Adult_Cohort_.Jan_2019..Clinical.Data.Haematology.and.biochemistry.tests.Screening.eosinophils..x10.3.uL..

#################Visualize clusters#####################
fviz_cluster(df_2$chclust1, data = df_2, ellipse.type = "norm")



############################# NEW PREWEIGHTED SPARSE CLUSTERING BASED ON df_2 dataset (the one used to obtain K = 3)

df_clinical = read.csv("/rds/general/user/md2620/home/asthma/Data/UBIOPRED_CLINICAL.csv")

###### Rename patient to X in df_clinical ######
names(df_clinical)[names(df_clinical) == "Patient"] <- "X"

####### NOW MERGE df and df2 ########
df_final = merge(df_clinical,df_2, by="X")

#Use df_2_prot (same as df_2 but with X as index) for preweighted clustering 

#Check for variables class in dataframe - KMeans require variables to be either numeric or integer.
View(sapply(df_2_prot, class))

#Transform factor variables into integers for preweighted K-Means
library(tidyverse)
df_2_prot <- df_2_prot %>% mutate_if(is.factor,as.integer)
View(sapply(df_2_prot, class))

#Drop some variables that show NAs when trying to look for significant features
df_2_prot <- subset(df_2_prot, select=-c(Height.cm, Omics.Xylose,Omics.Xanthosine,Omics.alpha.Glutamyltyrosine,Omics.Serotonin,Omics.Saccharopine,Omics.Sarcosine,Omics.Phenylalanine,Omics.Phenyllactic.acid,
                                         Omics.O.Acetylserine,Omics.N.Methyl.D.aspartic.acid,Omics.N.Acetylputrescine,Omics.N.Acetylglutamic.acid,
                                         Biomarker.MMP.3.pg.ml.serum,Clinical.Screening.Sodium.mmol.L,Clinical.Screening.Systolic.Blood.Pressure,Omics.Furoylglycine,Omics.Hydroxykynurenine,Omics.Allantoin,Omics.Aspartic.acid,
                                         Omics.Carnosine,Omics.Cystathionine,Omics.Cytosine,Omics.Glucosamine,Omics.Glutamic.acid,Omics.Guanosine,Omics.Inosine,Omics.Isoleucine,Omics.Lysine,Omics.Maltose,Omics.Metanephrine,
Height.cm, Omics.Xylose,Omics.Xanthosine,Omics.alpha.Glutamyltyrosine,Omics.Serotonin,Omics.Saccharopine,Omics.Sarcosine,Omics.Phenylalanine,Omics.Phenyllactic.acid))
#Omics.O.Acetylserine,Omics.N.Methyl.D.aspartic.acid,Omics.N.Acetylputrescine,Omics.N.Acetylglutamic.acid
#Biomarker.MMP.3.pg.ml.serum,Clinical.Screening.Sodium.mmol.L,Clinical.Screening.Systolic.Blood.Pressure,Omics.Furoylglycine,Omics.Hydroxykynurenine,Omics.Allantoin,Omics.Aspartic.acid
#Omics.Carnosine,Omics.Cystathionine,Omics.Cytosine,Omics.Glucosamine,Omics.Glutamic.acid,Omics.Guanosine,Omics.Inosine,Omics.Isoleucine,Omics.Lysine,Omics.Maltose,Omics.Metanephrine
##################
#Cluster the data
##################
##################
permout_proto <- KMeansSparseCluster.permute(df_2_prot, K = 2, nperm=5)
sparse_proto <- KMeansSparseCluster(df_2_prot, K = 2, wbounds=permout_proto$bestw)[[1]]
weightsPRIMARY_proto <- sparse_proto$ws



##################
#Determine weights
##################
##################
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
    
    if (true) {
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


##Determine input weights for second round
signif_proto <- sig.features(df_2_prot,sparse_proto$Cs)
threshold1_proto = 1*(signif_proto > (0.05/331))
weights_proto = threshold1_proto / sqrt(sum(threshold1_proto))



signif_proto
threshold1
signif
weights
threshold1_proto
weights_proto
weightsPRIMARY##################
#Cluster
##################
##################
permout2_proto <- KMeansSparseCluster.permute2(df_2_prot, yval, K=2, ws0=weights_proto, nperms=5)
index_proto <- which(permout2_proto$gaps == max(permout2_proto$gaps))
tuning_proto <- permout2_proto$wbound[index_proto]
sspcl.out2_proto <- KMeansSparseCluster2(df_2_prot, K=2, ws0=weights_proto, wbounds=tuning_proto)
weightsSECONDARY_proto <- sspcl.out2_proto$ws
weightsSECONDARY_proto

##Determine input weights for third round
signif <- sig.features(df_2_prot,sspcl.out2_proto$Cs)
threshold2 = 1*(signif > (0.05/331))

threshold = ((unname(threshold2) + unname(threshold1_proto)) ==2) + 0

weights_3rd = threshold / sqrt(sum(threshold))


##################
#Cluster
##################
##################
perm_3rd <- KMeansSparseCluster.permute2(df_2_prot, yval, K=2, ws0=weights_3rd, nperms=5)
index_3rd <- which(perm_3rd$gaps == max(perm_3rd$gaps))
tuning_3rd <- perm_3rd$wbound[index_3rd]
sspcl.out_3rd <- KMeansSparseCluster2(df_2_prot, K=2, ws0=weights_3rd, wbounds=tuning_3rd)
weightsTERTIARY_proto <- sspcl.out_3rd[[1]]$ws

setEPS()
postscript("/rds/general/user/md2620/home/asthma/Malo/Figure1.eps")
par(mfrow=c(3,1))
plot(weightsPRIMARY_proto, ylab="Weight", xlab="Feature", main="Primary Cluster Weights")
plot(weightsSECONDARY_proto, ylab="Weight", xlab="Feature", main="Secondary Cluster Weights")
plot(weightsTERTIARY_proto, ylab="Weight", xlab="Feature", main="Tertiary Cluster Weights")
dev.off()


############################# NEW PREWEIGHTED SPARSE CLUSTERING BASED ON SCALED DATASET (the one used to obtain K = 3)

df_scaled = read.csv("/rds/general/user/md2620/home/asthma/Malo/Dataframes/df_for_preweighted_clustering.csv")

###### Rename patient to X in df_clinical ######
names(df_scaled)[names(df_scaled) == "Patient"] <- "X"


#Check for variables class in dataframe - KMeans require variables to be either numeric or integer.
View(sapply(df_scaled, class))

#Check NaNs in df_scaled
colSums(is.na(df_scaled))
#Drop some variables that show NAs when trying to look for significant features
df_scaled <- subset(df_scaled, select=-c(Height.cm, Omics.Xylose,Omics.Xanthosine,Omics.alpha.Glutamyltyrosine,Omics.Serotonin,Omics.Saccharopine,Omics.Sarcosine,Omics.Phenylalanine,Omics.Phenyllactic.acid,
                                         Omics.O.Acetylserine,Omics.N.Methyl.D.aspartic.acid,Omics.N.Acetylputrescine,Omics.N.Acetylglutamic.acid,
                                         Biomarker.MMP.3.pg.ml.serum,Clinical.Screening.Sodium.mmol.L,Clinical.Screening.Systolic.Blood.Pressure,Omics.Furoylglycine,Omics.Hydroxykynurenine,Omics.Allantoin,Omics.Aspartic.acid,
                                         Omics.Carnosine,Omics.Cystathionine,Omics.Cytosine,Omics.Glucosamine,Omics.Glutamic.acid,Omics.Guanosine,Omics.Inosine,Omics.Isoleucine,Omics.Lysine,Omics.Maltose,Omics.Metanephrine,
                                         Height.cm, Omics.Xylose,Omics.Xanthosine,Omics.alpha.Glutamyltyrosine,Omics.Serotonin,Omics.Saccharopine,Omics.Sarcosine,Omics.Phenylalanine,Omics.Phenyllactic.acid))
#Omics.O.Acetylserine,Omics.N.Methyl.D.aspartic.acid,Omics.N.Acetylputrescine,Omics.N.Acetylglutamic.acid
#Biomarker.MMP.3.pg.ml.serum,Clinical.Screening.Sodium.mmol.L,Clinical.Screening.Systolic.Blood.Pressure,Omics.Furoylglycine,Omics.Hydroxykynurenine,Omics.Allantoin,Omics.Aspartic.acid
#Omics.Carnosine,Omics.Cystathionine,Omics.Cytosine,Omics.Glucosamine,Omics.Glutamic.acid,Omics.Guanosine,Omics.Inosine,Omics.Isoleucine,Omics.Lysine,Omics.Maltose,Omics.Metanephrine

#Set X (Patient ID) as index
row.names(df_scaled) <- df_scaled$X
df_scaled <- df_scaled[-1]
View(df_scaled)

##################
#Cluster the data
##################
##################
permout_scaled <- KMeansSparseCluster.permute(df_scaled, K = 2, nperm=5)
sparse_scaled <- KMeansSparseCluster(df_scaled, K = 2, wbounds=permout_scaled$bestw)[[1]]
weightsPRIMARY_scaled <- sparse_scaled$ws


weightsPRIMARY_proto
##################
#Determine weights
##################
##################
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
    
    if (true) {
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


##Determine input weights for second round
signif_scaled <- sig.features(df_scaled,sparse_scaled$Cs)
threshold1_scaled = 1*(signif_scaled > (0.05/331))
weights_scaled = threshold1_scaled / sqrt(sum(threshold1_scaled))


#Cluster
##################
##################
permout2_scaled <- KMeansSparseCluster.permute2(df_scaled, yval, K=2, ws0=weights_scaled, nperms=5)
index_scaled <- which(permout2_scaled$gaps == max(permout2_scaled$gaps))
tuning_scaled <- permout2_scaled$wbound[index_scaled]
sspcl.out2_scaled <- KMeansSparseCluster2(df_scaled, K=2, ws0=weights_scaled, wbounds=tuning_scaled)
weightsSECONDARY_scaled<- sspcl.out2_scaled$ws
weightsSECONDARY_scaled

##Determine input weights for third round
signif_scaled_2 <- sig.features(df_scaled,sspcl.out2_scaled$Cs)
threshold2_scaled = 1*(signif_scaled_2 > (0.05/331))

threshold_scaled = ((unname(threshold2_scaled) + unname(threshold1_scaled)) ==2) + 0

weights_3rd_scaled = threshold_scaled / sqrt(sum(threshold_scaled))


##################
#Cluster
##################
##################
perm_3rd_scaled <- KMeansSparseCluster.permute2(df_scaled, yval, K=2, ws0=weights_3rd_scaled, nperms=5)
index_3rd_scaled <- which(perm_3rd_scaled$gaps == max(perm_3rd_scaled$gaps))
tuning_3rd_scaled <- perm_3rd$wbound[index_3rd_scaled]
sspcl.out_3rd_scaled <- KMeansSparseCluster2(df_scaled, K=2, ws0=weights_3rd_scaled, wbounds=tuning_3rd_scaled)
weightsTERTIARY_scaled <- sspcl.out_3rd_scaled$ws

setEPS()
postscript("/rds/general/user/md2620/home/asthma/Malo/Results/Prim_Sec_Tert_clust_scaled.eps")
par(mfrow=c(3,1))
plot(weightsPRIMARY_scaled, ylab="Weight", xlab="Feature", main="Primary Cluster Weights")
plot(weightsSECONDARY_scaled, ylab="Weight", xlab="Feature", main="Secondary Cluster Weights")
plot(weightsTERTIARY_scaled, ylab="Weight", xlab="Feature", main="Tertiary Cluster Weights")
dev.off()
