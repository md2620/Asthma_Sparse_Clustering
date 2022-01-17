#Finishing Preprocessing for k-Prototypes dataset
#Clear environment
rm(list=ls())

#Reading the files
df = read.csv("/rds/general/user/md2620/home/asthma/Malo/asthma_k_prototypes.csv")

#Check missingness
#As expected categorical variables haven't been imputed but numerical variables have no missing values anymore
sapply(df, function(x) sum(is.na(x)))

#Check variable type in df
sapply(df, class)

#Convert all integer into factor variables
df[sapply(df, is.integer)] <- lapply(df[sapply(df, is.integer)], 
                                       as.factor)

#Check that integer variables have successfully been changed to factors
sapply(df, class)

#Select all factor variables 
fact <- df %>%
  select_if((is.factor))

#Re-write the dataset for k-prototypes with successful implementation of the conversion of variable type
setwd("/rds/general/user/md2620/home/asthma/Malo")
write.csv(df,"asthma_k_prototypes.csv")

