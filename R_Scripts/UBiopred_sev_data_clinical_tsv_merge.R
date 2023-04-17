##Merge UBIOPRED_CLINICAL.csv to data_clinical.tsv to have measures of severity for asthma

#Clear environment
rm(list=ls())

#Reading the files
df = read.csv("/rds/general/user/md2620/home/asthma/Malo/asthma_imputed.csv")
severity = read.csv("/rds/general/user/md2620/home/asthma/Data/UBIOPRED_CLINICAL.csv")

#Check unique Patient ID in both datasets
length(unique(df$X))
length(unique(severity$Patient))

#Rename X variable as Patient
df <- df %>% dplyr::rename(Patient = X )
colnames(df)

#Look at outcome variable and missingness
sapply(severity, function(x) sum(is.na(x)))
#Cohort variable is our outcome variable (measure of severity and has no missingness)

#Look at levels of outcome variable (4 of them)
severity$cohort

#Merge the 2 datasets
final_df <- merge(df, severity, by = "Patient")

#Drop redundant variables - Keep only variable named cohort from severity dataframe

#Create a list of variables to remove from that current dataframe to avoid redundant information
colnames(severity)
to_remove <- c("Subject", "Samples", "Subset","Trial","Sex","Age","Race","Sputum.._Eosinophils","Blood.Neutrophils_.","Sputum._Neutrophils","Blood.Eosinophils_.")

`%ni%` <- Negate(`%in%`)
final_df <- subset(final_df,select = names(final_df) %ni% to_remove)


