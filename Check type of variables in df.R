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

#Check how many variables of each type you have:
#We have a total of 361 variables without the cohort variable
#Total of 345 variables if you remove the 16 OMics variable with zero variance
library(tidyverse)
biomarkers <- final_df_un %>% dplyr::select(starts_with("Biomarker")) #31
clinical <- final_df_un %>% dplyr::select(starts_with("Clinical")) #48 + 5 = 53 dont 27 in screening phase
questionnaires <- final_df_un %>% dplyr::select(starts_with("Questionnaires")) #57
questions <- final_df_un %>% dplyr::select(starts_with("Questions")) #43
demographics <- final_df_un %>% dplyr::select(starts_with("Demographic")) #9+5 = 14
omics <- final_df_un %>% dplyr::select(starts_with("Omics")) #102 - 16(with 0 variance that are still in that dataset) = 86
exposures <- final_df_un %>% dplyr::select(starts_with("Expo")) #19                                    
diagnoses <- final_df_un %>% dplyr::select(ends_with("Diagnosed")) #15 + 16 = 31 others that are related to diagnoses or complications or symptoms
PE <- final_df_un %>% dplyr::select(starts_with("PE")) #8 
history <- final_df_un %>% dplyr::select(starts_with("Parental")) #3
#There are 5 more demographics (age, sex, race, body mass index and height) and 5 more clinical (eosinophils and neutrophils and exacerbations per year) 