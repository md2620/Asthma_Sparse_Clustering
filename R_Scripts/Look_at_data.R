#Clear environment
rm(list=ls())

#Reading the files
BloodEosNeu = read.csv("/rds/general/project/hda_students_data/live/summer_projects/asthma/Data/BloodEosNeu.csv")
data_clinical <- read.table(file="/rds/general/project/hda_students_data/live/summer_projects/asthma/Data/data_clinical.tsv",sep = '\t',header = TRUE)
exp_matrix_serum = read.csv("/rds/general/project/hda_students_data/live/summer_projects/asthma/Data/expression_matrix_serum.csv")
exp_matrix_sputum = read.csv("/rds/general/project/hda_students_data/live/summer_projects/asthma/Data/expression_matrix_sputum.csv")
phe_data_serum_eos = read.csv("/rds/general/project/hda_students_data/live/summer_projects/asthma/Data/phenotype_data_serum_eos.csv")
phe_data_serum_neut = read.csv("/rds/general/project/hda_students_data/live/summer_projects/asthma/Data/phenotype_data_serum_neut.csv")
phe_data_sputum_eos = read.csv("/rds/general/project/hda_students_data/live/summer_projects/asthma/Data/phenotype_data_sputum_eos.csv")
phe_data_sputum_neut = read.csv("/rds/general/project/hda_students_data/live/summer_projects/asthma/Data/phenotype_data_sputum_neut.csv")
probe_ids_serum = read.csv("/rds/general/project/hda_students_data/live/summer_projects/asthma/Data/probe_ids_serum.csv")
probe_ids_sputum = read.csv("/rds/general/project/hda_students_data/live/summer_projects/asthma/Data/probe_ids_sputum.csv")
SputumEosNeu = read.csv("/rds/general/project/hda_students_data/live/summer_projects/asthma/Data/SputumEosNeu.csv")


setwd('rds/general/project/hda_students_data/live/summer_projects/asthma')


# Load data

if (!require("tidyverse")) install.packages("tidyverse", repos = "http://cran.us.r-project.org")
if (!require("plyr")) install.packages("plyr", repos = "http://cran.us.r-project.org")
if (!require("data.table")) install.packages("data.table", repos = "http://cran.us.r-project.org")
if (!require("BiocManager")) install.packages("BiocManager", repos = "http://cran.us.r-project.org")

if (!requireNamespace("BiocManager", quietly = TRUE))
  install.packages("BiocManager")
BiocManager::install("Biobase")
#install.packages('Biobase')

head(exprset0307)
blood_expression_set <- exprset0307$..Baseline.Visit.Blood.GPL570_BLOOD
pheno <- pData(blood_expression_set)
expr <- as.data.frame(exprs(blood_expression_set))

load('/rds/general/project/hda_students_data/live/summer_projects/asthma/Data/UBIOPREDandADEPT.Rdata')
