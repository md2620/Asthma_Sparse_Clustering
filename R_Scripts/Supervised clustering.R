### MALO DIROU ###
### Sept 16th - 2021 ###
### Supervised K-prototypes with standardized numerical and non-processed categorical variables
rm(list=ls())
#Read File - Scaled categorical variables after having labeled-encoded them.
df_HTML_processed = read.csv("/rds/general/user/md2620/home/asthma/Malo/Dataframes/df_for_HTML_tables_processed.csv")
df_to_be_analyzed = read.csv("/rds/general/user/md2620/home/asthma/Malo/Dataframes/df_HTML_not_processed_HTML_tables.csv")

#Check Age of people (Min = 18, Max = 79 and Mean = 48.71, median = 50)
min(df_to_be_analyzed$Age)
mean(df_to_be_analyzed$Age)
max(df_to_be_analyzed$Age)
median(df_to_be_analyzed$Age)

#Check gender proportion of individuals - 344 females and 246 males
table(df_to_be_analyzed$Sex)

#Install package fro descriptive stats
#install.packages("gtsummary") 
library(gtsummary)
#install.packages("webshot")
library(webshot)
#Merge dataset with clinical.csv to have your outcome variable / severity measure
outcome_df = read.csv("/rds/general/user/md2620/home/asthma/Data/UBIOPRED_CLINICAL.csv")

###### Rename X to patient in df_HTML_not_processed ######
names(df_HTML_processed)[names(df_HTML_processed) == "X"] <- "Patient"

####### NOW MERGE df_HTML_not_processed and df2 ########
df_HTML_processed_final = merge(df_HTML_processed,outcome_df, by="Patient")

#Set Subject.ID as index
df_HTML_processed_2 <- df_HTML_processed_final[-1]
row.names(df_HTML_processed_2) <- df_HTML_processed_final$Patient
View(df_HTML_processed_2)

#Drop cluster variable
df_HTML_processed_2 = subset(df_HTML_processed_2, select = -c(cluster))

#Remove observations that belong to column cohort_v:
df_HTML_processed_2 <- subset(df_HTML_processed_2, cohort!="cohort_v")

#Look at columns to drop
colnames(outcome_df)

#Drop the columns you don't want
df_HTML_processed_2 = subset(df_HTML_processed_2, select = -c(Subject,Samples,Subset,Trial))

#Drop X variable in df_to_be_analyzed
df_to_be_analyzed = subset(df_to_be_analyzed, select = -c(X))

#Set Patient as index in df_to_be_analyzed
df_to_be_analyzed_2 <- df_to_be_analyzed[-1]
row.names(df_to_be_analyzed_2) <- df_to_be_analyzed$Patient
View(df_to_be_analyzed_2)

#Relabel few variables in df_to_be_analyzed_2
names(df_to_be_analyzed_2)[names(df_to_be_analyzed_2) == "X.UBIOPRED.Adult_Cohort_.Jan_2019..Questions.Screening.Family.History.Eczema.Mother."] <- "Questions.Screening.Eczema.Mother"
names(df_to_be_analyzed_2)[names(df_to_be_analyzed_2) == "X.UBIOPRED.Adult_Cohort_.Jan_2019..OMICS.DATA.Metabolomics.Baseline.Visit.Urine.Xanthine."] <- "Omics.Xanthine"
names(df_to_be_analyzed_2)[names(df_to_be_analyzed_2) == "X.UBIOPRED.Adult_Cohort_.Jan_2019..OMICS.DATA.Metabolomics.Baseline.Visit.Urine.Glutamic.acid."] <- "Omics.Glutamic.acid"
names(df_to_be_analyzed_2)[names(df_to_be_analyzed_2) == "uestionnaires.SNOT.Question.14"] <- "Questionnaires.SNOT.Question.14"

#Keep only the variables of df_HTML_processed_2 in df_to_be_analyzed_2
common_names <- intersect(names(df_HTML_processed_2), names(df_to_be_analyzed_2))
df_to_be_analyzed_2 <- df_to_be_analyzed_2[,common_names]

#Get list of variables in df_to_be_analyzed_2
data.frame(colnames(df_to_be_analyzed_2))

######################## RERUN WHAT IS ABOVE IF YOU WANT YOUR DATASET FOR UNSUPERVISED CLUSTERING #########
#Drop insignificant remaining variables from dataset and duplicated variables (or same meaning)
df_to_be_analyzed_2 = subset(df_to_be_analyzed_2, select = -c(Onset.Breathing.Problems.Uncertain,
                                                              Onset.OR.First.Diagnosis.Uncertain,
                                                              Clinical.Screening.eosinophils.x10.3.uL,
                                                              Clinical.Screening.Neutrophils,
                                                              Demographic.Age,
                                                              Demographic.Ethnic.origin.Mother,
                                                              Demographic.Sex))

#check missingness in dataset so far
sapply(df_to_be_analyzed_2, function(x) sum(is.na(x)))

#Get class of variables in dataset
sapply(df_to_be_analyzed_2,class)

demographics_trial <- df_to_be_analyzed_2 %>% select(cohort,Sex,Age, Body.Mass.Index.kg.m2, Height.cm)

tbl_summary_demo <- tbl_summary(demographics_trial)
tbl_summary_demo

sapply(demographics_trial,class)
levels(demographics_trial$cohort)

#webshot::install_phantomjs()
tbl_summary_4 <- demographics_trial %>%
  tbl_summary(
    by = cohort,
    type = all_continuous() ~ "continuous2",
    statistic = list(
      all_continuous() ~ c("{mean} ({sd})","{median} ({p25}, {p75})", "{min}, {max}"),
      all_categorical() ~ "{n} / {N} ({p}%)"), 
    label = list (Body.Mass.Index.kg.m2 ~ "BMI (kg.m2)",
                  Height.cm ~ "Height (cm)")) %>%
     #add_p(test = all_continuous() ~ "t.test",
       #pvalue_fun = function(x) style_pvalue(x, digits = 2))%>% 
  bold_labels()%>% 
  # convert from gtsummary object to gt object
  as_gt() %>%
  # modify with gt functions
  gt::tab_header("Table 1: Baseline Demographics Of Asthma Patients Grouped By Severity Level")%>%
  gt::gtsave( vwidth=800,vheight=400,            # save table as image
    filename = "/rds/general/user/md2620/home/asthma/Malo/Baseline_Demographics.png"
  ) 

#List of demographics
df_to_be_analyzed_2$Demographic.Highest.Level.Education
df_to_be_analyzed_2$Demographic.Marital.Status
df_to_be_analyzed_2$Demographic.occupational.employed
df_to_be_analyzed_2$Demographic.occupational.keeping.house
df_to_be_analyzed_2$Demographic.occupational.other
df_to_be_analyzed_2$Demographic.occupational.retired
df_to_be_analyzed_2$Demographic.occupational.student
df_to_be_analyzed_2$Demographic.occupational.unemployed
df_to_be_analyzed_2$Demographic.occupational.volunteer
df_to_be_analyzed_2$Age
df_to_be_analyzed_2$Sex
df_to_be_analyzed_2$Body.Mass.Index.kg.m2
df_to_be_analyzed_2$Height.cm
df_to_be_analyzed_2$Race

######################### BAG IMPUTE THE REMAINING EOSINOPHILS AND NEUTROPHILS VARIABLES ######################
#Make a final numerical dataframe with only numerical variables (that is removing the factor variables you newly created)
df_impute_unsup <- df_to_be_analyzed_2 %>%
    select_if(Negate(is.factor))
df_impute_unsup2 <- df_impute_unsup %>% dplyr::select(!starts_with("Questions."))
df_impute_unsup3 <- df_impute_unsup2 %>% dplyr::select(!starts_with("Questionnaires."))
df_impute_unsup4 <- df_impute_unsup3 %>% dplyr::select(!starts_with("Expo."))
df_impute_unsup5 <- df_impute_unsup4 %>% dplyr::select(!starts_with("Clinical"))

#Make a categorical dataframe that you keep as it is
df_cat = df_to_be_analyzed_2 %>% dplyr::select(!starts_with("Biomarker"))
df_cat2 = df_cat %>% dplyr::select(!starts_with("Omics"))
df_cat3 = df_cat2 %>% dplyr::select(!starts_with("Clinical"))
df_cat_final = subset(df_cat3, select = -c(Exacerbation.Per.Year.,
                                           Questionnaires.ACQ.FEV1.Precentage,
                                           Questionnaires.AQLQ.Emotional.Total.Imputed,
                                           Questionnaires.ESS.Total.Imputed,
                                           Questionnaires.HADS.Anxiety.Total.Imputed,
                                           Questionnaires.HADS.Depression.Total.Imputed,
                                           Questionnaires.SNOT.Total.Imputed,
                                           Body.Mass.Index.kg.m2,
                                           Height.cm,
                                           Respiratory.History.Onset.OR.First.Diagnosis.Age.years,
                                           Age,
                                           Sputum.._Eosinophils,
                                           Blood.Neutrophils_.,
                                           Sputum._Neutrophils,
                                           Blood.Eosinophils_.
                                           ))


#Find variables that are different from the df_cat_final to impute eosinophils and neutrophils
common_names_2 <- intersect(names(df_cat_final), names(df_to_be_analyzed_2))
num_df_unsup <- df_to_be_analyzed_2 %>% dplyr::select(!common_names_2)
num_df_unsup_final <- subset(num_df_unsup, select = -c(Clinical.Atopy.All.Allergens,
                                                       Clinical.Atopy.Food.Allergens,
                                                  Clinical.Atopy.Regional.Aeroallergens,
                                                  Clinical.Atopy.IgE.Assay.Result,
                                                  Clinical.Atopy.Skin.Prick.Test.Result,
                                                  Clinical.Screening.Ecg.Interpretation,
                                                  Clinical.NO.Other.Flow.Rates,
                                                  Clinical.Airflow.Limitation.Quanjer.ERS.guidelines,
                                                  Clinical.Airflow.Limitation.Ten.Brinke.et.al,
                                                  Clinical.Fot.Performed,
                                                  Omics.ORAL.CORTICOSTEROIDS.DETECTED,
                                                  Omics.b.AGONISTS.DETECTED,
                                                  Omics.Lipidomics.Plasma,
                                                  Omics.Proteomics.Plasma,
                                                  Omics.Proteomics.Serum,
                                                  Omics.Transcriptomics.Blood))

#Get list of variables in num_df_unsup_final
data.frame(colnames(num_df_unsup_final))

#Find final categorical variables in df_to_be_analyzed2 that are not use to impute eosinophils and neutrophils
common_names_3 <- intersect(names(num_df_unsup_final), names(df_to_be_analyzed_2))
cat_df_unsup <- df_to_be_analyzed_2 %>% dplyr::select(!common_names_3)

#Imputation of numerical variables with caret
#Split the numerical dataset num_asthma_3 75/25%
set.seed(106)
train_df_to_be_analyzed_2_index <- sample(nrow(num_df_unsup_final), 465)

train_df_impute_unsup_5 <- num_df_unsup_final[train_df_to_be_analyzed_2_index, ]
test_df_impute_unsup_5 <- num_df_unsup_final[-train_df_to_be_analyzed_2_index, ]

#Check missingness in train and test sets before imputation
sapply(train_df_impute_unsup_5, function(x) sum(is.na(x)))
sapply(test_df_impute_unsup_5, function(x) sum(is.na(x)))

#Impute numerical variables in train and test sets
preprocess_values_train_df_impute_unsup_5 = preProcess(train_df_impute_unsup_5, method = "bagImpute")
trainset_df_impute_unsup_5 = predict(preprocess_values_train_df_impute_unsup_5, train_df_impute_unsup_5)
testset_df_impute_unsup_5 = predict(preprocess_values_train_df_impute_unsup_5,test_df_impute_unsup_5)

#Check missingness in train and test sets after imputation
sapply(trainset_df_impute_unsup_5, function(x) sum(is.na(x)))
sapply(testset_df_impute_unsup_5, function(x) sum(is.na(x)))

#Combine train and test sets
df_to_be_analyzed_2_imputed_unsup_5 <- rbind(trainset_df_impute_unsup_5, testset_df_impute_unsup_5)

#Check missingness in df after imputation - should be 0
sapply(df_to_be_analyzed_2_imputed_unsup_5, function(x) sum(is.na(x)))

#Combine num_asthma_2_imputed with categorical variables (that didn't need imputation for k-prototypes)
df_to_be_analyzed_2_imputed_unsup <- cbind(df_to_be_analyzed_2_imputed_unsup_5, cat_df_unsup)
#df_final <- cbind(df_asthma_2,fact_asthma_2)

#Check missingness in final dataset - looking only at missingness in numerical variables
sapply(df_to_be_analyzed_2_imputed_unsup, function(x) sum(is.na(x)))

####Write csv for variables to keep in unsupervised clustering
##### SAVE df_to_be_analyzed_2 for future preprocessing and visualization.
    #Write to a csv
write.csv(df_to_be_analyzed_2_imputed_unsup,"/rds/general/user/md2620/home/asthma/Malo/Dataframes/Final/df_unsup_not_processed.csv")
######################## STOP THERE FOR UNSUPERVISED DATASET ################################# 

#Import 

### Look at variables that are significantly associated with outcome. 
#Define your controls (tests used and measures we look at)
mycontrols_sup  <- tableby.control(test=TRUE, total=FALSE,
                               numeric.test="anova", cat.test="chisq",
                               numeric.stats=c("Nmiss","meansd"),
                               cat.stats=c("countpct"),
                               stats.labels=list(Nmiss="missing", meansd='Mean(sd)'))


#Get list of variables
data.frame(colnames(df_HTML_processed_2))
data.frame(colnames(df_to_be_analyzed_2))
#Summary table for significant biomarkers - only numerical variables
tab_biomarkers_sup <- summary(sort(tableby(cohort ~ Biomarker.C5a.pg.ml.serum
                                       + Biomarker.CD30.pg.ml.serum
                                       +Biomarker.CD40L.pg.ml.serum
                                       +Biomarker.DPPIV.pg.ml.serum
                                       +Biomarker.Galectin.3.pg.ml.serum
                                       +Biomarker.IL.18.pg.ml.serum
                                       +Biomarker.IL.1alpha.pg.ml.serum
                                       +Biomarker.IL.6Ralpha.pg.ml.serum
                                       +Biomarker.LBP.pg.ml.serum
                                       +Biomarker.Lumican.pg.ml.serum
                                       +Biomarker.MCP.4.pg.ml.serum
                                       +Biomarker.MMP.3.pg.ml.serum
                                       +Biomarker.RAGE.pg.ml.serum
                                       +Biomarker.SHBG.pg.ml.serum
                                       +Biomarker.Serpin.E1.pg.ml.serum
                                       +Biomarker.alpha1.microglobulin.pg.ml.serum
                                       +Biomarker.CCL17.pg.ml.MSD.BL.plasma
                                       +Biomarker.CCL22.pg.ml.MSD.BL.plasma
                                       +Biomarker.EOTAXIN.pg.ml.MSD.BL.plasma
                                       +Biomarker.EOTAXIN3.pg.ml.MSD.BL.plasma
                                       +Biomarker.Baseline.BI.Cytokines.Chemokines.IFNg.pg.ml.MSD.BL.plasma
                                       +Biomarker.Baseline.BI.Cytokines.Chemokines.IL6.pg.ml.MSD.BL.plasma
                                       +Biomarker.IL8.pg.ml.MSD.BL.plasma
                                       +Biomarker.IP10.pg.ml.MSD.BL.plasma
                                       +Biomarker.MCP1.pg.ml.MSD.BL.plasma
                                       +Biomarker.MIP1b.pg.ml.MSD.BL.plasma
                                       +Biomarker.TNFa.pg.ml.MSD.BL.plasma
                                       +Biomarker.Genentech.CCL18.pg.ml.IMPACT.BL.serum
                                       +Biomarker.Genentech.IL13.pg.ml.IMPACT.BL.serum
                                       +Biomarker.Genentech.Periostin.ng.ml.ELECSYS.BL.serum
                                       +Biomarker.Karolinska.hsCRP.hCRP.mg.L
                                       ,data = df_to_be_analyzed_2,control = mycontrols_sup , simulate.p.value=TRUE, B=500, digits = 1),decreasing=FALSE),title="Measures of association between biomarkers and outcome variable ", pfootnote=TRUE)
print(tab_biomarkers_sup)
library(xtable)
print(xtable(tab_biomarkers_sup, type = "latex"), file = "filename2.tex")
#Write tab_biomarkers to an HTML document
write2pdf(tab_biomarkers_sup, "/rds/general/user/md2620/home/asthma/Malo/Results/Supervised_significant_variables/biomarkers_characteristics_by_severity_form.pdf")

#Drop insignificant biomarkers from dataset
df_to_be_analyzed_2 = subset(df_to_be_analyzed_2, select = -c(Biomarker.Lumican.pg.ml.serum,
                                                              Biomarker.EOTAXIN3.pg.ml.MSD.BL.plasma,
                                                              Biomarker.CCL22.pg.ml.MSD.BL.plasma,
                                                              Biomarker.Baseline.BI.Cytokines.Chemokines.IFNg.pg.ml.MSD.BL.plasma,
                                                              Biomarker.IP10.pg.ml.MSD.BL.plasma,
                                                              Biomarker.CD30.pg.ml.serum,
                                                              Biomarker.IL8.pg.ml.MSD.BL.plasma))


#Summary table for significant clinical data
tab_clinical_sup <- summary(sort(tableby(cohort ~ Clinical.Atopy.Total.IgE.IU.ml
                                        + Clinical.Screening.Electrocardiogram.Qtc.Interval.
                                        +Clinical.NO.Standard.Flow.Rate
                                        +Clinical.Screening.Albumin.g.dL
                                        +Clinical.Screening.Alkaline.Phosphatase.U.L
                                        +Clinical.Screening.Alt.U.L.
                                        +Clinical.Screening.Ast.U.L
                                        +Clinical.Screening.Basophils
                                        +Clinical.Screening.Blood.Urea.Nitrogen.mg.dL
                                        +Clinical.Screening.Creatinine.umol.L
                                        +Clinical.Screening.Gamma.Gt.U.L
                                        +Clinical.Screening.Haemoglobin.g.dL
                                        +Clinical.Screening.Lymphocytes
                                        +Clinical.Screening.Monocytes
                                        +Clinical.Screening.Platelets.x10.3.uL
                                        +Clinical.Screening.Potassium.mmol.L
                                        +Clinical.Screening.Sodium.mmol.L
                                        +Clinical.Screening.Total.Bilirubin.umol.L
                                        +Clinical.Screening.Total.Protein.g.dL
                                        +Clinical.Screening.Wbcs.x10.3.uL
                                        +Clinical.Screening.basophils.x10.3.u
                                        +Clinical.Screening.lymphocytes.x10.3.uL
                                        +Clinical.Screening.monocytes.x10.3.uL
                                        +Clinical.FEF.25.75.Absolute.Change.L.sec
                                        +Clinical.FEF.25.75.Change.L.sec
                                        +Clinical.FEV1.Change
                                        +Clinical.FEV1.FVC.Ratio.Predicted.LLN
                                        +Clinical.FEV1.FVC.Ratio.Predicted
                                        +Clinical.FVC.Absolute.Change.L
                                        +Clinical.PEF.Change.L.sec
                                        +Clinical.Baseline.PEF
                                        +Clinical.Screening.Diastolic.Blood.Pressure
                                        +Clinical.Screening.Heart.Rate
                                        +Clinical.Screening.Respiratory.Rate
                                        +Clinical.Screening.Systolic.Blood.Pressure
                                        +Clinical.Screening.Haematology.Not.Done
                                        +Clinical.Plethysmography.Not.Done
                                        +Clinical.Vital.Signs.Not.Done
                                        +Clinical.Atopy.All.Allergens
                                        +Clinical.Atopy.Food.Allergens
                                        +Clinical.Atopy.Regional.Aeroallergens
                                        +Clinical.Atopy.IgE.Assay.Result
                                        +Clinical.Atopy.Skin.Prick.Test.Result
                                        +Clinical.Screening.Ecg.Interpretation
                                        +Clinical.NO.Other.Flow.Rates
                                        +Clinical.Airflow.Limitation.Quanjer.ERS.guidelines
                                        +Clinical.Airflow.Limitation.Ten.Brinke.et.al
                                        +Clinical.Fot.Performed
                                        ,data = df_to_be_analyzed_2,control = mycontrols_sup , simulate.p.value=TRUE, B=500, digits = 1),decreasing=FALSE),title="Measures of association between clinical data and outcome variable ", pfootnote=TRUE)

#Write tab_biomarkers to an HTML document
write2pdf(tab_clinical_sup, "/rds/general/user/md2620/home/asthma/Malo/Results/Supervised_significant_variables/clinical_characteristics_by_severity_form.pdf")

#Drop insignificant clinical data from dataset
df_to_be_analyzed_2 = subset(df_to_be_analyzed_2, select = -c(Clinical.Screening.Electrocardiogram.Qtc.Interval.,
                                                              Clinical.Screening.monocytes.x10.3.uL,
                                                              Clinical.Screening.Ecg.Interpretation,
                                                              Clinical.FEV1.Change,
                                                              Clinical.PEF.Change.L.sec,
                                                              Clinical.Screening.Haemoglobin.g.dL,
                                                              Clinical.Airflow.Limitation.Ten.Brinke.et.al,
                                                              Clinical.Plethysmography.Not.Done,
                                                              Clinical.NO.Other.Flow.Rates,
                                                              Clinical.Screening.Potassium.mmol.L,
                                                              Clinical.Vital.Signs.Not.Done,
                                                              Clinical.Airflow.Limitation.Quanjer.ERS.guidelines,
                                                              Clinical.Atopy.Food.Allergens,
                                                              Clinical.Atopy.Regional.Aeroallergens,
                                                              Clinical.Screening.Basophils,
                                                              Clinical.Atopy.IgE.Assay.Result,
                                                              Clinical.Atopy.Skin.Prick.Test.Result,
                                                              Clinical.Fot.Performed,
                                                              Clinical.Atopy.All.Allergens,
                                                              Clinical.Screening.basophils.x10.3.u,
                                                              Clinical.FEV1.FVC.Ratio.Predicted,
                                                              Clinical.FEF.25.75.Change.L.sec))


#Summary table for significant Omics
tab_omics_sup <- summary(sort(tableby(cohort ~ Omics.dehydroTXB2.ng.mmolC
                                           +Omics.dinor.11.B.PGF2a.ng.mmolC
                                           +Omics.dinor.8.isoPGF2a.ng.mmolC
                                           +Omics.dinor.TXB2.ng.mmolC
                                           +Omics.iso.iPF2a.VI.ng.mmolC
                                           +Omics.isoPGF2a.ng.mmolC
                                           +Omics.LTE4.ng.mmolC
                                           +Omics.PGE2.ng.mmolC
                                           +Omics.PGF2a.ng.mmolC
                                           +Omics.tetranorPGDM.ng.mmolC
                                           +Omics.tetranorPGEM.ng.mmolC
                                           +Omics.Trimethyluric.acid
                                           +Omics.Dimethyluric.acid
                                           +Omics.Methyluric.acid
                                           +Omics.Furoylglycine
                                           +Omics.Hydroxybutyric.acid
                                           +Omics.Hydroxykynurenine
                                           +Omics.Hydroxyproline
                                           +Omics.Methylhistidine
                                           +Omics.Methylxanthine
                                           +Omics.Pyridoxic.acid
                                           +Omics.Acetylamino.6.formylamino.3.methyluracil
                                           +Omics.Aminolevulinic.acid
                                           +Omics.Hydroxyindoleacetic.acid
                                           +Omics.Methylguanine
                                           +Omics.Acetylcarnitine
                                           +Omics.Alanine
                                           +Omics.Allantoin
                                           +Omics.Aminocaproic.acid
                                           +Omics.Aminovaleric.acid
                                           +Omics.Aspartic.acid
                                           +Omics.Betaine
                                           +Omics.Biopterin
                                           +Omics.Caffeine
                                           +Omics.Carnitine
                                           +Omics.Carnosine
                                           +Omics.Choline
                                           +Omics.Citrulline
                                           +Omics.Cystathionine
                                           +Omics.Cytosine
                                           +Omics.Galacturonic.acid
                                           +Omics.Glucosamine
                                           +Omics.Glutamic.acid
                                           +Omics.Glutamine
                                           +Omics.Guanine
                                           +Omics.Guanosine
                                           +Omics.Hippuric.acid
                                           +Omics.Histidine
                                           +Omics.Hydroxyphenylacetic.acid
                                           +Omics.Hypoxanthine
                                           +Omics.Inosine
                                           +Omics.Isoleucine
                                           +Omics.Kynurenic.acid
                                           +Omics.Lysine
                                           +Omics.Maltose
                                           +Omics.Mannitol
                                           +Omics.Mesaconic.acid
                                           +Omics.Metanephrine
                                           +Omics.Methionine
                                           +Omics.Methylhippuric.acid
                                           +Omics.Methylthioadenosine
                                           +Omics.N.Acetylcarnosine
                                           +Omics.N.Acetylglutamic.acid
                                           +Omics.N.Acetylputrescine
                                           +Omics.N.Methyl.D.aspartic.acid
                                           +Omics.N.Methylhistamine
                                           +Omics.Nitrotyrosine
                                           +Omics.O.Acetylserine
                                           +Omics.Ornithine
                                           +Omics.Phenylacetylglutamine
                                           +Omics.Phenylalanine
                                           +Omics.Phenyllactic.acid
                                           +Omics.Pipecolic.acid
                                           +Omics.Proline
                                           +Omics.Propionylcarnitine
                                           +Omics.Pyridoxal
                                           +Omics.Pyroglutamic.acid
                                           +Omics.Pyroglutamylglycine
                                           +Omics.S.Adenosylhomocysteine
                                           +Omics.Saccharopine
                                           +Omics.Sarcosine
                                           +Omics.Serine
                                           +Omics.Serotonin
                                           +Omics.Sucrose
                                           +Omics.Taurine
                                           +Omics.Theobromine
                                           +Omics.Tryptamine
                                           +Omics.Tyramine
                                           +Omics.Tyrosine
                                           +Omics.Uracil
                                           +Omics.Uric.acid
                                           +Omics.Xanthine
                                           +Omics.Xanthosine
                                           +Omics.Xylose
                                           +Omics.alpha.Glutamyltyrosine
                                           +Omics.gamma.Aminobutyric.acid
                                           +Omics.ORAL.CORTICOSTEROIDS.DETECTED
                                           +Omics.b.AGONISTS.DETECTED
                                           +Omics.Lipidomics.Plasma
                                           +Omics.Proteomics.Plasma
                                           +Omics.Proteomics.Serum
                                           +Omics.Transcriptomics.Blood
                                           ,data = df_to_be_analyzed_2,control = mycontrols_sup , simulate.p.value=TRUE, B=500, digits = 1),decreasing=FALSE),title="Measures of association between omics measurements and outcome variable ", pfootnote=TRUE)

#Write tab_biomarkers to an HTML document
write2pdf(tab_omics_sup, "/rds/general/user/md2620/home/asthma/Malo/Results/Supervised_significant_variables/omics_measurements_by_severity_form.pdf")

#Drop insignificant OMICs data from dataset
df_to_be_analyzed_2 = subset(df_to_be_analyzed_2, select = -c(Omics.Phenyllactic.acid,
                                                              Omics.Tyramine,
                                                              Omics.Hydroxyphenylacetic.acid,
                                                              Omics.Betaine,
                                                              Omics.Tryptamine,
                                                              Omics.Hypoxanthine,
                                                              Omics.Caffeine,
                                                              Omics.Theobromine,
                                                              Omics.Furoylglycine,
                                                              Omics.Glutamic.acid,
                                                              Omics.Metanephrine,
                                                              Omics.Aminolevulinic.acid,
                                                              Omics.Carnosine,
                                                              Omics.Taurine,
                                                              Omics.ORAL.CORTICOSTEROIDS.DETECTED,
                                                              Omics.Cystathionine,
                                                              Omics.Hydroxyindoleacetic.acid,
                                                              Omics.Phenylalanine,
                                                              Omics.Lysine,
                                                              Omics.Cytosine,
                                                              Omics.Choline,
                                                              Omics.Xanthine,
                                                              Omics.PGE2.ng.mmolC,
                                                              Omics.Nitrotyrosine,
                                                              Omics.Hydroxybutyric.acid,
                                                              Omics.Aspartic.acid,
                                                              Omics.Phenylacetylglutamine,
                                                              Omics.Hydroxyproline,
                                                              Omics.Aminovaleric.acid,
                                                              Omics.Methylhistidine,
                                                              Omics.Hydroxykynurenine,
                                                              Omics.Hippuric.acid,
                                                              Omics.Pyridoxal,
                                                              Omics.Inosine,
                                                              Omics.Pyroglutamylglycine,
                                                              Omics.alpha.Glutamyltyrosine,
                                                              Omics.b.AGONISTS.DETECTED,
                                                              Omics.gamma.Aminobutyric.acid,
                                                              Omics.Pyroglutamic.acid,
                                                              Omics.Proline,
                                                              Omics.Allantoin,
                                                              Omics.Tyrosine,
                                                              Omics.O.Acetylserine,
                                                              Omics.Glucosamine,
                                                              Omics.Isoleucine,
                                                              Omics.iso.iPF2a.VI.ng.mmolC,
                                                              Omics.Proteomics.Plasma,
                                                              Omics.Methylguanine,
                                                              Omics.Citrulline,
                                                              Omics.Mesaconic.acid,
                                                              Omics.Transcriptomics.Blood,
                                                              Omics.Xanthosine,
                                                              Omics.Alanine,
                                                              Omics.Aminocaproic.acid,
                                                              Omics.Guanosine,
                                                              Omics.Uric.acid,
                                                              Omics.Ornithine,
                                                              Omics.Pyridoxic.acid,
                                                              Omics.Lipidomics.Plasma,
                                                              Omics.Biopterin,
                                                              Omics.Methylhippuric.acid))


#Summary table for significant questions data
tab_questions_sup <- summary(sort(tableby(cohort ~ Questions.Ec.Respiratory.Infection
                                          +Questions.Informed.Consent.Genetic
                                          +Questions.Mars.Completed
                                          +Questions.Medication.Since.Screening
                                          +Questions.Serious.Adverse.Event.Since.Screening
                                          +Questions.Screening.Env.Factors.Bird
                                          +Questions.Screening.Env.Factors.Cat
                                          +Questions.Screening.Env.Factors.Dog
                                          +Questions.Screening.Env.Factors.Rodent
                                          +Questions.Screening.Asthma.Father
                                          +Questions.Screening.Asthma.Mother
                                          +Questions.Screening.Asthma.Sibling
                                          +Questions.Screening.Cardiovascular.Father
                                          +Questions.Screening.Cardiovascular.Mother
                                          +Questions.Screening.Cardiovascular.Sibling
                                          +Questions.Screening.Copd.Father
                                          +Questions.Screening.Copd.Mother
                                          +Questions.Screening.Copd.Sibling
                                          +Questions.Screening.Diabetes.Sibling
                                          +Questions.Screening.Eczema.Mother
                                          +Questions.Screening.Eczema.Sibling
                                          +Questions.Screening.Hay.Fever.Father
                                          +Questions.Screening.Hay.Fever.Mother
                                          +Questions.Screening.Hay.Fever.Sibling
                                          +Questions.Screening.Inflammatory.Disease.Father
                                          +Questions.Screening.Inflammatory.Disease.Mother
                                          +Questions.Screening.Inflammatory.Disease.Sibling
                                          +Questions.Screening.Food.Allergy
                                          +Questions.Screening.Informed.Consent.Genetic
                                          +Questions.Screening.Residential.Location
                                          +Questions.Screening.Serious.Adverse.Event.Since.Consent
                                          +Questions.Screening.Second.Hand.Smoke
                                          +Questions.Screening.Smokeless.Tobacco.Status
                                          +Questions.Pregnancy.Test.Na
                                          +Questions.Urinary.Cotinine.Not.Done
                                          +Questions.Screening.Env.Factors.Other.1
                                          +Questions.Screening.Env.Factors.Other.2
                                          +Questions.Screening.Env.Factors.Other.3
                                          +Questions.Screening.Number.Of.Children
                                          +Questions.Screening.Number.Of.Siblings
                                          +Questions.Screening.Smoking.Cigarettes
                                          +Questions.Screening.Smoking.Cigars
                                          +Questions.Screening.Smoking.Pipe
                                           ,data = df_to_be_analyzed_2,control = mycontrols_sup , simulate.p.value=TRUE, B=500, digits = 1),decreasing=FALSE),title="Measures of association between questions and outcome variable ", pfootnote=TRUE)


#Write tab_questions to an HTML document
write2pdf(tab_questions_sup, "/rds/general/user/md2620/home/asthma/Malo/Results/Supervised_significant_variables/questions_severity_form.pdf")


#Drop insignificant questions data from dataset
df_to_be_analyzed_2 = subset(df_to_be_analyzed_2, select = -c(Questions.Ec.Respiratory.Infection,
                                                              Questions.Informed.Consent.Genetic,
                                                              Questions.Mars.Completed,
                                                              Questions.Medication.Since.Screening,
                                                              Questions.Serious.Adverse.Event.Since.Screening,
                                                              Questions.Screening.Env.Factors.Bird,
                                                              Questions.Screening.Env.Factors.Cat,
                                                              Questions.Screening.Env.Factors.Dog,
                                                              Questions.Screening.Env.Factors.Rodent,                               
                                                              Questions.Screening.Asthma.Mother,
                                                              Questions.Screening.Asthma.Sibling,
                                                              Questions.Screening.Cardiovascular.Father,
                                                              Questions.Screening.Cardiovascular.Mother,
                                                              Questions.Screening.Cardiovascular.Sibling,
                                                              Questions.Screening.Copd.Father,
                                                              Questions.Screening.Copd.Mother,
                                                              Questions.Screening.Copd.Sibling,
                                                              Questions.Screening.Diabetes.Sibling,
                                                              Questions.Screening.Eczema.Mother,
                                                              Questions.Screening.Eczema.Sibling,
                                                              Questions.Screening.Hay.Fever.Father,                         
                                                              Questions.Screening.Hay.Fever.Sibling,                   
                                                              Questions.Screening.Inflammatory.Disease.Mother,
                                                              Questions.Screening.Inflammatory.Disease.Sibling,
                                                              Questions.Screening.Food.Allergy,
                                                              Questions.Screening.Informed.Consent.Genetic,
                                                              Questions.Screening.Residential.Location,
                                                              Questions.Screening.Serious.Adverse.Event.Since.Consent,
                                                              Questions.Screening.Second.Hand.Smoke,
                                                              Questions.Screening.Smokeless.Tobacco.Status,
                                                              Questions.Pregnancy.Test.Na,
                                                              Questions.Urinary.Cotinine.Not.Done,
                                                              Questions.Screening.Env.Factors.Other.1,
                                                              Questions.Screening.Env.Factors.Other.2,
                                                              Questions.Screening.Env.Factors.Other.3,
                                                              Questions.Screening.Number.Of.Children,
                                                              Questions.Screening.Number.Of.Siblings,
                                                              Questions.Screening.Smoking.Cigarettes,
                                                              Questions.Screening.Smoking.Cigars,
                                                              Questions.Screening.Smoking.Pipe))

#Change numerical test for questionnaires data as it is ordinal data. We use kruskal wallis test instead of anova.
mycontrols_sup_questionnaires  <- tableby.control(test=TRUE, total=FALSE,
                                   numeric.test="kwt", cat.test="chisq",
                                   numeric.stats=c("Nmiss","meansd"),
                                   cat.stats=c("countpct"),
                                   stats.labels=list(Nmiss="missing", meansd='Mean(sd)'))

#Summary table for significant questionnaires data 
tab_questionnaires_sup <- summary(sort(tableby(cohort ~ Questionnaires.ACQ.FEV1.Precentage
                                               +Questionnaires.AQLQ.Emotional.Total.Imputed
                                               +Questionnaires.ESS.Total.Imputed
                                               +Questionnaires.HADS.Anxiety.Total.Imputed
                                               +Questionnaires.HADS.Depression.Total.Imputed
                                               +Questionnaires.SNOT.Total.Imputed
                                               +Questionnaires.ACQ.Question.1
                                               +Questionnaires.ACQ.Question.2
                                               +Questionnaires.ACQ.Question.3
                                               +Questionnaires.ACQ.Question.5
                                               +Questionnaires.ACQ.Question.6
                                               +Questionnaires.ACQ.Question.7
                                               +Questionnaires.AQLQ.Question.11
                                               +Questionnaires.AQLQ.Question.12
                                               +Questionnaires.AQLQ.Question.16
                                               +Questionnaires.AQLQ.Question.26
                                               +Questionnaires.AQLQ.Question.9
                                               +Questionnaires.ESS.Question.1
                                               +Questionnaires.ESS.Question.2
                                               +Questionnaires.ESS.Question.3
                                               +Questionnaires.ESS.Question.4
                                               +Questionnaires.ESS.Question.5
                                               +Questionnaires.ESS.Question.6
                                               +Questionnaires.ESS.Question.7
                                               +Questionnaires.ESS.Question.8
                                               +Questionnaires.HADS.Question.10
                                               +Questionnaires.HADS.Question.11
                                               +Questionnaires.HADS.Question.12
                                               +Questionnaires.HADS.Question.13
                                               +Questionnaires.HADS.Question.14
                                               +Questionnaires.HADS.Question.1
                                               +Questionnaires.HADS.Question.2
                                               +Questionnaires.HADS.Question.3
                                               +Questionnaires.HADS.Question.4
                                               +Questionnaires.HADS.Question.5
                                               +Questionnaires.HADS.Question.6
                                               +Questionnaires.HADS.Question.7
                                               +Questionnaires.HADS.Question.8
                                               +Questionnaires.HADS.Question.9
                                               +Questionnaires.MARS.Total.Imputed
                                               +Questionnaires.SNOT.Question.10
                                               +Questionnaires.SNOT.Question.11
                                               +Questionnaires.SNOT.Question.13
                            
                                               +Questionnaires.SNOT.Question.16
                                               +Questionnaires.SNOT.Question.17
                                               +Questionnaires.SNOT.Question.18
                                               +Questionnaires.SNOT.Question.19
                                               +Questionnaires.SNOT.Question.1
                                               +Questionnaires.SNOT.Question.20
                                               +Questionnaires.SNOT.Question.2
                                               +Questionnaires.SNOT.Question.3
                                               +Questionnaires.SNOT.Question.4
                                               +Questionnaires.SNOT.Question.5
                                               +Questionnaires.SNOT.Question.6
                                               +Questionnaires.SNOT.Question.7
                                               +Questionnaires.SNOT.Question.8
                                               +Questionnaires.SNOT.Question.9
                                           ,data = df_to_be_analyzed_2,control = mycontrols_sup_questionnaires , simulate.p.value=TRUE, B=500, digits = 1),decreasing=FALSE),title="Measures of association between questionnaires data and outcome variable ", pfootnote=TRUE)


#Write tab_questionnaires_sup to an HTML document
write2pdf(tab_questionnaires_sup, "/rds/general/user/md2620/home/asthma/Malo/Results/Supervised_significant_variables/questionnaires_characteristics_by_severity_form.pdf")


#Drop insignificant questionnaires data from dataset
df_to_be_analyzed_2 = subset(df_to_be_analyzed_2, select = -c(Questionnaires.ACQ.Question.1,
                                                              Questionnaires.ACQ.Question.2,
                                                              Questionnaires.ACQ.Question.3,
                                                              Questionnaires.ACQ.Question.5,
                                                              Questionnaires.ACQ.Question.6,
                                                              Questionnaires.ACQ.Question.7,
                                                              Questionnaires.AQLQ.Question.11,
                                                              Questionnaires.AQLQ.Question.12,
                                                              Questionnaires.AQLQ.Question.16,
                                                              Questionnaires.AQLQ.Question.26,
                                                              Questionnaires.AQLQ.Question.9,
                                                              Questionnaires.ESS.Question.1,
                                                              Questionnaires.ESS.Question.2,
                                                              Questionnaires.ESS.Question.3,
                                                              Questionnaires.ESS.Question.4,
                                                              Questionnaires.ESS.Question.5,
                                                              Questionnaires.ESS.Question.6,
                                                              Questionnaires.ESS.Question.7,
                                                              Questionnaires.ESS.Question.8,
                                                              Questionnaires.HADS.Question.10,
                                                              Questionnaires.HADS.Question.11,
                                                              Questionnaires.HADS.Question.12,
                                                              Questionnaires.HADS.Question.13,
                                                              Questionnaires.HADS.Question.14,
                                                              Questionnaires.HADS.Question.1,
                                                              Questionnaires.HADS.Question.2,
                                                              Questionnaires.HADS.Question.3,
                                                              Questionnaires.HADS.Question.4,
                                                              Questionnaires.HADS.Question.5,
                                                              Questionnaires.HADS.Question.6,
                                                              Questionnaires.HADS.Question.7,
                                                              Questionnaires.HADS.Question.8,
                                                              Questionnaires.HADS.Question.9,
                                                              Questionnaires.MARS.Total.Imputed,
                                                              Questionnaires.SNOT.Question.10,
                                                              Questionnaires.SNOT.Question.11,
                                                              Questionnaires.SNOT.Question.13,
                                                              Questionnaires.SNOT.Question.16,
                                                              Questionnaires.SNOT.Question.17,
                                                              Questionnaires.SNOT.Question.18,
                                                              Questionnaires.SNOT.Question.19,
                                                              Questionnaires.SNOT.Question.1,
                                                              Questionnaires.SNOT.Question.20,
                                                              Questionnaires.SNOT.Question.2,
                                                              Questionnaires.SNOT.Question.3,
                                                              Questionnaires.SNOT.Question.4,
                                                              Questionnaires.SNOT.Question.5,
                                                              Questionnaires.SNOT.Question.6,
                                                              Questionnaires.SNOT.Question.7,
                                                              Questionnaires.SNOT.Question.8,
                                                              Questionnaires.SNOT.Question.9))

#Summary table for significant exposures 
tab_expo_sup <- summary(sort(tableby(cohort ~ Expo.Aspirin
                                           +Expo.Barns
                                           +Expo.Cold.Air
                                           +Expo.Dust
                                           +Expo.Fungus
                                           +Expo.Menstrual.Cycle
                                           +Expo.Perfumes
                                           +Expo.Pets
                                           +Expo.Physical.Exercise
                                           +Expo.Pollen
                                           +Expo.Pollutants
                                           +Expo.Respiratory.Infections
                                           +Expo.Routine.Physical.Activities
                                           +Expo.Stress
                                           +Expo.Wood.Smoke
                                           +Expo.Baseline.Other1
                                           +Expo.Baseline.Other2
                                           +Expo.Baseline.Other3
                                           +Expo.Baseline.Other4
                                           ,data = df_to_be_analyzed_2,control = mycontrols_sup , simulate.p.value=TRUE, B=500, digits = 1),decreasing=FALSE),title="Measures of association between exposures and outcome variable ", pfootnote=TRUE)


#Write tab_expo_sup to an HTML document
write2pdf(tab_expo_sup, "/rds/general/user/md2620/home/asthma/Malo/Results/Supervised_significant_variables/exposures_characteristics_by_severity_form.pdf")

#Drop insignificant exposures from dataset - all
df_to_be_analyzed_2 = subset(df_to_be_analyzed_2, select = -c(Expo.Aspirin,
                                                              Expo.Barns,
                                                              Expo.Cold.Air,
                                                              Expo.Dust,
                                                              Expo.Fungus,
                                                              Expo.Menstrual.Cycle,
                                                              Expo.Perfumes,
                                                              Expo.Pets,
                                                              Expo.Physical.Exercise,
                                                              Expo.Pollen,
                                                              Expo.Pollutants,
                                                              Expo.Respiratory.Infections,
                                                              Expo.Routine.Physical.Activities,
                                                              Expo.Stress,
                                                              Expo.Wood.Smoke,
                                                              Expo.Baseline.Other1,
                                                              Expo.Baseline.Other2,
                                                              Expo.Baseline.Other3,
                                                              Expo.Baseline.Other4))


#Summary table for significant demographics 
tab_demographics_sup <- summary(sort(tableby(cohort ~ Exacerbation.Per.Year.
                                          + Body.Mass.Index.kg.m2
                                          + Height.cm
                                          + Demographic.Highest.Level.Education
                                          + Demographic.Marital.Status
                                          + Demographic.occupational.employed
                                          + Demographic.occupational.keeping.house
                                          + Demographic.occupational.other
                                          + Demographic.occupational.retired
                                          + Demographic.occupational.student
                                          + Demographic.occupational.unemployed
                                          + Demographic.occupational.volunteer
                                           ,data = df_to_be_analyzed_2,control = mycontrols_sup , simulate.p.value=TRUE, B=500, digits = 1),decreasing=FALSE),title="Measures of association between demographics and outcome variable ", pfootnote=TRUE)


#Write tab_demographics_sup to an HTML document
write2pdf(tab_demographics_sup, "/rds/general/user/md2620/home/asthma/Malo/Results/Supervised_significant_variables/demographics_characteristics_by_severity_form.pdf")

#Drop insignificant demographics from dataset - all
df_to_be_analyzed_2 = subset(df_to_be_analyzed_2, select = -c(Demographic.Highest.Level.Education,
                                                              Demographic.Marital.Status,
                                                              Demographic.occupational.employed,
                                                              Demographic.occupational.keeping.house,
                                                              Demographic.occupational.other,
                                                              Demographic.occupational.retired,
                                                              Demographic.occupational.student,
                                                              Demographic.occupational.unemployed,
                                                              Demographic.occupational.volunteer))

#Summary table for significant diagnosis 
tab_history_sup <- summary(sort(tableby(cohort ~ Respiratory.History.Onset.OR.First.Diagnosis.Age.years
                                        +Asthma.Diag.Fev.Variation
                                        +Asthma.Diag.Pef.Variation
                                        +Allergic.Rhinitis.Diagnosed
                                        +Congestive.Diagnosed
                                        +Coronary.Diagnosed
                                        +Diabetes.Diagnosed
                                        +Eczema.Diagnosed
                                        +Gerd.Diagnosed
                                        +Hay.Fever.Diagnosed
                                        +Hypertension.Diagnosed
                                        +Nasal.Polyps.Diagnosed
                                        +Non.Allergic.Rhinitis.Diagnosed
                                        +Osteoporosis.Diagnosed
                                        +Psychiatric.Disease.Diagnosed
                                        +Sinus.Surgery.Diagnosed
                                        +Sinusitis.Diagnosed
                                        +Vocal.Chord.Diagnosed
                                        +Parental.Asthma
                                        +Parental.Copd
                                        +Parental.Eczema
                                        +Breathing.Problems.in.12.months.prior.to.screening
                                        +Bronchitis
                                        +Chronic.Bronchitis
                                        +Emphysema.Or.Copd
                                        +Icu.Admission.Last.Year
                                        +Icu.Admission
                                        +Intubation
                                        +Overnight.Treatment
                                        +Pneumonia
                                        +Rescue.Inhaler.Use
                                        +Supplemental.Oxygen
                                        +Visited.a.ER.at.any.time.because.of.breathing.problems
                                        +Asthma.Diag.Historic.Test
                                        ,data = df_to_be_analyzed_2,control = mycontrols_sup , simulate.p.value=TRUE, B=500, digits = 1),decreasing=FALSE),title="Measures of association between diagnoses and outcome variable ", pfootnote=TRUE)

#Write tab_history_sup to an HTML document
write2pdf(tab_history_sup, "/rds/general/user/md2620/home/asthma/Malo/Results/Supervised_significant_variables/history_characteristics_by_severity_form.pdf")

#Drop insignificant diagnoses from dataset
df_to_be_analyzed_2 = subset(df_to_be_analyzed_2, select = -c(Asthma.Diag.Fev.Variation,
                                                              Asthma.Diag.Pef.Variation,
                                                              Allergic.Rhinitis.Diagnosed,
                                                              Congestive.Diagnosed,
                                                              Coronary.Diagnosed,
                                                              Diabetes.Diagnosed,
                                                              Eczema.Diagnosed,
                                                              Gerd.Diagnosed,                                      
                                                              Hypertension.Diagnosed,
                                                              Nasal.Polyps.Diagnosed,                   
                                                              Osteoporosis.Diagnosed,
                                                              Psychiatric.Disease.Diagnosed,
                                                              Sinus.Surgery.Diagnosed,
                                                              Sinusitis.Diagnosed,
                                                              Vocal.Chord.Diagnosed,                                                   
                                                              Parental.Copd,
                                                              Parental.Eczema,
                                                              Breathing.Problems.in.12.months.prior.to.screening,
                                                              Bronchitis,
                                                              Chronic.Bronchitis,
                                                              Emphysema.Or.Copd,
                                                              Icu.Admission.Last.Year,
                                                              Icu.Admission,
                                                              Intubation,
                                                              Overnight.Treatment,
                                                              Pneumonia,
                                                              Rescue.Inhaler.Use,
                                                              Supplemental.Oxygen,
                                                              Visited.a.ER.at.any.time.because.of.breathing.problems,
                                                              Asthma.Diag.Historic.Test))

#Summary table for significant PE
tab_PE_sup <- summary(sort(tableby(cohort ~ PE.Performed
                                   +PE.Screening.Bronchial.Finding
                                   +PE.Screening.Diminished.Finding
                                   +PE.Screening.Dullness.Finding
                                   +PE.Screening.Performed
                                   +PE.Screening.Rales.Finding
                                   +PE.Screening.Squeak.Finding
                                   +PE.Screening.Wheeze.Finding
                                             ,data = df_to_be_analyzed_2,control = mycontrols_sup , simulate.p.value=TRUE, B=500, digits = 1),decreasing=FALSE),title="Measures of association between PE and outcome variable ", pfootnote=TRUE)


#Write tab_PE_sup to an HTML document
write2pdf(tab_PE_sup, "/rds/general/user/md2620/home/asthma/Malo/Results/Supervised_significant_variables/PE_characteristics_by_severity_form.pdf")

#Drop insignificant PE variables from dataset - all
df_to_be_analyzed_2 = subset(df_to_be_analyzed_2, select = -c(PE.Performed,
                                                              PE.Screening.Bronchial.Finding,
                                                              PE.Screening.Diminished.Finding,
                                                              PE.Screening.Dullness.Finding,
                                                              PE.Screening.Performed,
                                                              PE.Screening.Rales.Finding,
                                                              PE.Screening.Squeak.Finding,
                                                              PE.Screening.Wheeze.Finding))



#Summary table for significant remaining variables
tab_remain_sup <- summary(sort(tableby(cohort ~ Clinical.Screening.Haematology.Not.Done
                                       +Age
                                       +Sex
                                       +Omics.Proteomics.Serum
                                       +Race
                                       +Sputum.._Eosinophils
                                       +Blood.Neutrophils_.
                                       +Sputum._Neutrophils
                                       +Blood.Eosinophils_.
                                   ,data = df_to_be_analyzed_2,control = mycontrols_sup , simulate.p.value=TRUE, B=500, digits = 1),decreasing=FALSE),title="Measures of association between remaining variables and outcome variable ", pfootnote=TRUE)


#Write tab_remain_sup to an HTML document
write2pdf(tab_remain_sup, "/rds/general/user/md2620/home/asthma/Malo/Results/Supervised_significant_variables/remainder_characteristics_by_severity_form.html")

#Drop insignificant remaining variables from dataset 
df_to_be_analyzed_2 = subset(df_to_be_analyzed_2, select = -c(Race))


# Final supervised dataframe has a size of 610*114 variables - we have to remove cohort when running k-prototypes
#Look at common variables in df_HTML_processed_2 and df_to_be_analyzed_2 and create your final dataframe for kproto.

intersection <- intersect(names(df_HTML_processed_2), names(df_to_be_analyzed_2))
df_final_sup <- df_HTML_processed_2[,intersection]

#Remove outcome variable cohort before running k-prototypes algo
df_final_sup = subset(df_final_sup, select = -c(cohort))

#Get list of variables
data.frame(colnames(df_final_sup))

#Get class of variables in df_final_sup
sapply(df_to_be_analyzed_2,class)

#check for null values in df
sum(is.na(df_to_be_analyzed_2))

#Replace null value in clinical.screening.haematology by mean
df_to_be_analyzed_2$Clinical.Screening.Haematology.Not.Done[is.na(df_to_be_analyzed_2$Clinical.Screening.Haematology.Not.Done)] <- mean(df_to_be_analyzed_2$Clinical.Screening.Haematology.Not.Done, na.rm = TRUE)

######################### BAG IMPUTE THE REMAINING EOSINOPHILS AND NEUTROPHILS VARIABLES ######################
#Make a categorical dataframe that you keep as it is
df_cat_sup = df_to_be_analyzed_2 %>% dplyr::select(!starts_with("Biomarker"))
df_cat2_sup = df_cat_sup %>% dplyr::select(!starts_with("Omics"))
df_cat3_sup = df_cat2_sup %>% dplyr::select(!starts_with("Clinical"))
df_cat_final_sup = subset(df_cat3_sup, select = -c(Exacerbation.Per.Year.,
                                           Questionnaires.ACQ.FEV1.Precentage,
                                           Questionnaires.AQLQ.Emotional.Total.Imputed,
                                           Questionnaires.ESS.Total.Imputed,
                                           Questionnaires.HADS.Anxiety.Total.Imputed,
                                           Questionnaires.HADS.Depression.Total.Imputed,
                                           Questionnaires.SNOT.Total.Imputed,
                                           Body.Mass.Index.kg.m2,
                                           Height.cm,
                                           Respiratory.History.Onset.OR.First.Diagnosis.Age.years,
                                           Age,
                                           Sputum.._Eosinophils,
                                           Blood.Neutrophils_.,
                                           Sputum._Neutrophils,
                                           Blood.Eosinophils_.
))


#Find variables that are different from the df_cat_final to impute eosinophils and neutrophils
common_names_4 <- intersect(names(df_cat_final_sup), names(df_to_be_analyzed_2))
num_df_sup <- df_to_be_analyzed_2 %>% dplyr::select(!common_names_4)

num_df_sup_final <- subset(num_df_sup, select = -c(Omics.Proteomics.Serum))

#Get list of variables in num_df_unsup_final
data.frame(colnames(num_df_sup_final))

#Find final categorical variables in df_to_be_analyzed2 that are not use to impute eosinophils and neutrophils
common_names_5 <- intersect(names(num_df_sup_final), names(df_to_be_analyzed_2))
cat_df_sup <- df_to_be_analyzed_2 %>% dplyr::select(!common_names_5)
#Imputation of numerical variables with caret
#Split the numerical dataset num_asthma_3 75/25%
set.seed(108)
train_df_to_be_analyzed_2_index_sup <- sample(nrow(num_df_sup_final), 465)

train_df_impute_sup_5 <- num_df_sup_final[train_df_to_be_analyzed_2_index_sup, ]
test_df_impute_sup_5 <- num_df_sup_final[-train_df_to_be_analyzed_2_index_sup, ]

#Check missingness in train and test sets before imputation
sapply(train_df_impute_sup_5, function(x) sum(is.na(x)))
sapply(test_df_impute_sup_5, function(x) sum(is.na(x)))

#Impute numerical variables in train and test sets
preprocess_values_train_df_impute_sup_5 = preProcess(train_df_impute_sup_5, method = "bagImpute")
trainset_df_impute_sup_5 = predict(preprocess_values_train_df_impute_sup_5, train_df_impute_sup_5)
testset_df_impute_sup_5 = predict(preprocess_values_train_df_impute_sup_5,test_df_impute_sup_5)

#Check missingness in train and test sets after imputation
sapply(trainset_df_impute_sup_5, function(x) sum(is.na(x)))
sapply(testset_df_impute_sup_5, function(x) sum(is.na(x)))

#Combine train and test sets
df_to_be_analyzed_2_imputed_sup_5 <- rbind(trainset_df_impute_sup_5, testset_df_impute_sup_5)

#Check missingness in df after imputation - should be 0
sapply(df_to_be_analyzed_2_imputed_sup_5, function(x) sum(is.na(x)))

#Combine num_asthma_2_imputed with categorical variables (that didn't need imputation for k-prototypes)
df_to_be_analyzed_2_imputed_sup <- cbind(df_to_be_analyzed_2_imputed_sup_5, cat_df_sup)

#Check missingness in final dataset - looking only at missingness in numerical variables
sapply(df_to_be_analyzed_2_imputed_sup, function(x) sum(is.na(x)))

####Write csv for variables to keep in unsupervised clustering
##### SAVE df_to_be_analyzed_2 for future preprocessing and visualization.
#Write to a csv
write.csv(df_to_be_analyzed_2_imputed_sup,"/rds/general/user/md2620/home/asthma/Malo/Dataframes/Final/df_sup_not_processed.csv")
##########################################################################


#################### 2ND PART AFTER PREPROCESSING IN PYTHON ######################

##### RUN KPROTO SUPERVISED WITH NUM VARIABLES PROCESSED ######
final_df_import = read.csv("/rds/general/user/md2620/home/asthma/Malo/Dataframes/Final/df_sup_num_processed.csv")

#Set Subject.ID as index
final_df <- final_df_import[-1]
row.names(final_df) <- final_df_import$Unnamed..0
View(final_df)

#Remove outcome variable cohort before running k-prototypes algo
final_df = subset(final_df, select = -c(cohort))

# apply k prototypes on final_df - use 2 to 15 clusters
kprot_sup2 <- kproto(final_df, 2, keep.data=TRUE)
kprot_sup3 <- kproto(final_df, 3, keep.data=TRUE)
kprot_sup4 <- kproto(final_df, 4, keep.data=TRUE)
kprot_sup5 <- kproto(final_df, 5, keep.data=TRUE)
kprot_sup6 <- kproto(final_df, 6, keep.data=TRUE)
kprot_sup7 <- kproto(final_df, 7, keep.data=TRUE)
kprot_sup8 <- kproto(final_df, 8, keep.data=TRUE)
kprot_sup9 <- kproto(final_df, 9, keep.data=TRUE)
kprot_sup10 <- kproto(final_df, 10, keep.data=TRUE)
kprot_sup11 <- kproto(final_df, 11, keep.data=TRUE)
kprot_sup12 <- kproto(final_df, 12, keep.data=TRUE)
kprot_sup13 <- kproto(final_df, 13, keep.data=TRUE)
kprot_sup14 <- kproto(final_df, 14, keep.data=TRUE)
kprot_sup15 <- kproto(final_df, 15, keep.data=TRUE)

#Visualize results
library(clusterCrit)
library(patchwork)

#K = 2
par(mfrow=c(2,3))
clprofiles(kprot_sup2,final_df)

#K = 5
par(mfrow=c(1,3))
clprofiles(kprot_sup5,final_df)

#K = 11
par(mfrow=c(1,1))
source("clprofiles2.R")
clprofiles2(kprot_sup11,final_df)

#Transform to matrix the dataframe, also it can be handled by intCriteria function
final_df_matrix = data.matrix(final_df)

results_kprot_sup2 = intCriteria(final_df_matrix,kprot_sup2$cluster,"all")
df_results_kprot_sup2 = as.data.frame(results_kprot_sup2,row.names = "2")
df_results_kprot_sup2$K <- row.names(df_results_kprot_sup2)

results_kprot_sup3 = intCriteria(final_df_matrix,kprot_sup3$cluster,"all")
df_results_kprot_sup3 = as.data.frame(results_kprot_sup3,row.names = "3")
df_results_kprot_sup3$K <- row.names(df_results_kprot_sup3)

#Append together df_results_kpres_2 and df_results_kpres_3
df_results_kprot_2_3 = dplyr::bind_rows(df_results_kprot_sup2,df_results_kprot_sup3)

results_kprot_sup4 = intCriteria(final_df_matrix,kprot_sup4$cluster,"all")
df_results_kprot_sup4 = as.data.frame(results_kprot_sup4,row.names = "4")
df_results_kprot_sup4$K <- row.names(df_results_kprot_sup4)

#Append together df_results_kprot_2_3 and df_results_kprot_sup4
df_results_kprot_2_3_4 = dplyr::bind_rows(df_results_kprot_2_3,df_results_kprot_sup4)

results_kprot_sup5 = intCriteria(final_df_matrix,kprot_sup5$cluster,"all")
df_results_kprot_sup5 = as.data.frame(results_kprot_sup5,row.names = "5")
df_results_kprot_sup5$K <- row.names(df_results_kprot_sup5)

#Append together df_results_kprot_2_3_4 and df_results_kprot_5
df_results_kprot_2_to_5 = dplyr::bind_rows(df_results_kprot_2_3_4,df_results_kprot_sup5)

results_kprot_sup6 = intCriteria(final_df_matrix,kprot_sup6$cluster,"all")
df_results_kprot_sup6 = as.data.frame(results_kprot_sup6,row.names = "6")
df_results_kprot_sup6$K <- row.names(df_results_kprot_sup6)

#Append together df_results_kprot_2_to_5 and df_results_kprot_sup6
df_results_kprot_2_to_6 = dplyr::bind_rows(df_results_kprot_2_to_5,df_results_kprot_sup6)

results_kprot_sup7 = intCriteria(final_df_matrix,kprot_sup7$cluster,"all")
df_results_kprot_sup7 = as.data.frame(results_kprot_sup7,row.names = "7")
df_results_kprot_sup7$K <- row.names(df_results_kprot_sup7)

#Append together df_results_kprot_2_to_6 and df_results_kprot_sup7
df_results_kprot_2_to_7 = dplyr::bind_rows(df_results_kprot_2_to_6,df_results_kprot_sup7)

results_kprot_sup8 = intCriteria(final_df_matrix,kprot_sup8$cluster,"all")
df_results_kprot_sup8 = as.data.frame(results_kprot_sup8,row.names = "8")
df_results_kprot_sup8$K <- row.names(df_results_kprot_sup8)

#Append together df_results_kprot_2_to_7 and df_results_kprot_sup8
df_results_kprot_2_to_8 = dplyr::bind_rows(df_results_kprot_2_to_7,df_results_kprot_sup8)

results_kprot_sup9 = intCriteria(final_df_matrix,kprot_sup9$cluster,"all")
df_results_kprot_sup9 = as.data.frame(results_kprot_sup9,row.names = "9")
df_results_kprot_sup9$K <- row.names(df_results_kprot_sup9)

#Append together df_results_kprot_2_to_8 and df_results_kprot_sup9
df_results_kprot_2_to_9 = dplyr::bind_rows(df_results_kprot_2_to_8,df_results_kprot_sup9)

results_kprot_sup10 = intCriteria(final_df_matrix,kprot_sup10$cluster,"all")
df_results_kprot_sup10 = as.data.frame(results_kprot_sup10,row.names = "10")
df_results_kprot_sup10$K <- row.names(df_results_kprot_sup10)

#Append together df_results_kprot_2_to_9 and df_results_kprot_sup10
df_results_kprot_2_to_10 = dplyr::bind_rows(df_results_kprot_2_to_9,df_results_kprot_sup10)

results_kprot_sup11 = intCriteria(final_df_matrix,kprot_sup11$cluster,"all")
df_results_kprot_sup11 = as.data.frame(results_kprot_sup11,row.names = "11")
df_results_kprot_sup11$K <- row.names(df_results_kprot_sup11)

#Append together df_results_kprot_2_to_10 and df_results_kprot_sup11
df_results_kprot_2_to_11 = dplyr::bind_rows(df_results_kprot_2_to_10,df_results_kprot_sup11)

results_kprot_sup12 = intCriteria(final_df_matrix,kprot_sup12$cluster,"all")
df_results_kprot_sup12 = as.data.frame(results_kprot_sup12,row.names = "12")
df_results_kprot_sup12$K <- row.names(df_results_kprot_sup12)

#Append together df_results_kprot_2_to_11 and df_results_kprot_sup12
df_results_kprot_2_to_12 = dplyr::bind_rows(df_results_kprot_2_to_11,df_results_kprot_sup12)

results_kprot_sup13 = intCriteria(final_df_matrix,kprot_sup13$cluster,"all")
df_results_kprot_sup13 = as.data.frame(results_kprot_sup13,row.names = "13")
df_results_kprot_sup13$K <- row.names(df_results_kprot_sup13)

#Append together df_results_kprot_2_to_12 and df_results_kprot_sup13
df_results_kprot_2_to_13 = dplyr::bind_rows(df_results_kprot_2_to_12,df_results_kprot_sup13)

results_kprot_sup14 = intCriteria(final_df_matrix,kprot_sup14$cluster,"all")
df_results_kprot_sup14 = as.data.frame(results_kprot_sup14,row.names = "14")
df_results_kprot_sup14$K <- row.names(df_results_kprot_sup14)

#Append together df_results_kprot_2_to_13 and df_results_kprot_sup14
df_results_kprot_2_to_14 = dplyr::bind_rows(df_results_kprot_2_to_13,df_results_kprot_sup14)

results_kprot_sup15 = intCriteria(final_df_matrix,kprot_sup15$cluster,"all")
df_results_kprot_sup15 = as.data.frame(results_kprot_sup15,row.names = "15")
df_results_kprot_sup15$K <- row.names(df_results_kprot_sup15)

#Append together df_results_kprot_2_to_14 and df_results_kprot_sup15
df_results_kprot_2_to_15 = dplyr::bind_rows(df_results_kprot_2_to_14,df_results_kprot_sup15)
## Dataframe is ready to be analyzed --> Choose optimal number of K ##
#################### Plot C_Index #####################
pdf(file = "/rds/general/user/md2620/home/asthma/Malo/Results/Optimal_K_sup/Cindex.pdf",   # The directory you want to save the file in
    width = 8, # The width of the plot in inches
    height = 8)
plot(df_results_kprot_2_to_15$K,df_results_kprot_2_to_15$c_index, frame = TRUE, 
     col = "black",
     type = "o",
     pch = 21,
     bg = ifelse(df_results_kprot_2_to_15$c_index == min(df_results_kprot_2_to_15$c_index), "red", "black"),
     xlab="Number of clusters K",
     ylab="C Index")
legend("topright", legend = "optimum criterion : min")
title(main = "C Index for K-prototypes")
dev.off()
#######################################################

#################### Plot Calinski-Harabasz score #####################
pdf(file = "/rds/general/user/md2620/home/asthma/Malo/Results/Optimal_K_sup/Calinski_Harabasz.pdf",   # The directory you want to save the file in
    width = 8, # The width of the plot in inches
    height = 8)
plot(df_results_kprot_2_to_15$K,df_results_kprot_2_to_15$calinski_harabasz, frame = TRUE, 
     col = "black",
     type = "o",
     pch = 21,
     bg = ifelse(df_results_kprot_2_to_15$calinski_harabasz == max(df_results_kprot_2_to_15$calinski_harabasz), "red", "black"),
     xlab="Number of clusters K",
     ylab="Calinski-Harabasz score")
legend("topright", legend = "optimum criterion : max")
title(main = "Calinski-Harabasz score for K-prototypes")
dev.off()
#######################################################

#################### Plot Dunn score #####################
pdf(file = "/rds/general/user/md2620/home/asthma/Malo/Results/Optimal_K_sup/Dunn.pdf",   # The directory you want to save the file in
    width = 8, # The width of the plot in inches
    height = 8)

plot(df_results_kprot_2_to_15$K,df_results_kprot_2_to_15$dunn, frame = TRUE, 
     col = "black",
     type = "o",
     pch = 21,
     bg = ifelse(df_results_kprot_2_to_15$dunn == max(df_results_kprot_2_to_15$dunn), "red", "black"),
     xlab="Number of clusters K",
     ylab="Dunn score")
legend("topright", legend = "optimum criterion : max")
title(main = "Dunn score for K-prototypes")

dev.off()
#######################################################

#################### Plot Gamma score #####################
pdf(file = "/rds/general/user/md2620/home/asthma/Malo/Results/Optimal_K_sup/gamma.pdf",   # The directory you want to save the file in
    width = 8, # The width of the plot in inches
    height = 8)

plot(df_results_kprot_2_to_15$K,df_results_kprot_2_to_15$gamma, frame = TRUE, 
     col = "black",
     type = "o",
     pch = 21,
     bg = ifelse(df_results_kprot_2_to_15$gamma == max(df_results_kprot_2_to_15$gamma), "red", "black"),
     xlab="Number of clusters K",
     ylab="Gamma score")
legend("topright", legend = "optimum criterion : max")
title(main = "Gamma score for K-prototypes")

dev.off()
#######################################################


#################### Plot Gplus score #####################
pdf(file = "/rds/general/user/md2620/home/asthma/Malo/Results/Optimal_K_sup/gplus.pdf",   # The directory you want to save the file in
    width = 8, # The width of the plot in inches
    height = 8)

plot(df_results_kprot_2_to_15$K,df_results_kprot_2_to_15$g_plus, frame = TRUE, 
     col = "black",
     type = "o",
     pch = 21,
     bg = ifelse(df_results_kprot_2_to_15$g_plus == max(df_results_kprot_2_to_15$g_plus), "red", "black"),
     xlab="Number of clusters K",
     ylab="Gplus score")
legend("topright", legend = "optimum criterion : max")
title(main = "Gplus score for K-prototypes")

dev.off()
#######################################################

#################### Plot mcclain score #####################
pdf(file = "/rds/general/user/md2620/home/asthma/Malo/Results/Optimal_K_sup/mcclain.pdf",   # The directory you want to save the file in
    width = 8, # The width of the plot in inches
    height = 8)

plot(df_results_kprot_2_to_15$K,df_results_kprot_2_to_15$mcclain_rao, frame = TRUE, 
     col = "black",
     type = "o",
     pch = 21,
     bg = ifelse(df_results_kprot_2_to_15$mcclain_rao == min(df_results_kprot_2_to_15$mcclain_rao), "red", "black"),
     xlab="Number of clusters K",
     ylab="Mcclain score")
legend("bottomright", legend = "optimum criterion : min")
title(main = "Mcclain score for K-prototypes")

dev.off()
#######################################################

#################### Plot point biserial score #####################
pdf(file = "/rds/general/user/md2620/home/asthma/Malo/Results/Optimal_K_sup/ptbiserial.pdf",   # The directory you want to save the file in
    width = 8, # The width of the plot in inches
    height = 8)

plot(df_results_kprot_2_to_15$K,df_results_kprot_2_to_15$point_biserial, frame = TRUE, 
     col = "black",
     type = "o",
     pch = 21,
     bg = ifelse(df_results_kprot_2_to_15$point_biserial == max(df_results_kprot_2_to_15$point_biserial), "red", "black"),
     xlab="Number of clusters K",
     ylab="Point Biserial score")
legend("bottomright", legend = "optimum criterion : max")
title(main = "Point Biserial score for K-prototypes")

dev.off()
#######################################################

#################### Plot tau score #####################
pdf(file = "/rds/general/user/md2620/home/asthma/Malo/Results/Optimal_K_sup/tau.pdf",   # The directory you want to save the file in
    width = 8, # The width of the plot in inches
    height = 8)

plot(df_results_kprot_2_to_15$K,df_results_kprot_2_to_15$tau, frame = TRUE, 
     col = "black",
     type = "o",
     pch = 21,
     bg = ifelse(df_results_kprot_2_to_15$tau == max(df_results_kprot_2_to_15$tau), "red", "black"),
     xlab="Number of clusters K",
     ylab="Tau score")
legend("topright", legend = "optimum criterion : max")
title(main = "Tau score for K-prototypes")

dev.off()
#######################################################

#################### Plot silhouette score #####################
pdf(file = "/rds/general/user/md2620/home/asthma/Malo/Results/Optimal_K_sup/silhouette.pdf",   # The directory you want to save the file in
    width = 8, # The width of the plot in inches
    height = 8)

plot(df_results_kprot_2_to_15$K,df_results_kprot_2_to_15$silhouette, frame = TRUE, 
     col = "black",
     type = "o",
     pch = 21,
     bg = ifelse(df_results_kprot_2_to_15$silhouette == max(df_results_kprot_2_to_15$silhouette), "red", "black"),
     xlab="Number of clusters K",
     ylab="Silhouette score")
legend("topright", legend = "optimum criterion : max")
title(main = "Silhouette score for K-prototypes")

dev.off()
#######################################################

#Go back to final_df before you removed cohort variable and re-run the lines of code
final_df <- final_df_import[-1]
row.names(final_df) <- final_df_import$Unnamed..0
View(final_df)

####### OPTIMAL NUMBER OF CLUSTERS SEEMS TO BE 2 or 11 ######
####### Attach them all

#Append clusters number to final_df --> K=2
final_df$cluster_K_2 <- factor(kprot_sup2$cluster)

#Append clusters number to final_df --> K=3
final_df$cluster_K_3 <- factor(kprot_sup3$cluster)

#Append clusters number to final_df --> K=4
final_df$cluster_K_4 <- factor(kprot_sup4$cluster)

#Append clusters number to final_df --> K=5
final_df$cluster_K_5 <- factor(kprot_sup5$cluster)

#Append clusters number to final_df --> K=6
final_df$cluster_K_6 <- factor(kprot_sup6$cluster)

#Append clusters number to final_df --> K=7
final_df$cluster_K_7 <- factor(kprot_sup7$cluster)

#Append clusters number to final_df --> K=8
final_df$cluster_K_8 <- factor(kprot_sup8$cluster)

#Append clusters number to final_df --> K=9
final_df$cluster_K_9 <- factor(kprot_sup9$cluster)

#Append clusters number to final_df --> K=10
final_df$cluster_K_10 <- factor(kprot_sup10$cluster)

#Append clusters number to final_df --> K=11
final_df$cluster_K_11 <- factor(kprot_sup11$cluster)

#Append clusters number to final_df --> K=12
final_df$cluster_K_12 <- factor(kprot_sup12$cluster)

#Append clusters number to final_df --> K=13
final_df$cluster_K_13 <- factor(kprot_sup13$cluster)

#Append clusters number to final_df --> K=14
final_df$cluster_K_14 <- factor(kprot_sup14$cluster)

#Append clusters number to final_df --> K=15
final_df$cluster_K_15 <- factor(kprot_sup15$cluster)

###### SUMMARY TABLE TO LOOK AT ASSOCIATION BETWEEN OUTCOME (SEVERITY STATUS) AND CLUSTERS
table(final_df$cohort)

#Create dummy variables for outcome in order to run logistic regressions (clusters against binary outcome)
final_df$Healthy <- ifelse(final_df$cohort == 'Healthy', "Yes", "No")
final_df$MildModerate <- ifelse(final_df$cohort == 'Mild/Moderate', "Yes", "No")
final_df$Severe <- ifelse(final_df$cohort == 'Severe', "Yes", "No")
final_df$Severe_Smoker <- ifelse(final_df$cohort == 'Severe_Smoker', "Yes", "No")
final_df$Severe_asthma <- ifelse(grepl('Severe', final_df$cohort),"Yes", "No")

#Not necessary
tab_cluster_outcome <- summary(sort(tableby(cohort ~ cluster,data=final_df, control = mycontrols , simulate.p.value=TRUE, B=500, digits = 1),pfootnote=TRUE),decreasing=TRUE)

#install.packages("finalfit")
#install.packages("rstan") not working
library(finalfit)
library(dplyr)
library(rstan)
explanatory = "cluster_K_11"
dependent = 'Healthy'
tab_cluster_outcome <- final_df %>%
  summary_factorlist(dependent, explanatory,p=TRUE, add_dependent_label=TRUE)

#Write to a csv
write.csv(tab_cluster_outcome,"/rds/general/user/md2620/home/asthma/Malo/Results/Sup_clust_results_K_11/tab_cluster_outcome.csv")

#Write to an HTML document
write2html(tab_cluster_outcome, "/rds/general/user/md2620/home/asthma/Malo/Results/Sup_clust_results_K_11/Healthy_clusters_assoc.html")

dependent_severe = 'Severe'
tab_cluster_severe <- final_df %>%
  summary_factorlist(dependent_severe, explanatory,p=TRUE, add_dependent_label=TRUE)

#Write to a csv
write.csv(tab_cluster_severe,"/rds/general/user/md2620/home/asthma/Malo/Results/Sup_clust_results_K_11/tab_cluster_severe.csv")

#Write to an HTML document
write2html(tab_cluster_severe, "/rds/general/user/md2620/home/asthma/Malo/Results/Sup_clust_results_K_11/Severe_clusters_assoc.html")

dependent_mild = 'MildModerate'
tab_cluster_mild <- final_df %>%
  summary_factorlist(dependent_mild, explanatory,p=TRUE, add_dependent_label=TRUE)

#Write to a csv
write.csv(tab_cluster_mild,"/rds/general/user/md2620/home/asthma/Malo/Results/Sup_clust_results_K_11/tab_cluster_mild.csv")

#Write to an HTML document
write2html(tab_cluster_mild, "/rds/general/user/md2620/home/asthma/Malo/Results/Sup_clust_results_K_11/mild_clusters_assoc.html")

dependent_severe_smok = 'Severe_Smoker'
tab_cluster_severe_smok <- final_df %>%
  summary_factorlist(dependent_severe_smok, explanatory,p=TRUE, add_dependent_label=TRUE)

#Write to a csv
write.csv(tab_cluster_severe_smok,"/rds/general/user/md2620/home/asthma/Malo/Results/Sup_clust_results_K_11/tab_cluster_severe_smok.csv")

#Write to an HTML document
write2html(tab_cluster_severe_smok, "/rds/general/user/md2620/home/asthma/Malo/Results/Sup_clust_results_K_11/severe_smok_clusters_assoc.html")

dependent_severe_asthma = 'Severe_asthma'
tab_cluster_severe_asthma <- final_df %>%
  summary_factorlist(dependent_severe_asthma, explanatory,p=TRUE, add_dependent_label=TRUE)

#Write to a csv
write.csv(tab_cluster_severe_asthma,"/rds/general/user/md2620/home/asthma/Malo/Results/Sup_clust_results_K_11/tab_cluster_severe_asthma.csv")

#Write to an HTML document
write2html(tab_cluster_severe_asthma, "/rds/general/user/md2620/home/asthma/Malo/Results/Sup_clust_results_K_11/severe_asthma_clusters_assoc.html")

##Redo the operation with K=2
explanatory = "cluster_K_2"
dependent = 'Healthy'
tab_cluster_outcome <- final_df %>%
  summary_factorlist(dependent, explanatory,p=TRUE, add_dependent_label=TRUE)

#Write to a csv
write.csv(tab_cluster_outcome,"/rds/general/user/md2620/home/asthma/Malo/Results/Sup_clust_results_K_2/tab_cluster_outcome_K_2.csv")

#Write to an HTML document
write2html(tab_cluster_outcome, "/rds/general/user/md2620/home/asthma/Malo/Results/Sup_clust_results_K_2/Healthy_clusters_assoc_K_2.html")

dependent_severe = 'Severe'
tab_cluster_severe <- final_df %>%
  summary_factorlist(dependent_severe, explanatory,p=TRUE, add_dependent_label=TRUE)

#Write to a csv
write.csv(tab_cluster_severe,"/rds/general/user/md2620/home/asthma/Malo/Results/Sup_clust_results_K_2/tab_cluster_severe_K_2.csv")

#Write to an HTML document
write2html(tab_cluster_severe, "/rds/general/user/md2620/home/asthma/Malo/Results/Sup_clust_results_K_2/Severe_clusters_assoc_K_2.html")

dependent_mild = 'MildModerate'
tab_cluster_mild <- final_df %>%
  summary_factorlist(dependent_mild, explanatory,p=TRUE, add_dependent_label=TRUE)

#Write to a csv
write.csv(tab_cluster_mild,"/rds/general/user/md2620/home/asthma/Malo/Results/Sup_clust_results_K_2/tab_cluster_mild_K_2.csv")

#Write to an HTML document
write2html(tab_cluster_mild, "/rds/general/user/md2620/home/asthma/Malo/Results/Sup_clust_results_K_2/mild_clusters_assoc_K_2.html")

dependent_severe_smok = 'Severe_Smoker'
tab_cluster_severe_smok <- final_df %>%
  summary_factorlist(dependent_severe_smok, explanatory,p=TRUE, add_dependent_label=TRUE)

#Write to a csv
write.csv(tab_cluster_severe_smok,"/rds/general/user/md2620/home/asthma/Malo/Results/Sup_clust_results_K_2/tab_cluster_severe_smok_K_2.csv")

#Write to an HTML document
write2html(tab_cluster_severe_smok, "/rds/general/user/md2620/home/asthma/Malo/Results/Sup_clust_results_K_2/severe_smok_clusters_assoc_K_2.html")

dependent_severe_asthma = 'Severe_asthma'
tab_cluster_severe_asthma <- final_df %>%
  summary_factorlist(dependent_severe_asthma, explanatory,p=TRUE, add_dependent_label=TRUE)

#Write to a csv
write.csv(tab_cluster_severe_asthma,"/rds/general/user/md2620/home/asthma/Malo/Results/Sup_clust_results_K_2/tab_cluster_severe_asthma_K_2.csv")

#Write to an HTML document
write2html(tab_cluster_severe_asthma, "/rds/general/user/md2620/home/asthma/Malo/Results/Sup_clust_results_K_2/severe_asthma_clusters_assoc_K_2.html")


#### Append new 11 clusters to df_HTML_not_processed_final #####
df_HTML_not_processed_final$cluster_K_11 <- factor(kprot_sup11$cluster)
######## LOOK AT MEDICATION FOR K=11 #######
#Summary table for medication
tab_medication_K_11 <- summary(sort(tableby(cluster_K_11 ~ Medication.Anti.IgE.Therapy
                                       + Medication.Antibiotic.Therapy
                                       + Medication.Cromones
                                       + Medication.Immunotherapy
                                       + Medication.Inhaled.Combinations		                                                       					
                                       + Medication.Injectable.Corticosteroids
                                       + Medication.Leukotriene.Modifiers
                                       + Medication.Long.Acting.Anticholinergics
                                       + Medication.Long.Acting.Beta.Agonist
                                       + Medication.Mucolytics
                                       + Medication.Oral.Corticosteroids
                                       + Medication.Short.Acting.Anticholinergics
                                       + Medication.Short.Acting.Beta.Agonist
                                       + Medication.Short.Acting.Combination
                                       + Medication.Theophilline
                                       + Medication.Inhaled.Corticosteroids
                                       , data=df_HTML_not_processed_final, control = mycontrols , simulate.p.value=TRUE, B=500, digits = 1),decreasing=FALSE),title= "Medication comparison across clusters", pfootnote=TRUE)

#Write tab_medication to an HTML document
write2html(tab_medication_K_11, "/rds/general/user/md2620/home/asthma/Malo/Results/Sup_clust_results_K_11/medication_characteristics_by_cluster_test.html")

## Make sure clusters variables are set as factor variables
final_df$cluster_K_2 = as.factor(final_df$cluster_K_2)
final_df$cluster_K_3 = as.factor(final_df$cluster_K_3)
final_df$cluster_K_4 = as.factor(final_df$cluster_K_4)
final_df$cluster_K_5 = as.factor(final_df$cluster_K_5)
final_df$cluster_K_6 = as.factor(final_df$cluster_K_6)
final_df$cluster_K_7 = as.factor(final_df$cluster_K_7)
final_df$cluster_K_8 = as.factor(final_df$cluster_K_8)
final_df$cluster_K_9 = as.factor(final_df$cluster_K_9)
final_df$cluster_K_10 = as.factor(final_df$cluster_K_10)
final_df$cluster_K_11 = as.factor(final_df$cluster_K_11)
final_df$cluster_K_12 = as.factor(final_df$cluster_K_12)
final_df$cluster_K_13 = as.factor(final_df$cluster_K_13)
final_df$cluster_K_14 = as.factor(final_df$cluster_K_14)
final_df$cluster_K_15 = as.factor(final_df$cluster_K_15)
final_df$Healthy = as.factor(final_df$Healthy)
final_df$MildModerate = as.factor(final_df$MildModerate)
final_df$Severe = as.factor(final_df$Severe)
final_df$Severe_Smoker = as.factor(final_df$Severe_Smoker)
final_df$Severe_asthma = as.factor(final_df$Severe_asthma)

#Check class of each variable before saving dataframe
sapply(final_df,class)

###################### SAVE final_df for supervision results ###################
#Write to a csv
write.csv(final_df,"/rds/general/user/md2620/home/asthma/Malo/Dataframes/Final/sup_df.csv")


######### ATTACH OPTIMAL CLUSTERS TO DF UNSTANDRDIZED --> FOR SHAP PLOTS AND CLUSTERS DIFFERENTIATION ########
## Attach optimal clusters from sup df to unsup df 
df_sup_not_stand = read.csv("/rds/general/user/md2620/home/asthma/Malo/Dataframes/Final/df_sup_not_processed.csv")
df_sup_stand = read.csv("/rds/general/user/md2620/home/asthma/Malo/Dataframes/Final/sup_df.csv")
#Append cluster_K_2 variable to df_sup_not_stand
df_sup_not_stand$cluster_optimal<- factor(df_sup_stand$cluster_K_2)

# drop all variables that have 0 variance
df_sup_not_stand = subset(df_sup_not_stand, select = -c(Omics.N.Acetylglutamic.acid))
df_sup_not_stand = subset(df_sup_not_stand, select = -c(Omics.N.Acetylputrescine))
df_sup_not_stand = subset(df_sup_not_stand, select = -c(Omics.Sarcosine))
df_sup_not_stand = subset(df_sup_not_stand, select = -c(Omics.N.Methyl.D.aspartic.acid))
df_sup_not_stand = subset(df_sup_not_stand, select = -c(Omics.Maltose))
df_sup_not_stand = subset(df_sup_not_stand, select = -c(Omics.Xylose))

#write csv
write.csv(df_sup_not_stand,"/rds/general/user/md2620/home/asthma/Malo/Dataframes/Final/df_for_results_sup_Oct28.csv")


############# APART #################
#Get list of variables for final supervised dataset
data.frame(colnames(final_df))

#Get list of variables for final unsupervised dataset
data.frame(colnames(df_to_be_analyzed_2))
