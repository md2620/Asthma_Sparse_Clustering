
#Import datasets
df_HTML_not_processed = read.csv("/rds/general/user/md2620/home/asthma/Malo/Dataframes/df_for_HTML_tables_non_processed.csv")
df_HTML_processed = read.csv("/rds/general/user/md2620/home/asthma/Malo/Dataframes/df_for_HTML_tables_processed.csv")

#Get category of each variable in df_HTML_not_processed
lapply(df_HTML_not_processed,class)

#Transform the variables you want by location to factor variables
list_to_transform_HTML <- dplyr::select(df_HTML_not_processed,206:413)
list_HTML <- names(list_to_transform_HTML)
df_HTML_not_processed[,list_HTML] <- lapply(df_HTML_not_processed[,list_HTML],factor)

####### RENAME VARIABLES FOR BETTER VIZ OF SUMMARY TABLES ###########
####### BEG BIOMARKER DATA #######
names(df_HTML_not_processed)[names(df_HTML_not_processed) == "X.UBIOPRED.Adult_Cohort_.Jan_2019..Biomarker.Data.Baseline.Visit.Luminex.Serum.C5a.pg.ml.Luminex.serum."] <- "Biomarker.C5a.pg.ml.serum"
names(df_HTML_not_processed)[names(df_HTML_not_processed) == "X.UBIOPRED.Adult_Cohort_.Jan_2019..Biomarker.Data.Baseline.Visit.Luminex.Serum.CD30.pg.ml.Luminex.serum."] <- "Biomarker.CD30.pg.ml.serum"
names(df_HTML_not_processed)[names(df_HTML_not_processed) == "X.UBIOPRED.Adult_Cohort_.Jan_2019..Biomarker.Data.Baseline.Visit.Luminex.Serum.CD40L.pg.ml.Luminex.serum."] <- "Biomarker.CD40L.pg.ml.serum"
names(df_HTML_not_processed)[names(df_HTML_not_processed) == "X.UBIOPRED.Adult_Cohort_.Jan_2019..Biomarker.Data.Baseline.Visit.Luminex.Serum.DPPIV.pg.ml.Luminex.serum."] <- "Biomarker.DPPIV.pg.ml.serum"
names(df_HTML_not_processed)[names(df_HTML_not_processed) == "X.UBIOPRED.Adult_Cohort_.Jan_2019..Biomarker.Data.Baseline.Visit.Luminex.Serum.Galectin.3.pg.ml.Luminex.serum."] <- "Biomarker.Galectin.3.pg.ml.serum"
names(df_HTML_not_processed)[names(df_HTML_not_processed) == "X.UBIOPRED.Adult_Cohort_.Jan_2019..Biomarker.Data.Baseline.Visit.Luminex.Serum.IL.18.pg.ml.Luminex.serum."] <- "Biomarker.IL.18.pg.ml.serum"
names(df_HTML_not_processed)[names(df_HTML_not_processed) == "X.UBIOPRED.Adult_Cohort_.Jan_2019..Biomarker.Data.Baseline.Visit.Luminex.Serum.IL.1alpha.pg.ml.Luminex.serum."] <- "Biomarker.IL.1alpha.pg.ml.serum"
names(df_HTML_not_processed)[names(df_HTML_not_processed) == "X.UBIOPRED.Adult_Cohort_.Jan_2019..Biomarker.Data.Baseline.Visit.Luminex.Serum.IL.6Ralpha.pg.ml.Luminex.serum."] <- "Biomarker.IL.6Ralpha.pg.ml.serum"
names(df_HTML_not_processed)[names(df_HTML_not_processed) == "X.UBIOPRED.Adult_Cohort_.Jan_2019..Biomarker.Data.Baseline.Visit.Luminex.Serum.LBP.pg.ml.Luminex.serum."] <- "Biomarker.LBP.pg.ml.serum"
names(df_HTML_not_processed)[names(df_HTML_not_processed) == "X.UBIOPRED.Adult_Cohort_.Jan_2019..Biomarker.Data.Baseline.Visit.Luminex.Serum.Lumican.pg.ml.Luminex.serum."] <- "Biomarker.Lumican.pg.ml.serum"
names(df_HTML_not_processed)[names(df_HTML_not_processed) == "X.UBIOPRED.Adult_Cohort_.Jan_2019..Biomarker.Data.Baseline.Visit.Luminex.Serum.MCP.4.pg.ml.Luminex.serum."] <- "Biomarker.MCP.4.pg.ml.serum"
names(df_HTML_not_processed)[names(df_HTML_not_processed) == "X.UBIOPRED.Adult_Cohort_.Jan_2019..Biomarker.Data.Baseline.Visit.Luminex.Serum.MMP.3.pg.ml.Luminex.serum."] <- "Biomarker.MMP.3.pg.ml.serum"
names(df_HTML_not_processed)[names(df_HTML_not_processed) == "X.UBIOPRED.Adult_Cohort_.Jan_2019..Biomarker.Data.Baseline.Visit.Luminex.Serum.RAGE.pg.ml.Luminex.serum."] <- "Biomarker.RAGE.pg.ml.serum"
names(df_HTML_not_processed)[names(df_HTML_not_processed) == "X.UBIOPRED.Adult_Cohort_.Jan_2019..Biomarker.Data.Baseline.Visit.Luminex.Serum.SHBG.pg.ml.Luminex.serum."] <- "Biomarker.SHBG.pg.ml.serum"
names(df_HTML_not_processed)[names(df_HTML_not_processed) == "X.UBIOPRED.Adult_Cohort_.Jan_2019..Biomarker.Data.Baseline.Visit.Luminex.Serum.Serpin.E1.pg.ml.Luminex.serum."] <- "Biomarker.Serpin.E1.pg.ml.serum"
names(df_HTML_not_processed)[names(df_HTML_not_processed) == "X.UBIOPRED.Adult_Cohort_.Jan_2019..Biomarker.Data.Baseline.Visit.Luminex.Serum.alpha1.microglobulin.pg.ml.Luminex.serum."] <- "Biomarker.alpha1.microglobulin.pg.ml.serum"
names(df_HTML_not_processed)[names(df_HTML_not_processed) == "X.UBIOPRED.Adult_Cohort_.Jan_2019..Biomarker.Data.Baseline.Visit.Plasma.BI.Cytokines.Chemokines.CCL17.pg.ml.MSD.BL.plasma."] <- "Biomarker.CCL17.pg.ml.MSD.BL.plasma"
names(df_HTML_not_processed)[names(df_HTML_not_processed) == "X.UBIOPRED.Adult_Cohort_.Jan_2019..Biomarker.Data.Baseline.Visit.Plasma.BI.Cytokines.Chemokines.CCL22.pg.ml.MSD.BL.plasma."] <- "Biomarker.CCL22.pg.ml.MSD.BL.plasma"
names(df_HTML_not_processed)[names(df_HTML_not_processed) == "X.UBIOPRED.Adult_Cohort_.Jan_2019..Biomarker.Data.Baseline.Visit.Plasma.BI.Cytokines.Chemokines.EOTAXIN.pg.ml.MSD.BL.plasma."] <- "Biomarker.EOTAXIN.pg.ml.MSD.BL.plasma"
names(df_HTML_not_processed)[names(df_HTML_not_processed) == "X.UBIOPRED.Adult_Cohort_.Jan_2019..Biomarker.Data.Baseline.Visit.Plasma.BI.Cytokines.Chemokines.EOTAXIN3.pg.ml.MSD.BL.plasma."] <- "Biomarker.EOTAXIN3.pg.ml.MSD.BL.plasma"
names(df_HTML_not_processed)[names(df_HTML_not_processed) == "X.UBIOPRED.Adult_Cohort_.Jan_2019..Biomarker.Data.Baseline.Visit.Plasma.BI.Cytokines.Chemokines.IFNg.pg.ml.MSD.BL.plasma."] <- "Biomarker.Baseline.BI.Cytokines.Chemokines.IFNg.pg.ml.MSD.BL.plasma"
names(df_HTML_not_processed)[names(df_HTML_not_processed) == "X.UBIOPRED.Adult_Cohort_.Jan_2019..Biomarker.Data.Baseline.Visit.Plasma.BI.Cytokines.Chemokines.IL6.pg.ml.MSD.BL.plasma."] <- "Biomarker.Baseline.BI.Cytokines.Chemokines.IL6.pg.ml.MSD.BL.plasma"
names(df_HTML_not_processed)[names(df_HTML_not_processed) == "X.UBIOPRED.Adult_Cohort_.Jan_2019..Biomarker.Data.Baseline.Visit.Plasma.BI.Cytokines.Chemokines.IL8.pg.ml.MSD.BL.plasma."] <- "Biomarker.IL8.pg.ml.MSD.BL.plasma"
names(df_HTML_not_processed)[names(df_HTML_not_processed) == "X.UBIOPRED.Adult_Cohort_.Jan_2019..Biomarker.Data.Baseline.Visit.Plasma.BI.Cytokines.Chemokines.IP10.pg.ml.MSD.BL.plasma."] <- "Biomarker.IP10.pg.ml.MSD.BL.plasma"
names(df_HTML_not_processed)[names(df_HTML_not_processed) == "X.UBIOPRED.Adult_Cohort_.Jan_2019..Biomarker.Data.Baseline.Visit.Plasma.BI.Cytokines.Chemokines.MCP1.pg.ml.MSD.BL.plasma."] <- "Biomarker.MCP1.pg.ml.MSD.BL.plasma"
names(df_HTML_not_processed)[names(df_HTML_not_processed) == "X.UBIOPRED.Adult_Cohort_.Jan_2019..Biomarker.Data.Baseline.Visit.Plasma.BI.Cytokines.Chemokines.MIP1b.pg.ml.MSD.BL.plasma."] <- "Biomarker.MIP1b.pg.ml.MSD.BL.plasma"
names(df_HTML_not_processed)[names(df_HTML_not_processed) == "X.UBIOPRED.Adult_Cohort_.Jan_2019..Biomarker.Data.Baseline.Visit.Plasma.BI.Cytokines.Chemokines.TNFa.pg.ml.MSD.BL.plasma."] <- "Biomarker.TNFa.pg.ml.MSD.BL.plasma"
names(df_HTML_not_processed)[names(df_HTML_not_processed) == "X.UBIOPRED.Adult_Cohort_.Jan_2019..Biomarker.Data.Baseline.Visit.Serum.Genentech.Cytokines.and.Periostin.CCL18.pg.ml.IMPACT.BL.serum."] <- "Biomarker.Genentech.CCL18.pg.ml.IMPACT.BL.serum"
names(df_HTML_not_processed)[names(df_HTML_not_processed) == "X.UBIOPRED.Adult_Cohort_.Jan_2019..Biomarker.Data.Baseline.Visit.Serum.Genentech.Cytokines.and.Periostin.IL13.pg.ml.IMPACT.BL.serum."] <- "Biomarker.Genentech.IL13.pg.ml.IMPACT.BL.serum"
names(df_HTML_not_processed)[names(df_HTML_not_processed) == "X.UBIOPRED.Adult_Cohort_.Jan_2019..Biomarker.Data.Baseline.Visit.Serum.Genentech.Cytokines.and.Periostin.Periostin.ng.ml.ELECSYS.BL.serum."] <- "Biomarker.Genentech.Periostin.ng.ml.ELECSYS.BL.serum"
names(df_HTML_not_processed)[names(df_HTML_not_processed) == "X.UBIOPRED.Adult_Cohort_.Jan_2019..Biomarker.Data.Baseline.Visit.Serum.Karolinska.hsCRP.hCRP..mg.L.."] <- "Biomarker.Karolinska.hsCRP.hCRP.mg.L"
####### END BIOMARKER DATA #######

####### BEG CLINICAL DATA #######
names(df_HTML_not_processed)[names(df_HTML_not_processed) == "X.UBIOPRED.Adult_Cohort_.Jan_2019..Clinical.Data.Atopy.Total.IgE..IU.ml.."] <- "Clinical.Atopy.Total.IgE.IU.ml"
names(df_HTML_not_processed)[names(df_HTML_not_processed) == "X.UBIOPRED.Adult_Cohort_.Jan_2019..Clinical.Data.Electrocardiogram.Screening.Qtc.Interval."] <- "Clinical.Screening.Electrocardiogram.Qtc.Interval."
names(df_HTML_not_processed)[names(df_HTML_not_processed) == "X.UBIOPRED.Adult_Cohort_.Jan_2019..Clinical.Data.Exhaled.nitric.oxide..NO..Baseline.NO.Standard.Flow.Rate."] <- "Clinical.NO.Standard.Flow.Rate"
names(df_HTML_not_processed)[names(df_HTML_not_processed) == "X.UBIOPRED.Adult_Cohort_.Jan_2019..Clinical.Data.Haematology.and.biochemistry.tests.Screening.Albumin..g.dL.."] <- "Clinical.Screening.Albumin.g.dL"
names(df_HTML_not_processed)[names(df_HTML_not_processed) == "X.UBIOPRED.Adult_Cohort_.Jan_2019..Clinical.Data.Haematology.and.biochemistry.tests.Screening.Alkaline.Phosphatase..U.L.."] <- "Clinical.Screening.Alkaline.Phosphatase.U.L"
names(df_HTML_not_processed)[names(df_HTML_not_processed) == "X.UBIOPRED.Adult_Cohort_.Jan_2019..Clinical.Data.Haematology.and.biochemistry.tests.Screening.Alt..U.L.."] <- "Clinical.Screening.Alt.U.L."
names(df_HTML_not_processed)[names(df_HTML_not_processed) == "X.UBIOPRED.Adult_Cohort_.Jan_2019..Clinical.Data.Haematology.and.biochemistry.tests.Screening.Ast..U.L.."] <- "Clinical.Screening.Ast.U.L"
names(df_HTML_not_processed)[names(df_HTML_not_processed) == "X.UBIOPRED.Adult_Cohort_.Jan_2019..Clinical.Data.Haematology.and.biochemistry.tests.Screening.Basophils..."] <- "Clinical.Screening.Basophils"
names(df_HTML_not_processed)[names(df_HTML_not_processed) == "X.UBIOPRED.Adult_Cohort_.Jan_2019..Clinical.Data.Haematology.and.biochemistry.tests.Screening.Blood.Urea.Nitrogen..mg.dL.."] <- "Clinical.Screening.Blood.Urea.Nitrogen.mg.dL"
names(df_HTML_not_processed)[names(df_HTML_not_processed) == "X.UBIOPRED.Adult_Cohort_.Jan_2019..Clinical.Data.Haematology.and.biochemistry.tests.Screening.Creatinine..umol.L.."] <- "Clinical.Screening.Creatinine.umol.L"
names(df_HTML_not_processed)[names(df_HTML_not_processed) == "X.UBIOPRED.Adult_Cohort_.Jan_2019..Clinical.Data.Haematology.and.biochemistry.tests.Screening.Gamma.Gt..U.L.."] <- "Clinical.Screening.Gamma.Gt.U.L"
names(df_HTML_not_processed)[names(df_HTML_not_processed) == "X.UBIOPRED.Adult_Cohort_.Jan_2019..Clinical.Data.Haematology.and.biochemistry.tests.Screening.Haemoglobin..g.dL.."] <- "Clinical.Screening.Haemoglobin.g.dL"
names(df_HTML_not_processed)[names(df_HTML_not_processed) == "X.UBIOPRED.Adult_Cohort_.Jan_2019..Clinical.Data.Haematology.and.biochemistry.tests.Screening.Lymphocytes..."] <- "Clinical.Screening.Lymphocytes"
names(df_HTML_not_processed)[names(df_HTML_not_processed) == "X.UBIOPRED.Adult_Cohort_.Jan_2019..Clinical.Data.Haematology.and.biochemistry.tests.Screening.Monocytes..."] <- "Clinical.Screening.Monocytes"
names(df_HTML_not_processed)[names(df_HTML_not_processed) == "X.UBIOPRED.Adult_Cohort_.Jan_2019..Clinical.Data.Haematology.and.biochemistry.tests.Screening.Neutrophils..."] <- "Clinical.Screening.Neutrophils"
names(df_HTML_not_processed)[names(df_HTML_not_processed) == "X.UBIOPRED.Adult_Cohort_.Jan_2019..Clinical.Data.Haematology.and.biochemistry.tests.Screening.Platelets...x10.3..uL.."] <- "Clinical.Screening.Platelets.x10.3.uL"
names(df_HTML_not_processed)[names(df_HTML_not_processed) == "X.UBIOPRED.Adult_Cohort_.Jan_2019..Clinical.Data.Haematology.and.biochemistry.tests.Screening.Potassium..mmol.L.."] <- "Clinical.Screening.Potassium.mmol.L"
names(df_HTML_not_processed)[names(df_HTML_not_processed) == "X.UBIOPRED.Adult_Cohort_.Jan_2019..Clinical.Data.Haematology.and.biochemistry.tests.Screening.Sodium..mmol.L.."] <- "Clinical.Screening.Sodium.mmol.L"
names(df_HTML_not_processed)[names(df_HTML_not_processed) == "X.UBIOPRED.Adult_Cohort_.Jan_2019..Clinical.Data.Haematology.and.biochemistry.tests.Screening.Total.Bilirubin..umol.L.."] <- "Clinical.Screening.Total.Bilirubin.umol.L"
names(df_HTML_not_processed)[names(df_HTML_not_processed) == "X.UBIOPRED.Adult_Cohort_.Jan_2019..Clinical.Data.Haematology.and.biochemistry.tests.Screening.Total.Protein..g.dL.."] <- "Clinical.Screening.Total.Protein.g.dL"
names(df_HTML_not_processed)[names(df_HTML_not_processed) == "X.UBIOPRED.Adult_Cohort_.Jan_2019..Clinical.Data.Haematology.and.biochemistry.tests.Screening.Wbcs..x10.3..uL.."] <- "Clinical.Screening.Wbcs.x10.3.uL"
names(df_HTML_not_processed)[names(df_HTML_not_processed) == "X.UBIOPRED.Adult_Cohort_.Jan_2019..Clinical.Data.Haematology.and.biochemistry.tests.Screening.basophils..x10.3.uL.."] <- "Clinical.Screening.basophils.x10.3.u"
names(df_HTML_not_processed)[names(df_HTML_not_processed) == "X.UBIOPRED.Adult_Cohort_.Jan_2019..Clinical.Data.Haematology.and.biochemistry.tests.Screening.eosinophils..x10.3.uL.."] <- "Clinical.Screening.eosinophils.x10.3.uL"
names(df_HTML_not_processed)[names(df_HTML_not_processed) == "X.UBIOPRED.Adult_Cohort_.Jan_2019..Clinical.Data.Haematology.and.biochemistry.tests.Screening.lymphocytes..x10.3.uL.."] <- "Clinical.Screening.lymphocytes.x10.3.uL"
names(df_HTML_not_processed)[names(df_HTML_not_processed) == "X.UBIOPRED.Adult_Cohort_.Jan_2019..Clinical.Data.Haematology.and.biochemistry.tests.Screening.monocytes..x10.3.uL.."] <- "Clinical.Screening.monocytes.x10.3.uL"
names(df_HTML_not_processed)[names(df_HTML_not_processed) == "X.UBIOPRED.Adult_Cohort_.Jan_2019..Clinical.Data.Lung.Function.Reversibility.Baseline.FEF.25.75.Absolute.Change..L.sec.."] <- "Clinical.FEF.25.75.Absolute.Change.L.sec"
names(df_HTML_not_processed)[names(df_HTML_not_processed) == "X.UBIOPRED.Adult_Cohort_.Jan_2019..Clinical.Data.Lung.Function.Reversibility.Baseline.FEF.25.75.Change..L.sec.."] <- "Clinical.FEF.25.75.Change.L.sec"
names(df_HTML_not_processed)[names(df_HTML_not_processed) == "X.UBIOPRED.Adult_Cohort_.Jan_2019..Clinical.Data.Lung.Function.Reversibility.Baseline.FEV1.Change."] <- "Clinical.FEV1.Change"
names(df_HTML_not_processed)[names(df_HTML_not_processed) == "X.UBIOPRED.Adult_Cohort_.Jan_2019..Clinical.Data.Lung.Function.Reversibility.Baseline.FEV1.FVC.Ratio.Predicted.LLN."] <- "Clinical.FEV1.FVC.Ratio.Predicted.LLN"
names(df_HTML_not_processed)[names(df_HTML_not_processed) == "X.UBIOPRED.Adult_Cohort_.Jan_2019..Clinical.Data.Lung.Function.Reversibility.Baseline.FEV1.FVC.Ratio.Predicted."] <- "Clinical.FEV1.FVC.Ratio.Predicted"
names(df_HTML_not_processed)[names(df_HTML_not_processed) == "X.UBIOPRED.Adult_Cohort_.Jan_2019..Clinical.Data.Lung.Function.Reversibility.Baseline.FVC.Absolute.Change..L.."] <- "Clinical.FVC.Absolute.Change.L"
names(df_HTML_not_processed)[names(df_HTML_not_processed) == "X.UBIOPRED.Adult_Cohort_.Jan_2019..Clinical.Data.Lung.Function.Reversibility.Baseline.PEF.Change...L.sec.."] <- "Clinical.PEF.Change.L.sec"
names(df_HTML_not_processed)[names(df_HTML_not_processed) == "X.UBIOPRED.Adult_Cohort_.Jan_2019..Clinical.Data.Lung.Function.Spirometry.Baseline.PEF..."] <- "Clinical.Baseline.PEF"
names(df_HTML_not_processed)[names(df_HTML_not_processed) == "X.UBIOPRED.Adult_Cohort_.Jan_2019..Clinical.Data.Vital.Signs.Screening.Diastolic.Blood.Pressure."] <- "Clinical.Screening.Diastolic.Blood.Pressure"
names(df_HTML_not_processed)[names(df_HTML_not_processed) == "X.UBIOPRED.Adult_Cohort_.Jan_2019..Clinical.Data.Vital.Signs.Screening.Heart.Rate."] <- "Clinical.Screening.Heart.Rate"
names(df_HTML_not_processed)[names(df_HTML_not_processed) == "X.UBIOPRED.Adult_Cohort_.Jan_2019..Clinical.Data.Vital.Signs.Screening.Respiratory.Rate."] <- "Clinical.Screening.Respiratory.Rate"
names(df_HTML_not_processed)[names(df_HTML_not_processed) == "X.UBIOPRED.Adult_Cohort_.Jan_2019..Clinical.Data.Vital.Signs.Screening.Systolic.Blood.Pressure."] <- "Clinical.Screening.Systolic.Blood.Pressure"
names(df_HTML_not_processed)[names(df_HTML_not_processed) == "X.UBIOPRED.Adult_Cohort_.Jan_2019..Clinical.Data.Atopy.Combined.Atopy.Result.All.Allergens."] <- "Clinical.Atopy.All.Allergens"
names(df_HTML_not_processed)[names(df_HTML_not_processed) == "X.UBIOPRED.Adult_Cohort_.Jan_2019..Clinical.Data.Atopy.Combined.Atopy.Result.Food.Allergens."] <- "Clinical.Atopy.Food.Allergens"
names(df_HTML_not_processed)[names(df_HTML_not_processed) == "X.UBIOPRED.Adult_Cohort_.Jan_2019..Clinical.Data.Atopy.Combined.Atopy.Result.Regional.Aeroallergens."] <- "Clinical.Atopy.Regional.Aeroallergens"
names(df_HTML_not_processed)[names(df_HTML_not_processed) == "X.UBIOPRED.Adult_Cohort_.Jan_2019..Clinical.Data.Atopy.IgE.Assay.Atopy.Result."] <- "Clinical.Atopy.IgE.Assay.Result"
names(df_HTML_not_processed)[names(df_HTML_not_processed) == "X.UBIOPRED.Adult_Cohort_.Jan_2019..Clinical.Data.Atopy.Skin.Prick.Test.Atopy.Result."] <- "Clinical.Atopy.Skin.Prick.Test.Result"
names(df_HTML_not_processed)[names(df_HTML_not_processed) == "X.UBIOPRED.Adult_Cohort_.Jan_2019..Clinical.Data.Electrocardiogram.Screening.Ecg.Interpretation."] <- "Clinical.Screening.Ecg.Interpretation"
names(df_HTML_not_processed)[names(df_HTML_not_processed) == "X.UBIOPRED.Adult_Cohort_.Jan_2019..Clinical.Data.Exhaled.nitric.oxide..NO..Baseline.NO.Other.Flow.Rates."] <- "Clinical.NO.Other.Flow.Rates"
names(df_HTML_not_processed)[names(df_HTML_not_processed) == "X.UBIOPRED.Adult_Cohort_.Jan_2019..Clinical.Data.Lung.Function.Airflow.Limitation.Baseline.Airflow.Limitation..Quanjer...ERS.guidelines.."] <- "Clinical.Airflow.Limitation.Quanjer.ERS.guidelines"
names(df_HTML_not_processed)[names(df_HTML_not_processed) == "X.UBIOPRED.Adult_Cohort_.Jan_2019..Clinical.Data.Lung.Function.Airflow.Limitation.Baseline.Airflow.Limitation..Ten.Brinke.et.al.."] <- "Clinical.Airflow.Limitation.Ten.Brinke.et.al"
names(df_HTML_not_processed)[names(df_HTML_not_processed) == "X.UBIOPRED.Adult_Cohort_.Jan_2019..Clinical.Data.Lung.Function.Forced.Oscillation.Technique..FOT..Baseline.Fot.Performed."] <- "Clinical.Fot.Performed"
names(df_HTML_not_processed)[names(df_HTML_not_processed) == "X.UBIOPRED.Adult_Cohort_.Jan_2019..Clinical.Data.Haematology.and.biochemistry.tests.Screening.Haematology.Not.Done."] <- "Clinical.Screening.Haematology.Not.Done"
names(df_HTML_not_processed)[names(df_HTML_not_processed) == "X.UBIOPRED.Adult_Cohort_.Jan_2019..Clinical.Data.Lung.Function.Plethysmography.Baseline.Plethysmography.Not.Done."] <- "Clinical.Plethysmography.Not.Done"
names(df_HTML_not_processed)[names(df_HTML_not_processed) == "X.UBIOPRED.Adult_Cohort_.Jan_2019..Clinical.Data.Vital.Signs.Baseline.Day.1.Vital.Signs.Not.Done."] <- "Clinical.Vital.Signs.Not.Done"
####### END CLINICAL DATA #######

####### BEG OMICS DATA #######
names(df_HTML_not_processed)[names(df_HTML_not_processed) == "X.UBIOPRED.Adult_Cohort_.Jan_2019..OMICS.DATA.Drugomics.Baseline.Visit.Urine.Karolinska.Drug.Levels.16a.OH.Prednisolone..ng.mL.."] <- "Omics.Drugs.16a.OH.Prednisolone.ng.mL"
names(df_HTML_not_processed)[names(df_HTML_not_processed) == "X.UBIOPRED.Adult_Cohort_.Jan_2019..OMICS.DATA.Drugomics.Baseline.Visit.Urine.Karolinska.Drug.Levels.19.norandrosterone.glucuronide..ng.mL.."] <- "Omics.Drugs.19.norandrosterone.glucuronide.ng.mL"
names(df_HTML_not_processed)[names(df_HTML_not_processed) == "X.UBIOPRED.Adult_Cohort_.Jan_2019..OMICS.DATA.Drugomics.Baseline.Visit.Urine.Karolinska.Drug.Levels.20B.dihydro.prednisolone..ng.mL.."] <- "Omics.Drugs.20B.dihydro.prednisolone.ng.mL"
names(df_HTML_not_processed)[names(df_HTML_not_processed) == "X.UBIOPRED.Adult_Cohort_.Jan_2019..OMICS.DATA.Drugomics.Baseline.Visit.Urine.Karolinska.Drug.Levels.5alpha.Androstane.3alpha.17beta.diol.17beta.D.glucuronide..ng.mL.."] <- "Omics.Drugs.5alpha.Androstane.3alpha.17beta.diol.17beta.D.glucuronide.ng.mL"
names(df_HTML_not_processed)[names(df_HTML_not_processed) == "X.UBIOPRED.Adult_Cohort_.Jan_2019..OMICS.DATA.Drugomics.Baseline.Visit.Urine.Karolinska.Drug.Levels.5alpha.Androstane.3beta.17beta.diol.3.glucuronide..ng.mL.."] <- "Omics.Drugs.5alpha.Androstane.3beta.17beta.diol.3.glucuronide.ng.mL"
names(df_HTML_not_processed)[names(df_HTML_not_processed) == "X.UBIOPRED.Adult_Cohort_.Jan_2019..OMICS.DATA.Drugomics.Baseline.Visit.Urine.Karolinska.Drug.Levels.5beta.Androstane.3alpha.17beta.diol.17beta.D.glucuronide..ng.mL.."] <- "Omics.Drugs.5beta.Androstane.3alpha.17beta.diol.17beta.D.glucuronide.ng.mL"
names(df_HTML_not_processed)[names(df_HTML_not_processed) == "X.UBIOPRED.Adult_Cohort_.Jan_2019..OMICS.DATA.Drugomics.Baseline.Visit.Urine.Karolinska.Drug.Levels.5beta.Androstane.3alpha.17beta.diol.3alpha.glucuronide..ng.mL.."] <- "Omics.Drugs.5beta.Androstane.3alpha.17beta.diol.3alpha.glucuronide.ng.mL"
names(df_HTML_not_processed)[names(df_HTML_not_processed) == "X.UBIOPRED.Adult_Cohort_.Jan_2019..OMICS.DATA.Drugomics.Baseline.Visit.Urine.Karolinska.Drug.Levels.Androsterone.sulfate..ng.mL.."] <- "Omics.Drugs.Androsterone.sulfate.ng.mL"
names(df_HTML_not_processed)[names(df_HTML_not_processed) == "X.UBIOPRED.Adult_Cohort_.Jan_2019..OMICS.DATA.Drugomics.Baseline.Visit.Urine.Karolinska.Drug.Levels.Cortisol..ng.mL.."] <- "Omics.Drugs.Cortisol.ng.mL"
names(df_HTML_not_processed)[names(df_HTML_not_processed) == "X.UBIOPRED.Adult_Cohort_.Jan_2019..OMICS.DATA.Drugomics.Baseline.Visit.Urine.Karolinska.Drug.Levels.Cortisone...ng.mL.."] <- "Omics.Drugs.Cortisone.ng.mL"
names(df_HTML_not_processed)[names(df_HTML_not_processed) == "X.UBIOPRED.Adult_Cohort_.Jan_2019..OMICS.DATA.Drugomics.Baseline.Visit.Urine.Karolinska.Drug.Levels.Dehydroepiandrosterone.sulfate..ng.mL...DHEA.s.."] <- "Omics.Drugs.Dehydroepiandrosterone.sulfate.ng.mL.DHEA.s"
names(df_HTML_not_processed)[names(df_HTML_not_processed) == "X.UBIOPRED.Adult_Cohort_.Jan_2019..OMICS.DATA.Drugomics.Baseline.Visit.Urine.Karolinska.Drug.Levels.Dihydrotestosterone.glucuronide..ng.mL.."] <- "Omics.Drugs.Dihydrotestosterone.glucuronide.ng.mL"
names(df_HTML_not_processed)[names(df_HTML_not_processed) == "X.UBIOPRED.Adult_Cohort_.Jan_2019..OMICS.DATA.Drugomics.Baseline.Visit.Urine.Karolinska.Drug.Levels.Dihydrotestosterone.sulfate..ng.mL.."] <- "Omics.Drugs.Dihydrotestosterone.sulfate.ng.mL"
names(df_HTML_not_processed)[names(df_HTML_not_processed) == "X.UBIOPRED.Adult_Cohort_.Jan_2019..OMICS.DATA.Drugomics.Baseline.Visit.Urine.Karolinska.Drug.Levels.Epitestosterone.glucuronide..ng.mL.."] <- "Omics.Drugs.Epitestosterone.glucuronide.ng.mL"
names(df_HTML_not_processed)[names(df_HTML_not_processed) == "X.UBIOPRED.Adult_Cohort_.Jan_2019..OMICS.DATA.Drugomics.Baseline.Visit.Urine.Karolinska.Drug.Levels.Etiocholanolone.glucuronide..ng.mL.."] <- "Omics.Drugs.Etiocholanolone.glucuronide.ng.mL"
names(df_HTML_not_processed)[names(df_HTML_not_processed) == "X.UBIOPRED.Adult_Cohort_.Jan_2019..OMICS.DATA.Drugomics.Baseline.Visit.Urine.Karolinska.Drug.Levels.Etiocholanolone.sulfate..ng.mL.."] <- "Omics.Drugs.Etiocholanolone.sulfate.ng.mL"
names(df_HTML_not_processed)[names(df_HTML_not_processed) == "X.UBIOPRED.Adult_Cohort_.Jan_2019..OMICS.DATA.Drugomics.Baseline.Visit.Urine.Karolinska.Drug.Levels.Fenoterol..ng.mL.."] <- "Omics.Drugs.Fenoterol.ng.mL"
names(df_HTML_not_processed)[names(df_HTML_not_processed) == "X.UBIOPRED.Adult_Cohort_.Jan_2019..OMICS.DATA.Drugomics.Baseline.Visit.Urine.Karolinska.Drug.Levels.Methylprednisolone..ng.mL.."] <- "Omics.Drugs.Methylprednisolone.ng.mL"
names(df_HTML_not_processed)[names(df_HTML_not_processed) == "X.UBIOPRED.Adult_Cohort_.Jan_2019..OMICS.DATA.Drugomics.Baseline.Visit.Urine.Karolinska.Drug.Levels.Prednisolone..ng.mL.."] <- "Omics.Drugs.Prednisolone.ng.mL"
names(df_HTML_not_processed)[names(df_HTML_not_processed) == "X.UBIOPRED.Adult_Cohort_.Jan_2019..OMICS.DATA.Drugomics.Baseline.Visit.Urine.Karolinska.Drug.Levels.Prednisone..ng.mL.."] <- "Omics.Drugs.Prednisone.ng.mL"
names(df_HTML_not_processed)[names(df_HTML_not_processed) == "X.UBIOPRED.Adult_Cohort_.Jan_2019..OMICS.DATA.Drugomics.Baseline.Visit.Urine.Karolinska.Drug.Levels.Salbutamol..ng.mL.."] <- "Omics.Drugs.Salbutamol.ng.mL"
names(df_HTML_not_processed)[names(df_HTML_not_processed) == "X.UBIOPRED.Adult_Cohort_.Jan_2019..OMICS.DATA.Drugomics.Baseline.Visit.Urine.Karolinska.Drug.Levels.Terbutaline..ng.mL.."] <- "Omics.Drugs.Terbutaline.ng.mL"
names(df_HTML_not_processed)[names(df_HTML_not_processed) == "X.UBIOPRED.Adult_Cohort_.Jan_2019..OMICS.DATA.Drugomics.Baseline.Visit.Urine.Karolinska.Drug.Levels.Testosterone.glucuronide..ng.mL.."] <- "Omics.Drugs.Testosterone.glucuronide.ng.mL"
names(df_HTML_not_processed)[names(df_HTML_not_processed) == "X.UBIOPRED.Adult_Cohort_.Jan_2019..OMICS.DATA.Drugomics.Baseline.Visit.Urine.Karolinska.Drug.Levels.Testosterone.sulfate..ng.mL.."] <- "Omics.Drugs.Testosterone.sulfate.ng.mL"
names(df_HTML_not_processed)[names(df_HTML_not_processed) == "X.UBIOPRED.Adult_Cohort_.Jan_2019..OMICS.DATA.Drugomics.Baseline.Visit.Urine.Karolinska.Drug.Levels.desacetyl.deflazacort...ng.mL.."] <- "Omics.Drugs.desacetyl.deflazacort.ng.mL"
names(df_HTML_not_processed)[names(df_HTML_not_processed) == "X.UBIOPRED.Adult_Cohort_.Jan_2019..OMICS.DATA.Lipidomics.Baseline.Visit.Urine.Karolinska.Eicosanoid.Lipidomics.11.dehydroTXB2..ng.mmolC.."] <- "Omics.dehydroTXB2.ng.mmolC"
names(df_HTML_not_processed)[names(df_HTML_not_processed) == "X.UBIOPRED.Adult_Cohort_.Jan_2019..OMICS.DATA.Lipidomics.Baseline.Visit.Urine.Karolinska.Eicosanoid.Lipidomics.2.3.dinor.11.B.PGF2a..ng.mmolC.."] <- "Omics.dinor.11.B.PGF2a.ng.mmolC"
names(df_HTML_not_processed)[names(df_HTML_not_processed) == "X.UBIOPRED.Adult_Cohort_.Jan_2019..OMICS.DATA.Lipidomics.Baseline.Visit.Urine.Karolinska.Eicosanoid.Lipidomics.2.3.dinor.8.isoPGF2a..ng.mmolC.."] <- "Omics.dinor.8.isoPGF2a.ng.mmolC"
names(df_HTML_not_processed)[names(df_HTML_not_processed) == "X.UBIOPRED.Adult_Cohort_.Jan_2019..OMICS.DATA.Lipidomics.Baseline.Visit.Urine.Karolinska.Eicosanoid.Lipidomics.2.3.dinor.TXB2..ng.mmolC.."] <- "Omics.dinor.TXB2.ng.mmolC"
names(df_HTML_not_processed)[names(df_HTML_not_processed) == "X.UBIOPRED.Adult_Cohort_.Jan_2019..OMICS.DATA.Lipidomics.Baseline.Visit.Urine.Karolinska.Eicosanoid.Lipidomics.8.12.iso.iPF2a.VI..ng.mmolC.."] <- "Omics.iso.iPF2a.VI.ng.mmolC"
names(df_HTML_not_processed)[names(df_HTML_not_processed) == "X.UBIOPRED.Adult_Cohort_.Jan_2019..OMICS.DATA.Lipidomics.Baseline.Visit.Urine.Karolinska.Eicosanoid.Lipidomics.8.isoPGF2a..ng.mmolC.."] <- "Omics.isoPGF2a.ng.mmolC"
names(df_HTML_not_processed)[names(df_HTML_not_processed) == "X.UBIOPRED.Adult_Cohort_.Jan_2019..OMICS.DATA.Lipidomics.Baseline.Visit.Urine.Karolinska.Eicosanoid.Lipidomics.LTE4..ng.mmolC.."] <- "Omics.LTE4.ng.mmolC"
names(df_HTML_not_processed)[names(df_HTML_not_processed) == "X.UBIOPRED.Adult_Cohort_.Jan_2019..OMICS.DATA.Lipidomics.Baseline.Visit.Urine.Karolinska.Eicosanoid.Lipidomics.PGE2..ng.mmolC.."] <- "Omics.PGE2.ng.mmolC"
names(df_HTML_not_processed)[names(df_HTML_not_processed) == "X.UBIOPRED.Adult_Cohort_.Jan_2019..OMICS.DATA.Lipidomics.Baseline.Visit.Urine.Karolinska.Eicosanoid.Lipidomics.PGF2a..ng.mmolC.."] <- "Omics.PGF2a.ng.mmolC"
names(df_HTML_not_processed)[names(df_HTML_not_processed) == "X.UBIOPRED.Adult_Cohort_.Jan_2019..OMICS.DATA.Lipidomics.Baseline.Visit.Urine.Karolinska.Eicosanoid.Lipidomics.tetranorPGDM..ng.mmolC.."] <- "Omics.tetranorPGDM.ng.mmolC"
names(df_HTML_not_processed)[names(df_HTML_not_processed) == "X.UBIOPRED.Adult_Cohort_.Jan_2019..OMICS.DATA.Lipidomics.Baseline.Visit.Urine.Karolinska.Eicosanoid.Lipidomics.tetranorPGEM..ng.mmolC.."] <- "Omics.tetranorPGEM.ng.mmolC"
names(df_HTML_not_processed)[names(df_HTML_not_processed) == "X.UBIOPRED.Adult_Cohort_.Jan_2019..OMICS.DATA.Metabolomics.Baseline.Visit.Urine.1.3.7.Trimethyluric.acid."] <- "Omics.Trimethyluric.acid"
names(df_HTML_not_processed)[names(df_HTML_not_processed) == "X.UBIOPRED.Adult_Cohort_.Jan_2019..OMICS.DATA.Metabolomics.Baseline.Visit.Urine.1.7.Dimethyluric.acid."] <- "Omics.Dimethyluric.acid"
names(df_HTML_not_processed)[names(df_HTML_not_processed) == "X.UBIOPRED.Adult_Cohort_.Jan_2019..OMICS.DATA.Metabolomics.Baseline.Visit.Urine.1.Methyluric.acid."] <- "Omics.Methyluric.acid"
names(df_HTML_not_processed)[names(df_HTML_not_processed) == "X.UBIOPRED.Adult_Cohort_.Jan_2019..OMICS.DATA.Metabolomics.Baseline.Visit.Urine.2.Furoylglycine."] <- "Omics.Furoylglycine"
names(df_HTML_not_processed)[names(df_HTML_not_processed) == "X.UBIOPRED.Adult_Cohort_.Jan_2019..OMICS.DATA.Metabolomics.Baseline.Visit.Urine.3.Hydroxybutyric.acid."] <- "Omics.Hydroxybutyric.acid"
names(df_HTML_not_processed)[names(df_HTML_not_processed) == "X.UBIOPRED.Adult_Cohort_.Jan_2019..OMICS.DATA.Metabolomics.Baseline.Visit.Urine.3.Hydroxykynurenine."] <- "Omics.Hydroxykynurenine"
names(df_HTML_not_processed)[names(df_HTML_not_processed) == "X.UBIOPRED.Adult_Cohort_.Jan_2019..OMICS.DATA.Metabolomics.Baseline.Visit.Urine.3.Hydroxyproline."] <- "Omics.Hydroxyproline"
names(df_HTML_not_processed)[names(df_HTML_not_processed) == "X.UBIOPRED.Adult_Cohort_.Jan_2019..OMICS.DATA.Metabolomics.Baseline.Visit.Urine.3.Methylhistidine."] <- "Omics.Methylhistidine"
names(df_HTML_not_processed)[names(df_HTML_not_processed) == "X.UBIOPRED.Adult_Cohort_.Jan_2019..OMICS.DATA.Metabolomics.Baseline.Visit.Urine.3.Methylxanthine."] <- "Omics.Methylxanthine"
names(df_HTML_not_processed)[names(df_HTML_not_processed) == "X.UBIOPRED.Adult_Cohort_.Jan_2019..OMICS.DATA.Metabolomics.Baseline.Visit.Urine.4.Pyridoxic.acid."] <- "Omics.Pyridoxic.acid"
names(df_HTML_not_processed)[names(df_HTML_not_processed) == "X.UBIOPRED.Adult_Cohort_.Jan_2019..OMICS.DATA.Metabolomics.Baseline.Visit.Urine.5.Acetylamino.6.formylamino.3.methyluracil."] <- "Omics.Acetylamino.6.formylamino.3.methyluracil"
names(df_HTML_not_processed)[names(df_HTML_not_processed) == "X.UBIOPRED.Adult_Cohort_.Jan_2019..OMICS.DATA.Metabolomics.Baseline.Visit.Urine.5.Aminolevulinic.acid."] <- "Omics.Aminolevulinic.acid"
names(df_HTML_not_processed)[names(df_HTML_not_processed) == "X.UBIOPRED.Adult_Cohort_.Jan_2019..OMICS.DATA.Metabolomics.Baseline.Visit.Urine.5.Hydroxyindoleacetic.acid."] <- "Omics.Hydroxyindoleacetic.acid"
names(df_HTML_not_processed)[names(df_HTML_not_processed) == "X.UBIOPRED.Adult_Cohort_.Jan_2019..OMICS.DATA.Metabolomics.Baseline.Visit.Urine.7.Methylguanine."] <- "Omics.Methylguanine"
names(df_HTML_not_processed)[names(df_HTML_not_processed) == "X.UBIOPRED.Adult_Cohort_.Jan_2019..OMICS.DATA.Metabolomics.Baseline.Visit.Urine.Acetylcarnitine."] <- "Omics.Acetylcarnitine"
names(df_HTML_not_processed)[names(df_HTML_not_processed) == "X.UBIOPRED.Adult_Cohort_.Jan_2019..OMICS.DATA.Metabolomics.Baseline.Visit.Urine.Alanine."] <- "Omics.Alanine"
names(df_HTML_not_processed)[names(df_HTML_not_processed) == "X.UBIOPRED.Adult_Cohort_.Jan_2019..OMICS.DATA.Metabolomics.Baseline.Visit.Urine.Allantoin."] <- "Omics.Allantoin"
names(df_HTML_not_processed)[names(df_HTML_not_processed) == "X.UBIOPRED.Adult_Cohort_.Jan_2019..OMICS.DATA.Metabolomics.Baseline.Visit.Urine.Aminocaproic.acid."] <- "Omics.Aminocaproic.acid"
names(df_HTML_not_processed)[names(df_HTML_not_processed) == "X.UBIOPRED.Adult_Cohort_.Jan_2019..OMICS.DATA.Metabolomics.Baseline.Visit.Urine.Aminovaleric.acid."] <- "Omics.Aminovaleric.acid"
names(df_HTML_not_processed)[names(df_HTML_not_processed) == "X.UBIOPRED.Adult_Cohort_.Jan_2019..OMICS.DATA.Metabolomics.Baseline.Visit.Urine.Aspartic.acid."] <- "Omics.Aspartic.acid"
names(df_HTML_not_processed)[names(df_HTML_not_processed) == "X.UBIOPRED.Adult_Cohort_.Jan_2019..OMICS.DATA.Metabolomics.Baseline.Visit.Urine.Betaine."] <- "Omics.Betaine"
names(df_HTML_not_processed)[names(df_HTML_not_processed) == "X.UBIOPRED.Adult_Cohort_.Jan_2019..OMICS.DATA.Metabolomics.Baseline.Visit.Urine.Biopterin."] <- "Omics.Biopterin"
names(df_HTML_not_processed)[names(df_HTML_not_processed) == "X.UBIOPRED.Adult_Cohort_.Jan_2019..OMICS.DATA.Metabolomics.Baseline.Visit.Urine.Caffeine."] <- "Omics.Caffeine"
names(df_HTML_not_processed)[names(df_HTML_not_processed) == "X.UBIOPRED.Adult_Cohort_.Jan_2019..OMICS.DATA.Metabolomics.Baseline.Visit.Urine.Carnitine."] <- "Omics.Carnitine"
names(df_HTML_not_processed)[names(df_HTML_not_processed) == "X.UBIOPRED.Adult_Cohort_.Jan_2019..OMICS.DATA.Metabolomics.Baseline.Visit.Urine.Carnosine."] <- "Omics.Carnosine"
names(df_HTML_not_processed)[names(df_HTML_not_processed) == "X.UBIOPRED.Adult_Cohort_.Jan_2019..OMICS.DATA.Metabolomics.Baseline.Visit.Urine.Choline."] <- "Omics.Choline"
names(df_HTML_not_processed)[names(df_HTML_not_processed) == "X.UBIOPRED.Adult_Cohort_.Jan_2019..OMICS.DATA.Metabolomics.Baseline.Visit.Urine.Citrulline."] <- "Omics.Citrulline"
names(df_HTML_not_processed)[names(df_HTML_not_processed) == "X.UBIOPRED.Adult_Cohort_.Jan_2019..OMICS.DATA.Metabolomics.Baseline.Visit.Urine.Cystathionine."] <- "Omics.Cystathionine"
names(df_HTML_not_processed)[names(df_HTML_not_processed) == "X.UBIOPRED.Adult_Cohort_.Jan_2019..OMICS.DATA.Metabolomics.Baseline.Visit.Urine.Cytosine."] <- "Omics.Cytosine"
names(df_HTML_not_processed)[names(df_HTML_not_processed) == "X.UBIOPRED.Adult_Cohort_.Jan_2019..OMICS.DATA.Metabolomics.Baseline.Visit.Urine.Galacturonic.acid."] <- "Omics.Galacturonic.acid"
names(df_HTML_not_processed)[names(df_HTML_not_processed) == "X.UBIOPRED.Adult_Cohort_.Jan_2019..OMICS.DATA.Metabolomics.Baseline.Visit.Urine.Glucosamine."] <- "Omics.Glucosamine"
names(df_HTML_not_processed)[names(df_HTML_not_processed) == "X.UBIOPRED.Adult_Cohort_.Jan_2019..OMICS.DATA.Metabolomics.Baseline.Visit.Urine.Glutamic.acid"] <- "Omics.Glutamic.acid"
names(df_HTML_not_processed)[names(df_HTML_not_processed) == "X.UBIOPRED.Adult_Cohort_.Jan_2019..OMICS.DATA.Metabolomics.Baseline.Visit.Urine.Glutamine."] <- "Omics.Glutamine"
names(df_HTML_not_processed)[names(df_HTML_not_processed) == "X.UBIOPRED.Adult_Cohort_.Jan_2019..OMICS.DATA.Metabolomics.Baseline.Visit.Urine.Guanine."] <- "Omics.Guanine"
names(df_HTML_not_processed)[names(df_HTML_not_processed) == "X.UBIOPRED.Adult_Cohort_.Jan_2019..OMICS.DATA.Metabolomics.Baseline.Visit.Urine.Guanosine."] <- "Omics.Guanosine"
names(df_HTML_not_processed)[names(df_HTML_not_processed) == "X.UBIOPRED.Adult_Cohort_.Jan_2019..OMICS.DATA.Metabolomics.Baseline.Visit.Urine.Hippuric.acid."] <- "Omics.Hippuric.acid"
names(df_HTML_not_processed)[names(df_HTML_not_processed) == "X.UBIOPRED.Adult_Cohort_.Jan_2019..OMICS.DATA.Metabolomics.Baseline.Visit.Urine.Histidine."] <- "Omics.Histidine"
names(df_HTML_not_processed)[names(df_HTML_not_processed) == "X.UBIOPRED.Adult_Cohort_.Jan_2019..OMICS.DATA.Metabolomics.Baseline.Visit.Urine.Hydroxyphenylacetic.acid."] <- "Omics.Hydroxyphenylacetic.acid"
names(df_HTML_not_processed)[names(df_HTML_not_processed) == "X.UBIOPRED.Adult_Cohort_.Jan_2019..OMICS.DATA.Metabolomics.Baseline.Visit.Urine.Hypoxanthine."] <- "Omics.Hypoxanthine"
names(df_HTML_not_processed)[names(df_HTML_not_processed) == "X.UBIOPRED.Adult_Cohort_.Jan_2019..OMICS.DATA.Metabolomics.Baseline.Visit.Urine.Inosine."] <- "Omics.Inosine"
names(df_HTML_not_processed)[names(df_HTML_not_processed) == "X.UBIOPRED.Adult_Cohort_.Jan_2019..OMICS.DATA.Metabolomics.Baseline.Visit.Urine.Isoleucine."] <- "Omics.Isoleucine"
names(df_HTML_not_processed)[names(df_HTML_not_processed) == "X.UBIOPRED.Adult_Cohort_.Jan_2019..OMICS.DATA.Metabolomics.Baseline.Visit.Urine.Kynurenic.acid."] <- "Omics.Kynurenic.acid"
names(df_HTML_not_processed)[names(df_HTML_not_processed) == "X.UBIOPRED.Adult_Cohort_.Jan_2019..OMICS.DATA.Metabolomics.Baseline.Visit.Urine.Lysine."] <- "Omics.Lysine"
names(df_HTML_not_processed)[names(df_HTML_not_processed) == "X.UBIOPRED.Adult_Cohort_.Jan_2019..OMICS.DATA.Metabolomics.Baseline.Visit.Urine.Maltose."] <- "Omics.Maltose"
names(df_HTML_not_processed)[names(df_HTML_not_processed) == "X.UBIOPRED.Adult_Cohort_.Jan_2019..OMICS.DATA.Metabolomics.Baseline.Visit.Urine.Mannitol."] <- "Omics.Mannitol"
names(df_HTML_not_processed)[names(df_HTML_not_processed) == "X.UBIOPRED.Adult_Cohort_.Jan_2019..OMICS.DATA.Metabolomics.Baseline.Visit.Urine.Mesaconic.acid."] <- "Omics.Mesaconic.acid"
names(df_HTML_not_processed)[names(df_HTML_not_processed) == "X.UBIOPRED.Adult_Cohort_.Jan_2019..OMICS.DATA.Metabolomics.Baseline.Visit.Urine.Metanephrine."] <- "Omics.Metanephrine"
names(df_HTML_not_processed)[names(df_HTML_not_processed) == "X.UBIOPRED.Adult_Cohort_.Jan_2019..OMICS.DATA.Metabolomics.Baseline.Visit.Urine.Methionine."] <- "Omics.Methionine"
names(df_HTML_not_processed)[names(df_HTML_not_processed) == "X.UBIOPRED.Adult_Cohort_.Jan_2019..OMICS.DATA.Metabolomics.Baseline.Visit.Urine.Methylhippuric.acid."] <- "Omics.Methylhippuric.acid"
names(df_HTML_not_processed)[names(df_HTML_not_processed) == "X.UBIOPRED.Adult_Cohort_.Jan_2019..OMICS.DATA.Metabolomics.Baseline.Visit.Urine.Methylthioadenosine."] <- "Omics.Methylthioadenosine"
names(df_HTML_not_processed)[names(df_HTML_not_processed) == "X.UBIOPRED.Adult_Cohort_.Jan_2019..OMICS.DATA.Metabolomics.Baseline.Visit.Urine.N.Acetylcarnosine."] <- "Omics.N.Acetylcarnosine"
names(df_HTML_not_processed)[names(df_HTML_not_processed) == "X.UBIOPRED.Adult_Cohort_.Jan_2019..OMICS.DATA.Metabolomics.Baseline.Visit.Urine.N.Acetylglutamic.acid."] <- "Omics.N.Acetylglutamic.acid"
names(df_HTML_not_processed)[names(df_HTML_not_processed) == "X.UBIOPRED.Adult_Cohort_.Jan_2019..OMICS.DATA.Metabolomics.Baseline.Visit.Urine.N.Acetylputrescine."] <- "Omics.N.Acetylputrescine"
names(df_HTML_not_processed)[names(df_HTML_not_processed) == "X.UBIOPRED.Adult_Cohort_.Jan_2019..OMICS.DATA.Metabolomics.Baseline.Visit.Urine.N.Methyl.D.aspartic.acid."] <- "Omics.N.Methyl.D.aspartic.acid"
names(df_HTML_not_processed)[names(df_HTML_not_processed) == "X.UBIOPRED.Adult_Cohort_.Jan_2019..OMICS.DATA.Metabolomics.Baseline.Visit.Urine.N.Methylhistamine."] <- "Omics.N.Methylhistamine"
names(df_HTML_not_processed)[names(df_HTML_not_processed) == "X.UBIOPRED.Adult_Cohort_.Jan_2019..OMICS.DATA.Metabolomics.Baseline.Visit.Urine.Nitrotyrosine."] <- "Omics.Nitrotyrosine"
names(df_HTML_not_processed)[names(df_HTML_not_processed) == "X.UBIOPRED.Adult_Cohort_.Jan_2019..OMICS.DATA.Metabolomics.Baseline.Visit.Urine.O.Acetylserine."] <- "Omics.O.Acetylserine"
names(df_HTML_not_processed)[names(df_HTML_not_processed) == "X.UBIOPRED.Adult_Cohort_.Jan_2019..OMICS.DATA.Metabolomics.Baseline.Visit.Urine.Ornithine."] <- "Omics.Ornithine"
names(df_HTML_not_processed)[names(df_HTML_not_processed) == "X.UBIOPRED.Adult_Cohort_.Jan_2019..OMICS.DATA.Metabolomics.Baseline.Visit.Urine.Phenylacetylglutamine."] <- "Omics.Phenylacetylglutamine"
names(df_HTML_not_processed)[names(df_HTML_not_processed) == "X.UBIOPRED.Adult_Cohort_.Jan_2019..OMICS.DATA.Metabolomics.Baseline.Visit.Urine.Phenylalanine."] <- "Omics.Phenylalanine"
names(df_HTML_not_processed)[names(df_HTML_not_processed) == "X.UBIOPRED.Adult_Cohort_.Jan_2019..OMICS.DATA.Metabolomics.Baseline.Visit.Urine.Phenyllactic.acid."] <- "Omics.Phenyllactic.acid"
names(df_HTML_not_processed)[names(df_HTML_not_processed) == "X.UBIOPRED.Adult_Cohort_.Jan_2019..OMICS.DATA.Metabolomics.Baseline.Visit.Urine.Pipecolic.acid."] <- "Omics.Pipecolic.acid"
names(df_HTML_not_processed)[names(df_HTML_not_processed) == "X.UBIOPRED.Adult_Cohort_.Jan_2019..OMICS.DATA.Metabolomics.Baseline.Visit.Urine.Proline."] <- "Omics.Proline"
names(df_HTML_not_processed)[names(df_HTML_not_processed) == "X.UBIOPRED.Adult_Cohort_.Jan_2019..OMICS.DATA.Metabolomics.Baseline.Visit.Urine.Propionylcarnitine."] <- "Omics.Propionylcarnitine"
names(df_HTML_not_processed)[names(df_HTML_not_processed) == "X.UBIOPRED.Adult_Cohort_.Jan_2019..OMICS.DATA.Metabolomics.Baseline.Visit.Urine.Pyridoxal."] <- "Omics.Pyridoxal"
names(df_HTML_not_processed)[names(df_HTML_not_processed) == "X.UBIOPRED.Adult_Cohort_.Jan_2019..OMICS.DATA.Metabolomics.Baseline.Visit.Urine.Pyroglutamic.acid."] <- "Omics.Pyroglutamic.acid"
names(df_HTML_not_processed)[names(df_HTML_not_processed) == "X.UBIOPRED.Adult_Cohort_.Jan_2019..OMICS.DATA.Metabolomics.Baseline.Visit.Urine.Pyroglutamylglycine."] <- "Omics.Pyroglutamylglycine"
names(df_HTML_not_processed)[names(df_HTML_not_processed) == "X.UBIOPRED.Adult_Cohort_.Jan_2019..OMICS.DATA.Metabolomics.Baseline.Visit.Urine.S.Adenosylhomocysteine."] <- "Omics.S.Adenosylhomocysteine"
names(df_HTML_not_processed)[names(df_HTML_not_processed) == "X.UBIOPRED.Adult_Cohort_.Jan_2019..OMICS.DATA.Metabolomics.Baseline.Visit.Urine.Saccharopine."] <- "Omics.Saccharopine"
names(df_HTML_not_processed)[names(df_HTML_not_processed) == "X.UBIOPRED.Adult_Cohort_.Jan_2019..OMICS.DATA.Metabolomics.Baseline.Visit.Urine.Sarcosine."] <- "Omics.Sarcosine"
names(df_HTML_not_processed)[names(df_HTML_not_processed) == "X.UBIOPRED.Adult_Cohort_.Jan_2019..OMICS.DATA.Metabolomics.Baseline.Visit.Urine.Serine."] <- "Omics.Serine"
names(df_HTML_not_processed)[names(df_HTML_not_processed) == "X.UBIOPRED.Adult_Cohort_.Jan_2019..OMICS.DATA.Metabolomics.Baseline.Visit.Urine.Serotonin."] <- "Omics.Serotonin"
names(df_HTML_not_processed)[names(df_HTML_not_processed) == "X.UBIOPRED.Adult_Cohort_.Jan_2019..OMICS.DATA.Metabolomics.Baseline.Visit.Urine.Sucrose."] <- "Omics.Sucrose"
names(df_HTML_not_processed)[names(df_HTML_not_processed) == "X.UBIOPRED.Adult_Cohort_.Jan_2019..OMICS.DATA.Metabolomics.Baseline.Visit.Urine.Taurine."] <- "Omics.Taurine"
names(df_HTML_not_processed)[names(df_HTML_not_processed) == "X.UBIOPRED.Adult_Cohort_.Jan_2019..OMICS.DATA.Metabolomics.Baseline.Visit.Urine.Theobromine."] <- "Omics.Theobromine"
names(df_HTML_not_processed)[names(df_HTML_not_processed) == "X.UBIOPRED.Adult_Cohort_.Jan_2019..OMICS.DATA.Metabolomics.Baseline.Visit.Urine.Tryptamine."] <- "Omics.Tryptamine"
names(df_HTML_not_processed)[names(df_HTML_not_processed) == "X.UBIOPRED.Adult_Cohort_.Jan_2019..OMICS.DATA.Metabolomics.Baseline.Visit.Urine.Tyramine."] <- "Omics.Tyramine"
names(df_HTML_not_processed)[names(df_HTML_not_processed) == "X.UBIOPRED.Adult_Cohort_.Jan_2019..OMICS.DATA.Metabolomics.Baseline.Visit.Urine.Tyrosine."] <- "Omics.Tyrosine"
names(df_HTML_not_processed)[names(df_HTML_not_processed) == "X.UBIOPRED.Adult_Cohort_.Jan_2019..OMICS.DATA.Metabolomics.Baseline.Visit.Urine.Uracil."] <- "Omics.Uracil"
names(df_HTML_not_processed)[names(df_HTML_not_processed) == "X.UBIOPRED.Adult_Cohort_.Jan_2019..OMICS.DATA.Metabolomics.Baseline.Visit.Urine.Uric.acid."] <- "Omics.Uric.acid"
names(df_HTML_not_processed)[names(df_HTML_not_processed) == ".UBIOPRED.Adult_Cohort_.Jan_2019..OMICS.DATA.Metabolomics.Baseline.Visit.Urine.Xanthine."] <- "Omics.Xanthine"
names(df_HTML_not_processed)[names(df_HTML_not_processed) == "X.UBIOPRED.Adult_Cohort_.Jan_2019..OMICS.DATA.Metabolomics.Baseline.Visit.Urine.Xanthosine."] <- "Omics.Xanthosine"
names(df_HTML_not_processed)[names(df_HTML_not_processed) == "X.UBIOPRED.Adult_Cohort_.Jan_2019..OMICS.DATA.Metabolomics.Baseline.Visit.Urine.Xylose."] <- "Omics.Xylose"
names(df_HTML_not_processed)[names(df_HTML_not_processed) == "X.UBIOPRED.Adult_Cohort_.Jan_2019..OMICS.DATA.Metabolomics.Baseline.Visit.Urine.alpha.Glutamyltyrosine."] <- "Omics.alpha.Glutamyltyrosine"
names(df_HTML_not_processed)[names(df_HTML_not_processed) == "X.UBIOPRED.Adult_Cohort_.Jan_2019..OMICS.DATA.Metabolomics.Baseline.Visit.Urine.gamma.Aminobutyric.acid."] <- "Omics.gamma.Aminobutyric.acid"
names(df_HTML_not_processed)[names(df_HTML_not_processed) == "X.UBIOPRED.Adult_Cohort_.Jan_2019..OMICS.DATA.Drugomics.Baseline.Visit.Urine.Karolinska.Drug.Levels.ORAL.CORTICOSTEROIDS.DETECTED."] <- "Omics.ORAL.CORTICOSTEROIDS.DETECTED"
names(df_HTML_not_processed)[names(df_HTML_not_processed) == "X.UBIOPRED.Adult_Cohort_.Jan_2019..OMICS.DATA.Drugomics.Baseline.Visit.Urine.Karolinska.Drug.Levels.b.AGONISTS.DETECTED."] <- "Omics.b.AGONISTS.DETECTED"
names(df_HTML_not_processed)[names(df_HTML_not_processed) == "X.UBIOPRED.Adult_Cohort_.Jan_2019..OMICS.DATA.Lipidomics.Baseline.Visit.Plasma."] <- "Omics.Lipidomics.Plasma"
names(df_HTML_not_processed)[names(df_HTML_not_processed) == "X.UBIOPRED.Adult_Cohort_.Jan_2019..OMICS.DATA.Proteomics.Baseline.Visit.Plasma."] <- "Omics.Proteomics.Plasma"
names(df_HTML_not_processed)[names(df_HTML_not_processed) == "X.UBIOPRED.Adult_Cohort_.Jan_2019..OMICS.DATA.Proteomics.Baseline.Visit.Serum."] <- "Omics.Proteomics.Serum"
names(df_HTML_not_processed)[names(df_HTML_not_processed) == "X.UBIOPRED.Adult_Cohort_.Jan_2019..OMICS.DATA.Transcriptomics.Baseline.Visit.Blood."] <- "Omics.Transcriptomics.Blood"
####### END OMICS DATA #######

####### BEG QUESTIONNAIRES DATA #######
names(df_HTML_not_processed)[names(df_HTML_not_processed) == "X.UBIOPRED.Adult_Cohort_.Jan_2019..Questionnaires.Asthma.Control.Questionnaire..ACQ..Baseline.Visit.FEV1.Precentage."] <- "Questionnaires.ACQ.FEV1.Precentage"
names(df_HTML_not_processed)[names(df_HTML_not_processed) == "X.UBIOPRED.Adult_Cohort_.Jan_2019..Questionnaires.Asthma.Quality.of.Life.Questionnaire..AQLQ..Baseline.Visit.Emotional.Total..Imputed.."] <- "Questionnaires.AQLQ.Emotional.Total.Imputed"
names(df_HTML_not_processed)[names(df_HTML_not_processed) == "X.UBIOPRED.Adult_Cohort_.Jan_2019..Questionnaires.Epworth.Sleepiness.Scale..ESS..Baseline.Visit.Total..Imputed.."] <- "Questionnaires.ESS.Total.Imputed"
names(df_HTML_not_processed)[names(df_HTML_not_processed) == "X.UBIOPRED.Adult_Cohort_.Jan_2019..Questionnaires.Hospital.Anxiety.and.Depression.Scale..HADS..Baseline.Visit.Anxiety.Total..Imputed.."] <- "Questionnaires.HADS.Anxiety.Total.Imputed"
names(df_HTML_not_processed)[names(df_HTML_not_processed) == "X.UBIOPRED.Adult_Cohort_.Jan_2019..Questionnaires.Hospital.Anxiety.and.Depression.Scale..HADS..Baseline.Visit.Depression.Total..Imputed.."] <- "Questionnaires.HADS.Depression.Total.Imputed"
names(df_HTML_not_processed)[names(df_HTML_not_processed) == "X.UBIOPRED.Adult_Cohort_.Jan_2019..Questionnaires.SinoNasal.Outcomes.Test..SNOT..Baseline.Visit.Total..Imputed.."] <- "Questionnaires.SNOT.Total.Imputed"
names(df_HTML_not_processed)[names(df_HTML_not_processed) == "X.UBIOPRED.Adult_Cohort_.Jan_2019..Questionnaires.Asthma.Control.Questionnaire..ACQ..Baseline.Visit.Question.1."] <- "Questionnaires.ACQ.Question.1"
names(df_HTML_not_processed)[names(df_HTML_not_processed) == "X.UBIOPRED.Adult_Cohort_.Jan_2019..Questionnaires.Asthma.Control.Questionnaire..ACQ..Baseline.Visit.Question.2."] <- "Questionnaires.ACQ.Question.2"
names(df_HTML_not_processed)[names(df_HTML_not_processed) == "X.UBIOPRED.Adult_Cohort_.Jan_2019..Questionnaires.Asthma.Control.Questionnaire..ACQ..Baseline.Visit.Question.3."] <- "Questionnaires.ACQ.Question.3"
names(df_HTML_not_processed)[names(df_HTML_not_processed) == "X.UBIOPRED.Adult_Cohort_.Jan_2019..Questionnaires.Asthma.Control.Questionnaire..ACQ..Baseline.Visit.Question.5."] <- "Questionnaires.ACQ.Question.5"
names(df_HTML_not_processed)[names(df_HTML_not_processed) == "X.UBIOPRED.Adult_Cohort_.Jan_2019..Questionnaires.Asthma.Control.Questionnaire..ACQ..Baseline.Visit.Question.6."] <- "Questionnaires.ACQ.Question.6"
names(df_HTML_not_processed)[names(df_HTML_not_processed) == "X.UBIOPRED.Adult_Cohort_.Jan_2019..Questionnaires.Asthma.Control.Questionnaire..ACQ..Baseline.Visit.Question.7."] <- "Questionnaires.ACQ.Question.7"
names(df_HTML_not_processed)[names(df_HTML_not_processed) == "X.UBIOPRED.Adult_Cohort_.Jan_2019..Questionnaires.Asthma.Quality.of.Life.Questionnaire..AQLQ..Baseline.Visit.Question.11."] <- "Questionnaires.AQLQ.Question.11"
names(df_HTML_not_processed)[names(df_HTML_not_processed) == "X.UBIOPRED.Adult_Cohort_.Jan_2019..Questionnaires.Asthma.Quality.of.Life.Questionnaire..AQLQ..Baseline.Visit.Question.12."] <- "Questionnaires.AQLQ.Question.12"
names(df_HTML_not_processed)[names(df_HTML_not_processed) == "X.UBIOPRED.Adult_Cohort_.Jan_2019..Questionnaires.Asthma.Quality.of.Life.Questionnaire..AQLQ..Baseline.Visit.Question.16."] <- "Questionnaires.AQLQ.Question.16"
names(df_HTML_not_processed)[names(df_HTML_not_processed) == "X.UBIOPRED.Adult_Cohort_.Jan_2019..Questionnaires.Asthma.Quality.of.Life.Questionnaire..AQLQ..Baseline.Visit.Question.26."] <- "Questionnaires.AQLQ.Question.26"
names(df_HTML_not_processed)[names(df_HTML_not_processed) == "X.UBIOPRED.Adult_Cohort_.Jan_2019..Questionnaires.Asthma.Quality.of.Life.Questionnaire..AQLQ..Baseline.Visit.Question.9."] <- "Questionnaires.AQLQ.Question.9"
names(df_HTML_not_processed)[names(df_HTML_not_processed) == "X.UBIOPRED.Adult_Cohort_.Jan_2019..Questionnaires.Epworth.Sleepiness.Scale..ESS..Baseline.Visit.Question.1."] <- "Questionnaires.ESS.Question.1"
names(df_HTML_not_processed)[names(df_HTML_not_processed) == "X.UBIOPRED.Adult_Cohort_.Jan_2019..Questionnaires.Epworth.Sleepiness.Scale..ESS..Baseline.Visit.Question.2."] <- "Questionnaires.ESS.Question.2"
names(df_HTML_not_processed)[names(df_HTML_not_processed) == "X.UBIOPRED.Adult_Cohort_.Jan_2019..Questionnaires.Epworth.Sleepiness.Scale..ESS..Baseline.Visit.Question.3."] <- "Questionnaires.ESS.Question.3"
names(df_HTML_not_processed)[names(df_HTML_not_processed) == "X.UBIOPRED.Adult_Cohort_.Jan_2019..Questionnaires.Epworth.Sleepiness.Scale..ESS..Baseline.Visit.Question.4."] <- "Questionnaires.ESS.Question.4"
names(df_HTML_not_processed)[names(df_HTML_not_processed) == "X.UBIOPRED.Adult_Cohort_.Jan_2019..Questionnaires.Epworth.Sleepiness.Scale..ESS..Baseline.Visit.Question.5."] <- "Questionnaires.ESS.Question.5"
names(df_HTML_not_processed)[names(df_HTML_not_processed) == "X.UBIOPRED.Adult_Cohort_.Jan_2019..Questionnaires.Epworth.Sleepiness.Scale..ESS..Baseline.Visit.Question.6."] <- "Questionnaires.ESS.Question.6"
names(df_HTML_not_processed)[names(df_HTML_not_processed) == "X.UBIOPRED.Adult_Cohort_.Jan_2019..Questionnaires.Epworth.Sleepiness.Scale..ESS..Baseline.Visit.Question.7."] <- "Questionnaires.ESS.Question.7"
names(df_HTML_not_processed)[names(df_HTML_not_processed) == "X.UBIOPRED.Adult_Cohort_.Jan_2019..Questionnaires.Epworth.Sleepiness.Scale..ESS..Baseline.Visit.Question.8."] <- "Questionnaires.ESS.Question.8"
names(df_HTML_not_processed)[names(df_HTML_not_processed) == "X.UBIOPRED.Adult_Cohort_.Jan_2019..Questionnaires.Hospital.Anxiety.and.Depression.Scale..HADS..Baseline.Visit.Question.10."] <- "Questionnaires.HADS.Question.10"
names(df_HTML_not_processed)[names(df_HTML_not_processed) == "X.UBIOPRED.Adult_Cohort_.Jan_2019..Questionnaires.Hospital.Anxiety.and.Depression.Scale..HADS..Baseline.Visit.Question.11."] <- "Questionnaires.HADS.Question.11"
names(df_HTML_not_processed)[names(df_HTML_not_processed) == "X.UBIOPRED.Adult_Cohort_.Jan_2019..Questionnaires.Hospital.Anxiety.and.Depression.Scale..HADS..Baseline.Visit.Question.12."] <- "Questionnaires.HADS.Question.12"
names(df_HTML_not_processed)[names(df_HTML_not_processed) == "X.UBIOPRED.Adult_Cohort_.Jan_2019..Questionnaires.Hospital.Anxiety.and.Depression.Scale..HADS..Baseline.Visit.Question.13."] <- "Questionnaires.HADS.Question.13"
names(df_HTML_not_processed)[names(df_HTML_not_processed) == "X.UBIOPRED.Adult_Cohort_.Jan_2019..Questionnaires.Hospital.Anxiety.and.Depression.Scale..HADS..Baseline.Visit.Question.14."] <- "Questionnaires.HADS.Question.14"
names(df_HTML_not_processed)[names(df_HTML_not_processed) == "X.UBIOPRED.Adult_Cohort_.Jan_2019..Questionnaires.Hospital.Anxiety.and.Depression.Scale..HADS..Baseline.Visit.Question.1."] <- "Questionnaires.HADS.Question.1"
names(df_HTML_not_processed)[names(df_HTML_not_processed) == "X.UBIOPRED.Adult_Cohort_.Jan_2019..Questionnaires.Hospital.Anxiety.and.Depression.Scale..HADS..Baseline.Visit.Question.2."] <- "Questionnaires.HADS.Question.2"
names(df_HTML_not_processed)[names(df_HTML_not_processed) == "X.UBIOPRED.Adult_Cohort_.Jan_2019..Questionnaires.Hospital.Anxiety.and.Depression.Scale..HADS..Baseline.Visit.Question.3."] <- "Questionnaires.HADS.Question.3"
names(df_HTML_not_processed)[names(df_HTML_not_processed) == "X.UBIOPRED.Adult_Cohort_.Jan_2019..Questionnaires.Hospital.Anxiety.and.Depression.Scale..HADS..Baseline.Visit.Question.4."] <- "Questionnaires.HADS.Question.4"
names(df_HTML_not_processed)[names(df_HTML_not_processed) == "X.UBIOPRED.Adult_Cohort_.Jan_2019..Questionnaires.Hospital.Anxiety.and.Depression.Scale..HADS..Baseline.Visit.Question.5."] <- "Questionnaires.HADS.Question.5"
names(df_HTML_not_processed)[names(df_HTML_not_processed) == "X.UBIOPRED.Adult_Cohort_.Jan_2019..Questionnaires.Hospital.Anxiety.and.Depression.Scale..HADS..Baseline.Visit.Question.6."] <- "Questionnaires.HADS.Question.6"
names(df_HTML_not_processed)[names(df_HTML_not_processed) == "X.UBIOPRED.Adult_Cohort_.Jan_2019..Questionnaires.Hospital.Anxiety.and.Depression.Scale..HADS..Baseline.Visit.Question.7."] <- "Questionnaires.HADS.Question.7"
names(df_HTML_not_processed)[names(df_HTML_not_processed) == "X.UBIOPRED.Adult_Cohort_.Jan_2019..Questionnaires.Hospital.Anxiety.and.Depression.Scale..HADS..Baseline.Visit.Question.8."] <- "Questionnaires.HADS.Question.8"
names(df_HTML_not_processed)[names(df_HTML_not_processed) == "X.UBIOPRED.Adult_Cohort_.Jan_2019..Questionnaires.Hospital.Anxiety.and.Depression.Scale..HADS..Baseline.Visit.Question.9."] <- "Questionnaires.HADS.Question.9"
names(df_HTML_not_processed)[names(df_HTML_not_processed) == "X.UBIOPRED.Adult_Cohort_.Jan_2019..Questionnaires.Medication.Adherence.Rating.Scale..MARS..Baseline.Visit.Total..Imputed.."] <- "Questionnaires.MARS.Total.Imputed"
names(df_HTML_not_processed)[names(df_HTML_not_processed) == "X.UBIOPRED.Adult_Cohort_.Jan_2019..Questionnaires.SinoNasal.Outcomes.Test..SNOT..Baseline.Visit.Question.10."] <- "Questionnaires.SNOT.Question.10"
names(df_HTML_not_processed)[names(df_HTML_not_processed) == "X.UBIOPRED.Adult_Cohort_.Jan_2019..Questionnaires.SinoNasal.Outcomes.Test..SNOT..Baseline.Visit.Question.11."] <- "Questionnaires.SNOT.Question.11"
names(df_HTML_not_processed)[names(df_HTML_not_processed) == "X.UBIOPRED.Adult_Cohort_.Jan_2019..Questionnaires.SinoNasal.Outcomes.Test..SNOT..Baseline.Visit.Question.13."] <- "Questionnaires.SNOT.Question.13"
names(df_HTML_not_processed)[names(df_HTML_not_processed) == "X.UBIOPRED.Adult_Cohort_.Jan_2019..Questionnaires.SinoNasal.Outcomes.Test..SNOT..Baseline.Visit.Question.14."] <- "uestionnaires.SNOT.Question.14"
names(df_HTML_not_processed)[names(df_HTML_not_processed) == "X.UBIOPRED.Adult_Cohort_.Jan_2019..Questionnaires.SinoNasal.Outcomes.Test..SNOT..Baseline.Visit.Question.16."] <- "Questionnaires.SNOT.Question.16"
names(df_HTML_not_processed)[names(df_HTML_not_processed) == "X.UBIOPRED.Adult_Cohort_.Jan_2019..Questionnaires.SinoNasal.Outcomes.Test..SNOT..Baseline.Visit.Question.17."] <- "Questionnaires.SNOT.Question.17"
names(df_HTML_not_processed)[names(df_HTML_not_processed) == "X.UBIOPRED.Adult_Cohort_.Jan_2019..Questionnaires.SinoNasal.Outcomes.Test..SNOT..Baseline.Visit.Question.18."] <- "Questionnaires.SNOT.Question.18"
names(df_HTML_not_processed)[names(df_HTML_not_processed) == "X.UBIOPRED.Adult_Cohort_.Jan_2019..Questionnaires.SinoNasal.Outcomes.Test..SNOT..Baseline.Visit.Question.19."] <- "Questionnaires.SNOT.Question.19"
names(df_HTML_not_processed)[names(df_HTML_not_processed) == "X.UBIOPRED.Adult_Cohort_.Jan_2019..Questionnaires.SinoNasal.Outcomes.Test..SNOT..Baseline.Visit.Question.1."] <- "Questionnaires.SNOT.Question.1"
names(df_HTML_not_processed)[names(df_HTML_not_processed) == "X.UBIOPRED.Adult_Cohort_.Jan_2019..Questionnaires.SinoNasal.Outcomes.Test..SNOT..Baseline.Visit.Question.20."] <- "Questionnaires.SNOT.Question.20"
names(df_HTML_not_processed)[names(df_HTML_not_processed) == "X.UBIOPRED.Adult_Cohort_.Jan_2019..Questionnaires.SinoNasal.Outcomes.Test..SNOT..Baseline.Visit.Question.2."] <- "Questionnaires.SNOT.Question.2"
names(df_HTML_not_processed)[names(df_HTML_not_processed) == "X.UBIOPRED.Adult_Cohort_.Jan_2019..Questionnaires.SinoNasal.Outcomes.Test..SNOT..Baseline.Visit.Question.3."] <- "Questionnaires.SNOT.Question.3"
names(df_HTML_not_processed)[names(df_HTML_not_processed) == "X.UBIOPRED.Adult_Cohort_.Jan_2019..Questionnaires.SinoNasal.Outcomes.Test..SNOT..Baseline.Visit.Question.4."] <- "Questionnaires.SNOT.Question.4"
names(df_HTML_not_processed)[names(df_HTML_not_processed) == "X.UBIOPRED.Adult_Cohort_.Jan_2019..Questionnaires.SinoNasal.Outcomes.Test..SNOT..Baseline.Visit.Question.5."] <- "Questionnaires.SNOT.Question.5"
names(df_HTML_not_processed)[names(df_HTML_not_processed) == "X.UBIOPRED.Adult_Cohort_.Jan_2019..Questionnaires.SinoNasal.Outcomes.Test..SNOT..Baseline.Visit.Question.6."] <- "Questionnaires.SNOT.Question.6"
names(df_HTML_not_processed)[names(df_HTML_not_processed) == "X.UBIOPRED.Adult_Cohort_.Jan_2019..Questionnaires.SinoNasal.Outcomes.Test..SNOT..Baseline.Visit.Question.7."] <- "Questionnaires.SNOT.Question.7"
names(df_HTML_not_processed)[names(df_HTML_not_processed) == "X.UBIOPRED.Adult_Cohort_.Jan_2019..Questionnaires.SinoNasal.Outcomes.Test..SNOT..Baseline.Visit.Question.8."] <- "Questionnaires.SNOT.Question.8"
names(df_HTML_not_processed)[names(df_HTML_not_processed) == "X.UBIOPRED.Adult_Cohort_.Jan_2019..Questionnaires.SinoNasal.Outcomes.Test..SNOT..Baseline.Visit.Question.9."] <- "Questionnaires.SNOT.Question.9"
####### END QUESTIONNAIRES DATA #######

####### BEG DEMOGRAPHIC DATA #######
names(df_HTML_not_processed)[names(df_HTML_not_processed) == "X.UBIOPRED.Adult_Cohort_.Jan_2019..Demographic.Data.Age."] <- "Demographic.Age"
names(df_HTML_not_processed)[names(df_HTML_not_processed) == "X.UBIOPRED.Adult_Cohort_.Jan_2019..Demographic.Data.Ethnic.origin.Mother."] <- "Demographic.Ethnic.origin.Mother"
names(df_HTML_not_processed)[names(df_HTML_not_processed) == "X.UBIOPRED.Adult_Cohort_.Jan_2019..Demographic.Data.Highest.Level.Education."] <- "Demographic.Highest.Level.Education"
names(df_HTML_not_processed)[names(df_HTML_not_processed) == "X.UBIOPRED.Adult_Cohort_.Jan_2019..Demographic.Data.Marital.Status."] <- "Demographic.Marital.Status"
names(df_HTML_not_processed)[names(df_HTML_not_processed) == "X.UBIOPRED.Adult_Cohort_.Jan_2019..Demographic.Data.Sex."] <- "Demographic.Sex"
names(df_HTML_not_processed)[names(df_HTML_not_processed) == "X.UBIOPRED.Adult_Cohort_.Jan_2019..Demographic.Data.Occupation.occupational.employed."] <- "Demographic.occupational.employed"
names(df_HTML_not_processed)[names(df_HTML_not_processed) == "X.UBIOPRED.Adult_Cohort_.Jan_2019..Demographic.Data.Occupation.occupational.keeping.house."] <- "Demographic.occupational.keeping.house"
names(df_HTML_not_processed)[names(df_HTML_not_processed) == "X.UBIOPRED.Adult_Cohort_.Jan_2019..Demographic.Data.Occupation.occupational.other."] <- "Demographic.occupational.other"
names(df_HTML_not_processed)[names(df_HTML_not_processed) == "X.UBIOPRED.Adult_Cohort_.Jan_2019..Demographic.Data.Occupation.occupational.retired."] <- "Demographic.occupational.retired"
names(df_HTML_not_processed)[names(df_HTML_not_processed) == "X.UBIOPRED.Adult_Cohort_.Jan_2019..Demographic.Data.Occupation.occupational.student."] <- "Demographic.occupational.student"
names(df_HTML_not_processed)[names(df_HTML_not_processed) == "X.UBIOPRED.Adult_Cohort_.Jan_2019..Demographic.Data.Occupation.occupational.unemployed."] <- "Demographic.occupational.unemployed"
names(df_HTML_not_processed)[names(df_HTML_not_processed) == "X.UBIOPRED.Adult_Cohort_.Jan_2019..Demographic.Data.Occupation.occupational.volunteer."] <- "Demographic.occupational.volunteer"
####### END DEMOGRAPHIC DATA #######

####### BEG SUBJECT BODY DATA #######
names(df_HTML_not_processed)[names(df_HTML_not_processed) == "X.UBIOPRED.Adult_Cohort_.Jan_2019..Subject.Body.Measurements.Body.Mass.Index..kg.m2.."] <- "Body.Mass.Index.kg.m2"
names(df_HTML_not_processed)[names(df_HTML_not_processed) == "X.UBIOPRED.Adult_Cohort_.Jan_2019..Subject.Body.Measurements.Height..cm.."] <- "Height.cm"
####### END SUBJECT BODY DATA #######

####### BEG SUBJECT CLUSTERS DATA ####### YOU MIGHT WANT TO REMOVE THOSE
names(df_HTML_not_processed)[names(df_HTML_not_processed) == "X.UBIOPRED.Adult_Cohort_.Jan_2019..Subject.Clusters.Classification.by.eosinophil.count.eosinophils..10.3.per.uL.."] <- "Sub.Cluster.Classification.by.eosinophil.count.eosinophils.10.3.per.uL"
names(df_HTML_not_processed)[names(df_HTML_not_processed) == "X.UBIOPRED.Adult_Cohort_.Jan_2019..Subject.Clusters.Classification.by.eosinophil.count.clinical.cutoff."] <- "Sub.Cluster.Classification.by.eosinophil.count.clinical.cutoff"
names(df_HTML_not_processed)[names(df_HTML_not_processed) == "X.UBIOPRED.Adult_Cohort_.Jan_2019..Subject.Clusters.Classification.by.eosinophil.count.cohort.eosinophil."] <- "Sub.Cluster.Classification.by.eosinophil.count.cohort.eosinophil"
names(df_HTML_not_processed)[names(df_HTML_not_processed) == "X.UBIOPRED.Adult_Cohort_.Jan_2019..Subject.Clusters.Classification.by.eosinophil.count.eosinophil.clinical.level."] <- "Sub.Cluster.Classification.by.eosinophil.count.eosinophil.clinical.level"
names(df_HTML_not_processed)[names(df_HTML_not_processed) == "X.UBIOPRED.Adult_Cohort_.Jan_2019..Subject.Clusters.Classification.by.eosinophil.count.tertile.cutoff."] <- "Sub.Cluster.Classification.by.eosinophil.count.tertile.cutoff"
####### END SUBJECT CLUSTERS DATA #######

####### END SUBJECT HISTORY DATA #######
names(df_HTML_not_processed)[names(df_HTML_not_processed) == "X.UBIOPRED.Adult_Cohort_.Jan_2019..Subject.History.Respiratory.History.Onset.OR.First.Diagnosis.Age..years.."] <- "Respiratory.History.Onset.OR.First.Diagnosis.Age.years"
names(df_HTML_not_processed)[names(df_HTML_not_processed) == "X.UBIOPRED.Adult_Cohort_.Jan_2019..Subject.History.Medical.or.Surgical.History.Allergic.Rhinitis.Diagnosed."] <- "Allergic.Rhinitis.Diagnosed"
names(df_HTML_not_processed)[names(df_HTML_not_processed) == "X.UBIOPRED.Adult_Cohort_.Jan_2019..Subject.History.Medical.or.Surgical.History.Congestive.Diagnosed."] <- "Congestive.Diagnosed"
names(df_HTML_not_processed)[names(df_HTML_not_processed) == "X.UBIOPRED.Adult_Cohort_.Jan_2019..Subject.History.Medical.or.Surgical.History.Coronary.Diagnosed."] <- "Coronary.Diagnosed"
names(df_HTML_not_processed)[names(df_HTML_not_processed) == "X.UBIOPRED.Adult_Cohort_.Jan_2019..Subject.History.Medical.or.Surgical.History.Diabetes.Diagnosed."] <- "Diabetes.Diagnosed"
names(df_HTML_not_processed)[names(df_HTML_not_processed) == "X.UBIOPRED.Adult_Cohort_.Jan_2019..Subject.History.Medical.or.Surgical.History.Eczema.Diagnosed."] <- "Eczema.Diagnosed"
names(df_HTML_not_processed)[names(df_HTML_not_processed) == "X.UBIOPRED.Adult_Cohort_.Jan_2019..Subject.History.Medical.or.Surgical.History.Gerd.Diagnosed."] <- "Gerd.Diagnosed"
names(df_HTML_not_processed)[names(df_HTML_not_processed) == "X.UBIOPRED.Adult_Cohort_.Jan_2019..Subject.History.Medical.or.Surgical.History.Hay.Fever.Diagnosed."] <- "Hay.Fever.Diagnosed"
names(df_HTML_not_processed)[names(df_HTML_not_processed) == "X.UBIOPRED.Adult_Cohort_.Jan_2019..Subject.History.Medical.or.Surgical.History.Hypertension.Diagnosed."] <- "Hypertension.Diagnosed"
names(df_HTML_not_processed)[names(df_HTML_not_processed) == "X.UBIOPRED.Adult_Cohort_.Jan_2019..Subject.History.Medical.or.Surgical.History.Nasal.Polyps.Diagnosed."] <- "Nasal.Polyps.Diagnosed"
names(df_HTML_not_processed)[names(df_HTML_not_processed) == "X.UBIOPRED.Adult_Cohort_.Jan_2019..Subject.History.Medical.or.Surgical.History.Non.Allergic.Rhinitis.Diagnosed."] <- "Non.Allergic.Rhinitis.Diagnosed"
names(df_HTML_not_processed)[names(df_HTML_not_processed) == "X.UBIOPRED.Adult_Cohort_.Jan_2019..Subject.History.Medical.or.Surgical.History.Osteoporosis.Diagnosed."] <- "Osteoporosis.Diagnosed"
names(df_HTML_not_processed)[names(df_HTML_not_processed) == "X.UBIOPRED.Adult_Cohort_.Jan_2019..Subject.History.Medical.or.Surgical.History.Psychiatric.Disease.Diagnosed."] <- "Psychiatric.Disease.Diagnosed"
names(df_HTML_not_processed)[names(df_HTML_not_processed) == "X.UBIOPRED.Adult_Cohort_.Jan_2019..Subject.History.Medical.or.Surgical.History.Sinus.Surgery.Diagnosed."] <- "Sinus.Surgery.Diagnosed"
names(df_HTML_not_processed)[names(df_HTML_not_processed) == "X.UBIOPRED.Adult_Cohort_.Jan_2019..Subject.History.Medical.or.Surgical.History.Sinusitis.Diagnosed."] <- "Sinusitis.Diagnosed"
names(df_HTML_not_processed)[names(df_HTML_not_processed) == "X.UBIOPRED.Adult_Cohort_.Jan_2019..Subject.History.Medical.or.Surgical.History.Vocal.Chord.Diagnosed."] <- "Vocal.Chord.Diagnosed"
names(df_HTML_not_processed)[names(df_HTML_not_processed) == "X.UBIOPRED.Adult_Cohort_.Jan_2019..Subject.History.Parental.Asthma."] <- "Parental.Asthma"
names(df_HTML_not_processed)[names(df_HTML_not_processed) == "X.UBIOPRED.Adult_Cohort_.Jan_2019..Subject.History.Parental.Copd."] <- "Parental.Copd"
names(df_HTML_not_processed)[names(df_HTML_not_processed) == "X.UBIOPRED.Adult_Cohort_.Jan_2019..Subject.History.Parental.Eczema."] <- "Parental.Eczema"
names(df_HTML_not_processed)[names(df_HTML_not_processed) == "X.UBIOPRED.Adult_Cohort_.Jan_2019..Subject.History.Respiratory.History.Breathing.Problems.in.12.months.prior.to.screening."] <- "Breathing.Problems.in.12.months.prior.to.screening"
names(df_HTML_not_processed)[names(df_HTML_not_processed) == "X.UBIOPRED.Adult_Cohort_.Jan_2019..Subject.History.Respiratory.History.Bronchitis."] <- "Bronchitis"
names(df_HTML_not_processed)[names(df_HTML_not_processed) == "X.UBIOPRED.Adult_Cohort_.Jan_2019..Subject.History.Respiratory.History.Chronic.Bronchitis."] <- "Chronic.Bronchitis"
names(df_HTML_not_processed)[names(df_HTML_not_processed) == "X.UBIOPRED.Adult_Cohort_.Jan_2019..Subject.History.Respiratory.History.Emphysema.Or.Copd."] <- "Emphysema.Or.Copd"
names(df_HTML_not_processed)[names(df_HTML_not_processed) == "X.UBIOPRED.Adult_Cohort_.Jan_2019..Subject.History.Respiratory.History.Icu.Admission.Last.Year."] <- "Icu.Admission.Last.Year"
names(df_HTML_not_processed)[names(df_HTML_not_processed) == "X.UBIOPRED.Adult_Cohort_.Jan_2019..Subject.History.Respiratory.History.Icu.Admission."] <- "Icu.Admission"
names(df_HTML_not_processed)[names(df_HTML_not_processed) == "X.UBIOPRED.Adult_Cohort_.Jan_2019..Subject.History.Respiratory.History.Intubation."] <- "Intubation"
names(df_HTML_not_processed)[names(df_HTML_not_processed) == "X.UBIOPRED.Adult_Cohort_.Jan_2019..Subject.History.Respiratory.History.Overnight.Treatment."] <- "Overnight.Treatment"
names(df_HTML_not_processed)[names(df_HTML_not_processed) == "X.UBIOPRED.Adult_Cohort_.Jan_2019..Subject.History.Respiratory.History.Pneumonia."] <- "Pneumonia"
names(df_HTML_not_processed)[names(df_HTML_not_processed) == "X.UBIOPRED.Adult_Cohort_.Jan_2019..Subject.History.Respiratory.History.Rescue.Inhaler.Use."] <- "Rescue.Inhaler.Use"
names(df_HTML_not_processed)[names(df_HTML_not_processed) == "X.UBIOPRED.Adult_Cohort_.Jan_2019..Subject.History.Respiratory.History.Supplemental.Oxygen."] <- "Supplemental.Oxygen"
names(df_HTML_not_processed)[names(df_HTML_not_processed) == "X.UBIOPRED.Adult_Cohort_.Jan_2019..Subject.History.Respiratory.History.Visited.a.ER.at.any.time.because.of.breathing.problems."] <- "Visited.a.ER.at.any.time.because.of.breathing.problems"
names(df_HTML_not_processed)[names(df_HTML_not_processed) == "X.UBIOPRED.Adult_Cohort_.Jan_2019..Subject.History.Respiratory.History.Onset.Breathing.Problems.Uncertain."] <- "Onset.Breathing.Problems.Uncertain"
names(df_HTML_not_processed)[names(df_HTML_not_processed) == "X.UBIOPRED.Adult_Cohort_.Jan_2019..Subject.History.Respiratory.History.Onset.OR.First.Diagnosis.Uncertain."] <- "Onset.OR.First.Diagnosis.Uncertain"
####### END SUBJECT HISTORY DATA #######

####### BEG VISIT INFORMATION DATA #######
names(df_HTML_not_processed)[names(df_HTML_not_processed) == "X.UBIOPRED.Adult_Cohort_.Jan_2019..Visit.Information.Baseline.Visit.Days.Since.Screening..Baseline.Day.1.."] <- "Days.Since.Screening.Baseline.Day.1"
names(df_HTML_not_processed)[names(df_HTML_not_processed) == "X.UBIOPRED.Adult_Cohort_.Jan_2019..Visit.Information.Baseline.Visit.Days.Since.Screening..Baseline.Day.2.."] <- "Days.Since.Screening.Baseline.Day.2"
####### END VISIT INFORMATION DATA #######

####### BEG ASTHMA DIAGNOSIS DATA #######
names(df_HTML_not_processed)[names(df_HTML_not_processed) == "X.UBIOPRED.Adult_Cohort_.Jan_2019..Asthma.Diagnosis.Diagnosis.Fev.Variation."] <- "Asthma.Diag.Fev.Variation"
names(df_HTML_not_processed)[names(df_HTML_not_processed) == "X.UBIOPRED.Adult_Cohort_.Jan_2019..Asthma.Diagnosis.Diagnosis.Pef.Variation."] <- "Asthma.Diag.Pef.Variation"
names(df_HTML_not_processed)[names(df_HTML_not_processed) == "X.UBIOPRED.Adult_Cohort_.Jan_2019..Asthma.Diagnosis.Historic.Test."] <- "Asthma.Diag.Historic.Test"
####### END ASTHMA DIAGNOSIS DATA #######

####### BEG EXPOSURES AND TRIGGERS DATA #######
names(df_HTML_not_processed)[names(df_HTML_not_processed) == "X.UBIOPRED.Adult_Cohort_.Jan_2019..Atopy.Exposures.and.Triggers.Baseline.Aspirin."] <- "Expo.Aspirin"
names(df_HTML_not_processed)[names(df_HTML_not_processed) == "X.UBIOPRED.Adult_Cohort_.Jan_2019..Atopy.Exposures.and.Triggers.Baseline.Barns."] <- "Expo.Barns"
names(df_HTML_not_processed)[names(df_HTML_not_processed) == "X.UBIOPRED.Adult_Cohort_.Jan_2019..Atopy.Exposures.and.Triggers.Baseline.Cold.Air."] <- "Expo.Cold.Air"
names(df_HTML_not_processed)[names(df_HTML_not_processed) == "X.UBIOPRED.Adult_Cohort_.Jan_2019..Atopy.Exposures.and.Triggers.Baseline.Dust."] <- "Expo.Dust"
names(df_HTML_not_processed)[names(df_HTML_not_processed) == "X.UBIOPRED.Adult_Cohort_.Jan_2019..Atopy.Exposures.and.Triggers.Baseline.Fungus."] <- "Expo.Fungus"
names(df_HTML_not_processed)[names(df_HTML_not_processed) == "X.UBIOPRED.Adult_Cohort_.Jan_2019..Atopy.Exposures.and.Triggers.Baseline.Menstrual.Cycle."] <- "Expo.Menstrual.Cycle"
names(df_HTML_not_processed)[names(df_HTML_not_processed) == "X.UBIOPRED.Adult_Cohort_.Jan_2019..Atopy.Exposures.and.Triggers.Baseline.Perfumes."] <- "Expo.Perfumes"
names(df_HTML_not_processed)[names(df_HTML_not_processed) == "X.UBIOPRED.Adult_Cohort_.Jan_2019..Atopy.Exposures.and.Triggers.Baseline.Pets."] <- "Expo.Pets"
names(df_HTML_not_processed)[names(df_HTML_not_processed) == "X.UBIOPRED.Adult_Cohort_.Jan_2019..Atopy.Exposures.and.Triggers.Baseline.Physical.Exercise."] <- "Expo.Physical.Exercise"
names(df_HTML_not_processed)[names(df_HTML_not_processed) == "X.UBIOPRED.Adult_Cohort_.Jan_2019..Atopy.Exposures.and.Triggers.Baseline.Pollen."] <- "Expo.Pollen"
names(df_HTML_not_processed)[names(df_HTML_not_processed) == "X.UBIOPRED.Adult_Cohort_.Jan_2019..Atopy.Exposures.and.Triggers.Baseline.Pollutants."] <- "Expo.Pollutants"
names(df_HTML_not_processed)[names(df_HTML_not_processed) == "X.UBIOPRED.Adult_Cohort_.Jan_2019..Atopy.Exposures.and.Triggers.Baseline.Respiratory.Infections."] <- "Expo.Respiratory.Infections"
names(df_HTML_not_processed)[names(df_HTML_not_processed) == "X.UBIOPRED.Adult_Cohort_.Jan_2019..Atopy.Exposures.and.Triggers.Baseline.Routine.Physical.Activities."] <- "Expo.Routine.Physical.Activities"
names(df_HTML_not_processed)[names(df_HTML_not_processed) == "X.UBIOPRED.Adult_Cohort_.Jan_2019..Atopy.Exposures.and.Triggers.Baseline.Stress."] <- "Expo.Stress"
names(df_HTML_not_processed)[names(df_HTML_not_processed) == "X.UBIOPRED.Adult_Cohort_.Jan_2019..Atopy.Exposures.and.Triggers.Baseline.Wood.Smoke."] <- "Expo.Wood.Smoke"
names(df_HTML_not_processed)[names(df_HTML_not_processed) == "X.UBIOPRED.Adult_Cohort_.Jan_2019..Atopy.Exposures.and.Triggers.Baseline.Other1."] <- "Expo.Baseline.Other1"
names(df_HTML_not_processed)[names(df_HTML_not_processed) == "X.UBIOPRED.Adult_Cohort_.Jan_2019..Atopy.Exposures.and.Triggers.Baseline.Other2."] <- "Expo.Baseline.Other2"
names(df_HTML_not_processed)[names(df_HTML_not_processed) == "X.UBIOPRED.Adult_Cohort_.Jan_2019..Atopy.Exposures.and.Triggers.Baseline.Other3."] <- "Expo.Baseline.Other3"
names(df_HTML_not_processed)[names(df_HTML_not_processed) == "X.UBIOPRED.Adult_Cohort_.Jan_2019..Atopy.Exposures.and.Triggers.Baseline.Other4."] <- "Expo.Baseline.Other4"
####### END EXPOSURES AND TRIGGERS DATA #######

####### BEG MEDICATION DATA #######
names(df_HTML_not_processed)[names(df_HTML_not_processed) == "X.UBIOPRED.Adult_Cohort_.Jan_2019..Medication.Baseline.Anti.IgE.Therapy."] <- "Medication.Anti.IgE.Therapy"
names(df_HTML_not_processed)[names(df_HTML_not_processed) == "X.UBIOPRED.Adult_Cohort_.Jan_2019..Medication.Baseline.Antibiotic.Therapy."] <- "Medication.Antibiotic.Therapy"
names(df_HTML_not_processed)[names(df_HTML_not_processed) == "X.UBIOPRED.Adult_Cohort_.Jan_2019..Medication.Baseline.Cromones."] <- "Medication.Cromones"
names(df_HTML_not_processed)[names(df_HTML_not_processed) == "X.UBIOPRED.Adult_Cohort_.Jan_2019..Medication.Baseline.Immunotherapy."] <- "Medication.Immunotherapy"
names(df_HTML_not_processed)[names(df_HTML_not_processed) == "X.UBIOPRED.Adult_Cohort_.Jan_2019..Medication.Baseline.Inhaled.Combinations."] <- "Medication.Inhaled.Combinations"
names(df_HTML_not_processed)[names(df_HTML_not_processed) == "X.UBIOPRED.Adult_Cohort_.Jan_2019..Medication.Baseline.Inhaled.Corticosteroids."] <- "Medication.Inhaled.Corticosteroids"
names(df_HTML_not_processed)[names(df_HTML_not_processed) == "X.UBIOPRED.Adult_Cohort_.Jan_2019..Medication.Baseline.Injectable.Corticosteroids."] <- "Medication.Injectable.Corticosteroids"
names(df_HTML_not_processed)[names(df_HTML_not_processed) == "X.UBIOPRED.Adult_Cohort_.Jan_2019..Medication.Baseline.Leukotriene.Modifiers."] <- "Medication.Leukotriene.Modifiers"
names(df_HTML_not_processed)[names(df_HTML_not_processed) == "X.UBIOPRED.Adult_Cohort_.Jan_2019..Medication.Baseline.Long.Acting.Anticholinergics."] <- "Medication.Long.Acting.Anticholinergics"
names(df_HTML_not_processed)[names(df_HTML_not_processed) == "X.UBIOPRED.Adult_Cohort_.Jan_2019..Medication.Baseline.Long.Acting.Beta.Agonist."] <- "Medication.Long.Acting.Beta.Agonist"
names(df_HTML_not_processed)[names(df_HTML_not_processed) == "X.UBIOPRED.Adult_Cohort_.Jan_2019..Medication.Baseline.Mucolytics."] <- "Medication.Mucolytics"
names(df_HTML_not_processed)[names(df_HTML_not_processed) == "X.UBIOPRED.Adult_Cohort_.Jan_2019..Medication.Baseline.Oral.Corticosteroids."] <- "Medication.Oral.Corticosteroids"
names(df_HTML_not_processed)[names(df_HTML_not_processed) == "X.UBIOPRED.Adult_Cohort_.Jan_2019..Medication.Baseline.Short.Acting.Anticholinergics."] <- "Medication.Short.Acting.Anticholinergics"
names(df_HTML_not_processed)[names(df_HTML_not_processed) == "X.UBIOPRED.Adult_Cohort_.Jan_2019..Medication.Baseline.Short.Acting.Beta.Agonist."] <- "Medication.Short.Acting.Beta.Agonist"
names(df_HTML_not_processed)[names(df_HTML_not_processed) == "X.UBIOPRED.Adult_Cohort_.Jan_2019..Medication.Baseline.Short.Acting.Combination."] <- "Medication.Short.Acting.Combination"
names(df_HTML_not_processed)[names(df_HTML_not_processed) == "X.UBIOPRED.Adult_Cohort_.Jan_2019..Medication.Baseline.Theophilline."] <- "Medication.Theophilline"
####### END MEDICATION DATA #######

####### BEG PHYSICAL EXAMINATION DATA #######
names(df_HTML_not_processed)[names(df_HTML_not_processed) == "X.UBIOPRED.Adult_Cohort_.Jan_2019..Physical.Examination.Baseline.Day.1.Physical.Examination.Performed."] <- "PE.Performed"
names(df_HTML_not_processed)[names(df_HTML_not_processed) == "X.UBIOPRED.Adult_Cohort_.Jan_2019..Physical.Examination.Screening.Bronchial.Finding."] <- "PE.Screening.Bronchial.Finding"
names(df_HTML_not_processed)[names(df_HTML_not_processed) == "X.UBIOPRED.Adult_Cohort_.Jan_2019..Physical.Examination.Screening.Diminished.Finding."] <- "PE.Screening.Diminished.Finding"
names(df_HTML_not_processed)[names(df_HTML_not_processed) == "X.UBIOPRED.Adult_Cohort_.Jan_2019..Physical.Examination.Screening.Dullness.Finding."] <- "PE.Screening.Dullness.Finding"
names(df_HTML_not_processed)[names(df_HTML_not_processed) == "X.UBIOPRED.Adult_Cohort_.Jan_2019..Physical.Examination.Screening.Physical.Examination.Performed."] <- "PE.Screening.Performed"
names(df_HTML_not_processed)[names(df_HTML_not_processed) == "X.UBIOPRED.Adult_Cohort_.Jan_2019..Physical.Examination.Screening.Rales.Finding."] <- "PE.Screening.Rales.Finding"
names(df_HTML_not_processed)[names(df_HTML_not_processed) == "X.UBIOPRED.Adult_Cohort_.Jan_2019..Physical.Examination.Screening.Squeak.Finding."] <- "PE.Screening.Squeak.Finding"
names(df_HTML_not_processed)[names(df_HTML_not_processed) == "X.UBIOPRED.Adult_Cohort_.Jan_2019..Physical.Examination.Screening.Wheeze.Finding."] <- "PE.Screening.Wheeze.Finding"
####### END PHYSICAL EXAMINATION DATA #######

####### BEG QUESTIONS DATA #######
names(df_HTML_not_processed)[names(df_HTML_not_processed) == "X.UBIOPRED.Adult_Cohort_.Jan_2019..Questions.Baseline.Day.1.Ec.Respiratory.Infection."] <- "Questions.Ec.Respiratory.Infection"
names(df_HTML_not_processed)[names(df_HTML_not_processed) == "X.UBIOPRED.Adult_Cohort_.Jan_2019..Questions.Baseline.Day.1.Informed.Consent.Genetic."] <- "Questions.Informed.Consent.Genetic"
names(df_HTML_not_processed)[names(df_HTML_not_processed) == "X.UBIOPRED.Adult_Cohort_.Jan_2019..Questions.Baseline.Day.1.Mars.Completed."] <- "Questions.Mars.Completed"
names(df_HTML_not_processed)[names(df_HTML_not_processed) == "X.UBIOPRED.Adult_Cohort_.Jan_2019..Questions.Baseline.Day.1.Medication.Since.Screening."] <- "Questions.Medication.Since.Screening"
names(df_HTML_not_processed)[names(df_HTML_not_processed) == "X.UBIOPRED.Adult_Cohort_.Jan_2019..Questions.Baseline.Day.1.Serious.Adverse.Event.Since.Screening."] <- "Questions.Serious.Adverse.Event.Since.Screening"
names(df_HTML_not_processed)[names(df_HTML_not_processed) == "X.UBIOPRED.Adult_Cohort_.Jan_2019..Questions.Screening.Environmental.Factors.Enviromental.Bird."] <- "Questions.Screening.Env.Factors.Bird"
names(df_HTML_not_processed)[names(df_HTML_not_processed) == "X.UBIOPRED.Adult_Cohort_.Jan_2019..Questions.Screening.Environmental.Factors.Enviromental.Cat."] <- "Questions.Screening.Env.Factors.Cat"
names(df_HTML_not_processed)[names(df_HTML_not_processed) == "X.UBIOPRED.Adult_Cohort_.Jan_2019..Questions.Screening.Environmental.Factors.Enviromental.Dog."] <- "Questions.Screening.Env.Factors.Dog"
names(df_HTML_not_processed)[names(df_HTML_not_processed) == "X.UBIOPRED.Adult_Cohort_.Jan_2019..Questions.Screening.Environmental.Factors.Enviromental.Rodent."] <- "Questions.Screening.Env.Factors.Rodent"
names(df_HTML_not_processed)[names(df_HTML_not_processed) == "X.UBIOPRED.Adult_Cohort_.Jan_2019..Questions.Screening.Family.History.Asthma.Father."] <- "Questions.Screening.Asthma.Father"
names(df_HTML_not_processed)[names(df_HTML_not_processed) == "X.UBIOPRED.Adult_Cohort_.Jan_2019..Questions.Screening.Family.History.Asthma.Mother."] <- "Questions.Screening.Asthma.Mother"
names(df_HTML_not_processed)[names(df_HTML_not_processed) == "X.UBIOPRED.Adult_Cohort_.Jan_2019..Questions.Screening.Family.History.Asthma.Sibling."] <- "Questions.Screening.Asthma.Sibling"
names(df_HTML_not_processed)[names(df_HTML_not_processed) == "X.UBIOPRED.Adult_Cohort_.Jan_2019..Questions.Screening.Family.History.Cardiovascular.Father."] <- "Questions.Screening.Cardiovascular.Father"
names(df_HTML_not_processed)[names(df_HTML_not_processed) == "X.UBIOPRED.Adult_Cohort_.Jan_2019..Questions.Screening.Family.History.Cardiovascular.Mother."] <- "Questions.Screening.Cardiovascular.Mother"
names(df_HTML_not_processed)[names(df_HTML_not_processed) == "X.UBIOPRED.Adult_Cohort_.Jan_2019..Questions.Screening.Family.History.Cardiovascular.Sibling."] <- "Questions.Screening.Cardiovascular.Sibling"
names(df_HTML_not_processed)[names(df_HTML_not_processed) == "X.UBIOPRED.Adult_Cohort_.Jan_2019..Questions.Screening.Family.History.Copd.Father."] <- "Questions.Screening.Copd.Father"
names(df_HTML_not_processed)[names(df_HTML_not_processed) == "X.UBIOPRED.Adult_Cohort_.Jan_2019..Questions.Screening.Family.History.Copd.Mother."] <- "Questions.Screening.Copd.Mother"
names(df_HTML_not_processed)[names(df_HTML_not_processed) == "X.UBIOPRED.Adult_Cohort_.Jan_2019..Questions.Screening.Family.History.Copd.Sibling."] <- "Questions.Screening.Copd.Sibling"
names(df_HTML_not_processed)[names(df_HTML_not_processed) == "X.UBIOPRED.Adult_Cohort_.Jan_2019..Questions.Screening.Family.History.Diabetes.Sibling."] <- "Questions.Screening.Diabetes.Sibling"
names(df_HTML_not_processed)[names(df_HTML_not_processed) == "X.UBIOPRED.Adult_Cohort_.Jan_2019..Questions.Screening.Family.History.Eczema.Mother"] <- "Questions.Screening.Eczema.Mother"
names(df_HTML_not_processed)[names(df_HTML_not_processed) == "X.UBIOPRED.Adult_Cohort_.Jan_2019..Questions.Screening.Family.History.Eczema.Sibling."] <- "Questions.Screening.Eczema.Sibling"
names(df_HTML_not_processed)[names(df_HTML_not_processed) == "X.UBIOPRED.Adult_Cohort_.Jan_2019..Questions.Screening.Family.History.Hay.Fever.Father."] <- "Questions.Screening.Hay.Fever.Father"
names(df_HTML_not_processed)[names(df_HTML_not_processed) == "X.UBIOPRED.Adult_Cohort_.Jan_2019..Questions.Screening.Family.History.Hay.Fever.Mother."] <- "Questions.Screening.Hay.Fever.Mother"
names(df_HTML_not_processed)[names(df_HTML_not_processed) == "X.UBIOPRED.Adult_Cohort_.Jan_2019..Questions.Screening.Family.History.Hay.Fever.Sibling."] <- "Questions.Screening.Hay.Fever.Sibling"
names(df_HTML_not_processed)[names(df_HTML_not_processed) == "X.UBIOPRED.Adult_Cohort_.Jan_2019..Questions.Screening.Family.History.Inflammatory.Disease.Father."] <- "Questions.Screening.Inflammatory.Disease.Father"
names(df_HTML_not_processed)[names(df_HTML_not_processed) == "X.UBIOPRED.Adult_Cohort_.Jan_2019..Questions.Screening.Family.History.Inflammatory.Disease.Mother."] <- "Questions.Screening.Inflammatory.Disease.Mother"
names(df_HTML_not_processed)[names(df_HTML_not_processed) == "X.UBIOPRED.Adult_Cohort_.Jan_2019..Questions.Screening.Family.History.Inflammatory.Disease.Sibling."] <- "Questions.Screening.Inflammatory.Disease.Sibling"
names(df_HTML_not_processed)[names(df_HTML_not_processed) == "X.UBIOPRED.Adult_Cohort_.Jan_2019..Questions.Screening.Food.Allergies.Food.Allergy."] <- "Questions.Screening.Food.Allergy"
names(df_HTML_not_processed)[names(df_HTML_not_processed) == "X.UBIOPRED.Adult_Cohort_.Jan_2019..Questions.Screening.Informed.Consent.Informed.Consent.Genetic."] <- "Questions.Screening.Informed.Consent.Genetic"
names(df_HTML_not_processed)[names(df_HTML_not_processed) == "X.UBIOPRED.Adult_Cohort_.Jan_2019..Questions.Screening.Residential.Location."] <- "Questions.Screening.Residential.Location"
names(df_HTML_not_processed)[names(df_HTML_not_processed) == "X.UBIOPRED.Adult_Cohort_.Jan_2019..Questions.Screening.Serious.Adverse.Event.Since.Consent."] <- "Questions.Screening.Serious.Adverse.Event.Since.Consent"
names(df_HTML_not_processed)[names(df_HTML_not_processed) == "X.UBIOPRED.Adult_Cohort_.Jan_2019..Questions.Screening.Smoking.History.Second.Hand.Smoke."] <- "Questions.Screening.Second.Hand.Smoke"
names(df_HTML_not_processed)[names(df_HTML_not_processed) == "X.UBIOPRED.Adult_Cohort_.Jan_2019..Questions.Screening.Smoking.History.Smokeless.Tobacco.Status."] <- "Questions.Screening.Smokeless.Tobacco.Status"
names(df_HTML_not_processed)[names(df_HTML_not_processed) == "X.UBIOPRED.Adult_Cohort_.Jan_2019..Questions.Baseline.Day.1.Pregnancy.Test.Na."] <- "Questions.Pregnancy.Test.Na"
names(df_HTML_not_processed)[names(df_HTML_not_processed) == "X.UBIOPRED.Adult_Cohort_.Jan_2019..Questions.Baseline.Day.1.Urinary.Cotinine.Not.Done."] <- "Questions.Urinary.Cotinine.Not.Done"
names(df_HTML_not_processed)[names(df_HTML_not_processed) == "X.UBIOPRED.Adult_Cohort_.Jan_2019..Questions.Screening.Environmental.Factors.Enviromental.Other.1."] <- "Questions.Screening.Env.Factors.Other.1"
names(df_HTML_not_processed)[names(df_HTML_not_processed) == "X.UBIOPRED.Adult_Cohort_.Jan_2019..Questions.Screening.Environmental.Factors.Enviromental.Other.2."] <- "Questions.Screening.Env.Factors.Other.2"
names(df_HTML_not_processed)[names(df_HTML_not_processed) == "X.UBIOPRED.Adult_Cohort_.Jan_2019..Questions.Screening.Environmental.Factors.Enviromental.Other.3."] <- "Questions.Screening.Env.Factors.Other.3"
names(df_HTML_not_processed)[names(df_HTML_not_processed) == "X.UBIOPRED.Adult_Cohort_.Jan_2019..Questions.Screening.Family.History.Number.Of.Children."] <- "Questions.Screening.Number.Of.Children"
names(df_HTML_not_processed)[names(df_HTML_not_processed) == "X.UBIOPRED.Adult_Cohort_.Jan_2019..Questions.Screening.Family.History.Number.Of.Siblings."] <- "Questions.Screening.Number.Of.Siblings"
names(df_HTML_not_processed)[names(df_HTML_not_processed) == "X.UBIOPRED.Adult_Cohort_.Jan_2019..Questions.Screening.Smoking.History.Smoking.Cigarettes."] <- "Questions.Screening.Smoking.Cigarettes"
names(df_HTML_not_processed)[names(df_HTML_not_processed) == "X.UBIOPRED.Adult_Cohort_.Jan_2019..Questions.Screening.Smoking.History.Smoking.Cigars."] <- "Questions.Screening.Smoking.Cigars"
names(df_HTML_not_processed)[names(df_HTML_not_processed) == "X.UBIOPRED.Adult_Cohort_.Jan_2019..Questions.Screening.Smoking.History.Smoking.Pipe."] <- "Questions.Screening.Smoking.Pipe"
####### END QUESTIONS DATA #######

####### BEG EXACERBATIONS DATA #######
names(df_HTML_not_processed)[names(df_HTML_not_processed) == "X.UBIOPRED.Adult_Cohort_.Jan_2019..Exacerbations.Recent.Asthma.Exacerbation.History.Baseline.Exacerbation.Per.Year."] <- "Exacerbation.Per.Year."
####### END EXACERBATIONS DATA ####### 

###### Rename X to patient in df_HTML_not_processed ######
names(df_HTML_not_processed)[names(df_HTML_not_processed) == "X"] <- "Patient"

####### NOW MERGE df_HTML_not_processed and df2 ########
df_HTML_not_processed_final = merge(df_HTML_not_processed,df2, by="Patient")

####### Write a csv file now that your data is ready to be analyzed
setwd("/rds/general/user/md2620/home/asthma/Malo/Dataframes")
write.csv(df_HTML_not_processed_final,"df_HTML_not_processed_HTML_tables.csv")

############ END RENAMING OF VARIABLES ###############

#Get list of variables that we actually use for clustering (also processed)
data.frame(colnames(df_HTML_processed))
data.frame(colnames(df_HTML_not_processed_final))
#Check variable type in df
data_type <- as.data.frame(sapply(df_HTML_not_processed_final, class))
############SUMMARY TABLES###############

#Rename levels for clusters in df
levels(df_HTML_not_processed_final$cluster)
library(dplyr)
levels(df_HTML_not_processed_final$cluster) <- c("1", "2", "3")

#Remove observations that belong to column cohort_v:
df_HTML_not_processed_final <- subset(df_HTML_not_processed_final, cohort!="cohort_v")


#Define your controls (tests used and measures we look at)
mycontrols  <- tableby.control(test=FALSE, total=FALSE,
                               numeric.test="anova", cat.test="chisq",
                               numeric.stats=c("Nmiss","mean","median", "q1q3"),
                               cat.stats=c("countpct"),
                               digits = 0L, digits.count = 0L, 
                               digits.pct = 0L, digits.p = 0L,
                               stats.labels=list(Nmiss="missing", meansd='Mean', median='Median', q1q3='Q1,Q3'))


#Summary table for biomarkers per cluster - only numerical variables
tab_biomarkers <- summary(tableby(cluster ~ Biomarker.C5a.pg.ml.serum + Biomarker.CD30.pg.ml.serum
                                       + Biomarker.CD40L.pg.ml.serum + Biomarker.DPPIV.pg.ml.serum
                                       + Biomarker.Galectin.3.pg.ml.serum + Biomarker.IL.18.pg.ml.serum
                                       + Biomarker.IL.1alpha.pg.ml.serum + Biomarker.IL.6Ralpha.pg.ml.serum
                                       + Biomarker.LBP.pg.ml.serum + Biomarker.Lumican.pg.ml.serum
                                       + Biomarker.MCP.4.pg.ml.serum + Biomarker.MMP.3.pg.ml.serum
                                       + Biomarker.RAGE.pg.ml.serum + Biomarker.SHBG.pg.ml.serum
                                       + Biomarker.Serpin.E1.pg.ml.serum + Biomarker.alpha1.microglobulin.pg.ml.serum
                                       + Biomarker.CCL17.pg.ml.MSD.BL.plasma + Biomarker.CCL22.pg.ml.MSD.BL.plasma
                                       + Biomarker.EOTAXIN.pg.ml.MSD.BL.plasma + Biomarker.EOTAXIN3.pg.ml.MSD.BL.plasma
                                       + Biomarker.Baseline.BI.Cytokines.Chemokines.IFNg.pg.ml.MSD.BL.plasma +
                                         Biomarker.Baseline.BI.Cytokines.Chemokines.IL6.pg.ml.MSD.BL.plasma + 
                                         Biomarker.IL8.pg.ml.MSD.BL.plasma + Biomarker.IP10.pg.ml.MSD.BL.plasma
                                       + Biomarker.MCP1.pg.ml.MSD.BL.plasma + Biomarker.MIP1b.pg.ml.MSD.BL.plasma
                                       + Biomarker.TNFa.pg.ml.MSD.BL.plasma + Biomarker.Genentech.CCL18.pg.ml.IMPACT.BL.serum
                                       + Biomarker.Genentech.IL13.pg.ml.IMPACT.BL.serum
                                       + Biomarker.Genentech.Periostin.ng.ml.ELECSYS.BL.serum
                                       + Biomarker.Karolinska.hsCRP.hCRP.mg.L,data=df_HTML_not_processed_final, control = mycontrols),title="Individual biomarker's Mean, Median and 25th-75th Quantiles for each cluster ")


#Write tab_biomarkers to an HTML document
write2html(tab_biomarkers, "/rds/general/user/md2620/home/asthma/Malo/Results/biomarkers_characteristics_by_cluster_test.html")
write2pdf(tab_biomarkers,"/rds/general/user/md2620/home/asthma/Malo/Results/test_oct_24.pdf")
write2word(tab_biomarkers, "/rds/general/user/md2620/home/asthma/Malo/Results/test_oct_24.doc")

?write2pdf
#Rename levels for demographic.occupational in df
levels(df_HTML_not_processed_final$Demographic.occupational.employed)
library(dplyr)
levels(df_HTML_not_processed_final$Demographic.occupational.employed) <- c("No", "Yes")
levels(df_HTML_not_processed_final$Demographic.occupational.keeping.house) <- c("No", "Yes")
levels(df_HTML_not_processed_final$Demographic.occupational.other) <- c("No", "Yes")
levels(df_HTML_not_processed_final$Demographic.occupational.retired) <- c("No", "Yes")
levels(df_HTML_not_processed_final$Demographic.occupational.student) <- c("No", "Yes")
levels(df_HTML_not_processed_final$Demographic.occupational.unemployed) <- c("No", "Yes")
levels(df_HTML_not_processed_final$Demographic.occupational.volunteer) <- c("No", "Yes")
#Summary table for demographics 
tab_demographics <- summary(sort(tableby(cluster ~ Demographic.Ethnic.origin.Mother
                                         + Demographic.Highest.Level.Education
                                         + Demographic.Marital.Status
                                         + Demographic.Sex
                                         + Demographic.occupational.employed
                                         + Demographic.occupational.keeping.house
                                         + Demographic.occupational.other
                                         + Demographic.occupational.retired
                                         + Demographic.occupational.student
                                         + Demographic.occupational.unemployed
                                         + Demographic.occupational.volunteer
                                         + Body.Mass.Index.kg.m2
                                         + Height.cm
                                         + Demographic.Age, data=df_HTML_not_processed_final, control = mycontrols , simulate.p.value=TRUE, B=500, digits = 1),decreasing=FALSE),title= "Comparison of demographics across clusters", pfootnote=TRUE)

#Write tab_demgraphics to an HTML document
write2html(tab_demographics, "/rds/general/user/md2620/home/asthma/Malo/Results/demographics_characteristics_by_cluster_test.html")

#Summary table for exposures
tab_exposures <- summary(sort(tableby(cluster ~ Expo.Aspirin
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
                                         +Expo.Wood.Smoke, data=df_HTML_not_processed_final, control = mycontrols , simulate.p.value=TRUE, B=500, digits = 1),decreasing=FALSE),title= "Comparison of exposures across clusters", pfootnote=TRUE)

#Write tab_exposures to an HTML document
write2html(tab_exposures, "/rds/general/user/md2620/home/asthma/Malo/Results/exposures_characteristics_by_cluster_test.html")


#Summary table for medication
tab_medication <- summary(sort(tableby(cluster ~ Medication.Anti.IgE.Therapy
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
write2html(tab_medication, "/rds/general/user/md2620/home/asthma/Malo/Results/medication_characteristics_by_cluster_test.html")


#Summary table for Physical Activity
tab_PE <- summary(sort(tableby(cluster ~ PE.Performed
                               + PE.Screening.Bronchial.Finding
                               + PE.Screening.Diminished.Finding
                               + PE.Screening.Dullness.Finding
                               + PE.Screening.Performed
                               + PE.Screening.Rales.Finding
                               + PE.Screening.Squeak.Finding
                               + PE.Screening.Wheeze.Finding, data=df_HTML_not_processed_final, control = mycontrols , simulate.p.value=TRUE, B=500, digits = 1),decreasing=FALSE),title= "Medication comparison across clusters", pfootnote=TRUE)

#Write tab_medication to an HTML document
write2html(tab_PE, "/rds/general/user/md2620/home/asthma/Malo/Results/PE_characteristics_by_cluster_test.html")

#Summary table for eosinophils and neutrophils
tab_neut_eosi <- summary(sort(tableby(cluster ~ Sputum.._Eosinophils
                               + Blood.Neutrophils_.
                               + Sputum._Neutrophils
                               + Blood.Eosinophils_., data=df_HTML_not_processed_final, control = mycontrols , simulate.p.value=TRUE, B=500, digits = 1),decreasing=FALSE),title= "Sputum and blood eosinophils and neutrophils comparison across clusters", pfootnote=TRUE)

#Write tab_medication to an HTML document
write2html(tab_neut_eosi, "/rds/general/user/md2620/home/asthma/Malo/Results/NeutEosi_characteristics_by_cluster_test.html")

#Summary table for cluster 1 20 most important variables - Comparison with other clusters
tab_cluster1 <- summary(sort(tableby(cluster ~ Omics.Nitrotyrosine + Omics.Methionine + Omics.Kynurenic.acid
                                     + Omics.N.Acetylcarnosine
                                     + Omics.Pyroglutamylglycine
                                     + Omics.N.Methylhistamine
                                     + Omics.Glutamine
                                     + Omics.Proline
                                     + Omics.Serine
                                     + Omics.Methylguanine
                                     + Omics.Histidine
                                     + Omics.Tyrosine
                                     + Omics.Biopterin
                                     + Omics.Hypoxanthine
                                     + Omics.Aminolevulinic.acid
                                     + Omics.Hydroxybutyric.acid
                                     + Omics.Alanine
                                     + Biomarker.SHBG.pg.ml.serum
                                     + Clinical.Screening.Lymphocytes
                                     + Clinical.FEF.25.75.Change.L.sec, data=df_HTML_not_processed_final, control = mycontrols , simulate.p.value=TRUE, B=500, digits = 1),decreasing=FALSE),title= "Values for cluster 1 most important variables relative to other clusters", pfootnote=TRUE)
                                     
#Write tab_biomarkers to an HTML document
write2html(tab_cluster1, "/rds/general/user/md2620/home/asthma/Malo/Results/cluster1_comparison_to_others.html")

###### SUMMARY TABLE TO LOOK AT ASSOCIATION BETWEEN OUTCOME (SEVERITY STATUS) AND CLUSTERS
table(df_HTML_not_processed_final$cohort)

#Create dummy variables for outcome in order to run logistic regressions (clusters against binary outcome)
df_HTML_not_processed_final$Healthy <- ifelse(df_HTML_not_processed_final$cohort == 'Healthy', "Yes", "No")
df_HTML_not_processed_final$MildModerate <- ifelse(df_HTML_not_processed_final$cohort == 'Mild/Moderate', "Yes", "No")
df_HTML_not_processed_final$Severe <- ifelse(df_HTML_not_processed_final$cohort == 'Severe', "Yes", "No")
df_HTML_not_processed_final$Severe_Smoker <- ifelse(df_HTML_not_processed_final$cohort == 'Severe_Smoker', "Yes", "No")
df_HTML_not_processed_final$Severe_asthma <- ifelse(grepl('Severe', df_HTML_not_processed_final$cohort),"Yes", "No")

#Not necessary
tab_cluster_outcome <- summary(sort(tableby(cohort ~ cluster,data=df_HTML_not_processed_final, control = mycontrols , simulate.p.value=TRUE, B=500, digits = 1),pfootnote=TRUE),decreasing=TRUE)

#install.packages("finalfit")
#install.packages("rstan") not working
library(finalfit)
library(dplyr)
library(rstan)
explanatory = "cluster"
dependent = 'Healthy'
tab_cluster_outcome <- df_HTML_not_processed_final %>%
  summary_factorlist(dependent, explanatory,p=TRUE, add_dependent_label=TRUE)

#Write to a csv
write.csv(tab_cluster_outcome,"/rds/general/user/md2620/home/asthma/Malo/Results/tab_cluster_outcome.csv")

#Write to an HTML document
write2html(tab_cluster_outcome, "/rds/general/user/md2620/home/asthma/Malo/Results/Healthy_clusters_assoc.html")

dependent_severe = 'Severe'
tab_cluster_severe <- df_HTML_not_processed_final %>%
  summary_factorlist(dependent_severe, explanatory,p=TRUE, add_dependent_label=TRUE)

#Write to a csv
write.csv(tab_cluster_severe,"/rds/general/user/md2620/home/asthma/Malo/Results/tab_cluster_severe.csv")

#Write to an HTML document
write2html(tab_cluster_severe, "/rds/general/user/md2620/home/asthma/Malo/Results/Severe_clusters_assoc.html")

dependent_mild = 'MildModerate'
tab_cluster_mild <- df_HTML_not_processed_final %>%
  summary_factorlist(dependent_mild, explanatory,p=TRUE, add_dependent_label=TRUE)

#Write to a csv
write.csv(tab_cluster_mild,"/rds/general/user/md2620/home/asthma/Malo/Results/tab_cluster_mild.csv")

#Write to an HTML document
write2html(tab_cluster_mild, "/rds/general/user/md2620/home/asthma/Malo/Results/mild_clusters_assoc.html")

dependent_severe_smok = 'Severe_Smoker'
tab_cluster_severe_smok <- df_HTML_not_processed_final %>%
  summary_factorlist(dependent_severe_smok, explanatory,p=TRUE, add_dependent_label=TRUE)

#Write to a csv
write.csv(tab_cluster_severe_smok,"/rds/general/user/md2620/home/asthma/Malo/Results/tab_cluster_severe_smok.csv")

#Write to an HTML document
write2html(tab_cluster_severe_smok, "/rds/general/user/md2620/home/asthma/Malo/Results/severe_smok_clusters_assoc.html")

dependent_severe_asthma = 'Severe_asthma'
tab_cluster_severe_asthma <- df_HTML_not_processed_final %>%
  summary_factorlist(dependent_severe_asthma, explanatory,p=TRUE, add_dependent_label=TRUE)

#Write to a csv
write.csv(tab_cluster_severe_asthma,"/rds/general/user/md2620/home/asthma/Malo/Results/tab_cluster_severe_asthma.csv")

#Write to an HTML document
write2html(tab_cluster_severe_asthma, "/rds/general/user/md2620/home/asthma/Malo/Results/severe_asthma_clusters_assoc.html")


#Merge 2 datasets
tab_biomarkers <- summary(merge(tableby(cluster ~ Biomarker.C5a.pg.ml.serum + Biomarker.CD30.pg.ml.serum
                                          ,data=df_HTML_not_processed_final, numeric.simplify=TRUE, numeric.stats=c("Nmiss", "meansd")),
                                  tableby(cluster ~ includeNA(Demographic.Highest.Level.Education) + 
                                            includeNA(Demographic.Ethnic.origin.Mother)+
                                            includeNA(Demographic.Marital.Status)+
                                            includeNA(Demographic.Sex)+
                                            includeNA(Demographic.occupational.employed)+
                                            includeNA(Demographic.occupational.keeping.house)+
                                            includeNA(Demographic.occupational.other)+
                                            includeNA(Demographic.occupational.retired)+
                                            includeNA(Demographic.occupational.student)+
                                            includeNA(Demographic.occupational.unemployed)+
                                            includeNA(Demographic.occupational.volunteer), data = df_HTML_not_processed_final, cat.stats = "countpct"),all=TRUE))

tab_biomarkers
#Add title to demographics table
summary(tab_biomarkers, title='Biomarkers for each cluster')

#Wite to an HTML document
write2html(tab_biomarkers, "/rds/general/user/md2620/home/asthma/Malo/Results/biomarkers_characteristics_by_cluster_test.html")


#Define your controls (tests used and measures we look at)
mycontrols  <- tableby.control(test=TRUE, total=TRUE,
                               numeric.test="anova", cat.test="chisq",
                               numeric.stats=c("Nmiss","meansd","median", "q1q3"),
                               cat.stats=c("countpct"),
                               stats.labels=list(Nmiss="missing", meansd='Mean(sd)', median='Median', q1q3='Q1,Q3'))



#Summary table for biomarkers - only numerical variables
tab_biomarkers <- summary(sort(tableby(cluster ~ Biomarker.C5a.pg.ml.serum + Biomarker.CD30.pg.ml.serum
                                        + Biomarker.CD40L.pg.ml.serum + Biomarker.DPPIV.pg.ml.serum
                                        + Biomarker.Galectin.3.pg.ml.serum + Biomarker.IL.18.pg.ml.serum
                                        + Biomarker.IL.1alpha.pg.ml.serum + Biomarker.IL.6Ralpha.pg.ml.serum
                                        + Biomarker.LBP.pg.ml.serum + Biomarker.Lumican.pg.ml.serum
                                        + Biomarker.MCP.4.pg.ml.serum + Biomarker.MMP.3.pg.ml.serum
                                        + Biomarker.RAGE.pg.ml.serum + Biomarker.SHBG.pg.ml.serum
                                        + Biomarker.Serpin.E1.pg.ml.serum + Biomarker.alpha1.microglobulin.pg.ml.serum
                                        + Biomarker.CCL17.pg.ml.MSD.BL.plasma + Biomarker.CCL22.pg.ml.MSD.BL.plasma
                                        + Biomarker.EOTAXIN.pg.ml.MSD.BL.plasma + Biomarker.EOTAXIN3.pg.ml.MSD.BL.plasma
                                        + Biomarker.Baseline.BI.Cytokines.Chemokines.IFNg.pg.ml.MSD.BL.plasma +
                                          Biomarker.Baseline.BI.Cytokines.Chemokines.IL6.pg.ml.MSD.BL.plasma + 
                                          Biomarker.IL8.pg.ml.MSD.BL.plasma + Biomarker.IP10.pg.ml.MSD.BL.plasma
                                        + Biomarker.MCP1.pg.ml.MSD.BL.plasma + Biomarker.MIP1b.pg.ml.MSD.BL.plasma
                                        + Biomarker.TNFa.pg.ml.MSD.BL.plasma + Biomarker.Genentech.CCL18.pg.ml.IMPACT.BL.serum
                                        + Biomarker.Genentech.IL13.pg.ml.IMPACT.BL.serum
                                        + Biomarker.Genentech.Periostin.ng.ml.ELECSYS.BL.serum
                                        + Biomarker.Karolinska.hsCRP.hCRP.mg.L,data=df_HTML_not_processed_final, control = mycontrols , simulate.p.value=TRUE, B=500, digits = 1),decreasing=FALSE),title="Individual biomarker's Mean, Median and 25th-75th Quantiles for each cluster ", pfootnote=TRUE)
                          

#Wite to an HTML document
write2html(tab_biomarkers, "/rds/general/user/md2620/home/asthma/Malo/Results/biomarkers_characteristics_by_cluster_test.html")


###### SUMMARY TABLE TO LOOK AT ASSOCIATION BETWEEN OUTCOME (SEVERITY STATUS) AND CLUSTERS
tab_cluster_outcome <- summary(sort(tableby(cohort ~ cluster,data=df_HTML_not_processed_final, control = mycontrols , simulate.p.value=TRUE, B=500, digits = 1),pfootnote=TRUE),decreasing=TRUE)

#Wite to an HTML document
write2html(tab_cluster_outcome, "/rds/general/user/md2620/home/asthma/Malo/Results/outcome_characteristics_by_cluster_test.html")


####################### DRAFT
#create summary table
tab <- summary(tableby(cluster ~ X.UBIOPRED.Adult_Cohort_.Jan_2019..Subject.Body.Measurements.Height..cm.., data = df_HTML_not_processed_final))
tab
#Turns list into dataframe
as.data.frame(tab)

df_HTML_not_processed$X.UBIOPRED.Adult_Cohort_.Jan_2019..Subject.Body.Measurements.Height..cm..
# will select the names starting with the Demographic Data ignoring whether variable name is in capital letters or not
demo_var <- tidyselect::vars_select(names(df_HTML_not_processed), starts_with('X.UBIOPRED.Adult_Cohort_.Jan_2019..Demographic.Data.', ignore.case = TRUE))
demo_var

bio_var <- tidyselect::vars_select(names(df_HTML_not_processed), starts_with('X.UBIOPRED.Adult_Cohort_.Jan_2019..Biomarker.Data.', ignore.case = TRUE))
bio_var

#Create summary table for numeric variables
#Takes one row if there no missing value - 2 otherwise
tab_demographics_num <- summary(tableby(cluster ~ X.UBIOPRED.Adult_Cohort_.Jan_2019..Demographic.Data.Age.
                                        , data=df_HTML_not_processed, numeric.simplify=TRUE, numeric.stats=c("Nmiss", "meansd")))


tab_demographics_cat <- summary(tableby(cluster ~ includeNA(X.UBIOPRED.Adult_Cohort_.Jan_2019..Demographic.Data.Ethnic.origin.Father.) + 
                                          includeNA(X.UBIOPRED.Adult_Cohort_.Jan_2019..Demographic.Data.Ethnic.origin.Mother.)+
                                          includeNA(X.UBIOPRED.Adult_Cohort_.Jan_2019..Demographic.Data.Highest.Level.Education.)+
                                          includeNA(X.UBIOPRED.Adult_Cohort_.Jan_2019..Demographic.Data.Marital.Status.)+
                                          includeNA(X.UBIOPRED.Adult_Cohort_.Jan_2019..Demographic.Data.Race.)+
                                          includeNA(X.UBIOPRED.Adult_Cohort_.Jan_2019..Demographic.Data.Sex.), data = df_HTML_not_processed, cat.stats = "countpct"))

#If you want to sort variables by p-value
summary(sort(tab_demographics_cat, decreasing = TRUE))

#Merge 2 datasets
tab_demographics <- summary(merge(tableby(cluster ~ Biomarker.C5a.pg.ml.serum,
                                          , data=df_HTML_not_processed_final, numeric.simplify=TRUE, numeric.stats=c("Nmiss", "meansd")),
                                  tableby(cluster ~ includeNA(Demographic.Highest.Level.Education) + 
                                            includeNA(Demographic.Ethnic.origin.Mother)+
                                            includeNA(Demographic.Marital.Status)+
                                            includeNA(Demographic.Sex)+
                                            includeNA(Demographic.occupational.employed)+
                                            includeNA(Demographic.occupational.keeping.house)+
                                            includeNA(Demographic.occupational.other)+
                                            includeNA(Demographic.occupational.retired)+
                                            includeNA(Demographic.occupational.student)+
                                            includeNA(Demographic.occupational.unemployed)+
                                            includeNA(Demographic.occupational.volunteer), data = df_HTML_not_processed_final, cat.stats = "countpct"),all=TRUE))

tab_demographics
#Add title to demographics table
summary(tab_demographics, title='Demographics')

#Wite to an HTML document
write2html(tab_demographics, "/rds/general/user/md2620/home/asthma/Malo/demographics_characteristics_by_cluster_test.html")

#Create summary stats table
tab_biomarkers <- summary(tableby(cluster ~ X.UBIOPRED.Adult_Cohort_.Jan_2019..Biomarker.Data.Baseline.Visit.Luminex.Serum.C5a.pg.ml.Luminex.serum. + 
                                    X.UBIOPRED.Adult_Cohort_.Jan_2019..Biomarker.Data.Baseline.Visit.Luminex.Serum.CD30.pg.ml.Luminex.serum.+
                                    X.UBIOPRED.Adult_Cohort_.Jan_2019..Biomarker.Data.Baseline.Visit.Luminex.Serum.CD40L.pg.ml.Luminex.serum.+
                                    X.UBIOPRED.Adult_Cohort_.Jan_2019..Biomarker.Data.Baseline.Visit.Luminex.Serum.DPPIV.pg.ml.Luminex.serum., data=df_HTML_not_processed, numeric.stats=c("Nmiss", "meansd"),
                                  digits=2))