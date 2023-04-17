#Reading the files
df_viz = read.csv("/rds/general/user/md2620/home/asthma/Malo/Dataframes/"df_to_be_clustered_08+_with_ID_plus_stand.csv")

#Get list of variables
data.frame(colnames(df))

#Get variable type
lapply(df,class)

#Drop useless variables
df = dplyr::select(df, -c(X.UBIOPRED.Adult_Cohort_.Jan_2019..Sample.Identifiers.Baseline.visit.kitID.,X.1))

####### RENAME VARIABLES FOR BETTER VIZ OF SUMMARY TABLES ###########
####### BEG BIOMARKER DATA #######
names(df)[names(df) == "X.UBIOPRED.Adult_Cohort_.Jan_2019..Biomarker.Data.Baseline.Visit.Luminex.Serum.C5a.pg.ml.Luminex.serum."] <- "Biomarker.C5a.pg.ml.serum"
names(df)[names(df) == "X.UBIOPRED.Adult_Cohort_.Jan_2019..Biomarker.Data.Baseline.Visit.Luminex.Serum.CD30.pg.ml.Luminex.serum."] <- "Biomarker.CD30.pg.ml.serum"
names(df)[names(df) == "X.UBIOPRED.Adult_Cohort_.Jan_2019..Biomarker.Data.Baseline.Visit.Luminex.Serum.CD40L.pg.ml.Luminex.serum."] <- "Biomarker.CD40L.pg.ml.serum"
names(df)[names(df) == "X.UBIOPRED.Adult_Cohort_.Jan_2019..Biomarker.Data.Baseline.Visit.Luminex.Serum.DPPIV.pg.ml.Luminex.serum."] <- "Biomarker.DPPIV.pg.ml.serum"
names(df)[names(df) == "X.UBIOPRED.Adult_Cohort_.Jan_2019..Biomarker.Data.Baseline.Visit.Luminex.Serum.Galectin.3.pg.ml.Luminex.serum."] <- "Biomarker.Galectin.3.pg.ml.serum"
names(df)[names(df) == "X.UBIOPRED.Adult_Cohort_.Jan_2019..Biomarker.Data.Baseline.Visit.Luminex.Serum.IL.18.pg.ml.Luminex.serum."] <- "Biomarker.IL.18.pg.ml.serum"
names(df)[names(df) == "X.UBIOPRED.Adult_Cohort_.Jan_2019..Biomarker.Data.Baseline.Visit.Luminex.Serum.IL.1alpha.pg.ml.Luminex.serum."] <- "Biomarker.IL.1alpha.pg.ml.serum"
names(df)[names(df) == "X.UBIOPRED.Adult_Cohort_.Jan_2019..Biomarker.Data.Baseline.Visit.Luminex.Serum.IL.6Ralpha.pg.ml.Luminex.serum."] <- "Biomarker.IL.6Ralpha.pg.ml.serum"
names(df)[names(df) == "X.UBIOPRED.Adult_Cohort_.Jan_2019..Biomarker.Data.Baseline.Visit.Luminex.Serum.LBP.pg.ml.Luminex.serum."] <- "Biomarker.LBP.pg.ml.serum"
names(df)[names(df) == "X.UBIOPRED.Adult_Cohort_.Jan_2019..Biomarker.Data.Baseline.Visit.Luminex.Serum.Lumican.pg.ml.Luminex.serum."] <- "Biomarker.Lumican.pg.ml.serum"
names(df)[names(df) == "X.UBIOPRED.Adult_Cohort_.Jan_2019..Biomarker.Data.Baseline.Visit.Luminex.Serum.MCP.4.pg.ml.Luminex.serum."] <- "Biomarker.MCP.4.pg.ml.serum"
names(df)[names(df) == "X.UBIOPRED.Adult_Cohort_.Jan_2019..Biomarker.Data.Baseline.Visit.Luminex.Serum.MMP.3.pg.ml.Luminex.serum."] <- "Biomarker.MMP.3.pg.ml.serum"
names(df)[names(df) == "X.UBIOPRED.Adult_Cohort_.Jan_2019..Biomarker.Data.Baseline.Visit.Luminex.Serum.RAGE.pg.ml.Luminex.serum."] <- "Biomarker.RAGE.pg.ml.serum"
names(df)[names(df) == "X.UBIOPRED.Adult_Cohort_.Jan_2019..Biomarker.Data.Baseline.Visit.Luminex.Serum.SHBG.pg.ml.Luminex.serum."] <- "Biomarker.SHBG.pg.ml.serum"
names(df)[names(df) == "X.UBIOPRED.Adult_Cohort_.Jan_2019..Biomarker.Data.Baseline.Visit.Luminex.Serum.Serpin.E1.pg.ml.Luminex.serum."] <- "Biomarker.Serpin.E1.pg.ml.serum"
names(df)[names(df) == "X.UBIOPRED.Adult_Cohort_.Jan_2019..Biomarker.Data.Baseline.Visit.Luminex.Serum.alpha1.microglobulin.pg.ml.Luminex.serum."] <- "Biomarker.alpha1.microglobulin.pg.ml.serum"
names(df)[names(df) == "X.UBIOPRED.Adult_Cohort_.Jan_2019..Biomarker.Data.Baseline.Visit.Plasma.BI.Cytokines.Chemokines.CCL17.pg.ml.MSD.BL.plasma."] <- "Biomarker.CCL17.pg.ml.MSD.BL.plasma"
names(df)[names(df) == "X.UBIOPRED.Adult_Cohort_.Jan_2019..Biomarker.Data.Baseline.Visit.Plasma.BI.Cytokines.Chemokines.CCL22.pg.ml.MSD.BL.plasma."] <- "Biomarker.CCL22.pg.ml.MSD.BL.plasma"
names(df)[names(df) == "X.UBIOPRED.Adult_Cohort_.Jan_2019..Biomarker.Data.Baseline.Visit.Plasma.BI.Cytokines.Chemokines.EOTAXIN.pg.ml.MSD.BL.plasma."] <- "Biomarker.EOTAXIN.pg.ml.MSD.BL.plasma"
names(df)[names(df) == "X.UBIOPRED.Adult_Cohort_.Jan_2019..Biomarker.Data.Baseline.Visit.Plasma.BI.Cytokines.Chemokines.EOTAXIN3.pg.ml.MSD.BL.plasma."] <- "Biomarker.EOTAXIN3.pg.ml.MSD.BL.plasma"
names(df)[names(df) == "X.UBIOPRED.Adult_Cohort_.Jan_2019..Biomarker.Data.Baseline.Visit.Plasma.BI.Cytokines.Chemokines.IFNg.pg.ml.MSD.BL.plasma."] <- "Biomarker.Baseline.BI.Cytokines.Chemokines.IFNg.pg.ml.MSD.BL.plasma"
names(df)[names(df) == "X.UBIOPRED.Adult_Cohort_.Jan_2019..Biomarker.Data.Baseline.Visit.Plasma.BI.Cytokines.Chemokines.IL6.pg.ml.MSD.BL.plasma."] <- "Biomarker.Baseline.BI.Cytokines.Chemokines.IL6.pg.ml.MSD.BL.plasma"
names(df)[names(df) == "X.UBIOPRED.Adult_Cohort_.Jan_2019..Biomarker.Data.Baseline.Visit.Plasma.BI.Cytokines.Chemokines.IL8.pg.ml.MSD.BL.plasma."] <- "Biomarker.IL8.pg.ml.MSD.BL.plasma"
names(df)[names(df) == "X.UBIOPRED.Adult_Cohort_.Jan_2019..Biomarker.Data.Baseline.Visit.Plasma.BI.Cytokines.Chemokines.IP10.pg.ml.MSD.BL.plasma."] <- "Biomarker.IP10.pg.ml.MSD.BL.plasma"
names(df)[names(df) == "X.UBIOPRED.Adult_Cohort_.Jan_2019..Biomarker.Data.Baseline.Visit.Plasma.BI.Cytokines.Chemokines.MCP1.pg.ml.MSD.BL.plasma."] <- "Biomarker.MCP1.pg.ml.MSD.BL.plasma"
names(df)[names(df) == "X.UBIOPRED.Adult_Cohort_.Jan_2019..Biomarker.Data.Baseline.Visit.Plasma.BI.Cytokines.Chemokines.MIP1b.pg.ml.MSD.BL.plasma."] <- "Biomarker.MIP1b.pg.ml.MSD.BL.plasma"
names(df)[names(df) == "X.UBIOPRED.Adult_Cohort_.Jan_2019..Biomarker.Data.Baseline.Visit.Plasma.BI.Cytokines.Chemokines.TNFa.pg.ml.MSD.BL.plasma."] <- "Biomarker.TNFa.pg.ml.MSD.BL.plasma"
names(df)[names(df) == "X.UBIOPRED.Adult_Cohort_.Jan_2019..Biomarker.Data.Baseline.Visit.Serum.Genentech.Cytokines.and.Periostin.CCL18.pg.ml.IMPACT.BL.serum."] <- "Biomarker.Genentech.CCL18.pg.ml.IMPACT.BL.serum"
names(df)[names(df) == "X.UBIOPRED.Adult_Cohort_.Jan_2019..Biomarker.Data.Baseline.Visit.Serum.Genentech.Cytokines.and.Periostin.IL13.pg.ml.IMPACT.BL.serum."] <- "Biomarker.Genentech.IL13.pg.ml.IMPACT.BL.serum"
names(df)[names(df) == "X.UBIOPRED.Adult_Cohort_.Jan_2019..Biomarker.Data.Baseline.Visit.Serum.Genentech.Cytokines.and.Periostin.Periostin.ng.ml.ELECSYS.BL.serum."] <- "Biomarker.Genentech.Periostin.ng.ml.ELECSYS.BL.serum"
names(df)[names(df) == "X.UBIOPRED.Adult_Cohort_.Jan_2019..Biomarker.Data.Baseline.Visit.Serum.Karolinska.hsCRP.hCRP..mg.L.."] <- "Biomarker.Karolinska.hsCRP.hCRP.mg.L"
####### END BIOMARKER DATA #######

####### BEG CLINICAL DATA #######
names(df)[names(df) == "X.UBIOPRED.Adult_Cohort_.Jan_2019..Clinical.Data.Atopy.Total.IgE..IU.ml.."] <- "Clinical.Atopy.Total.IgE.IU.ml"
names(df)[names(df) == "X.UBIOPRED.Adult_Cohort_.Jan_2019..Clinical.Data.Electrocardiogram.Screening.Qtc.Interval."] <- "Clinical.Screening.Electrocardiogram.Qtc.Interval."
names(df)[names(df) == "X.UBIOPRED.Adult_Cohort_.Jan_2019..Clinical.Data.Exhaled.nitric.oxide..NO..Baseline.NO.Standard.Flow.Rate."] <- "Clinical.NO.Standard.Flow.Rate"
names(df)[names(df) == "X.UBIOPRED.Adult_Cohort_.Jan_2019..Clinical.Data.Haematology.and.biochemistry.tests.Screening.Albumin..g.dL.."] <- "Clinical.Screening.Albumin.g.dL"
names(df)[names(df) == "X.UBIOPRED.Adult_Cohort_.Jan_2019..Clinical.Data.Haematology.and.biochemistry.tests.Screening.Alkaline.Phosphatase..U.L.."] <- "Clinical.Screening.Alkaline.Phosphatase.U.L"
names(df)[names(df) == "X.UBIOPRED.Adult_Cohort_.Jan_2019..Clinical.Data.Haematology.and.biochemistry.tests.Screening.Alt..U.L.."] <- "Clinical.Screening.Alt.U.L."
names(df)[names(df) == "X.UBIOPRED.Adult_Cohort_.Jan_2019..Clinical.Data.Haematology.and.biochemistry.tests.Screening.Ast..U.L.."] <- "Clinical.Screening.Ast.U.L"
names(df)[names(df) == "X.UBIOPRED.Adult_Cohort_.Jan_2019..Clinical.Data.Haematology.and.biochemistry.tests.Screening.Basophils..."] <- "Clinical.Screening.Basophils"
names(df)[names(df) == "X.UBIOPRED.Adult_Cohort_.Jan_2019..Clinical.Data.Haematology.and.biochemistry.tests.Screening.Blood.Urea.Nitrogen..mg.dL.."] <- "Clinical.Screening.Blood.Urea.Nitrogen.mg.dL"
names(df)[names(df) == "X.UBIOPRED.Adult_Cohort_.Jan_2019..Clinical.Data.Haematology.and.biochemistry.tests.Screening.Creatinine..umol.L.."] <- "Clinical.Screening.Creatinine.umol.L"
names(df)[names(df) == "X.UBIOPRED.Adult_Cohort_.Jan_2019..Clinical.Data.Haematology.and.biochemistry.tests.Screening.Gamma.Gt..U.L.."] <- "Clinical.Screening.Gamma.Gt.U.L"
names(df)[names(df) == "X.UBIOPRED.Adult_Cohort_.Jan_2019..Clinical.Data.Haematology.and.biochemistry.tests.Screening.Haemoglobin..g.dL.."] <- "Clinical.Screening.Haemoglobin.g.dL"
names(df)[names(df) == "X.UBIOPRED.Adult_Cohort_.Jan_2019..Clinical.Data.Haematology.and.biochemistry.tests.Screening.Lymphocytes..."] <- "Clinical.Screening.Lymphocytes"
names(df)[names(df) == "X.UBIOPRED.Adult_Cohort_.Jan_2019..Clinical.Data.Haematology.and.biochemistry.tests.Screening.Monocytes..."] <- "Clinical.Screening.Monocytes"
names(df)[names(df) == "X.UBIOPRED.Adult_Cohort_.Jan_2019..Clinical.Data.Haematology.and.biochemistry.tests.Screening.Neutrophils..."] <- "Clinical.Screening.Neutrophils"
names(df)[names(df) == "X.UBIOPRED.Adult_Cohort_.Jan_2019..Clinical.Data.Haematology.and.biochemistry.tests.Screening.Platelets...x10.3..uL.."] <- "Clinical.Screening.Platelets.x10.3.uL"
names(df)[names(df) == "X.UBIOPRED.Adult_Cohort_.Jan_2019..Clinical.Data.Haematology.and.biochemistry.tests.Screening.Potassium..mmol.L.."] <- "Clinical.Screening.Potassium.mmol.L"
names(df)[names(df) == "X.UBIOPRED.Adult_Cohort_.Jan_2019..Clinical.Data.Haematology.and.biochemistry.tests.Screening.Sodium..mmol.L.."] <- "Clinical.Screening.Sodium.mmol.L"
names(df)[names(df) == "X.UBIOPRED.Adult_Cohort_.Jan_2019..Clinical.Data.Haematology.and.biochemistry.tests.Screening.Total.Bilirubin..umol.L.."] <- "Clinical.Screening.Total.Bilirubin.umol.L"
names(df)[names(df) == "X.UBIOPRED.Adult_Cohort_.Jan_2019..Clinical.Data.Haematology.and.biochemistry.tests.Screening.Total.Protein..g.dL.."] <- "Clinical.Screening.Total.Protein.g.dL"
names(df)[names(df) == "X.UBIOPRED.Adult_Cohort_.Jan_2019..Clinical.Data.Haematology.and.biochemistry.tests.Screening.Wbcs..x10.3..uL.."] <- "Clinical.Screening.Wbcs.x10.3.uL"
names(df)[names(df) == "X.UBIOPRED.Adult_Cohort_.Jan_2019..Clinical.Data.Haematology.and.biochemistry.tests.Screening.basophils..x10.3.uL.."] <- "Clinical.Screening.basophils.x10.3.u"
names(df)[names(df) == "X.UBIOPRED.Adult_Cohort_.Jan_2019..Clinical.Data.Haematology.and.biochemistry.tests.Screening.eosinophils..x10.3.uL.."] <- "Clinical.Screening.eosinophils.x10.3.uL"
names(df)[names(df) == "X.UBIOPRED.Adult_Cohort_.Jan_2019..Clinical.Data.Haematology.and.biochemistry.tests.Screening.lymphocytes..x10.3.uL.."] <- "Clinical.Screening.lymphocytes.x10.3.uL"
names(df)[names(df) == "X.UBIOPRED.Adult_Cohort_.Jan_2019..Clinical.Data.Haematology.and.biochemistry.tests.Screening.monocytes..x10.3.uL.."] <- "Clinical.Screening.monocytes.x10.3.uL"
names(df)[names(df) == "X.UBIOPRED.Adult_Cohort_.Jan_2019..Clinical.Data.Lung.Function.Reversibility.Baseline.FEF.25.75.Absolute.Change..L.sec.."] <- "Clinical.FEF.25.75.Absolute.Change.L.sec"
names(df)[names(df) == "X.UBIOPRED.Adult_Cohort_.Jan_2019..Clinical.Data.Lung.Function.Reversibility.Baseline.FEF.25.75.Change..L.sec.."] <- "Clinical.FEF.25.75.Change.L.sec"
names(df)[names(df) == "X.UBIOPRED.Adult_Cohort_.Jan_2019..Clinical.Data.Lung.Function.Reversibility.Baseline.FEV1.Change."] <- "Clinical.FEV1.Change"
names(df)[names(df) == "X.UBIOPRED.Adult_Cohort_.Jan_2019..Clinical.Data.Lung.Function.Reversibility.Baseline.FEV1.FVC.Ratio.Predicted.LLN."] <- "Clinical.FEV1.FVC.Ratio.Predicted.LLN"
names(df)[names(df) == "X.UBIOPRED.Adult_Cohort_.Jan_2019..Clinical.Data.Lung.Function.Reversibility.Baseline.FEV1.FVC.Ratio.Predicted."] <- "Clinical.FEV1.FVC.Ratio.Predicted"
names(df)[names(df) == "X.UBIOPRED.Adult_Cohort_.Jan_2019..Clinical.Data.Lung.Function.Reversibility.Baseline.FVC.Absolute.Change..L.."] <- "Clinical.FVC.Absolute.Change.L"
names(df)[names(df) == "X.UBIOPRED.Adult_Cohort_.Jan_2019..Clinical.Data.Lung.Function.Reversibility.Baseline.PEF.Change...L.sec.."] <- "Clinical.PEF.Change.L.sec"
names(df)[names(df) == "X.UBIOPRED.Adult_Cohort_.Jan_2019..Clinical.Data.Lung.Function.Spirometry.Baseline.PEF..."] <- "Clinical.Baseline.PEF"
names(df)[names(df) == "X.UBIOPRED.Adult_Cohort_.Jan_2019..Clinical.Data.Vital.Signs.Screening.Diastolic.Blood.Pressure."] <- "Clinical.Screening.Diastolic.Blood.Pressure"
names(df)[names(df) == "X.UBIOPRED.Adult_Cohort_.Jan_2019..Clinical.Data.Vital.Signs.Screening.Heart.Rate."] <- "Clinical.Screening.Heart.Rate"
names(df)[names(df) == "X.UBIOPRED.Adult_Cohort_.Jan_2019..Clinical.Data.Vital.Signs.Screening.Respiratory.Rate."] <- "Clinical.Screening.Respiratory.Rate"
names(df)[names(df) == "X.UBIOPRED.Adult_Cohort_.Jan_2019..Clinical.Data.Vital.Signs.Screening.Systolic.Blood.Pressure."] <- "Clinical.Screening.Systolic.Blood.Pressure"
names(df)[names(df) == "X.UBIOPRED.Adult_Cohort_.Jan_2019..Clinical.Data.Atopy.Combined.Atopy.Result.All.Allergens."] <- "Clinical.Atopy.All.Allergens"
names(df)[names(df) == "X.UBIOPRED.Adult_Cohort_.Jan_2019..Clinical.Data.Atopy.Combined.Atopy.Result.Food.Allergens."] <- "Clinical.Atopy.Food.Allergens"
names(df)[names(df) == "X.UBIOPRED.Adult_Cohort_.Jan_2019..Clinical.Data.Atopy.Combined.Atopy.Result.Regional.Aeroallergens."] <- "Clinical.Atopy.Regional.Aeroallergens"
names(df)[names(df) == "X.UBIOPRED.Adult_Cohort_.Jan_2019..Clinical.Data.Atopy.IgE.Assay.Atopy.Result."] <- "Clinical.Atopy.IgE.Assay.Result"
names(df)[names(df) == "X.UBIOPRED.Adult_Cohort_.Jan_2019..Clinical.Data.Atopy.Skin.Prick.Test.Atopy.Result."] <- "Clinical.Atopy.Skin.Prick.Test.Result"
names(df)[names(df) == "X.UBIOPRED.Adult_Cohort_.Jan_2019..Clinical.Data.Electrocardiogram.Screening.Ecg.Interpretation."] <- "Clinical.Screening.Ecg.Interpretation"
names(df)[names(df) == "X.UBIOPRED.Adult_Cohort_.Jan_2019..Clinical.Data.Exhaled.nitric.oxide..NO..Baseline.NO.Other.Flow.Rates."] <- "Clinical.NO.Other.Flow.Rates"
names(df)[names(df) == "X.UBIOPRED.Adult_Cohort_.Jan_2019..Clinical.Data.Lung.Function.Airflow.Limitation.Baseline.Airflow.Limitation..Quanjer...ERS.guidelines.."] <- "Clinical.Airflow.Limitation.Quanjer.ERS.guidelines"
names(df)[names(df) == "X.UBIOPRED.Adult_Cohort_.Jan_2019..Clinical.Data.Lung.Function.Airflow.Limitation.Baseline.Airflow.Limitation..Ten.Brinke.et.al.."] <- "Clinical.Airflow.Limitation.Ten.Brinke.et.al"
names(df)[names(df) == "X.UBIOPRED.Adult_Cohort_.Jan_2019..Clinical.Data.Lung.Function.Forced.Oscillation.Technique..FOT..Baseline.Fot.Performed."] <- "Clinical.Fot.Performed"
names(df)[names(df) == "X.UBIOPRED.Adult_Cohort_.Jan_2019..Clinical.Data.Haematology.and.biochemistry.tests.Screening.Haematology.Not.Done."] <- "Clinical.Screening.Haematology.Not.Done"
names(df)[names(df) == "X.UBIOPRED.Adult_Cohort_.Jan_2019..Clinical.Data.Lung.Function.Plethysmography.Baseline.Plethysmography.Not.Done."] <- "Clinical.Plethysmography.Not.Done"
names(df)[names(df) == "X.UBIOPRED.Adult_Cohort_.Jan_2019..Clinical.Data.Vital.Signs.Baseline.Day.1.Vital.Signs.Not.Done."] <- "Clinical.Vital.Signs.Not.Done"
####### END CLINICAL DATA #######

####### BEG OMICS DATA #######
names(df)[names(df) == "X.UBIOPRED.Adult_Cohort_.Jan_2019..OMICS.DATA.Drugomics.Baseline.Visit.Urine.Karolinska.Drug.Levels.16a.OH.Prednisolone..ng.mL.."] <- "Omics.Drugs.16a.OH.Prednisolone.ng.mL"
names(df)[names(df) == "X.UBIOPRED.Adult_Cohort_.Jan_2019..OMICS.DATA.Drugomics.Baseline.Visit.Urine.Karolinska.Drug.Levels.19.norandrosterone.glucuronide..ng.mL.."] <- "Omics.Drugs.19.norandrosterone.glucuronide.ng.mL"
names(df)[names(df) == "X.UBIOPRED.Adult_Cohort_.Jan_2019..OMICS.DATA.Drugomics.Baseline.Visit.Urine.Karolinska.Drug.Levels.20B.dihydro.prednisolone..ng.mL.."] <- "Omics.Drugs.20B.dihydro.prednisolone.ng.mL"
names(df)[names(df) == "X.UBIOPRED.Adult_Cohort_.Jan_2019..OMICS.DATA.Drugomics.Baseline.Visit.Urine.Karolinska.Drug.Levels.5alpha.Androstane.3alpha.17beta.diol.17beta.D.glucuronide..ng.mL.."] <- "Omics.Drugs.5alpha.Androstane.3alpha.17beta.diol.17beta.D.glucuronide.ng.mL"
names(df)[names(df) == "X.UBIOPRED.Adult_Cohort_.Jan_2019..OMICS.DATA.Drugomics.Baseline.Visit.Urine.Karolinska.Drug.Levels.5alpha.Androstane.3beta.17beta.diol.3.glucuronide..ng.mL.."] <- "Omics.Drugs.5alpha.Androstane.3beta.17beta.diol.3.glucuronide.ng.mL"
names(df)[names(df) == "X.UBIOPRED.Adult_Cohort_.Jan_2019..OMICS.DATA.Drugomics.Baseline.Visit.Urine.Karolinska.Drug.Levels.5beta.Androstane.3alpha.17beta.diol.17beta.D.glucuronide..ng.mL.."] <- "Omics.Drugs.5beta.Androstane.3alpha.17beta.diol.17beta.D.glucuronide.ng.mL"
names(df)[names(df) == "X.UBIOPRED.Adult_Cohort_.Jan_2019..OMICS.DATA.Drugomics.Baseline.Visit.Urine.Karolinska.Drug.Levels.5beta.Androstane.3alpha.17beta.diol.3alpha.glucuronide..ng.mL.."] <- "Omics.Drugs.5beta.Androstane.3alpha.17beta.diol.3alpha.glucuronide.ng.mL"
names(df)[names(df) == "X.UBIOPRED.Adult_Cohort_.Jan_2019..OMICS.DATA.Drugomics.Baseline.Visit.Urine.Karolinska.Drug.Levels.Androsterone.sulfate..ng.mL.."] <- "Omics.Drugs.Androsterone.sulfate.ng.mL"
names(df)[names(df) == "X.UBIOPRED.Adult_Cohort_.Jan_2019..OMICS.DATA.Drugomics.Baseline.Visit.Urine.Karolinska.Drug.Levels.Cortisol..ng.mL.."] <- "Omics.Drugs.Cortisol.ng.mL"
names(df)[names(df) == "X.UBIOPRED.Adult_Cohort_.Jan_2019..OMICS.DATA.Drugomics.Baseline.Visit.Urine.Karolinska.Drug.Levels.Cortisone...ng.mL.."] <- "Omics.Drugs.Cortisone.ng.mL"
names(df)[names(df) == "X.UBIOPRED.Adult_Cohort_.Jan_2019..OMICS.DATA.Drugomics.Baseline.Visit.Urine.Karolinska.Drug.Levels.Dehydroepiandrosterone.sulfate..ng.mL...DHEA.s.."] <- "Omics.Drugs.Dehydroepiandrosterone.sulfate.ng.mL.DHEA.s"
names(df)[names(df) == "X.UBIOPRED.Adult_Cohort_.Jan_2019..OMICS.DATA.Drugomics.Baseline.Visit.Urine.Karolinska.Drug.Levels.Dihydrotestosterone.glucuronide..ng.mL.."] <- "Omics.Drugs.Dihydrotestosterone.glucuronide.ng.mL"
names(df)[names(df) == "X.UBIOPRED.Adult_Cohort_.Jan_2019..OMICS.DATA.Drugomics.Baseline.Visit.Urine.Karolinska.Drug.Levels.Dihydrotestosterone.sulfate..ng.mL.."] <- "Omics.Drugs.Dihydrotestosterone.sulfate.ng.mL"
names(df)[names(df) == "X.UBIOPRED.Adult_Cohort_.Jan_2019..OMICS.DATA.Drugomics.Baseline.Visit.Urine.Karolinska.Drug.Levels.Epitestosterone.glucuronide..ng.mL.."] <- "Omics.Drugs.Epitestosterone.glucuronide.ng.mL"
names(df)[names(df) == "X.UBIOPRED.Adult_Cohort_.Jan_2019..OMICS.DATA.Drugomics.Baseline.Visit.Urine.Karolinska.Drug.Levels.Etiocholanolone.glucuronide..ng.mL.."] <- "Omics.Drugs.Etiocholanolone.glucuronide.ng.mL"
names(df)[names(df) == "X.UBIOPRED.Adult_Cohort_.Jan_2019..OMICS.DATA.Drugomics.Baseline.Visit.Urine.Karolinska.Drug.Levels.Etiocholanolone.sulfate..ng.mL.."] <- "Omics.Drugs.Etiocholanolone.sulfate.ng.mL"
names(df)[names(df) == "X.UBIOPRED.Adult_Cohort_.Jan_2019..OMICS.DATA.Drugomics.Baseline.Visit.Urine.Karolinska.Drug.Levels.Fenoterol..ng.mL.."] <- "Omics.Drugs.Fenoterol.ng.mL"
names(df)[names(df) == "X.UBIOPRED.Adult_Cohort_.Jan_2019..OMICS.DATA.Drugomics.Baseline.Visit.Urine.Karolinska.Drug.Levels.Methylprednisolone..ng.mL.."] <- "Omics.Drugs.Methylprednisolone.ng.mL"
names(df)[names(df) == "X.UBIOPRED.Adult_Cohort_.Jan_2019..OMICS.DATA.Drugomics.Baseline.Visit.Urine.Karolinska.Drug.Levels.Prednisolone..ng.mL.."] <- "Omics.Drugs.Prednisolone.ng.mL"
names(df)[names(df) == "X.UBIOPRED.Adult_Cohort_.Jan_2019..OMICS.DATA.Drugomics.Baseline.Visit.Urine.Karolinska.Drug.Levels.Prednisone..ng.mL.."] <- "Omics.Drugs.Prednisone.ng.mL"
names(df)[names(df) == "X.UBIOPRED.Adult_Cohort_.Jan_2019..OMICS.DATA.Drugomics.Baseline.Visit.Urine.Karolinska.Drug.Levels.Salbutamol..ng.mL.."] <- "Omics.Drugs.Salbutamol.ng.mL"
names(df)[names(df) == "X.UBIOPRED.Adult_Cohort_.Jan_2019..OMICS.DATA.Drugomics.Baseline.Visit.Urine.Karolinska.Drug.Levels.Terbutaline..ng.mL.."] <- "Omics.Drugs.Terbutaline.ng.mL"
names(df)[names(df) == "X.UBIOPRED.Adult_Cohort_.Jan_2019..OMICS.DATA.Drugomics.Baseline.Visit.Urine.Karolinska.Drug.Levels.Testosterone.glucuronide..ng.mL.."] <- "Omics.Drugs.Testosterone.glucuronide.ng.mL"
names(df)[names(df) == "X.UBIOPRED.Adult_Cohort_.Jan_2019..OMICS.DATA.Drugomics.Baseline.Visit.Urine.Karolinska.Drug.Levels.Testosterone.sulfate..ng.mL.."] <- "Omics.Drugs.Testosterone.sulfate.ng.mL"
names(df)[names(df) == "X.UBIOPRED.Adult_Cohort_.Jan_2019..OMICS.DATA.Drugomics.Baseline.Visit.Urine.Karolinska.Drug.Levels.desacetyl.deflazacort...ng.mL.."] <- "Omics.Drugs.desacetyl.deflazacort.ng.mL"
names(df)[names(df) == "X.UBIOPRED.Adult_Cohort_.Jan_2019..OMICS.DATA.Lipidomics.Baseline.Visit.Urine.Karolinska.Eicosanoid.Lipidomics.11.dehydroTXB2..ng.mmolC.."] <- "Omics.dehydroTXB2.ng.mmolC"
names(df)[names(df) == "X.UBIOPRED.Adult_Cohort_.Jan_2019..OMICS.DATA.Lipidomics.Baseline.Visit.Urine.Karolinska.Eicosanoid.Lipidomics.2.3.dinor.11.B.PGF2a..ng.mmolC.."] <- "Omics.dinor.11.B.PGF2a.ng.mmolC"
names(df)[names(df) == "X.UBIOPRED.Adult_Cohort_.Jan_2019..OMICS.DATA.Lipidomics.Baseline.Visit.Urine.Karolinska.Eicosanoid.Lipidomics.2.3.dinor.8.isoPGF2a..ng.mmolC.."] <- "Omics.dinor.8.isoPGF2a.ng.mmolC"
names(df)[names(df) == "X.UBIOPRED.Adult_Cohort_.Jan_2019..OMICS.DATA.Lipidomics.Baseline.Visit.Urine.Karolinska.Eicosanoid.Lipidomics.2.3.dinor.TXB2..ng.mmolC.."] <- "Omics.dinor.TXB2.ng.mmolC"
names(df)[names(df) == "X.UBIOPRED.Adult_Cohort_.Jan_2019..OMICS.DATA.Lipidomics.Baseline.Visit.Urine.Karolinska.Eicosanoid.Lipidomics.8.12.iso.iPF2a.VI..ng.mmolC.."] <- "Omics.iso.iPF2a.VI.ng.mmolC"
names(df)[names(df) == "X.UBIOPRED.Adult_Cohort_.Jan_2019..OMICS.DATA.Lipidomics.Baseline.Visit.Urine.Karolinska.Eicosanoid.Lipidomics.8.isoPGF2a..ng.mmolC.."] <- "Omics.isoPGF2a.ng.mmolC"
names(df)[names(df) == "X.UBIOPRED.Adult_Cohort_.Jan_2019..OMICS.DATA.Lipidomics.Baseline.Visit.Urine.Karolinska.Eicosanoid.Lipidomics.LTE4..ng.mmolC.."] <- "Omics.LTE4.ng.mmolC"
names(df)[names(df) == "X.UBIOPRED.Adult_Cohort_.Jan_2019..OMICS.DATA.Lipidomics.Baseline.Visit.Urine.Karolinska.Eicosanoid.Lipidomics.PGE2..ng.mmolC.."] <- "Omics.PGE2.ng.mmolC"
names(df)[names(df) == "X.UBIOPRED.Adult_Cohort_.Jan_2019..OMICS.DATA.Lipidomics.Baseline.Visit.Urine.Karolinska.Eicosanoid.Lipidomics.PGF2a..ng.mmolC.."] <- "Omics.PGF2a.ng.mmolC"
names(df)[names(df) == "X.UBIOPRED.Adult_Cohort_.Jan_2019..OMICS.DATA.Lipidomics.Baseline.Visit.Urine.Karolinska.Eicosanoid.Lipidomics.tetranorPGDM..ng.mmolC.."] <- "Omics.tetranorPGDM.ng.mmolC"
names(df)[names(df) == "X.UBIOPRED.Adult_Cohort_.Jan_2019..OMICS.DATA.Lipidomics.Baseline.Visit.Urine.Karolinska.Eicosanoid.Lipidomics.tetranorPGEM..ng.mmolC.."] <- "Omics.tetranorPGEM.ng.mmolC"
names(df)[names(df) == "X.UBIOPRED.Adult_Cohort_.Jan_2019..OMICS.DATA.Metabolomics.Baseline.Visit.Urine.1.3.7.Trimethyluric.acid."] <- "Omics.Trimethyluric.acid"
names(df)[names(df) == "X.UBIOPRED.Adult_Cohort_.Jan_2019..OMICS.DATA.Metabolomics.Baseline.Visit.Urine.1.7.Dimethyluric.acid."] <- "Omics.Dimethyluric.acid"
names(df)[names(df) == "X.UBIOPRED.Adult_Cohort_.Jan_2019..OMICS.DATA.Metabolomics.Baseline.Visit.Urine.1.Methyluric.acid."] <- "Omics.Methyluric.acid"
names(df)[names(df) == "X.UBIOPRED.Adult_Cohort_.Jan_2019..OMICS.DATA.Metabolomics.Baseline.Visit.Urine.2.Furoylglycine."] <- "Omics.Furoylglycine"
names(df)[names(df) == "X.UBIOPRED.Adult_Cohort_.Jan_2019..OMICS.DATA.Metabolomics.Baseline.Visit.Urine.3.Hydroxybutyric.acid."] <- "Omics.Hydroxybutyric.acid"
names(df)[names(df) == "X.UBIOPRED.Adult_Cohort_.Jan_2019..OMICS.DATA.Metabolomics.Baseline.Visit.Urine.3.Hydroxykynurenine."] <- "Omics.Hydroxykynurenine"
names(df)[names(df) == "X.UBIOPRED.Adult_Cohort_.Jan_2019..OMICS.DATA.Metabolomics.Baseline.Visit.Urine.3.Hydroxyproline."] <- "Omics.Hydroxyproline"
names(df)[names(df) == "X.UBIOPRED.Adult_Cohort_.Jan_2019..OMICS.DATA.Metabolomics.Baseline.Visit.Urine.3.Methylhistidine."] <- "Omics.Methylhistidine"
names(df)[names(df) == "X.UBIOPRED.Adult_Cohort_.Jan_2019..OMICS.DATA.Metabolomics.Baseline.Visit.Urine.3.Methylxanthine."] <- "Omics.Methylxanthine"
names(df)[names(df) == "X.UBIOPRED.Adult_Cohort_.Jan_2019..OMICS.DATA.Metabolomics.Baseline.Visit.Urine.4.Pyridoxic.acid."] <- "Omics.Pyridoxic.acid"
names(df)[names(df) == "X.UBIOPRED.Adult_Cohort_.Jan_2019..OMICS.DATA.Metabolomics.Baseline.Visit.Urine.5.Acetylamino.6.formylamino.3.methyluracil."] <- "Omics.Acetylamino.6.formylamino.3.methyluracil"
names(df)[names(df) == "X.UBIOPRED.Adult_Cohort_.Jan_2019..OMICS.DATA.Metabolomics.Baseline.Visit.Urine.5.Aminolevulinic.acid."] <- "Omics.Aminolevulinic.acid"
names(df)[names(df) == "X.UBIOPRED.Adult_Cohort_.Jan_2019..OMICS.DATA.Metabolomics.Baseline.Visit.Urine.5.Hydroxyindoleacetic.acid."] <- "Omics.Hydroxyindoleacetic.acid"
names(df)[names(df) == "X.UBIOPRED.Adult_Cohort_.Jan_2019..OMICS.DATA.Metabolomics.Baseline.Visit.Urine.7.Methylguanine."] <- "Omics.Methylguanine"
names(df)[names(df) == "X.UBIOPRED.Adult_Cohort_.Jan_2019..OMICS.DATA.Metabolomics.Baseline.Visit.Urine.Acetylcarnitine."] <- "Omics.Acetylcarnitine"
names(df)[names(df) == "X.UBIOPRED.Adult_Cohort_.Jan_2019..OMICS.DATA.Metabolomics.Baseline.Visit.Urine.Alanine."] <- "Omics.Alanine"
names(df)[names(df) == "X.UBIOPRED.Adult_Cohort_.Jan_2019..OMICS.DATA.Metabolomics.Baseline.Visit.Urine.Allantoin."] <- "Omics.Allantoin"
names(df)[names(df) == "X.UBIOPRED.Adult_Cohort_.Jan_2019..OMICS.DATA.Metabolomics.Baseline.Visit.Urine.Aminocaproic.acid."] <- "Omics.Aminocaproic.acid"
names(df)[names(df) == "X.UBIOPRED.Adult_Cohort_.Jan_2019..OMICS.DATA.Metabolomics.Baseline.Visit.Urine.Aminovaleric.acid."] <- "Omics.Aminovaleric.acid"
names(df)[names(df) == "X.UBIOPRED.Adult_Cohort_.Jan_2019..OMICS.DATA.Metabolomics.Baseline.Visit.Urine.Aspartic.acid."] <- "Omics.Aspartic.acid"
names(df)[names(df) == "X.UBIOPRED.Adult_Cohort_.Jan_2019..OMICS.DATA.Metabolomics.Baseline.Visit.Urine.Betaine."] <- "Omics.Betaine"
names(df)[names(df) == "X.UBIOPRED.Adult_Cohort_.Jan_2019..OMICS.DATA.Metabolomics.Baseline.Visit.Urine.Biopterin."] <- "Omics.Biopterin"
names(df)[names(df) == "X.UBIOPRED.Adult_Cohort_.Jan_2019..OMICS.DATA.Metabolomics.Baseline.Visit.Urine.Caffeine."] <- "Omics.Caffeine"
names(df)[names(df) == "X.UBIOPRED.Adult_Cohort_.Jan_2019..OMICS.DATA.Metabolomics.Baseline.Visit.Urine.Carnitine."] <- "Omics.Carnitine"
names(df)[names(df) == "X.UBIOPRED.Adult_Cohort_.Jan_2019..OMICS.DATA.Metabolomics.Baseline.Visit.Urine.Carnosine."] <- "Omics.Carnosine"
names(df)[names(df) == "X.UBIOPRED.Adult_Cohort_.Jan_2019..OMICS.DATA.Metabolomics.Baseline.Visit.Urine.Choline."] <- "Omics.Choline"
names(df)[names(df) == "X.UBIOPRED.Adult_Cohort_.Jan_2019..OMICS.DATA.Metabolomics.Baseline.Visit.Urine.Citrulline."] <- "Omics.Citrulline"
names(df)[names(df) == "X.UBIOPRED.Adult_Cohort_.Jan_2019..OMICS.DATA.Metabolomics.Baseline.Visit.Urine.Cystathionine."] <- "Omics.Cystathionine"
names(df)[names(df) == "X.UBIOPRED.Adult_Cohort_.Jan_2019..OMICS.DATA.Metabolomics.Baseline.Visit.Urine.Cytosine."] <- "Omics.Cytosine"
names(df)[names(df) == "X.UBIOPRED.Adult_Cohort_.Jan_2019..OMICS.DATA.Metabolomics.Baseline.Visit.Urine.Galacturonic.acid."] <- "Omics.Galacturonic.acid"
names(df)[names(df) == "X.UBIOPRED.Adult_Cohort_.Jan_2019..OMICS.DATA.Metabolomics.Baseline.Visit.Urine.Glucosamine."] <- "Omics.Glucosamine"
names(df)[names(df) == "X.UBIOPRED.Adult_Cohort_.Jan_2019..OMICS.DATA.Metabolomics.Baseline.Visit.Urine.Glutamic.acid."] <- "Omics.Glutamic.acid"
names(df)[names(df) == "X.UBIOPRED.Adult_Cohort_.Jan_2019..OMICS.DATA.Metabolomics.Baseline.Visit.Urine.Glutamine."] <- "Omics.Glutamine"
names(df)[names(df) == "X.UBIOPRED.Adult_Cohort_.Jan_2019..OMICS.DATA.Metabolomics.Baseline.Visit.Urine.Guanine."] <- "Omics.Guanine"
names(df)[names(df) == "X.UBIOPRED.Adult_Cohort_.Jan_2019..OMICS.DATA.Metabolomics.Baseline.Visit.Urine.Guanosine."] <- "Omics.Guanosine"
names(df)[names(df) == "X.UBIOPRED.Adult_Cohort_.Jan_2019..OMICS.DATA.Metabolomics.Baseline.Visit.Urine.Hippuric.acid."] <- "Omics.Hippuric.acid"
names(df)[names(df) == "X.UBIOPRED.Adult_Cohort_.Jan_2019..OMICS.DATA.Metabolomics.Baseline.Visit.Urine.Histidine."] <- "Omics.Histidine"
names(df)[names(df) == "X.UBIOPRED.Adult_Cohort_.Jan_2019..OMICS.DATA.Metabolomics.Baseline.Visit.Urine.Hydroxyphenylacetic.acid."] <- "Omics.Hydroxyphenylacetic.acid"
names(df)[names(df) == "X.UBIOPRED.Adult_Cohort_.Jan_2019..OMICS.DATA.Metabolomics.Baseline.Visit.Urine.Hypoxanthine."] <- "Omics.Hypoxanthine"
names(df)[names(df) == "X.UBIOPRED.Adult_Cohort_.Jan_2019..OMICS.DATA.Metabolomics.Baseline.Visit.Urine.Inosine."] <- "Omics.Inosine"
names(df)[names(df) == "X.UBIOPRED.Adult_Cohort_.Jan_2019..OMICS.DATA.Metabolomics.Baseline.Visit.Urine.Isoleucine."] <- "Omics.Isoleucine"
names(df)[names(df) == "X.UBIOPRED.Adult_Cohort_.Jan_2019..OMICS.DATA.Metabolomics.Baseline.Visit.Urine.Kynurenic.acid."] <- "Omics.Kynurenic.acid"
names(df)[names(df) == "X.UBIOPRED.Adult_Cohort_.Jan_2019..OMICS.DATA.Metabolomics.Baseline.Visit.Urine.Lysine."] <- "Omics.Lysine"
names(df)[names(df) == "X.UBIOPRED.Adult_Cohort_.Jan_2019..OMICS.DATA.Metabolomics.Baseline.Visit.Urine.Maltose."] <- "Omics.Maltose"
names(df)[names(df) == "X.UBIOPRED.Adult_Cohort_.Jan_2019..OMICS.DATA.Metabolomics.Baseline.Visit.Urine.Mannitol."] <- "Omics.Mannitol"
names(df)[names(df) == "X.UBIOPRED.Adult_Cohort_.Jan_2019..OMICS.DATA.Metabolomics.Baseline.Visit.Urine.Mesaconic.acid."] <- "Omics.Mesaconic.acid"
names(df)[names(df) == "X.UBIOPRED.Adult_Cohort_.Jan_2019..OMICS.DATA.Metabolomics.Baseline.Visit.Urine.Metanephrine."] <- "Omics.Metanephrine"
names(df)[names(df) == "X.UBIOPRED.Adult_Cohort_.Jan_2019..OMICS.DATA.Metabolomics.Baseline.Visit.Urine.Methionine."] <- "Omics.Methionine"
names(df)[names(df) == "X.UBIOPRED.Adult_Cohort_.Jan_2019..OMICS.DATA.Metabolomics.Baseline.Visit.Urine.Methylhippuric.acid."] <- "Omics.Methylhippuric.acid"
names(df)[names(df) == "X.UBIOPRED.Adult_Cohort_.Jan_2019..OMICS.DATA.Metabolomics.Baseline.Visit.Urine.Methylthioadenosine."] <- "Omics.Methylthioadenosine"
names(df)[names(df) == "X.UBIOPRED.Adult_Cohort_.Jan_2019..OMICS.DATA.Metabolomics.Baseline.Visit.Urine.N.Acetylcarnosine."] <- "Omics.N.Acetylcarnosine"
names(df)[names(df) == "X.UBIOPRED.Adult_Cohort_.Jan_2019..OMICS.DATA.Metabolomics.Baseline.Visit.Urine.N.Acetylglutamic.acid."] <- "Omics.N.Acetylglutamic.acid"
names(df)[names(df) == "X.UBIOPRED.Adult_Cohort_.Jan_2019..OMICS.DATA.Metabolomics.Baseline.Visit.Urine.N.Acetylputrescine."] <- "Omics.N.Acetylputrescine"
names(df)[names(df) == "X.UBIOPRED.Adult_Cohort_.Jan_2019..OMICS.DATA.Metabolomics.Baseline.Visit.Urine.N.Methyl.D.aspartic.acid."] <- "Omics.N.Methyl.D.aspartic.acid"
names(df)[names(df) == "X.UBIOPRED.Adult_Cohort_.Jan_2019..OMICS.DATA.Metabolomics.Baseline.Visit.Urine.N.Methylhistamine."] <- "Omics.N.Methylhistamine"
names(df)[names(df) == "X.UBIOPRED.Adult_Cohort_.Jan_2019..OMICS.DATA.Metabolomics.Baseline.Visit.Urine.Nitrotyrosine."] <- "Omics.Nitrotyrosine"
names(df)[names(df) == "X.UBIOPRED.Adult_Cohort_.Jan_2019..OMICS.DATA.Metabolomics.Baseline.Visit.Urine.O.Acetylserine."] <- "Omics.O.Acetylserine"
names(df)[names(df) == "X.UBIOPRED.Adult_Cohort_.Jan_2019..OMICS.DATA.Metabolomics.Baseline.Visit.Urine.Ornithine."] <- "Omics.Ornithine"
names(df)[names(df) == "X.UBIOPRED.Adult_Cohort_.Jan_2019..OMICS.DATA.Metabolomics.Baseline.Visit.Urine.Phenylacetylglutamine."] <- "Omics.Phenylacetylglutamine"
names(df)[names(df) == "X.UBIOPRED.Adult_Cohort_.Jan_2019..OMICS.DATA.Metabolomics.Baseline.Visit.Urine.Phenylalanine."] <- "Omics.Phenylalanine"
names(df)[names(df) == "X.UBIOPRED.Adult_Cohort_.Jan_2019..OMICS.DATA.Metabolomics.Baseline.Visit.Urine.Phenyllactic.acid."] <- "Omics.Phenyllactic.acid"
names(df)[names(df) == "X.UBIOPRED.Adult_Cohort_.Jan_2019..OMICS.DATA.Metabolomics.Baseline.Visit.Urine.Pipecolic.acid."] <- "Omics.Pipecolic.acid"
names(df)[names(df) == "X.UBIOPRED.Adult_Cohort_.Jan_2019..OMICS.DATA.Metabolomics.Baseline.Visit.Urine.Proline."] <- "Omics.Proline"
names(df)[names(df) == "X.UBIOPRED.Adult_Cohort_.Jan_2019..OMICS.DATA.Metabolomics.Baseline.Visit.Urine.Propionylcarnitine."] <- "Omics.Propionylcarnitine"
names(df)[names(df) == "X.UBIOPRED.Adult_Cohort_.Jan_2019..OMICS.DATA.Metabolomics.Baseline.Visit.Urine.Pyridoxal."] <- "Omics.Pyridoxal"
names(df)[names(df) == "X.UBIOPRED.Adult_Cohort_.Jan_2019..OMICS.DATA.Metabolomics.Baseline.Visit.Urine.Pyroglutamic.acid."] <- "Omics.Pyroglutamic.acid"
names(df)[names(df) == "X.UBIOPRED.Adult_Cohort_.Jan_2019..OMICS.DATA.Metabolomics.Baseline.Visit.Urine.Pyroglutamylglycine."] <- "Omics.Pyroglutamylglycine"
names(df)[names(df) == "X.UBIOPRED.Adult_Cohort_.Jan_2019..OMICS.DATA.Metabolomics.Baseline.Visit.Urine.S.Adenosylhomocysteine."] <- "Omics.S.Adenosylhomocysteine"
names(df)[names(df) == "X.UBIOPRED.Adult_Cohort_.Jan_2019..OMICS.DATA.Metabolomics.Baseline.Visit.Urine.Saccharopine."] <- "Omics.Saccharopine"
names(df)[names(df) == "X.UBIOPRED.Adult_Cohort_.Jan_2019..OMICS.DATA.Metabolomics.Baseline.Visit.Urine.Sarcosine."] <- "Omics.Sarcosine"
names(df)[names(df) == "X.UBIOPRED.Adult_Cohort_.Jan_2019..OMICS.DATA.Metabolomics.Baseline.Visit.Urine.Serine."] <- "Omics.Serine"
names(df)[names(df) == "X.UBIOPRED.Adult_Cohort_.Jan_2019..OMICS.DATA.Metabolomics.Baseline.Visit.Urine.Serotonin."] <- "Omics.Serotonin"
names(df)[names(df) == "X.UBIOPRED.Adult_Cohort_.Jan_2019..OMICS.DATA.Metabolomics.Baseline.Visit.Urine.Sucrose."] <- "Omics.Sucrose"
names(df)[names(df) == "X.UBIOPRED.Adult_Cohort_.Jan_2019..OMICS.DATA.Metabolomics.Baseline.Visit.Urine.Taurine."] <- "Omics.Taurine"
names(df)[names(df) == "X.UBIOPRED.Adult_Cohort_.Jan_2019..OMICS.DATA.Metabolomics.Baseline.Visit.Urine.Theobromine."] <- "Omics.Theobromine"
names(df)[names(df) == "X.UBIOPRED.Adult_Cohort_.Jan_2019..OMICS.DATA.Metabolomics.Baseline.Visit.Urine.Tryptamine."] <- "Omics.Tryptamine"
names(df)[names(df) == "X.UBIOPRED.Adult_Cohort_.Jan_2019..OMICS.DATA.Metabolomics.Baseline.Visit.Urine.Tyramine."] <- "Omics.Tyramine"
names(df)[names(df) == "X.UBIOPRED.Adult_Cohort_.Jan_2019..OMICS.DATA.Metabolomics.Baseline.Visit.Urine.Tyrosine."] <- "Omics.Tyrosine"
names(df)[names(df) == "X.UBIOPRED.Adult_Cohort_.Jan_2019..OMICS.DATA.Metabolomics.Baseline.Visit.Urine.Uracil."] <- "Omics.Uracil"
names(df)[names(df) == "X.UBIOPRED.Adult_Cohort_.Jan_2019..OMICS.DATA.Metabolomics.Baseline.Visit.Urine.Uric.acid."] <- "Omics.Uric.acid"
names(df)[names(df) == "X.UBIOPRED.Adult_Cohort_.Jan_2019..OMICS.DATA.Metabolomics.Baseline.Visit.Urine.Xanthine."] <- "Omics.Xanthine"
names(df)[names(df) == "X.UBIOPRED.Adult_Cohort_.Jan_2019..OMICS.DATA.Metabolomics.Baseline.Visit.Urine.Xanthosine."] <- "Omics.Xanthosine"
names(df)[names(df) == "X.UBIOPRED.Adult_Cohort_.Jan_2019..OMICS.DATA.Metabolomics.Baseline.Visit.Urine.Xylose."] <- "Omics.Xylose"
names(df)[names(df) == "X.UBIOPRED.Adult_Cohort_.Jan_2019..OMICS.DATA.Metabolomics.Baseline.Visit.Urine.alpha.Glutamyltyrosine."] <- "Omics.alpha.Glutamyltyrosine"
names(df)[names(df) == "X.UBIOPRED.Adult_Cohort_.Jan_2019..OMICS.DATA.Metabolomics.Baseline.Visit.Urine.gamma.Aminobutyric.acid."] <- "Omics.gamma.Aminobutyric.acid"
names(df)[names(df) == "X.UBIOPRED.Adult_Cohort_.Jan_2019..OMICS.DATA.Drugomics.Baseline.Visit.Urine.Karolinska.Drug.Levels.ORAL.CORTICOSTEROIDS.DETECTED."] <- "Omics.ORAL.CORTICOSTEROIDS.DETECTED"
names(df)[names(df) == "X.UBIOPRED.Adult_Cohort_.Jan_2019..OMICS.DATA.Drugomics.Baseline.Visit.Urine.Karolinska.Drug.Levels.b.AGONISTS.DETECTED."] <- "Omics.b.AGONISTS.DETECTED"
names(df)[names(df) == "X.UBIOPRED.Adult_Cohort_.Jan_2019..OMICS.DATA.Lipidomics.Baseline.Visit.Plasma."] <- "Omics.Lipidomics.Plasma"
names(df)[names(df) == "X.UBIOPRED.Adult_Cohort_.Jan_2019..OMICS.DATA.Proteomics.Baseline.Visit.Plasma."] <- "Omics.Proteomics.Plasma"
names(df)[names(df) == "X.UBIOPRED.Adult_Cohort_.Jan_2019..OMICS.DATA.Proteomics.Baseline.Visit.Serum."] <- "Omics.Proteomics.Serum"
names(df)[names(df) == "X.UBIOPRED.Adult_Cohort_.Jan_2019..OMICS.DATA.Transcriptomics.Baseline.Visit.Blood."] <- "Omics.Transcriptomics.Blood"
####### END OMICS DATA #######

####### BEG QUESTIONNAIRES DATA #######
names(df)[names(df) == "X.UBIOPRED.Adult_Cohort_.Jan_2019..Questionnaires.Asthma.Control.Questionnaire..ACQ..Baseline.Visit.FEV1.Precentage."] <- "Questionnaires.ACQ.FEV1.Precentage"
names(df)[names(df) == "X.UBIOPRED.Adult_Cohort_.Jan_2019..Questionnaires.Asthma.Quality.of.Life.Questionnaire..AQLQ..Baseline.Visit.Emotional.Total..Imputed.."] <- "Questionnaires.AQLQ.Emotional.Total.Imputed"
names(df)[names(df) == "X.UBIOPRED.Adult_Cohort_.Jan_2019..Questionnaires.Epworth.Sleepiness.Scale..ESS..Baseline.Visit.Total..Imputed.."] <- "Questionnaires.ESS.Total.Imputed"
names(df)[names(df) == "X.UBIOPRED.Adult_Cohort_.Jan_2019..Questionnaires.Hospital.Anxiety.and.Depression.Scale..HADS..Baseline.Visit.Anxiety.Total..Imputed.."] <- "Questionnaires.HADS.Anxiety.Total.Imputed"
names(df)[names(df) == "X.UBIOPRED.Adult_Cohort_.Jan_2019..Questionnaires.Hospital.Anxiety.and.Depression.Scale..HADS..Baseline.Visit.Depression.Total..Imputed.."] <- "Questionnaires.HADS.Depression.Total.Imputed"
names(df)[names(df) == "X.UBIOPRED.Adult_Cohort_.Jan_2019..Questionnaires.SinoNasal.Outcomes.Test..SNOT..Baseline.Visit.Total..Imputed.."] <- "Questionnaires.SNOT.Total.Imputed"
names(df)[names(df) == "X.UBIOPRED.Adult_Cohort_.Jan_2019..Questionnaires.Asthma.Control.Questionnaire..ACQ..Baseline.Visit.Question.1."] <- "Questionnaires.ACQ.Question.1"
names(df)[names(df) == "X.UBIOPRED.Adult_Cohort_.Jan_2019..Questionnaires.Asthma.Control.Questionnaire..ACQ..Baseline.Visit.Question.2."] <- "Questionnaires.ACQ.Question.2"
names(df)[names(df) == "X.UBIOPRED.Adult_Cohort_.Jan_2019..Questionnaires.Asthma.Control.Questionnaire..ACQ..Baseline.Visit.Question.3."] <- "Questionnaires.ACQ.Question.3"
names(df)[names(df) == "X.UBIOPRED.Adult_Cohort_.Jan_2019..Questionnaires.Asthma.Control.Questionnaire..ACQ..Baseline.Visit.Question.5."] <- "Questionnaires.ACQ.Question.5"
names(df)[names(df) == "X.UBIOPRED.Adult_Cohort_.Jan_2019..Questionnaires.Asthma.Control.Questionnaire..ACQ..Baseline.Visit.Question.6."] <- "Questionnaires.ACQ.Question.6"
names(df)[names(df) == "X.UBIOPRED.Adult_Cohort_.Jan_2019..Questionnaires.Asthma.Control.Questionnaire..ACQ..Baseline.Visit.Question.7."] <- "Questionnaires.ACQ.Question.7"
names(df)[names(df) == "X.UBIOPRED.Adult_Cohort_.Jan_2019..Questionnaires.Asthma.Quality.of.Life.Questionnaire..AQLQ..Baseline.Visit.Question.11."] <- "Questionnaires.AQLQ.Question.11"
names(df)[names(df) == "X.UBIOPRED.Adult_Cohort_.Jan_2019..Questionnaires.Asthma.Quality.of.Life.Questionnaire..AQLQ..Baseline.Visit.Question.12."] <- "Questionnaires.AQLQ.Question.12"
names(df)[names(df) == "X.UBIOPRED.Adult_Cohort_.Jan_2019..Questionnaires.Asthma.Quality.of.Life.Questionnaire..AQLQ..Baseline.Visit.Question.16."] <- "Questionnaires.AQLQ.Question.16"
names(df)[names(df) == "X.UBIOPRED.Adult_Cohort_.Jan_2019..Questionnaires.Asthma.Quality.of.Life.Questionnaire..AQLQ..Baseline.Visit.Question.26."] <- "Questionnaires.AQLQ.Question.26"
names(df)[names(df) == "X.UBIOPRED.Adult_Cohort_.Jan_2019..Questionnaires.Asthma.Quality.of.Life.Questionnaire..AQLQ..Baseline.Visit.Question.9."] <- "Questionnaires.AQLQ.Question.9"
names(df)[names(df) == "X.UBIOPRED.Adult_Cohort_.Jan_2019..Questionnaires.Epworth.Sleepiness.Scale..ESS..Baseline.Visit.Question.1."] <- "Questionnaires.ESS.Question.1"
names(df)[names(df) == "X.UBIOPRED.Adult_Cohort_.Jan_2019..Questionnaires.Epworth.Sleepiness.Scale..ESS..Baseline.Visit.Question.2."] <- "Questionnaires.ESS.Question.2"
names(df)[names(df) == "X.UBIOPRED.Adult_Cohort_.Jan_2019..Questionnaires.Epworth.Sleepiness.Scale..ESS..Baseline.Visit.Question.3."] <- "Questionnaires.ESS.Question.3"
names(df)[names(df) == "X.UBIOPRED.Adult_Cohort_.Jan_2019..Questionnaires.Epworth.Sleepiness.Scale..ESS..Baseline.Visit.Question.4."] <- "Questionnaires.ESS.Question.4"
names(df)[names(df) == "X.UBIOPRED.Adult_Cohort_.Jan_2019..Questionnaires.Epworth.Sleepiness.Scale..ESS..Baseline.Visit.Question.5."] <- "Questionnaires.ESS.Question.5"
names(df)[names(df) == "X.UBIOPRED.Adult_Cohort_.Jan_2019..Questionnaires.Epworth.Sleepiness.Scale..ESS..Baseline.Visit.Question.6."] <- "Questionnaires.ESS.Question.6"
names(df)[names(df) == "X.UBIOPRED.Adult_Cohort_.Jan_2019..Questionnaires.Epworth.Sleepiness.Scale..ESS..Baseline.Visit.Question.7."] <- "Questionnaires.ESS.Question.7"
names(df)[names(df) == "X.UBIOPRED.Adult_Cohort_.Jan_2019..Questionnaires.Epworth.Sleepiness.Scale..ESS..Baseline.Visit.Question.8."] <- "Questionnaires.ESS.Question.8"
names(df)[names(df) == "X.UBIOPRED.Adult_Cohort_.Jan_2019..Questionnaires.Hospital.Anxiety.and.Depression.Scale..HADS..Baseline.Visit.Question.10."] <- "Questionnaires.HADS.Question.10"
names(df)[names(df) == "X.UBIOPRED.Adult_Cohort_.Jan_2019..Questionnaires.Hospital.Anxiety.and.Depression.Scale..HADS..Baseline.Visit.Question.11."] <- "Questionnaires.HADS.Question.11"
names(df)[names(df) == "X.UBIOPRED.Adult_Cohort_.Jan_2019..Questionnaires.Hospital.Anxiety.and.Depression.Scale..HADS..Baseline.Visit.Question.12."] <- "Questionnaires.HADS.Question.12"
names(df)[names(df) == "X.UBIOPRED.Adult_Cohort_.Jan_2019..Questionnaires.Hospital.Anxiety.and.Depression.Scale..HADS..Baseline.Visit.Question.13."] <- "Questionnaires.HADS.Question.13"
names(df)[names(df) == "X.UBIOPRED.Adult_Cohort_.Jan_2019..Questionnaires.Hospital.Anxiety.and.Depression.Scale..HADS..Baseline.Visit.Question.14."] <- "Questionnaires.HADS.Question.14"
names(df)[names(df) == "X.UBIOPRED.Adult_Cohort_.Jan_2019..Questionnaires.Hospital.Anxiety.and.Depression.Scale..HADS..Baseline.Visit.Question.1."] <- "Questionnaires.HADS.Question.1"
names(df)[names(df) == "X.UBIOPRED.Adult_Cohort_.Jan_2019..Questionnaires.Hospital.Anxiety.and.Depression.Scale..HADS..Baseline.Visit.Question.2."] <- "Questionnaires.HADS.Question.2"
names(df)[names(df) == "X.UBIOPRED.Adult_Cohort_.Jan_2019..Questionnaires.Hospital.Anxiety.and.Depression.Scale..HADS..Baseline.Visit.Question.3."] <- "Questionnaires.HADS.Question.3"
names(df)[names(df) == "X.UBIOPRED.Adult_Cohort_.Jan_2019..Questionnaires.Hospital.Anxiety.and.Depression.Scale..HADS..Baseline.Visit.Question.4."] <- "Questionnaires.HADS.Question.4"
names(df)[names(df) == "X.UBIOPRED.Adult_Cohort_.Jan_2019..Questionnaires.Hospital.Anxiety.and.Depression.Scale..HADS..Baseline.Visit.Question.5."] <- "Questionnaires.HADS.Question.5"
names(df)[names(df) == "X.UBIOPRED.Adult_Cohort_.Jan_2019..Questionnaires.Hospital.Anxiety.and.Depression.Scale..HADS..Baseline.Visit.Question.6."] <- "Questionnaires.HADS.Question.6"
names(df)[names(df) == "X.UBIOPRED.Adult_Cohort_.Jan_2019..Questionnaires.Hospital.Anxiety.and.Depression.Scale..HADS..Baseline.Visit.Question.7."] <- "Questionnaires.HADS.Question.7"
names(df)[names(df) == "X.UBIOPRED.Adult_Cohort_.Jan_2019..Questionnaires.Hospital.Anxiety.and.Depression.Scale..HADS..Baseline.Visit.Question.8."] <- "Questionnaires.HADS.Question.8"
names(df)[names(df) == "X.UBIOPRED.Adult_Cohort_.Jan_2019..Questionnaires.Hospital.Anxiety.and.Depression.Scale..HADS..Baseline.Visit.Question.9."] <- "Questionnaires.HADS.Question.9"
names(df)[names(df) == "X.UBIOPRED.Adult_Cohort_.Jan_2019..Questionnaires.Medication.Adherence.Rating.Scale..MARS..Baseline.Visit.Total..Imputed.."] <- "Questionnaires.MARS.Total.Imputed"
names(df)[names(df) == "X.UBIOPRED.Adult_Cohort_.Jan_2019..Questionnaires.SinoNasal.Outcomes.Test..SNOT..Baseline.Visit.Question.10."] <- "Questionnaires.SNOT.Question.10"
names(df)[names(df) == "X.UBIOPRED.Adult_Cohort_.Jan_2019..Questionnaires.SinoNasal.Outcomes.Test..SNOT..Baseline.Visit.Question.11."] <- "Questionnaires.SNOT.Question.11"
names(df)[names(df) == "X.UBIOPRED.Adult_Cohort_.Jan_2019..Questionnaires.SinoNasal.Outcomes.Test..SNOT..Baseline.Visit.Question.13."] <- "Questionnaires.SNOT.Question.13"
names(df)[names(df) == "X.UBIOPRED.Adult_Cohort_.Jan_2019..Questionnaires.SinoNasal.Outcomes.Test..SNOT..Baseline.Visit.Question.14."] <- "uestionnaires.SNOT.Question.14"
names(df)[names(df) == "X.UBIOPRED.Adult_Cohort_.Jan_2019..Questionnaires.SinoNasal.Outcomes.Test..SNOT..Baseline.Visit.Question.16."] <- "Questionnaires.SNOT.Question.16"
names(df)[names(df) == "X.UBIOPRED.Adult_Cohort_.Jan_2019..Questionnaires.SinoNasal.Outcomes.Test..SNOT..Baseline.Visit.Question.17."] <- "Questionnaires.SNOT.Question.17"
names(df)[names(df) == "X.UBIOPRED.Adult_Cohort_.Jan_2019..Questionnaires.SinoNasal.Outcomes.Test..SNOT..Baseline.Visit.Question.18."] <- "Questionnaires.SNOT.Question.18"
names(df)[names(df) == "X.UBIOPRED.Adult_Cohort_.Jan_2019..Questionnaires.SinoNasal.Outcomes.Test..SNOT..Baseline.Visit.Question.19."] <- "Questionnaires.SNOT.Question.19"
names(df)[names(df) == "X.UBIOPRED.Adult_Cohort_.Jan_2019..Questionnaires.SinoNasal.Outcomes.Test..SNOT..Baseline.Visit.Question.1."] <- "Questionnaires.SNOT.Question.1"
names(df)[names(df) == "X.UBIOPRED.Adult_Cohort_.Jan_2019..Questionnaires.SinoNasal.Outcomes.Test..SNOT..Baseline.Visit.Question.20."] <- "Questionnaires.SNOT.Question.20"
names(df)[names(df) == "X.UBIOPRED.Adult_Cohort_.Jan_2019..Questionnaires.SinoNasal.Outcomes.Test..SNOT..Baseline.Visit.Question.2."] <- "Questionnaires.SNOT.Question.2"
names(df)[names(df) == "X.UBIOPRED.Adult_Cohort_.Jan_2019..Questionnaires.SinoNasal.Outcomes.Test..SNOT..Baseline.Visit.Question.3."] <- "Questionnaires.SNOT.Question.3"
names(df)[names(df) == "X.UBIOPRED.Adult_Cohort_.Jan_2019..Questionnaires.SinoNasal.Outcomes.Test..SNOT..Baseline.Visit.Question.4."] <- "Questionnaires.SNOT.Question.4"
names(df)[names(df) == "X.UBIOPRED.Adult_Cohort_.Jan_2019..Questionnaires.SinoNasal.Outcomes.Test..SNOT..Baseline.Visit.Question.5."] <- "Questionnaires.SNOT.Question.5"
names(df)[names(df) == "X.UBIOPRED.Adult_Cohort_.Jan_2019..Questionnaires.SinoNasal.Outcomes.Test..SNOT..Baseline.Visit.Question.6."] <- "Questionnaires.SNOT.Question.6"
names(df)[names(df) == "X.UBIOPRED.Adult_Cohort_.Jan_2019..Questionnaires.SinoNasal.Outcomes.Test..SNOT..Baseline.Visit.Question.7."] <- "Questionnaires.SNOT.Question.7"
names(df)[names(df) == "X.UBIOPRED.Adult_Cohort_.Jan_2019..Questionnaires.SinoNasal.Outcomes.Test..SNOT..Baseline.Visit.Question.8."] <- "Questionnaires.SNOT.Question.8"
names(df)[names(df) == "X.UBIOPRED.Adult_Cohort_.Jan_2019..Questionnaires.SinoNasal.Outcomes.Test..SNOT..Baseline.Visit.Question.9."] <- "Questionnaires.SNOT.Question.9"
####### END QUESTIONNAIRES DATA #######

####### BEG DEMOGRAPHIC DATA #######
names(df)[names(df) == "X.UBIOPRED.Adult_Cohort_.Jan_2019..Demographic.Data.Age."] <- "Demographic.Age"
names(df)[names(df) == "X.UBIOPRED.Adult_Cohort_.Jan_2019..Demographic.Data.Ethnic.origin.Mother."] <- "Demographic.Ethnic.origin.Mother"
names(df)[names(df) == "X.UBIOPRED.Adult_Cohort_.Jan_2019..Demographic.Data.Highest.Level.Education."] <- "Demographic.Highest.Level.Education"
names(df)[names(df) == "X.UBIOPRED.Adult_Cohort_.Jan_2019..Demographic.Data.Marital.Status."] <- "Demographic.Marital.Status"
names(df)[names(df) == "X.UBIOPRED.Adult_Cohort_.Jan_2019..Demographic.Data.Sex."] <- "Demographic.Sex"
names(df)[names(df) == "X.UBIOPRED.Adult_Cohort_.Jan_2019..Demographic.Data.Occupation.occupational.employed."] <- "Demographic.occupational.employed"
names(df)[names(df) == "X.UBIOPRED.Adult_Cohort_.Jan_2019..Demographic.Data.Occupation.occupational.keeping.house."] <- "Demographic.occupational.keeping.house"
names(df)[names(df) == "X.UBIOPRED.Adult_Cohort_.Jan_2019..Demographic.Data.Occupation.occupational.other."] <- "Demographic.occupational.other"
names(df)[names(df) == "X.UBIOPRED.Adult_Cohort_.Jan_2019..Demographic.Data.Occupation.occupational.retired."] <- "Demographic.occupational.retired"
names(df)[names(df) == "X.UBIOPRED.Adult_Cohort_.Jan_2019..Demographic.Data.Occupation.occupational.student."] <- "Demographic.occupational.student"
names(df)[names(df) == "X.UBIOPRED.Adult_Cohort_.Jan_2019..Demographic.Data.Occupation.occupational.unemployed."] <- "Demographic.occupational.unemployed"
names(df)[names(df) == "X.UBIOPRED.Adult_Cohort_.Jan_2019..Demographic.Data.Occupation.occupational.volunteer."] <- "Demographic.occupational.volunteer"
####### END DEMOGRAPHIC DATA #######

####### BEG SUBJECT BODY DATA #######
names(df)[names(df) == "X.UBIOPRED.Adult_Cohort_.Jan_2019..Subject.Body.Measurements.Body.Mass.Index..kg.m2.."] <- "Body.Mass.Index.kg.m2"
names(df)[names(df) == "X.UBIOPRED.Adult_Cohort_.Jan_2019..Subject.Body.Measurements.Height..cm.."] <- "Height.cm"
####### END SUBJECT BODY DATA #######

####### BEG SUBJECT CLUSTERS DATA ####### YOU MIGHT WANT TO REMOVE THOSE
names(df)[names(df) == "X.UBIOPRED.Adult_Cohort_.Jan_2019..Subject.Clusters.Classification.by.eosinophil.count.eosinophils..10.3.per.uL.."] <- "Sub.Cluster.Classification.by.eosinophil.count.eosinophils.10.3.per.uL"
names(df)[names(df) == "X.UBIOPRED.Adult_Cohort_.Jan_2019..Subject.Clusters.Classification.by.eosinophil.count.clinical.cutoff."] <- "Sub.Cluster.Classification.by.eosinophil.count.clinical.cutoff"
names(df)[names(df) == "X.UBIOPRED.Adult_Cohort_.Jan_2019..Subject.Clusters.Classification.by.eosinophil.count.cohort.eosinophil."] <- "Sub.Cluster.Classification.by.eosinophil.count.cohort.eosinophil"
names(df)[names(df) == "X.UBIOPRED.Adult_Cohort_.Jan_2019..Subject.Clusters.Classification.by.eosinophil.count.eosinophil.clinical.level."] <- "Sub.Cluster.Classification.by.eosinophil.count.eosinophil.clinical.level"
names(df)[names(df) == "X.UBIOPRED.Adult_Cohort_.Jan_2019..Subject.Clusters.Classification.by.eosinophil.count.tertile.cutoff."] <- "Sub.Cluster.Classification.by.eosinophil.count.tertile.cutoff"
####### END SUBJECT CLUSTERS DATA #######

####### END SUBJECT HISTORY DATA #######
names(df)[names(df) == "X.UBIOPRED.Adult_Cohort_.Jan_2019..Subject.History.Respiratory.History.Onset.OR.First.Diagnosis.Age..years.."] <- "Respiratory.History.Onset.OR.First.Diagnosis.Age.years"
names(df)[names(df) == "X.UBIOPRED.Adult_Cohort_.Jan_2019..Subject.History.Medical.or.Surgical.History.Allergic.Rhinitis.Diagnosed."] <- "Allergic.Rhinitis.Diagnosed"
names(df)[names(df) == "X.UBIOPRED.Adult_Cohort_.Jan_2019..Subject.History.Medical.or.Surgical.History.Congestive.Diagnosed."] <- "Congestive.Diagnosed"
names(df)[names(df) == "X.UBIOPRED.Adult_Cohort_.Jan_2019..Subject.History.Medical.or.Surgical.History.Coronary.Diagnosed."] <- "Coronary.Diagnosed"
names(df)[names(df) == "X.UBIOPRED.Adult_Cohort_.Jan_2019..Subject.History.Medical.or.Surgical.History.Diabetes.Diagnosed."] <- "Diabetes.Diagnosed"
names(df)[names(df) == "X.UBIOPRED.Adult_Cohort_.Jan_2019..Subject.History.Medical.or.Surgical.History.Eczema.Diagnosed."] <- "Eczema.Diagnosed"
names(df)[names(df) == "X.UBIOPRED.Adult_Cohort_.Jan_2019..Subject.History.Medical.or.Surgical.History.Gerd.Diagnosed."] <- "Gerd.Diagnosed"
names(df)[names(df) == "X.UBIOPRED.Adult_Cohort_.Jan_2019..Subject.History.Medical.or.Surgical.History.Hay.Fever.Diagnosed."] <- "Hay.Fever.Diagnosed"
names(df)[names(df) == "X.UBIOPRED.Adult_Cohort_.Jan_2019..Subject.History.Medical.or.Surgical.History.Hypertension.Diagnosed."] <- "Hypertension.Diagnosed"
names(df)[names(df) == "X.UBIOPRED.Adult_Cohort_.Jan_2019..Subject.History.Medical.or.Surgical.History.Nasal.Polyps.Diagnosed."] <- "Nasal.Polyps.Diagnosed"
names(df)[names(df) == "X.UBIOPRED.Adult_Cohort_.Jan_2019..Subject.History.Medical.or.Surgical.History.Non.Allergic.Rhinitis.Diagnosed."] <- "Non.Allergic.Rhinitis.Diagnosed"
names(df)[names(df) == "X.UBIOPRED.Adult_Cohort_.Jan_2019..Subject.History.Medical.or.Surgical.History.Osteoporosis.Diagnosed."] <- "Osteoporosis.Diagnosed"
names(df)[names(df) == "X.UBIOPRED.Adult_Cohort_.Jan_2019..Subject.History.Medical.or.Surgical.History.Psychiatric.Disease.Diagnosed."] <- "Psychiatric.Disease.Diagnosed"
names(df)[names(df) == "X.UBIOPRED.Adult_Cohort_.Jan_2019..Subject.History.Medical.or.Surgical.History.Sinus.Surgery.Diagnosed."] <- "Sinus.Surgery.Diagnosed"
names(df)[names(df) == "X.UBIOPRED.Adult_Cohort_.Jan_2019..Subject.History.Medical.or.Surgical.History.Sinusitis.Diagnosed."] <- "Sinusitis.Diagnosed"
names(df)[names(df) == "X.UBIOPRED.Adult_Cohort_.Jan_2019..Subject.History.Medical.or.Surgical.History.Vocal.Chord.Diagnosed."] <- "Vocal.Chord.Diagnosed"
names(df)[names(df) == "X.UBIOPRED.Adult_Cohort_.Jan_2019..Subject.History.Parental.Asthma."] <- "Parental.Asthma"
names(df)[names(df) == "X.UBIOPRED.Adult_Cohort_.Jan_2019..Subject.History.Parental.Copd."] <- "Parental.Copd"
names(df)[names(df) == "X.UBIOPRED.Adult_Cohort_.Jan_2019..Subject.History.Parental.Eczema."] <- "Parental.Eczema"
names(df)[names(df) == "X.UBIOPRED.Adult_Cohort_.Jan_2019..Subject.History.Respiratory.History.Breathing.Problems.in.12.months.prior.to.screening."] <- "Breathing.Problems.in.12.months.prior.to.screening"
names(df)[names(df) == "X.UBIOPRED.Adult_Cohort_.Jan_2019..Subject.History.Respiratory.History.Bronchitis."] <- "Bronchitis"
names(df)[names(df) == "X.UBIOPRED.Adult_Cohort_.Jan_2019..Subject.History.Respiratory.History.Chronic.Bronchitis."] <- "Chronic.Bronchitis"
names(df)[names(df) == "X.UBIOPRED.Adult_Cohort_.Jan_2019..Subject.History.Respiratory.History.Emphysema.Or.Copd."] <- "Emphysema.Or.Copd"
names(df)[names(df) == "X.UBIOPRED.Adult_Cohort_.Jan_2019..Subject.History.Respiratory.History.Icu.Admission.Last.Year."] <- "Icu.Admission.Last.Year"
names(df)[names(df) == "X.UBIOPRED.Adult_Cohort_.Jan_2019..Subject.History.Respiratory.History.Icu.Admission."] <- "Icu.Admission"
names(df)[names(df) == "X.UBIOPRED.Adult_Cohort_.Jan_2019..Subject.History.Respiratory.History.Intubation."] <- "Intubation"
names(df)[names(df) == "X.UBIOPRED.Adult_Cohort_.Jan_2019..Subject.History.Respiratory.History.Overnight.Treatment."] <- "Overnight.Treatment"
names(df)[names(df) == "X.UBIOPRED.Adult_Cohort_.Jan_2019..Subject.History.Respiratory.History.Pneumonia."] <- "Pneumonia"
names(df)[names(df) == "X.UBIOPRED.Adult_Cohort_.Jan_2019..Subject.History.Respiratory.History.Rescue.Inhaler.Use."] <- "Rescue.Inhaler.Use"
names(df)[names(df) == "X.UBIOPRED.Adult_Cohort_.Jan_2019..Subject.History.Respiratory.History.Supplemental.Oxygen."] <- "Supplemental.Oxygen"
names(df)[names(df) == "X.UBIOPRED.Adult_Cohort_.Jan_2019..Subject.History.Respiratory.History.Visited.a.ER.at.any.time.because.of.breathing.problems."] <- "Visited.a.ER.at.any.time.because.of.breathing.problems"
names(df)[names(df) == "X.UBIOPRED.Adult_Cohort_.Jan_2019..Subject.History.Respiratory.History.Onset.Breathing.Problems.Uncertain."] <- "Onset.Breathing.Problems.Uncertain"
names(df)[names(df) == "X.UBIOPRED.Adult_Cohort_.Jan_2019..Subject.History.Respiratory.History.Onset.OR.First.Diagnosis.Uncertain."] <- "Onset.OR.First.Diagnosis.Uncertain"
####### END SUBJECT HISTORY DATA #######

####### BEG VISIT INFORMATION DATA #######
names(df)[names(df) == "X.UBIOPRED.Adult_Cohort_.Jan_2019..Visit.Information.Baseline.Visit.Days.Since.Screening..Baseline.Day.1.."] <- "Days.Since.Screening.Baseline.Day.1"
names(df)[names(df) == "X.UBIOPRED.Adult_Cohort_.Jan_2019..Visit.Information.Baseline.Visit.Days.Since.Screening..Baseline.Day.2.."] <- "Days.Since.Screening.Baseline.Day.2"
####### END VISIT INFORMATION DATA #######

####### BEG ASTHMA DIAGNOSIS DATA #######
names(df)[names(df) == "X.UBIOPRED.Adult_Cohort_.Jan_2019..Asthma.Diagnosis.Diagnosis.Fev.Variation."] <- "Asthma.Diag.Fev.Variation"
names(df)[names(df) == "X.UBIOPRED.Adult_Cohort_.Jan_2019..Asthma.Diagnosis.Diagnosis.Pef.Variation."] <- "Asthma.Diag.Pef.Variation"
names(df)[names(df) == "X.UBIOPRED.Adult_Cohort_.Jan_2019..Asthma.Diagnosis.Historic.Test."] <- "Asthma.Diag.Historic.Test"
####### END ASTHMA DIAGNOSIS DATA #######

####### BEG EXPOSURES AND TRIGGERS DATA #######
names(df)[names(df) == "X.UBIOPRED.Adult_Cohort_.Jan_2019..Atopy.Exposures.and.Triggers.Baseline.Aspirin."] <- "Expo.Aspirin"
names(df)[names(df) == "X.UBIOPRED.Adult_Cohort_.Jan_2019..Atopy.Exposures.and.Triggers.Baseline.Barns."] <- "Expo.Barns"
names(df)[names(df) == "X.UBIOPRED.Adult_Cohort_.Jan_2019..Atopy.Exposures.and.Triggers.Baseline.Cold.Air."] <- "Expo.Cold.Air"
names(df)[names(df) == "X.UBIOPRED.Adult_Cohort_.Jan_2019..Atopy.Exposures.and.Triggers.Baseline.Dust."] <- "Expo.Dust"
names(df)[names(df) == "X.UBIOPRED.Adult_Cohort_.Jan_2019..Atopy.Exposures.and.Triggers.Baseline.Fungus."] <- "Expo.Fungus"
names(df)[names(df) == "X.UBIOPRED.Adult_Cohort_.Jan_2019..Atopy.Exposures.and.Triggers.Baseline.Menstrual.Cycle."] <- "Expo.Menstrual.Cycle"
names(df)[names(df) == "X.UBIOPRED.Adult_Cohort_.Jan_2019..Atopy.Exposures.and.Triggers.Baseline.Perfumes."] <- "Expo.Perfumes"
names(df)[names(df) == "X.UBIOPRED.Adult_Cohort_.Jan_2019..Atopy.Exposures.and.Triggers.Baseline.Pets."] <- "Expo.Pets"
names(df)[names(df) == "X.UBIOPRED.Adult_Cohort_.Jan_2019..Atopy.Exposures.and.Triggers.Baseline.Physical.Exercise."] <- "Expo.Physical.Exercise"
names(df)[names(df) == "X.UBIOPRED.Adult_Cohort_.Jan_2019..Atopy.Exposures.and.Triggers.Baseline.Pollen."] <- "Expo.Pollen"
names(df)[names(df) == "X.UBIOPRED.Adult_Cohort_.Jan_2019..Atopy.Exposures.and.Triggers.Baseline.Pollutants."] <- "Expo.Pollutants"
names(df)[names(df) == "X.UBIOPRED.Adult_Cohort_.Jan_2019..Atopy.Exposures.and.Triggers.Baseline.Respiratory.Infections."] <- "Expo.Respiratory.Infections"
names(df)[names(df) == "X.UBIOPRED.Adult_Cohort_.Jan_2019..Atopy.Exposures.and.Triggers.Baseline.Routine.Physical.Activities."] <- "Expo.Routine.Physical.Activities"
names(df)[names(df) == "X.UBIOPRED.Adult_Cohort_.Jan_2019..Atopy.Exposures.and.Triggers.Baseline.Stress."] <- "Expo.Stress"
names(df)[names(df) == "X.UBIOPRED.Adult_Cohort_.Jan_2019..Atopy.Exposures.and.Triggers.Baseline.Wood.Smoke."] <- "Expo.Wood.Smoke"
names(df)[names(df) == "X.UBIOPRED.Adult_Cohort_.Jan_2019..Atopy.Exposures.and.Triggers.Baseline.Other1."] <- "Expo.Baseline.Other1"
names(df)[names(df) == "X.UBIOPRED.Adult_Cohort_.Jan_2019..Atopy.Exposures.and.Triggers.Baseline.Other2."] <- "Expo.Baseline.Other2"
names(df)[names(df) == "X.UBIOPRED.Adult_Cohort_.Jan_2019..Atopy.Exposures.and.Triggers.Baseline.Other3."] <- "Expo.Baseline.Other3"
names(df)[names(df) == "X.UBIOPRED.Adult_Cohort_.Jan_2019..Atopy.Exposures.and.Triggers.Baseline.Other4."] <- "Expo.Baseline.Other4"
####### END EXPOSURES AND TRIGGERS DATA #######

####### BEG MEDICATION DATA #######
names(df)[names(df) == "X.UBIOPRED.Adult_Cohort_.Jan_2019..Medication.Baseline.Anti.IgE.Therapy."] <- "Medication.Anti.IgE.Therapy"
names(df)[names(df) == "X.UBIOPRED.Adult_Cohort_.Jan_2019..Medication.Baseline.Antibiotic.Therapy."] <- "Medication.Antibiotic.Therapy"
names(df)[names(df) == "X.UBIOPRED.Adult_Cohort_.Jan_2019..Medication.Baseline.Cromones."] <- "Medication.Cromones"
names(df)[names(df) == "X.UBIOPRED.Adult_Cohort_.Jan_2019..Medication.Baseline.Immunotherapy."] <- "Medication.Immunotherapy"
names(df)[names(df) == "X.UBIOPRED.Adult_Cohort_.Jan_2019..Medication.Baseline.Inhaled.Combinations."] <- "Medication.Inhaled.Combinations"
names(df)[names(df) == "X.UBIOPRED.Adult_Cohort_.Jan_2019..Medication.Baseline.Inhaled.Corticosteroids."] <- "Medication.Inhaled.Corticosteroids"
names(df)[names(df) == "X.UBIOPRED.Adult_Cohort_.Jan_2019..Medication.Baseline.Injectable.Corticosteroids."] <- "Medication.Injectable.Corticosteroids"
names(df)[names(df) == "X.UBIOPRED.Adult_Cohort_.Jan_2019..Medication.Baseline.Leukotriene.Modifiers."] <- "Medication.Leukotriene.Modifiers"
names(df)[names(df) == "X.UBIOPRED.Adult_Cohort_.Jan_2019..Medication.Baseline.Long.Acting.Anticholinergics."] <- "Medication.Long.Acting.Anticholinergics"
names(df)[names(df) == "X.UBIOPRED.Adult_Cohort_.Jan_2019..Medication.Baseline.Long.Acting.Beta.Agonist."] <- "Medication.Long.Acting.Beta.Agonist"
names(df)[names(df) == "X.UBIOPRED.Adult_Cohort_.Jan_2019..Medication.Baseline.Mucolytics."] <- "Medication.Mucolytics"
names(df)[names(df) == "X.UBIOPRED.Adult_Cohort_.Jan_2019..Medication.Baseline.Oral.Corticosteroids."] <- "Medication.Oral.Corticosteroids"
names(df)[names(df) == "X.UBIOPRED.Adult_Cohort_.Jan_2019..Medication.Baseline.Short.Acting.Anticholinergics."] <- "Medication.Short.Acting.Anticholinergics"
names(df)[names(df) == "X.UBIOPRED.Adult_Cohort_.Jan_2019..Medication.Baseline.Short.Acting.Beta.Agonist."] <- "Medication.Short.Acting.Beta.Agonist"
names(df)[names(df) == "X.UBIOPRED.Adult_Cohort_.Jan_2019..Medication.Baseline.Short.Acting.Combination."] <- "Medication.Short.Acting.Combination"
names(df)[names(df) == "X.UBIOPRED.Adult_Cohort_.Jan_2019..Medication.Baseline.Theophilline."] <- "Medication.Theophilline"
####### END MEDICATION DATA #######

####### BEG PHYSICAL EXAMINATION DATA #######
names(df)[names(df) == "X.UBIOPRED.Adult_Cohort_.Jan_2019..Physical.Examination.Baseline.Day.1.Physical.Examination.Performed."] <- "PE.Performed"
names(df)[names(df) == "X.UBIOPRED.Adult_Cohort_.Jan_2019..Physical.Examination.Screening.Bronchial.Finding."] <- "PE.Screening.Bronchial.Finding"
names(df)[names(df) == "X.UBIOPRED.Adult_Cohort_.Jan_2019..Physical.Examination.Screening.Diminished.Finding."] <- "PE.Screening.Diminished.Finding"
names(df)[names(df) == "X.UBIOPRED.Adult_Cohort_.Jan_2019..Physical.Examination.Screening.Dullness.Finding."] <- "PE.Screening.Dullness.Finding"
names(df)[names(df) == "X.UBIOPRED.Adult_Cohort_.Jan_2019..Physical.Examination.Screening.Physical.Examination.Performed."] <- "PE.Screening.Performed"
names(df)[names(df) == "X.UBIOPRED.Adult_Cohort_.Jan_2019..Physical.Examination.Screening.Rales.Finding."] <- "PE.Screening.Rales.Finding"
names(df)[names(df) == "X.UBIOPRED.Adult_Cohort_.Jan_2019..Physical.Examination.Screening.Squeak.Finding."] <- "PE.Screening.Squeak.Finding"
names(df)[names(df) == "X.UBIOPRED.Adult_Cohort_.Jan_2019..Physical.Examination.Screening.Wheeze.Finding."] <- "PE.Screening.Wheeze.Finding"
####### END PHYSICAL EXAMINATION DATA #######

####### BEG QUESTIONS DATA #######
names(df)[names(df) == "X.UBIOPRED.Adult_Cohort_.Jan_2019..Questions.Baseline.Day.1.Ec.Respiratory.Infection."] <- "Questions.Ec.Respiratory.Infection"
names(df)[names(df) == "X.UBIOPRED.Adult_Cohort_.Jan_2019..Questions.Baseline.Day.1.Informed.Consent.Genetic."] <- "Questions.Informed.Consent.Genetic"
names(df)[names(df) == "X.UBIOPRED.Adult_Cohort_.Jan_2019..Questions.Baseline.Day.1.Mars.Completed."] <- "Questions.Mars.Completed"
names(df)[names(df) == "X.UBIOPRED.Adult_Cohort_.Jan_2019..Questions.Baseline.Day.1.Medication.Since.Screening."] <- "Questions.Medication.Since.Screening"
names(df)[names(df) == "X.UBIOPRED.Adult_Cohort_.Jan_2019..Questions.Baseline.Day.1.Serious.Adverse.Event.Since.Screening."] <- "Questions.Serious.Adverse.Event.Since.Screening"
names(df)[names(df) == "X.UBIOPRED.Adult_Cohort_.Jan_2019..Questions.Screening.Environmental.Factors.Enviromental.Bird."] <- "Questions.Screening.Env.Factors.Bird"
names(df)[names(df) == "X.UBIOPRED.Adult_Cohort_.Jan_2019..Questions.Screening.Environmental.Factors.Enviromental.Cat."] <- "Questions.Screening.Env.Factors.Cat"
names(df)[names(df) == "X.UBIOPRED.Adult_Cohort_.Jan_2019..Questions.Screening.Environmental.Factors.Enviromental.Dog."] <- "Questions.Screening.Env.Factors.Dog"
names(df)[names(df) == "X.UBIOPRED.Adult_Cohort_.Jan_2019..Questions.Screening.Environmental.Factors.Enviromental.Rodent."] <- "Questions.Screening.Env.Factors.Rodent"
names(df)[names(df) == "X.UBIOPRED.Adult_Cohort_.Jan_2019..Questions.Screening.Family.History.Asthma.Father."] <- "Questions.Screening.Asthma.Father"
names(df)[names(df) == "X.UBIOPRED.Adult_Cohort_.Jan_2019..Questions.Screening.Family.History.Asthma.Mother."] <- "Questions.Screening.Asthma.Mother"
names(df)[names(df) == "X.UBIOPRED.Adult_Cohort_.Jan_2019..Questions.Screening.Family.History.Asthma.Sibling."] <- "Questions.Screening.Asthma.Sibling"
names(df)[names(df) == "X.UBIOPRED.Adult_Cohort_.Jan_2019..Questions.Screening.Family.History.Cardiovascular.Father."] <- "Questions.Screening.Cardiovascular.Father"
names(df)[names(df) == "X.UBIOPRED.Adult_Cohort_.Jan_2019..Questions.Screening.Family.History.Cardiovascular.Mother."] <- "Questions.Screening.Cardiovascular.Mother"
names(df)[names(df) == "X.UBIOPRED.Adult_Cohort_.Jan_2019..Questions.Screening.Family.History.Cardiovascular.Sibling."] <- "Questions.Screening.Cardiovascular.Sibling"
names(df)[names(df) == "X.UBIOPRED.Adult_Cohort_.Jan_2019..Questions.Screening.Family.History.Copd.Father."] <- "Questions.Screening.Copd.Father"
names(df)[names(df) == "X.UBIOPRED.Adult_Cohort_.Jan_2019..Questions.Screening.Family.History.Copd.Mother."] <- "Questions.Screening.Copd.Mother"
names(df)[names(df) == "X.UBIOPRED.Adult_Cohort_.Jan_2019..Questions.Screening.Family.History.Copd.Sibling."] <- "Questions.Screening.Copd.Sibling"
names(df)[names(df) == "X.UBIOPRED.Adult_Cohort_.Jan_2019..Questions.Screening.Family.History.Diabetes.Sibling."] <- "Questions.Screening.Diabetes.Sibling"
names(df)[names(df) == "X.UBIOPRED.Adult_Cohort_.Jan_2019..Questions.Screening.Family.History.Eczema.Mother."] <- "Questions.Screening.Eczema.Mother"
names(df)[names(df) == "X.UBIOPRED.Adult_Cohort_.Jan_2019..Questions.Screening.Family.History.Eczema.Sibling."] <- "Questions.Screening.Eczema.Sibling"
names(df)[names(df) == "X.UBIOPRED.Adult_Cohort_.Jan_2019..Questions.Screening.Family.History.Hay.Fever.Father."] <- "Questions.Screening.Hay.Fever.Father"
names(df)[names(df) == "X.UBIOPRED.Adult_Cohort_.Jan_2019..Questions.Screening.Family.History.Hay.Fever.Mother."] <- "Questions.Screening.Hay.Fever.Mother"
names(df)[names(df) == "X.UBIOPRED.Adult_Cohort_.Jan_2019..Questions.Screening.Family.History.Hay.Fever.Sibling."] <- "Questions.Screening.Hay.Fever.Sibling"
names(df)[names(df) == "X.UBIOPRED.Adult_Cohort_.Jan_2019..Questions.Screening.Family.History.Inflammatory.Disease.Father."] <- "Questions.Screening.Inflammatory.Disease.Father"
names(df)[names(df) == "X.UBIOPRED.Adult_Cohort_.Jan_2019..Questions.Screening.Family.History.Inflammatory.Disease.Mother."] <- "Questions.Screening.Inflammatory.Disease.Mother"
names(df)[names(df) == "X.UBIOPRED.Adult_Cohort_.Jan_2019..Questions.Screening.Family.History.Inflammatory.Disease.Sibling."] <- "Questions.Screening.Inflammatory.Disease.Sibling"
names(df)[names(df) == "X.UBIOPRED.Adult_Cohort_.Jan_2019..Questions.Screening.Food.Allergies.Food.Allergy."] <- "Questions.Screening.Food.Allergy"
names(df)[names(df) == "X.UBIOPRED.Adult_Cohort_.Jan_2019..Questions.Screening.Informed.Consent.Informed.Consent.Genetic."] <- "Questions.Screening.Informed.Consent.Genetic"
names(df)[names(df) == "X.UBIOPRED.Adult_Cohort_.Jan_2019..Questions.Screening.Residential.Location."] <- "Questions.Screening.Residential.Location"
names(df)[names(df) == "X.UBIOPRED.Adult_Cohort_.Jan_2019..Questions.Screening.Serious.Adverse.Event.Since.Consent."] <- "Questions.Screening.Serious.Adverse.Event.Since.Consent"
names(df)[names(df) == "X.UBIOPRED.Adult_Cohort_.Jan_2019..Questions.Screening.Smoking.History.Second.Hand.Smoke."] <- "Questions.Screening.Second.Hand.Smoke"
names(df)[names(df) == "X.UBIOPRED.Adult_Cohort_.Jan_2019..Questions.Screening.Smoking.History.Smokeless.Tobacco.Status."] <- "Questions.Screening.Smokeless.Tobacco.Status"
names(df)[names(df) == "X.UBIOPRED.Adult_Cohort_.Jan_2019..Questions.Baseline.Day.1.Pregnancy.Test.Na."] <- "Questions.Pregnancy.Test.Na"
names(df)[names(df) == "X.UBIOPRED.Adult_Cohort_.Jan_2019..Questions.Baseline.Day.1.Urinary.Cotinine.Not.Done."] <- "Questions.Urinary.Cotinine.Not.Done"
names(df)[names(df) == "X.UBIOPRED.Adult_Cohort_.Jan_2019..Questions.Screening.Environmental.Factors.Enviromental.Other.1."] <- "Questions.Screening.Env.Factors.Other.1"
names(df)[names(df) == "X.UBIOPRED.Adult_Cohort_.Jan_2019..Questions.Screening.Environmental.Factors.Enviromental.Other.2."] <- "Questions.Screening.Env.Factors.Other.2"
names(df)[names(df) == "X.UBIOPRED.Adult_Cohort_.Jan_2019..Questions.Screening.Environmental.Factors.Enviromental.Other.3."] <- "Questions.Screening.Env.Factors.Other.3"
names(df)[names(df) == "X.UBIOPRED.Adult_Cohort_.Jan_2019..Questions.Screening.Family.History.Number.Of.Children."] <- "Questions.Screening.Number.Of.Children"
names(df)[names(df) == "X.UBIOPRED.Adult_Cohort_.Jan_2019..Questions.Screening.Family.History.Number.Of.Siblings."] <- "Questions.Screening.Number.Of.Siblings"
names(df)[names(df) == "X.UBIOPRED.Adult_Cohort_.Jan_2019..Questions.Screening.Smoking.History.Smoking.Cigarettes."] <- "Questions.Screening.Smoking.Cigarettes"
names(df)[names(df) == "X.UBIOPRED.Adult_Cohort_.Jan_2019..Questions.Screening.Smoking.History.Smoking.Cigars."] <- "Questions.Screening.Smoking.Cigars"
names(df)[names(df) == "X.UBIOPRED.Adult_Cohort_.Jan_2019..Questions.Screening.Smoking.History.Smoking.Pipe."] <- "Questions.Screening.Smoking.Pipe"
####### END QUESTIONS DATA #######

####### BEG EXACERBATIONS DATA #######
names(df)[names(df) == "X.UBIOPRED.Adult_Cohort_.Jan_2019..Exacerbations.Recent.Asthma.Exacerbation.History.Baseline.Exacerbation.Per.Year."] <- "Exacerbation.Per.Year."
####### END EXACERBATIONS DATA #######

##Make a copy of the dataframe in order to run k_proto
df_2 = data.table::copy(df)

#Get list of variables
data.frame(colnames(df_2))

#Remove the variables linked to drugs and medication in df_2. We don't want consequences of the disease to influence the clustering.
#Remove subject cluster information too (not relevant)
#Remove visit information too, does not seem relevant neither
df_2 <- df_2 %>%
  dplyr::select(-starts_with("Omics.Drugs."), -starts_with("Medication."),-starts_with("Sub.Cluster."))

df_2 = dplyr::select(df_2, -c(Days.Since.Screening.Baseline.Day.1,Days.Since.Screening.Baseline.Day.2))

#Check variable type for df_2
lapply(df_2,class)

#######NO NEED TO RUN BECAUSE FACTOR VARIABLES HAVE BEEN TRNSFORMED TO INTEGER AFTER LABEL ENCODER#####
#Transform variable type for all variables from position 285 to 363 in df_2
list_to_transform <- dplyr::select(df_2,177:363)
list <- names(list_to_transform)
df_2[,list] <- lapply(df_2[,list],factor)
####################################################

#Check variable type for df_2 once again
lapply(df_2,class)

#Check at missing values in df (numerical and integers shall have 0 missingness)
sapply(df_2, function(x) sum(is.na(x)))
#No missingness

#Check data tendency to cluster for entire dataset, not removing any variable (especially keeping OMICS, medication, subject cluster)
lapply(df,class)
list_to_transform_df <- dplyr::select(df,205:411)
list_df <- names(list_to_transform_df)
df[,list_df] <- lapply(df[,list_df],factor)

#Remove those 2 useless variables in terms of information it can provide
df = dplyr::select(df, -c(Days.Since.Screening.Baseline.Day.1,Days.Since.Screening.Baseline.Day.2))

#Get numerical variables in df
num_cols_df <- unlist(lapply(df, is.numeric))
data_num_df <- df[ , num_cols_df] 
# Compute Hopkins statistic for df dataset
res_df <- factoextra::get_clust_tendency(data_num_df, n = nrow(data_num_df)-1, graph = TRUE)
res_df$hopkins_stat
res_df$plot

#Check data tendency to cluster for df_2_prot (no omics.drugs, no medication, no)
num_cols <- unlist(lapply(df_2_prot, is.numeric))
data_num <- df_2_prot[ , num_cols] 
library(factoextra)
# Compute Hopkins statistic for df_2_prot dataset
res <- factoextra::get_clust_tendency(data_num, n = nrow(data_num)-1, graph = TRUE)
res$hopkins_stat
res$plot


# Random data generated from data_num data set
random_df <- apply(data_num, 2, 
                   function(x){runif(length(x), min(x), (max(x)))})
random_df <- as.data.frame(random_df)
# Compute Hopkins statistic for a random dataset
res_random <- get_clust_tendency(random_df, n = nrow(random_df)-1,
                                 graph = TRUE)
res_random$hopkins_stat
res_random$plot
##Good to go
#Make a copy of final dataframe that you save and then remove X (Patient ID) to run k_proto
df_3 = data.table::copy(df_2)

#Save dataset for k-prototypes
setwd("/rds/general/user/md2620/home/asthma/Malo/Dataframes")
write.csv(df_3,"K_proto_stand_removal_of_medication.csv")

