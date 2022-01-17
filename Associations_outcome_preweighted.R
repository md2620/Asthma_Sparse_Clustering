### INFORMATION GAIN TECHNIQUE FOR SUPERVISING CLUSTERS ###

#Clear environment
rm(list=ls())

#Reading the files - Only numerical variables as it is a KMeans algorithm
df_pre = read.csv("/rds/general/user/md2620/home/asthma/Malo/Dataframes/Final/df_preweighted_KMeans_with_results.csv")

#Define your controls (tests used and measures we look at)
mycontrols  <- tableby.control(test=TRUE, total=FALSE,
                                       numeric.test="anova", cat.test="chisq",
                                       numeric.stats=c("Nmiss","meansd"),
                                       cat.stats=c("countpct"),
                                       stats.labels=list(Nmiss="missing", meansd='Mean(sd)'))

#### PRIMARY RESULTS ####
#Summary table for biomarkers per cluster - only numerical variables
assoc_prim_outcome <- summary(sort(tableby(prim_K3 ~ cohort ,data=df_pre, control = mycontrols , simulate.p.value=TRUE, B=500, digits = 1),decreasing=FALSE),title="Association test between primary clusters (K=3) and asthma severity", pfootnote=TRUE)

#Write tab_biomarkers to an HTML document
write2pdf(assoc_prim_outcome, "/rds/general/user/md2620/home/asthma/Malo/Results/assoc_preweighted/prim_K_3_outcome.pdf")

#### SECONDARY RESULTS ####
#Summary table for biomarkers per cluster - only numerical variables
assoc_sec_outcome <- summary(sort(tableby(sec_K3 ~ cohort ,data=df_pre, control = mycontrols , simulate.p.value=TRUE, B=500, digits = 1),decreasing=FALSE),title="Association test between secondary clusters (K=3) and asthma severity", pfootnote=TRUE)

#Write tab_biomarkers to an HTML document
write2pdf(assoc_sec_outcome, "/rds/general/user/md2620/home/asthma/Malo/Results/assoc_preweighted/sec_K_3_outcome.pdf")

#### TERTIARY RESULTS ####
#Summary table for biomarkers per cluster - only numerical variables
assoc_tert_outcome <- summary(sort(tableby(tert_K3 ~ cohort ,data=df_pre, control = mycontrols , simulate.p.value=TRUE, B=500, digits = 1),decreasing=FALSE),title="Association test between tertiary clusters (K=3) and asthma severity", pfootnote=TRUE)

#Write tab_biomarkers to an HTML document
write2pdf(assoc_tert_outcome, "/rds/general/user/md2620/home/asthma/Malo/Results/assoc_preweighted/tert_K_3_outcome.pdf")


##### INVERSE COHORT AND CLUSTERS

#### PRIMARY RESULTS ####
#Summary table for biomarkers per cluster - only numerical variables
assoc_prim_outcome_1line <- summary(sort(tableby(cohort ~ prim_K3 ,data=df_pre, control = mycontrols , simulate.p.value=TRUE, B=500, digits = 1),decreasing=FALSE),title="Association test between primary clusters (K=3) and asthma severity", pfootnote=TRUE)

#Write tab_biomarkers to an HTML document
write2html(assoc_prim_outcome_1line, "/rds/general/user/md2620/home/asthma/Malo/Results/assoc_preweighted/prim_K_3_outcome_1line.html")

#### SECONDARY RESULTS ####
#Summary table for biomarkers per cluster - only numerical variables
assoc_sec_outcome_1line <- summary(sort(tableby(cohort ~ sec_K3  ,data=df_pre, control = mycontrols , simulate.p.value=TRUE, B=500, digits = 1),decreasing=FALSE),title="Association test between secondary clusters (K=3) and asthma severity", pfootnote=TRUE)

#Write tab_biomarkers to an HTML document
write2html(assoc_sec_outcome_1line, "/rds/general/user/md2620/home/asthma/Malo/Results/assoc_preweighted/sec_K_3_outcome_1line.html")

#### TERTIARY RESULTS ####
#Summary table for biomarkers per cluster - only numerical variables
assoc_tert_outcome_1line <- summary(sort(tableby(cohort ~ tert_K3 ,data=df_pre, control = mycontrols , simulate.p.value=TRUE, B=500, digits = 1),decreasing=FALSE),title="Association test between tertiary clusters (K=3) and asthma severity", pfootnote=TRUE)

#Write tab_biomarkers to an HTML document
write2html(assoc_tert_outcome_1line, "/rds/general/user/md2620/home/asthma/Malo/Results/assoc_preweighted/tert_K_3_outcome_1line.html")

