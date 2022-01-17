HTML_sup = read.csv("/rds/general/user/md2620/home/asthma/Malo/Dataframes/df_HTML_not_processed_HTML_tables.csv")

dataset_sup = read.csv("/rds/general/user/md2620/home/asthma/Malo/Dataframes/Final/sup_df.csv")

#Get list of variables in HTML_sup
data.frame(colnames(HTML_sup))

#Drop a few variables we are not interested about in HTML_sup
HTML_sup = subset(HTML_sup, select = -c(Onset.Breathing.Problems.Uncertain,
                                                              Onset.OR.First.Diagnosis.Uncertain,
                                                              Clinical.Screening.eosinophils.x10.3.uL,
                                                              Clinical.Screening.Neutrophils,
                                                              Demographic.Age,
                                                              Demographic.Ethnic.origin.Mother,
                                                              Demographic.Sex,cluster))


#Append clusters to HTML_sup
HTML_sup$K_2 <- factor(dataset_sup$cluster_K_2)
HTML_sup$K_3 <- factor(dataset_sup$cluster_K_3)
HTML_sup$K_4 <- factor(dataset_sup$cluster_K_4)
HTML_sup$K_5 <- factor(dataset_sup$cluster_K_5)
HTML_sup$K_6 <- factor(dataset_sup$cluster_K_6)
HTML_sup$K_7 <- factor(dataset_sup$cluster_K_7)
HTML_sup$K_8 <- factor(dataset_sup$cluster_K_8)
HTML_sup$K_9 <- factor(dataset_sup$cluster_K_9)
HTML_sup$K_10 <- factor(dataset_sup$cluster_K_10)
HTML_sup$K_11<- factor(dataset_sup$cluster_K_11)
HTML_sup$K_12 <- factor(dataset_sup$cluster_K_12)
HTML_sup$K_13 <- factor(dataset_sup$cluster_K_13)
HTML_sup$K_14 <- factor(dataset_sup$cluster_K_14)
HTML_sup$K_15 <- factor(dataset_sup$cluster_K_15)


####### Write a csv file now that your data is ready to be analyzed
setwd("/rds/general/user/md2620/home/asthma/Malo/Dataframes/Final")
write.csv(HTML_sup,"HTML_sup.csv")


################ DO THE SAME FOR THE UNSUPERVISED CLUSTERS #################
HTML_unsup = read.csv("/rds/general/user/md2620/home/asthma/Malo/Dataframes/df_HTML_not_processed_HTML_tables.csv")

#Drop a few variables we are not interested about in HTML_sup
HTML_unsup = subset(HTML_unsup, select = -c(Onset.Breathing.Problems.Uncertain,
                                        Onset.OR.First.Diagnosis.Uncertain,
                                        Clinical.Screening.eosinophils.x10.3.uL,
                                        Clinical.Screening.Neutrophils,
                                        Demographic.Age,
                                        Demographic.Ethnic.origin.Mother,
                                        Demographic.Sex,cluster))

#Read unsupervised df with unsupervised clusters to attach to HTML_unsup
dataset_unsup = read.csv("/rds/general/user/md2620/home/asthma/Malo/Dataframes/Final/unsup_df.csv")

#Append clusters to HTML_sup
HTML_unsup$K_2 <- factor(dataset_unsup$cluster_K_2)
HTML_unsup$K_3 <- factor(dataset_unsup$cluster_K_3)
HTML_unsup$K_4 <- factor(dataset_unsup$cluster_K_4)
HTML_unsup$K_5 <- factor(dataset_unsup$cluster_K_5)
HTML_unsup$K_6 <- factor(dataset_unsup$cluster_K_6)
HTML_unsup$K_7 <- factor(dataset_unsup$cluster_K_7)
HTML_unsup$K_8 <- factor(dataset_unsup$cluster_K_8)
HTML_unsup$K_9 <- factor(dataset_unsup$cluster_K_9)
HTML_unsup$K_10 <- factor(dataset_unsup$cluster_K_10)
HTML_unsup$K_11<- factor(dataset_unsup$cluster_K_11)
HTML_unsup$K_12 <- factor(dataset_unsup$cluster_K_12)
HTML_unsup$K_13 <- factor(dataset_unsup$cluster_K_13)
HTML_unsup$K_14 <- factor(dataset_unsup$cluster_K_14)
HTML_unsup$K_15 <- factor(dataset_unsup$cluster_K_15)

####### Write a csv file now that your data is ready to be analyzed
setwd("/rds/general/user/md2620/home/asthma/Malo/Dataframes/Final")
write.csv(HTML_unsup,"HTML_unsup.csv")
