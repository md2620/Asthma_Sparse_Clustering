#Data imputation Asthma dataset
#Use of MissForest Package
#use of preProcess function to scale variables - normalization

#Clear environment
rm(list=ls())

#Reading the files
asthma = read.csv("/rds/general/user/md2620/home/asthma/Malo/Dataframes/asthma_clinical_20%missingness.csv")

#Confirm presence of missingness in dataset
sapply(asthma, function(x) sum(is.na(x)))

#Look at which variables only have 1 factor level
values_count <- sapply(lapply(asthma, unique), length)  # Identify variables with 1 value
values_count  

########### TEST HOW TO USE VARIABLES AND AVOID UNUSED ARGUMENT ERROR #############
#Because of the MASS package, you have to write name of package in front of select function
#Create subset of dataset
#new_df <- subset(asthma, select=c("X", "Subject.ID","X.UBIOPRED.Adult_Cohort_.Jan_2019..Visit.Information.Baseline.Visit.Days.Since.Screening..Baseline.Day.2.."))

#Try to remove the 2 columns that starts with..
#new_df <- new_df %>% 
  #dplyr::select(-starts_with("X.UBIOPRED."), -starts_with("Subject."))

#Check colnames in your dataset
#colnames(new_df)

#Remove variables that are linked to study participation in asthma dataset
#Why ? Because they only have one level for most of them (4) and 10 in total are linked to Study Participation.
#Variables that are here only to make sure participants meet the eligibility criteria.
#If you want to check variables in asthma
ls(asthma)

#Remove the 10 variables linked to Study participation. 
asthma <- asthma %>%
  dplyr::select(-starts_with("X.UBIOPRED.Adult_Cohort_.Jan_2019..Study.participation.criteria.General.inclusion.criteria."), -starts_with("X.UBIOPRED.Adult_Cohort_.Jan_2019..Study.participation.criteria.General.exclusion.criteria.exclusion."))

#Check that you have no factor variable with only 1 level.
values_count <- sapply(lapply(asthma, unique), length)  # Identify variables with 1 value
values_count  

#############################################################################################
#IF YOU WANT TO USE MICE PACKAGE TO IMPUTE DF
#############################################################################################
#Imputation with mice
#library(mice)
#init = mice(asthma, maxit=0) 
#meth = init$method
#predM = init$predictorMatrix
#Specify the method for imputing missing values according to the type of variables you have
#############################################################################################
#END MICE
#############################################################################################


#Set Subject.ID as index and drop X colum (1st useless column)
#Drop X column from dataset
asthma <- asthma[ -c(1) ]
#Set Subject.ID as index
asthma_2 <- asthma[-1]
row.names(asthma_2) <- asthma$Subject.ID
View(asthma_2)

#Look at what you could consider as an outcome variable - Measure of severity for eosinophilic count in sputum: ≥ 300/μL - 0.3 here (as unit is x10.3)
###########Outcome variable - Severity Measure - Eosinophils %##################
asthma_2$X.UBIOPRED.Adult_Cohort_.Jan_2019..Clinical.Data.Haematology.and.biochemistry.tests.Screening.eosinophils..x10.3.uL..


############################################################################################
#METHOD IF INSTEAD YOU WANT TO IMPUTE ONLY NUMERICAL VARIABLES IN DF 
############################################################################################
#Check variables in dataframe
ls(asthma_2)

#Check variable type in df
sapply(asthma_2, class)

#Seperate numerical and categorical variables into 2 different dataframes
num_asthma_2 <- asthma_2 %>%
  select_if(Negate(is.factor))

cat_asthma_2 <- asthma_2 %>%
  select_if(Negate(is.numeric))

#Some factor variables are annotated as numeric, some dummies as numeric and some 'raws variables' are duplicates of 'imputed variables'
#Convert wrongly annotated numerical variables to factor variables
fact <- c(257:263,275:306,311:318,325:338,341:367,1:4,37,39,50,66,103,109:115,370:379,387,389)
num_asthma_2[,fact] <- lapply(num_asthma_2[,fact],factor)

#Check variable type in dataframe to make sure the right numerical variables have been converted into factor variables 
sapply(num_asthma_2,class)

#Check one variable levels to see the change has been well-implemented
num_asthma_2$X.UBIOPRED.Adult_Cohort_.Jan_2019..Questionnaires.Epworth.Sleepiness.Scale..ESS..Baseline.Visit.Question.8.

#dummies <- c(1:4,37,39,50,66,103,109:115,370:379,387,389) convert dummies into factor too.
#Raw are variables to remove because duplicated
#raws <- c(248,250,252,265,272,274,308,310,320,322,324,340,347,369)

#Remove 'raws variables' from dataset to avoid duplicated variables
num_asthma_2 = subset(num_asthma_2, select = -c(248,250,252,265,272,274,308,310,320,322,324,340,347,369))
ls(num_asthma_2)

#Keep all the factor variables that you just created in a separate dataframe
fact_asthma_2 <- num_asthma_2 %>%
  select_if(Negate(is.numeric))

#Make a final numerical dataframe with only numerical variables (that is removing the factor variables you newly created)
num_asthma_3 <- num_asthma_2 %>%
  select_if(Negate(is.factor))

#Imputation of numerical variables with caret
#Split the numerical dataset num_asthma_3 75/25%
set.seed(102)
train_index <- sample(nrow(num_asthma_3), 465)

train <- num_asthma_3[train_index, ]
test <- num_asthma_3[-train_index, ]

#Check missingness in train and test sets before imputation
sapply(train, function(x) sum(is.na(x)))
sapply(test, function(x) sum(is.na(x)))

#Impute numerical variables in train and test sets
preprocess_values_train = preProcess(train, method = "bagImpute")
trainset = predict(preprocess_values_train, train)
testset = predict(preprocess_values_train,test)

#Check missingness in train and test sets after imputation
sapply(trainset, function(x) sum(is.na(x)))
sapply(testset, function(x) sum(is.na(x)))

#Check first 5 columns
summary(trainset[, 1:5])
summary(testset[,1:5])

#Combine train and test sets
num_asthma_3_imputed <- rbind(trainset, testset)

#Check missingness in df after imputation - should be 0
sapply(num_asthma_3_imputed, function(x) sum(is.na(x)))

#Combine num_asthma_2_imputed with categorical variables (that didn't need imputation for k-prototypes)
df_asthma_2 <- cbind(num_asthma_3_imputed, cat_asthma_2)
df_final <- cbind(df_asthma_2,fact_asthma_2)

#Check missingness in final dataset - looking only at missingness in numerical variables
sapply(df_final, function(x) sum(is.na(x)))

#Write a dataset for k-prototypes
setwd("/rds/general/user/md2620/home/asthma/Malo/Dataframes")
write.csv(df_final,"asthma_k_prototypes_2.csv")

##################SAME THING BUT WITHOUT IMPUTATION#################################
#Write a dataset without imputing numerical variables
df_asthma_2 <- cbind(num_asthma_3, cat_asthma_2)
df_final <- cbind(df_asthma_2,fact_asthma_2)

#Check missingness in final dataset - looking only at missingness in numerical variables
sapply(df_final, function(x) sum(is.na(x)))

#Write a dataset for k-prototypes
setwd("/rds/general/user/md2620/home/asthma/Malo/Dataframes")
write.csv(df_final,"asthma_before_imp_and_stand.csv")
##################END DATASET WITH 620 OBSERVATIONS AND 527 VARIABLES#######################

############################################################################################
#METHOD IF INSTEAD YOU WANT TO IMPUTE EVERYTHING WITH DUMMIFICATION OF CATEGORICAL VARIABLES
############################################################################################

#Transform categorical data to a one-hot encoded structure
dummyModel <- dummyVars(" ~ .", data = train)
trainset <- as.data.frame(predict(dummyModel, newdata = train))

#Apply one-hot encoder to the test set
testset <- as.data.frame(predict(dummyModel, test))

#Check datatypes in train and test sets
str(trainset)
str(testset)

#Check missingness in train and test sets before imputation
sapply(trainset, function(x) sum(is.na(x)))
sapply(testset, function(x) sum(is.na(x)))

#Use missForest for imputation:
#install.packages("missForest")
library(missForest)

#impute missing values in trainset, using all parameters as default values
imp_train <- missForest(trainset,verbose=TRUE)$ximp

#Look if you successfully got rid of missingness in trainset
#Supposed to have 0 missing value / OK
#sapply(imp_train, function(x) sum(is.na(x)))

#Combine and impute test set
imp_train_test <- rbind(testset, imp_train)
imp_test <- missForest(imp_train_test,verbose=TRUE)$ximp[1:nrow(testset), ]

#Combine train and test sets
final_df <- rbind(imp_test, imp_train)

#Write a csv file now that data is imputed before standardization
setwd("/rds/general/user/md2620/home/asthma/Malo")
write.csv(final_df,"asthma_imputed.csv")

#Look if you successfully got rid of missingness in testset
#Supposed to have 0 missing value / OK
sapply(imp_test, function(x) sum(is.na(x)))

#check imputed values - Do not run
imp_train$ximp
imp_test$ximp

#Check imputed data matrices
imp_train$Ximp
imp_test$Ximp

#check estimated imputation error
imp_train$OOBerror
imp_test$OOBerror

#Check true imputation error
imp_train$error
imp_test$error

#3 iterations seem sufficient (performance-wise and accuracy wise), so we re-run with parallelization and max_iter = 3
#install.packages("doRNG")
doRNG::registerDoRNG(seed=1)
doParallel::registerDoParallel(cores = 4)
#You can choose either parallelize = variables or forests
#Variables is used if the data contains many variables and the computing the random forests is not taking too long. 
#Forests is used as opposite to variables
imp_train_2 <- missForest(trainset,parallelize = 'variables', verbose=T, maxiter = 3)
#Check out-of-bag error
imp_train_2$OOBerror

##### Add this part to complement imputation with standardization of numeric variables
##### NORMALIZATION OF VARIABLES #####
rangeModel <- preProcess(imp_train, method = "range")

#After creating model, apply it to train set.
imp_train <- predict(rangeModel, newdata = imp_train)
imp_train <- data.frame(imp_train)
View(imp_train)

#Then apply scaling model to test set
imp_test <- predict(rangeModel, imp_test)
imp_test <- data.frame(imp_test)
View(imp_test)

#Combine train and test sets
final_df <- rbind(imp_test, imp_train)

#Last sanity check to make sure all variables are imputed after concatenation of train and test sets.
sapply(final_df, function(x) sum(is.na(x)))
View(final_df)

#Write a csv file now that data is preprocessed
setwd("/rds/general/user/md2620/home/asthma/Malo")
write.csv(final_df,"asthma_preprocessed.csv")


final_df$target <- cut(final_df$X.UBIOPRED.Adult_Cohort_.Jan_2019..Clinical.Data.Haematology.and.biochemistry.tests.Screening.eosinophils..x10.3.uL.., breaks = c(-Inf, 0.3, +Inf), labels = c("not severe", "severe"))

table(final_df$target)

