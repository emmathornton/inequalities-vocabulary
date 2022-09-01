#load in required packages####
library(mice)
library(imputools)
library(miceadds)
library(glue)
library(lubridate)
#load in mcs ses comparison data and change names####
mcs_data<-read.csv("mcs_crossCohort_whiteSample.csv")
mcs_data[,1]<- NULL
names(mcs_data) <- c("id", "gender", "ethnicity", "EAL", 
                     "mothers_age", "accommodation_type",  
                     "highest_household_education",  "occupational_status",
                     "age5_vocab", "age5",
                     "age11_vocab", "age11", 
                     "age14_vocab", "age14" , 
                     "weight", "cohort")


#MCS multiple imputation - only need to run once, after this can load in imputed data.####
init_mcs = mice(mcs_data, maxit=0) 
meth_mcs = init_mcs$method
predM_mcs= init_mcs$predictorMatrix

#To impute the missing values, mice package use an algorithm in a such a way that use information 
#from other variables in dataset to predict and impute the missing values. 
#Therefore, you may not want to use certain variable as predictors. 
#For example the ID variable does not have any predictive value.

#If you want to skip a variable from imputation use the code below. 
#Keep in mind that this variable will be used for prediction.

meth_mcs[c("id")]=""
meth_mcs[c("cohort")]=""
meth_mcs[c("weight")]=""
#Now let specify the methods for imputing the missing values. 
#There are specific methods for continues, binary and ordinal variables. 
#I set different methods for each variable. You can add more than one variable in each methods.



mcs_data$occupational_status = as.factor(mcs_data$occupational_status)
mcs_data$ethnicity = as.factor(mcs_data$ethnicity)
mcs_data$highest_household_education = as.factor(mcs_data$highest_household_education)
mcs_data$gender = as.factor(mcs_data$gender)
mcs_data$EAL = as.factor(mcs_data$EAL)
mcs_data$accommodation_type=as.factor(mcs_data$accommodation_type)
mcs_data$cohort=as.factor(mcs_data$cohort)



methods(mice)


meth_mcs[c("gender")]="logreg"
meth_mcs[c("EAL")]="logreg"
meth_mcs[c("ethnicity")]="logreg"
meth_mcs[c("mothers_age")]="cart"
meth_mcs[c("highest_household_education")]="polyreg"
meth_mcs[c("occupational_status")]="polr"
meth_mcs[c("accommodation_type")]="polyreg"
meth_mcs[c("age5")]="cart" 
meth_mcs[c("age5_vocab")]="cart" 
meth_mcs[c("age11")]="cart" 
meth_mcs[c("age11_vocab")]="cart" 
meth_mcs[c("age14")]="cart" 
meth_mcs[c("age14_vocab")]="cart" 

#now lets run the imputation (m=20) imputations

blocksvec_mcs=names(meth_mcs)

# predM=0 --> variable not used to form imputation (i think - check)
predM_mcs = predM_mcs[blocksvec_mcs,]
predM_mcs[, c("id")]=0
predM_mcs[, c("cohort")]=0
#predM_mcs[, c("weight")]=0
mcs_imputation = mice(mcs_data, blocks=blocksvec_mcs, method=meth_mcs, seed = 1895, predictorMatrix=predM_mcs, m=25)


long_format_mcs <- mice::complete(mcs_imputation, "long", include=TRUE)
long_format_mcs$standardised_vocab5 <- with(long_format_mcs, scale(age5_vocab, center=TRUE, scale=TRUE))
long_format_mcs$standardised_age5 <- with(long_format_mcs, scale(age5, center=TRUE, scale=TRUE))
long_format_mcs$standardised_vocab5 <- as.numeric(long_format_mcs$standardised_vocab5)
long_format_mcs$standardised_age5 <- as.numeric(long_format_mcs$standardised_age5)

long_format_mcs$standardised_vocab11 <- with(long_format_mcs, scale(age11_vocab, center=TRUE, scale=TRUE))
long_format_mcs$standardised_age11 <- with(long_format_mcs, scale(age11, center=TRUE, scale=TRUE))
long_format_mcs$standardised_vocab11 <- as.numeric(long_format_mcs$standardised_vocab11)
long_format_mcs$standardised_age11 <- as.numeric(long_format_mcs$standardised_age11)

long_format_mcs$standardised_vocab14 <- with(long_format_mcs, scale(age14_vocab, center=TRUE, scale=TRUE))
long_format_mcs$standardised_age14 <- with(long_format_mcs, scale(age14, center=TRUE, scale=TRUE))
long_format_mcs$standardised_vocab14 <- as.numeric(long_format_mcs$standardised_vocab14)
long_format_mcs$standardised_age14 <- as.numeric(long_format_mcs$standardised_age14)

long_format_mcs$occupational_status <- with(long_format_mcs, relevel(occupational_status, ref = "2"))
long_format_mcs$occupational_status <- as.factor(long_format_mcs$occupational_status)

mcs_imputation<-as.mids(long_format_mcs)

#save mids object to working directory

write.mice.imputation(mi.res=mcs_imputation, name = glue("{today()}_mcs_ses_comparison_WHITE"), long=TRUE,dattype = "csv")

#load in imputed data ####
load("~/Documents/PhD/MCS DATASETS/SES_inequalities_language/Datasets/mcs_ses_comparison_data/mcs_ses_comparison_data.Rdata")
mcs_imputation=mi.res
#regression models####
#age 5
age5_occupation <- with(mcs_imputation, lm(standardised_vocab5 ~ gender + ethnicity + EAL + standardised_age5+  occupational_status, weights = weight)) #add weight back in when issue resolved
age5_education <- with(mcs_imputation, lm(standardised_vocab5 ~ gender + ethnicity + EAL + standardised_age5+ highest_household_education, weights = weight))#add weight back in when issue resolved
age5_confounders<- with(mcs_imputation, lm(standardised_vocab5 ~ gender + ethnicity + EAL + standardised_age5, weights = weight))#add weight back in when issue resolved

#age 11
age11_occupation <- with(mcs_imputation, lm(standardised_vocab11 ~ gender + ethnicity + EAL + standardised_age11+  occupational_status, weights = weight)) #add weight back in when issue resolved
age11_education <- with(mcs_imputation, lm(standardised_vocab11 ~ gender + ethnicity + EAL + standardised_age11+ highest_household_education, weights = weight))#add weight back in when issue resolved
age11_confounders<- with(mcs_imputation, lm(standardised_vocab11 ~ gender + ethnicity + EAL + standardised_age11,weights = weight))
#age 14
age14_occupation <- with(mcs_imputation, lm(standardised_vocab14 ~ gender + ethnicity + EAL + standardised_age14+  occupational_status,weights = weight)) #add weight back in when issue resolved
age14_education <- with(mcs_imputation, lm(standardised_vocab14 ~ gender + ethnicity + EAL + standardised_age14+ highest_household_education,weights = weight))#add weight back in when issue resolved
age14_confounders<- with(mcs_imputation, lm(standardised_vocab14 ~ gender + ethnicity + EAL + standardised_age14,weights = weight))

round(summary(pool(age5_occupation), conf.int =TRUE),2)
round(summary(pool(age5_education), conf.int =TRUE),2)
round(summary(pool(age11_occupation), conf.int =TRUE),2)
round(summary(pool(age11_education), conf.int =TRUE),2)
round(summary(pool(age14_occupation), conf.int =TRUE),2)
round(summary(pool(age14_education), conf.int =TRUE),2)

age5_occupation_r2 = round(pool.r.squared(age5_occupation),4)*100
age11_occupation_r2 = round(pool.r.squared(age11_occupation),4)*100
age14_occupation_r2 = round(pool.r.squared(age14_occupation),4)*100

age5_education_r2 = round(pool.r.squared(age5_education),4)*100
age11_education_r2= round(pool.r.squared(age11_education),4)*100
age14_education_r2 = round(pool.r.squared(age14_education),4)*100

age5_confounders_r2 = round(pool.r.squared(age5_confounders), 4)*100
age11_confounders_r2= round(pool.r.squared(age11_confounders), 4)*100
age14_confounders_r2= round(pool.r.squared(age14_confounders), 4)*100

#partial r squared
age5_occupation_r2 - age5_confounders_r2
age5_education_r2 - age5_confounders_r2
age11_occupation_r2 - age11_confounders_r2
age11_education_r2 - age11_confounders_r2
age14_occupation_r2 - age14_confounders_r2
age14_education_r2 - age14_confounders_r2

#AIC values ####

get_stats_on_fit_from_MI(mcs_imputation,"standardised_vocab5 ~ gender + ethnicity + EAL + standardised_age5+  occupational_status, weights = weight" )
get_stats_on_fit_from_MI(mcs_imputation,"standardised_vocab11 ~ gender + ethnicity + EAL + standardised_age11+  occupational_status, weights = weight" )
get_stats_on_fit_from_MI(mcs_imputation,"standardised_vocab14 ~ gender + ethnicity + EAL + standardised_age14+  occupational_status, weights = weight" )

get_stats_on_fit_from_MI(mcs_imputation,"standardised_vocab5 ~ gender + ethnicity + EAL + standardised_age5+  highest_household_education, weights = weight" )
get_stats_on_fit_from_MI(mcs_imputation,"standardised_vocab11 ~ gender + ethnicity + EAL + standardised_age11+   highest_household_education, weights = weight" )
get_stats_on_fit_from_MI(mcs_imputation,"standardised_vocab14 ~ gender + ethnicity + EAL + standardised_age14+   highest_household_education, weights = weight" )

#3. pool.compare (model with all SES predictors compared to model with one removed in turn. for each age.)####
#age 3
#model with both SES predictors included 

age5_both <- with(mcs_imputation, lm(standardised_vocab5 ~ gender + ethnicity + EAL + standardised_age5+  occupational_status + highest_household_education, weights = weight))
age11_both <- with(mcs_imputation, lm(standardised_vocab11 ~ gender + ethnicity + EAL + standardised_age11+  occupational_status + highest_household_education, weights = weight))
age14_both <- with(mcs_imputation, lm(standardised_vocab14 ~ gender + ethnicity + EAL + standardised_age14+  occupational_status + highest_household_education, weights = weight))

D1(age5_both, age5_education,method="wald")
D1(age5_both, age5_occupation,method="wald")

D1(age11_both, age510_education,method="wald")
D1(age11_both, age11_occupation,method="wald")

D1(age14_both, age14_education,method="wald")
D1(age14_both, age14_occupation,method="wald")



round(pool.r.squared(age5_both), 4)
round(pool.r.squared(age11_both), 4)
round(pool.r.squared(age14_both), 4)
