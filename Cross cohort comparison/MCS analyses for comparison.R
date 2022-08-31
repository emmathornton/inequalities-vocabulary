#load in required packages####
library(mice)
library(imputools)
library(miceadds)
library(glue)
library(lubridate)
#load in mcs ses comparison data and change names####
mcs_data<-read.csv("mcs_ses_comparison_data.csv")
mcs_data[,1]<- NULL
names(mcs_data) <- c("id", "gender", "ethnicity", "EAL", "mothers_age", "accommodation_type",  "highest_household_education",  "occupational_status", "age5_vocab", "age5", "age11_vocab", "age11", "age14_vocab", "age14" , "weight", "cohort")


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

write.mice.imputation(mi.res=mcs_imputation, name = glue("{today()}_mcs_ses_crossCohort"), long=TRUE,dattype = "csv")

