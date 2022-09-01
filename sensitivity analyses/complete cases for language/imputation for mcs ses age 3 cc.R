#load in required packages####
library(haven)
require(swfscMisc)
require(sjmisc)
require(Hmisc)
require(psych)
library(mice)
library(miceadds)
library(gtools)

#open mcs data####
mcs_ses<-read.csv("age3_ses_cc.csv")
mcs_ses[,1]<- NULL

#multiple imputation####
methods(mice)
init = mice(mcs_ses, maxit=0) 
meth = init$method
predM = init$predictorMatrix

#To impute the missing values, mice package use an algorithm in a such a way that use information 
#from other variables in dataset to predict and impute the missing values. 
#Therefore, you may not want to use certain variable as predictors. 
#For example the ID variable does not have any predictive value.



#If you want to skip a variable from imputation use the code below. 
#Keep in mind that this variable will be used for prediction.

meth[c("mcsid")]=""
meth[c("mcs2_weight")]=""


#Now let specify the methods for imputing the missing values. 
#There are specific methods for continues, binary and ordinal variables. 
#I set different methods for each variable. You can add more than one variable in each methods.

mcs_ses$gender=as.factor(mcs_ses$gender)
mcs_ses$ethnicity=as.factor(mcs_ses$ethnicity)
mcs_ses$language_used_at_home=as.factor(mcs_ses$language_used_at_home)
mcs_ses$income_quintiles=as.factor(mcs_ses$income_quintiles)
mcs_ses$imd=as.factor(mcs_ses$imd)
mcs_ses$occupational_status=as.factor(mcs_ses$occupational_status)
#mcs_ses$mothers_education=as.factor(mcs_ses$mothers_education)
#mcs_ses$fathers_education=as.factor(mcs_ses$fathers_education)
#mcs_ses$highest_household_education=as.factor(mcs_ses$highest_household_education)
#mcs_ses$mother_NVQ_level=as.factor(mcs_ses$mother_NVQ_level)
#mcs_ses$father_NVQ_level=as.factor(mcs_ses$father_NVQ_level)
mcs_ses$highest_NVQ=as.factor(mcs_ses$highest_NVQ)
mcs_ses$parents_in_house=as.factor(mcs_ses$parents_in_house)
mcs_ses$cm_breastfed=as.factor(mcs_ses$cm_breastfed)
mcs_ses$accommodation_type=as.factor(mcs_ses$accommodation_type)
mcs_ses$housing_tenure=as.factor(mcs_ses$housing_tenure)
methods(mice)

meth[c("gender")]="logreg"
meth[c("ethnicity")]="polyreg"
meth[c("language_used_at_home")]="polyreg"
meth[c("mothers_age")]="cart" 
#meth[c("highest_household_education")]="polyreg"
meth[c("highest_NVQ")]="polyreg"
meth[c("income_quintiles")]="polyreg"
meth[c("imd")]="polyreg"
meth[c("occupational_status")]="polyreg"
#meth[c("mothers_education")]="polyreg"
#meth[c("fathers_education")]="polyreg"
#meth[c("mother_NVQ_level")]="polyreg"
#meth[c("father_NVQ_level")]="polyreg"
meth[c("housing_tenure")]="polyreg"
meth[c("accommodation_type")]="polyreg"
meth[c("cm_breastfed")]="logreg"
meth[c("parents_in_house")]="logreg"
meth[c("mortgage")]="cart"
meth[c("house_value")]="cart"
meth[c("savings")]="cart"
meth[c("total_debt")]="cart"
meth[c("vocabulary_age3")]="cart" 
meth[c("vocabulary_age5")]="cart" 
meth[c("vocabulary_age11")]="cart" 
meth[c("vocabulary_age14")]="cart" 
#now lets run the imputation (m=20) imputations

blocksvec=names(meth)

# predM=0 --> variable not used to form imputation (i think - check)
predM = predM[blocksvec,]
predM[,c("mcsid")]=0
#predM[,c("mcs2_weight")]=0
#set.seed(103)
imputed_mcs2 = mice(mcs_ses, blocks=blocksvec, method=meth, seed=1895, predictorMatrix=predM, m=25) #can change this to a smaller numebr so runs quicker when figuring out. 

#deriving post imputation variables####
long_format_mcs <- mice::complete(imputed_mcs2, "long", include=TRUE)
long_format_mcs$age3_standardised <- with(long_format_mcs, scale(vocabulary_age3, center=TRUE, scale=TRUE))
long_format_mcs$age5_standardised <- with(long_format_mcs, scale(vocabulary_age5, center=TRUE, scale=TRUE))
long_format_mcs$age11_standardised <- with(long_format_mcs, scale(vocabulary_age11, center=TRUE, scale=TRUE))
long_format_mcs$age14_standardised <- with(long_format_mcs, scale(vocabulary_age14, center=TRUE, scale=TRUE))
long_format_mcs$age3_standardised <- as.numeric(long_format_mcs$age3_standardised)
long_format_mcs$age5_standardised <- as.numeric(long_format_mcs$age5_standardised)
long_format_mcs$age11_standardised <- as.numeric(long_format_mcs$age11_standardised)
long_format_mcs$age14_standardised <- as.numeric(long_format_mcs$age14_standardised)

#deriving  wealth variable
long_format_mcs$housing_wealth <- with(long_format_mcs, house_value - mortgage)
long_format_mcs$financial_wealth <- with(long_format_mcs, savings - total_debt)
long_format_mcs$net_wealth <- with(long_format_mcs, housing_wealth + financial_wealth)
long_format_mcs$standardised_wealth <- with(long_format_mcs, scale(net_wealth, center=TRUE, scale=TRUE))
long_format_mcs$standardised_wealth<- as.numeric(long_format_mcs$standardised_wealth)


long_format_mcs$highest_NVQ <- with(long_format_mcs, relevel(highest_NVQ, ref = "1"))
long_format_mcs$highest_NVQ <- as.factor(long_format_mcs$highest_NVQ)
long_format_mcs$occupational_status <- with(long_format_mcs, relevel(occupational_status, ref = "2"))
long_format_mcs$occupational_status <- as.factor(long_format_mcs$occupational_status)
long_format_mcs$wealth_quintiles <- with(long_format_mcs, quantcut(standardised_wealth,5))
levels(long_format_mcs$wealth_quintiles)[1] = "1"
levels(long_format_mcs$wealth_quintiles)[2] = "2"
levels(long_format_mcs$wealth_quintiles)[3] = "3"
levels(long_format_mcs$wealth_quintiles)[4] = "4"
levels(long_format_mcs$wealth_quintiles)[5] = "5"
long_format_mcs$wealth_quintiles <- as.factor(long_format_mcs$wealth_quintiles)

#convert back to mids object.
imputed_mcs2<-as.mids(long_format_mcs)

#save mids object to working directory####

write.mice.imputation(mi.res=imputed_mcs2, name = "age3_MCS_SES_complete-cases_imputed", long=TRUE,dattype = "csv")

#get each individual imputed dataset#####
imputed_mcs2_0 <- complete(imputed_mcs2)
imputed_mcs2_1 <- complete(imputed_mcs2,1)
imputed_mcs2_2 <- complete(imputed_mcs2,2)
imputed_mcs2_3 <- complete(imputed_mcs2,3)
imputed_mcs2_4 <- complete(imputed_mcs2,4)
imputed_mcs2_5 <- complete(imputed_mcs2,5)
imputed_mcs2_6 <- complete(imputed_mcs2,6)
imputed_mcs2_7 <- complete(imputed_mcs2,7)
imputed_mcs2_8 <- complete(imputed_mcs2,8)
imputed_mcs2_9 <- complete(imputed_mcs2,9)
imputed_mcs2_10 <- complete(imputed_mcs2,10)
imputed_mcs2_11 <- complete(imputed_mcs2,11)
imputed_mcs2_12 <- complete(imputed_mcs2,12)
imputed_mcs2_13 <- complete(imputed_mcs2,13)
imputed_mcs2_14 <- complete(imputed_mcs2,14)
imputed_mcs2_15 <- complete(imputed_mcs2,15)
imputed_mcs2_16 <- complete(imputed_mcs2,16)
imputed_mcs2_17 <- complete(imputed_mcs2,17)
imputed_mcs2_18 <- complete(imputed_mcs2,18)
imputed_mcs2_19 <- complete(imputed_mcs2,19)
imputed_mcs2_20<- complete(imputed_mcs2,20)
imputed_mcs2_21<- complete(imputed_mcs2,21)
imputed_mcs2_22<- complete(imputed_mcs2,22)
imputed_mcs2_23<- complete(imputed_mcs2,23)
imputed_mcs2_24<- complete(imputed_mcs2,24)
imputed_mcs2_25<- complete(imputed_mcs2,25)