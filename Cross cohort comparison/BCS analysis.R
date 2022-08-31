#load in required packages####
library(mice)
library(imputools)
library(miceadds)
library(glue)
library(lubridate)
#load in bcs ses data and change the names####
bcs_data<-read.csv("bcs_ses_comparison_data.csv")
bcs_data[,1]<- NULL
bcs_data[,8]<- NULL
bcs_data[,8]<- NULL
names(bcs_data) <- c("id", "gender", "ethnicity", "EAL", "mothers_age", "accommodation_type",  "highest_household_education",  "occupational_status", "age5_vocab", "age_5" ,"age10_vocab","age10", "age16_vocab", "age16","weight", "cohort")
#BCS multiple imputation. run once and save imputed dataset to load in in future####
init = mice(bcs_data, maxit=0) 
meth = init$method
predM = init$predictorMatrix

#To impute the missing values, mice package use an algorithm in a such a way that use information 
#from other variables in dataset to predict and impute the missing values. 
#Therefore, you may not want to use certain variable as predictors. 
#For example the ID variable does not have any predictive value.

#If you want to skip a variable from imputation use the code below. 
#Keep in mind that this variable will be used for prediction.

meth[c("id")]=""
meth[c("cohort")]=""
meth[c("weight")]=""
#Now let specify the methods for imputing the missing values. 
#There are specific methods for continues, binary and ordinal variables. 
#I set different methods for each variable. You can add more than one variable in each methods.



bcs_data$occupational_status = as.factor(bcs_data$occupational_status)
bcs_data$ethnicity = as.factor(bcs_data$ethnicity)
bcs_data$highest_household_education = as.factor(bcs_data$highest_household_education)
bcs_data$gender = as.factor(bcs_data$gender)
bcs_data$EAL = as.factor(bcs_data$EAL)
bcs_data$accommodation_type=as.factor(bcs_data$accommodation_type)
bcs_data$cohort=as.factor(bcs_data$cohort)



methods(mice)


meth[c("gender")]="logreg"
meth[c("EAL")]="logreg"
meth[c("ethnicity")]="logreg"
meth[c("mothers_age")]="cart"
meth[c("highest_household_education")]="polyreg"
meth[c("occupational_status")]="polr"
meth[c("accommodation_type")]="polyreg"
meth[c("age5_vocab")]="cart" 
meth[c("age10_vocab")]="cart" 
meth[c("age16_vocab")]="cart" 
meth[c("age_5")]="cart" 
meth[c("age10")]="cart" 
meth[c("age16")]="cart" 

#now lets run the imputation (m=20) imputations

blocksvec=names(meth)

# predM=0 --> variable not used to form imputation (i think - check)
predM = predM[blocksvec,]
predM[, c("id")]=0
predM[, c("cohort")]=0
#predM[, c("weight")]=0
#predM[,c("f117")]=0 
#predM[,c("bd2mal")]=0 
#predM[,c("antisocial_totalscore")]=0 
#predM[,c("neurotic_totalscore")]=0 
#predM[,c("age16MH_totalscore")]=0 
#predM[,c("standardised_vocab")]=0
#predM[,c("standardised_malaise")]=0




bcs_imputation = mice(bcs_data, blocks=blocksvec, method=meth, seed = 1895, predictorMatrix=predM, m=25)

long_format_bcs <- mice::complete(bcs_imputation, "long", include=TRUE)
long_format_bcs$standardised_vocab5 <- with(long_format_bcs, scale(age5_vocab, center=TRUE, scale=TRUE))
long_format_bcs$standardised_age5 <- with(long_format_bcs, scale(age_5, center=TRUE, scale=TRUE))
long_format_bcs$standardised_vocab5 <- as.numeric(long_format_bcs$standardised_vocab5)
long_format_bcs$standardised_age5 <- as.numeric(long_format_bcs$standardised_age5)

long_format_bcs$standardised_vocab10 <- with(long_format_bcs, scale(age10_vocab, center=TRUE, scale=TRUE))
long_format_bcs$standardised_age10 <- with(long_format_bcs, scale(age10, center=TRUE, scale=TRUE))
long_format_bcs$standardised_vocab10 <- as.numeric(long_format_bcs$standardised_vocab10)
long_format_bcs$standardised_age10 <- as.numeric(long_format_bcs$standardised_age10)

long_format_bcs$standardised_vocab16 <- with(long_format_bcs, scale(age16_vocab, center=TRUE, scale=TRUE))
long_format_bcs$standardised_age16 <- with(long_format_bcs, scale(age16, center=TRUE, scale=TRUE))
long_format_bcs$standardised_vocab16 <- as.numeric(long_format_bcs$standardised_vocab16)
long_format_bcs$standardised_age16 <- as.numeric(long_format_bcs$standardised_age16)

long_format_bcs$occupational_status <- with(long_format_bcs, relevel(occupational_status, ref = "2"))
long_format_bcs$occupational_status <- as.factor(long_format_bcs$occupational_status)


bcs_imputation<-as.mids(long_format_bcs)

#save mids object to working directory

write.mice.imputation(mi.res=bcs_imputation, name = glue("{today()}_bcs_ses_cross_cohort"), long=TRUE,dattype = "csv")

