#load in required packages####
library(mice)
library(imputools)
library(miceadds)
library(glue)
library(lubridate)
library(dplyr)
library(naniar)
#load in bcs ses data and change the names####
bcs_data<-read.csv("bcs_ses_comparison_data.csv")
bcs_data[,1]<- NULL
bcs_data = bcs_data %>% select(-e245, -e220, -accommodation_age5, -accommodation_birth) %>% 
  rename(sex = a0255, 
         age_atBirth = a0005a,
         highestEd1 = highestEd_recoded, 
         cm_age5 = age5_years,
         cm_age10 = bd3age, 
         cm_age16 = bd4age, 
         age10_vocab = b10bass, 
         age16_vocab = b16vocab, 
         accommodation_type = accommodation
         )

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

meth[c("bcsid")]=""
meth[c("weight")]=""
#Now let specify the methods for imputing the missing values. 
#There are specific methods for continuous, binary and ordinal variables. 
#I set different methods for each variable. You can add more than one variable in each methods.



bcs_data$highest_occupation = as.factor(bcs_data$highest_occupation)
bcs_data$ethnicity = as.factor(bcs_data$ethnicity)
bcs_data$highestEd1 = as.factor(bcs_data$highestEd1)
bcs_data$sex = as.factor(bcs_data$sex)
bcs_data$EAL = as.factor(bcs_data$EAL)
bcs_data$accommodation_type=as.factor(bcs_data$accommodation_type)
bcs_data$oecd_quintiles=as.factor(bcs_data$oecd_quintiles)
bcs_data$tenure=as.factor(bcs_data$tenure)



methods(mice)


meth[c("sex")]="logreg"
meth[c("EAL")]="logreg"
meth[c("ethnicity")]="logreg"
meth[c("age_atBirth")]="cart"
meth[c("highestEd1")]="polyreg"
meth[c("oecd_quintiles")]="polyreg"
meth[c("highest_occupation")]="polyreg"
meth[c("accommodation_type")]="polyreg"
meth[c("age5_vocab")]="cart" 
meth[c("age10_vocab")]="cart" 
meth[c("age16_vocab")]="cart" 
meth[c("b16vocab_harmonised")]="cart" 
meth[c("cm_age5")]="cart" 
meth[c("cm_age10")]="cart" 
meth[c("cm_age16")]="cart" 
#meth[c("weight")]="cart" 
#now lets run the imputation (m=20) imputations

blocksvec=names(meth)

# predM=0 --> variable not used to form imputation (i think - check)
predM = predM[blocksvec,]
predM[, c("bcsid")]=0
predM[c("age16_vocab"), 
      c("b16vocab_harmonised")] = 0
predM[c("b16vocab_harmonised"), 
      c("age16_vocab")] = 0


bcs_imputation = mice(bcs_data, blocks=blocksvec, method=meth, 
                      seed = 1895, predictorMatrix=predM, m=25)

long_format_bcs <- mice::complete(bcs_imputation, "long", include=TRUE)
long_format_bcs$standardised_vocab5 <- with(long_format_bcs, scale(age5_vocab, center=TRUE, scale=TRUE))
long_format_bcs$standardised_age5 <- with(long_format_bcs, scale(cm_age5, center=TRUE, scale=TRUE))
long_format_bcs$standardised_vocab5 <- as.numeric(long_format_bcs$standardised_vocab5)
long_format_bcs$standardised_age5 <- as.numeric(long_format_bcs$standardised_age5)

long_format_bcs$standardised_vocab10 <- with(long_format_bcs, scale(age10_vocab, center=TRUE, scale=TRUE))
long_format_bcs$standardised_age10 <- with(long_format_bcs, scale(cm_age10, center=TRUE, scale=TRUE))
long_format_bcs$standardised_vocab10 <- as.numeric(long_format_bcs$standardised_vocab10)
long_format_bcs$standardised_age10 <- as.numeric(long_format_bcs$standardised_age10)

long_format_bcs$standardised_vocab16 <- with(long_format_bcs, scale(age16_vocab, center=TRUE, scale=TRUE))
long_format_bcs$standardised_age16 <- with(long_format_bcs, scale(cm_age16, center=TRUE, scale=TRUE))
long_format_bcs$standardised_vocab16 <- as.numeric(long_format_bcs$standardised_vocab16)
long_format_bcs$standardised_age16 <- as.numeric(long_format_bcs$standardised_age16)

long_format_bcs$occupational_status <- with(long_format_bcs, relevel(highest_occupation, ref = "2"))
long_format_bcs$occupational_status <- as.factor(long_format_bcs$occupational_status)


bcs_imputation<-as.mids(long_format_bcs)

#save mids object to working directory

miceadds::write.mice.imputation(mi.res=bcs_imputation, 
                      name = glue("{today()}_bcs_ses_cross_cohort"), 
                      mids2spss=FALSE,
                      long=TRUE,dattype = "csv")

