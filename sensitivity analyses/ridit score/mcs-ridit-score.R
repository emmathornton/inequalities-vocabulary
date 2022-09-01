#ridit scores analysis 
#ridit score for occupation and education in each cohort 
#ridit score based on counts
#need to create a vector of counts in each category to get ridit score
#then create a new variable that converts the category into its corresponding ridit score
#do this for each imputed dataset, in both cohorts. 

library(ridittools)
library(mice)
library(pander)
library(xtable)
library(dplyr)
library(gt)
library(glue)
library(tidyverse)
library(flextable)
library(miceadds)
library(swfscMisc)
library(imputools)
library(haven)
library(sjmisc)
library(Hmisc)
library(psych)
library(lavaan)
library(dummies)

load("~/Documents/PhD/MCS DATASETS/SES_inequalities_language/inequalities-language-ability/Datasets/2021-02-26_mcs_ses_crossCohort/2021-02-26_mcs_ses_crossCohort.Rdata")
imputed_mcs2 = mi.res


#change order of occupaiton variable back to unemployed, routine (???)
long_format_mcs <- mice::complete(imputed_mcs2, "long", include=TRUE)
long_format_mcs$occupational_status <- with(long_format_mcs, relevel(occupational_status, ref = "1"))
long_format_mcs$occupational_status <- as.factor(long_format_mcs$occupational_status)
imputed_mcs2<-as.mids(long_format_mcs)

#MCS imputed datasets 1-25
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


#calculate ridit scores. EDUCATION ####

#imputed dataset 1
#get count data 
#then convert to ridit score with toridit()
mcs_education_ridit1 <- as.matrix(toridit(table(imputed_mcs2_1$highest_household_education)))
#add ridit score to dataset as new variable 
imputed_mcs2_1 = imputed_mcs2_1 %>% mutate(education_ridit = highest_household_education) 
imputed_mcs2_1$education_ridit <- as.numeric(imputed_mcs2_1$education_ridit)
#recode so is replaced with the ridit scores
imputed_mcs2_1$education_ridit[imputed_mcs2_1$education_ridit == 1] <- mcs_education_ridit1[1,]
imputed_mcs2_1$education_ridit[imputed_mcs2_1$education_ridit == 2] <- mcs_education_ridit1[2,]
imputed_mcs2_1$education_ridit[imputed_mcs2_1$education_ridit == 3] <- mcs_education_ridit1[3,]
imputed_mcs2_1$education_ridit[imputed_mcs2_1$education_ridit == 4] <- mcs_education_ridit1[4,]

#imputed dataset 2
#get count data 
#then convert to ridit score with toridit()
mcs_education_ridit2 <- as.matrix(toridit(table(imputed_mcs2_2$highest_household_education)))
#add ridit score to dataset as new variable 
imputed_mcs2_2 = imputed_mcs2_2 %>% mutate(education_ridit = highest_household_education) 
imputed_mcs2_2$education_ridit <- as.numeric(imputed_mcs2_2$education_ridit)
#recode so is replaced with the ridit scores
imputed_mcs2_2$education_ridit[imputed_mcs2_2$education_ridit == 1] <- mcs_education_ridit2[1,]
imputed_mcs2_2$education_ridit[imputed_mcs2_2$education_ridit == 2] <- mcs_education_ridit2[2,]
imputed_mcs2_2$education_ridit[imputed_mcs2_2$education_ridit == 3] <- mcs_education_ridit2[3,]
imputed_mcs2_2$education_ridit[imputed_mcs2_2$education_ridit == 4] <- mcs_education_ridit2[4,]

#imputed dataset 3
#get count data 
#then convert to ridit score with toridit()
mcs_education_ridit3 <- as.matrix(toridit(table(imputed_mcs2_3$highest_household_education)))
#add ridit score to dataset as new variable 
imputed_mcs2_3 = imputed_mcs2_3 %>% mutate(education_ridit = highest_household_education) 
imputed_mcs2_3$education_ridit <- as.numeric(imputed_mcs2_3$education_ridit)
#recode so is replaced with the ridit scores
imputed_mcs2_3$education_ridit[imputed_mcs2_3$education_ridit == 1] <- mcs_education_ridit3[1,]
imputed_mcs2_3$education_ridit[imputed_mcs2_3$education_ridit == 2] <- mcs_education_ridit3[2,]
imputed_mcs2_3$education_ridit[imputed_mcs2_3$education_ridit == 3] <- mcs_education_ridit3[3,]
imputed_mcs2_3$education_ridit[imputed_mcs2_3$education_ridit == 4] <- mcs_education_ridit3[4,]


#imputed dataset 4
#get count data 
#then convert to ridit score with toridit()
mcs_education_ridit4 <- as.matrix(toridit(table(imputed_mcs2_4$highest_household_education)))
#add ridit score to dataset as new variable 
imputed_mcs2_4 = imputed_mcs2_4 %>% mutate(education_ridit = highest_household_education) 
imputed_mcs2_4$education_ridit <- as.numeric(imputed_mcs2_4$education_ridit)
#recode so is replaced with the ridit scores
imputed_mcs2_4$education_ridit[imputed_mcs2_4$education_ridit == 1] <- mcs_education_ridit4[1,]
imputed_mcs2_4$education_ridit[imputed_mcs2_4$education_ridit == 2] <- mcs_education_ridit4[2,]
imputed_mcs2_4$education_ridit[imputed_mcs2_4$education_ridit == 3] <- mcs_education_ridit4[3,]
imputed_mcs2_4$education_ridit[imputed_mcs2_4$education_ridit == 4] <- mcs_education_ridit4[4,]

#imputed dataset 5
#get count data 
#then convert to ridit score with toridit()
mcs_education_ridit5 <- as.matrix(toridit(table(imputed_mcs2_5$highest_household_education)))
#add ridit score to dataset as new variable 
imputed_mcs2_5 = imputed_mcs2_5 %>% mutate(education_ridit = highest_household_education) 
imputed_mcs2_5$education_ridit <- as.numeric(imputed_mcs2_5$education_ridit)
#recode so is replaced with the ridit scores
imputed_mcs2_5$education_ridit[imputed_mcs2_5$education_ridit == 1] <- mcs_education_ridit5[1,]
imputed_mcs2_5$education_ridit[imputed_mcs2_5$education_ridit == 2] <- mcs_education_ridit5[2,]
imputed_mcs2_5$education_ridit[imputed_mcs2_5$education_ridit == 3] <- mcs_education_ridit5[3,]
imputed_mcs2_5$education_ridit[imputed_mcs2_5$education_ridit == 4] <- mcs_education_ridit5[4,]

#imputed dataset 6
#get count data 
#then convert to ridit score with toridit()
mcs_education_ridit6 <- as.matrix(toridit(table(imputed_mcs2_6$highest_household_education)))
#add ridit score to dataset as new variable 
imputed_mcs2_6 = imputed_mcs2_6 %>% mutate(education_ridit = highest_household_education) 
imputed_mcs2_6$education_ridit <- as.numeric(imputed_mcs2_6$education_ridit)
#recode so is replaced with the ridit scores
imputed_mcs2_6$education_ridit[imputed_mcs2_6$education_ridit == 1] <- mcs_education_ridit6[1,]
imputed_mcs2_6$education_ridit[imputed_mcs2_6$education_ridit == 2] <- mcs_education_ridit6[2,]
imputed_mcs2_6$education_ridit[imputed_mcs2_6$education_ridit == 3] <- mcs_education_ridit6[3,]
imputed_mcs2_6$education_ridit[imputed_mcs2_6$education_ridit == 4] <- mcs_education_ridit6[4,]

#imputed dataset 7
#get count data 
#then convert to ridit score with toridit()
mcs_education_ridit7 <- as.matrix(toridit(table(imputed_mcs2_7$highest_household_education)))
#add ridit score to dataset as new variable 
imputed_mcs2_7 = imputed_mcs2_7 %>% mutate(education_ridit = highest_household_education) 
imputed_mcs2_7$education_ridit <- as.numeric(imputed_mcs2_7$education_ridit)
#recode so is replaced with the ridit scores
imputed_mcs2_7$education_ridit[imputed_mcs2_7$education_ridit == 1] <- mcs_education_ridit7[1,]
imputed_mcs2_7$education_ridit[imputed_mcs2_7$education_ridit == 2] <- mcs_education_ridit7[2,]
imputed_mcs2_7$education_ridit[imputed_mcs2_7$education_ridit == 3] <- mcs_education_ridit7[3,]
imputed_mcs2_7$education_ridit[imputed_mcs2_7$education_ridit == 4] <- mcs_education_ridit7[4,]


#imputed dataset 8
#get count data 
#then convert to ridit score with toridit()
mcs_education_ridit8 <- as.matrix(toridit(table(imputed_mcs2_8$highest_household_education)))
#add ridit score to dataset as new variable 
imputed_mcs2_8 = imputed_mcs2_8 %>% mutate(education_ridit = highest_household_education) 
imputed_mcs2_8$education_ridit <- as.numeric(imputed_mcs2_8$education_ridit)
#recode so is replaced with the ridit scores
imputed_mcs2_8$education_ridit[imputed_mcs2_8$education_ridit == 1] <- mcs_education_ridit8[1,]
imputed_mcs2_8$education_ridit[imputed_mcs2_8$education_ridit == 2] <- mcs_education_ridit8[2,]
imputed_mcs2_8$education_ridit[imputed_mcs2_8$education_ridit == 3] <- mcs_education_ridit8[3,]
imputed_mcs2_8$education_ridit[imputed_mcs2_8$education_ridit == 4] <- mcs_education_ridit8[4,]

#imputed dataset 9
#get count data 
#then convert to ridit score with toridit()
mcs_education_ridit9 <- as.matrix(toridit(table(imputed_mcs2_9$highest_household_education)))
#add ridit score to dataset as new variable 
imputed_mcs2_9 = imputed_mcs2_9 %>% mutate(education_ridit = highest_household_education) 
imputed_mcs2_9$education_ridit <- as.numeric(imputed_mcs2_9$education_ridit)
#recode so is replaced with the ridit scores
imputed_mcs2_9$education_ridit[imputed_mcs2_9$education_ridit == 1] <- mcs_education_ridit9[1,]
imputed_mcs2_9$education_ridit[imputed_mcs2_9$education_ridit == 2] <- mcs_education_ridit9[2,]
imputed_mcs2_9$education_ridit[imputed_mcs2_9$education_ridit == 3] <- mcs_education_ridit9[3,]
imputed_mcs2_9$education_ridit[imputed_mcs2_9$education_ridit == 4] <- mcs_education_ridit9[4,]

#imputed dataset 10
#get count data 
#then convert to ridit score with toridit()
mcs_education_ridit10 <- as.matrix(toridit(table(imputed_mcs2_10$highest_household_education)))
#add ridit score to dataset as new variable 
imputed_mcs2_10 = imputed_mcs2_10 %>% mutate(education_ridit = highest_household_education) 
imputed_mcs2_10$education_ridit <- as.numeric(imputed_mcs2_10$education_ridit)
#recode so is replaced with the ridit scores
imputed_mcs2_10$education_ridit[imputed_mcs2_10$education_ridit == 1] <- mcs_education_ridit10[1,]
imputed_mcs2_10$education_ridit[imputed_mcs2_10$education_ridit == 2] <- mcs_education_ridit10[2,]
imputed_mcs2_10$education_ridit[imputed_mcs2_10$education_ridit == 3] <- mcs_education_ridit10[3,]
imputed_mcs2_10$education_ridit[imputed_mcs2_10$education_ridit == 4] <- mcs_education_ridit10[4,]

#imputed dataset 11
#get count data 
#then convert to ridit score with toridit()
mcs_education_ridit11 <- as.matrix(toridit(table(imputed_mcs2_11$highest_household_education)))
#add ridit score to dataset as new variable 
imputed_mcs2_11 = imputed_mcs2_11 %>% mutate(education_ridit = highest_household_education) 
imputed_mcs2_11$education_ridit <- as.numeric(imputed_mcs2_11$education_ridit)
#recode so is replaced with the ridit scores
imputed_mcs2_11$education_ridit[imputed_mcs2_11$education_ridit == 1] <- mcs_education_ridit11[1,]
imputed_mcs2_11$education_ridit[imputed_mcs2_11$education_ridit == 2] <- mcs_education_ridit11[2,]
imputed_mcs2_11$education_ridit[imputed_mcs2_11$education_ridit == 3] <- mcs_education_ridit11[3,]
imputed_mcs2_11$education_ridit[imputed_mcs2_11$education_ridit == 4] <- mcs_education_ridit11[4,]


#imputed dataset 12
#get count data 
#then convert to ridit score with toridit()
mcs_education_ridit12 <- as.matrix(toridit(table(imputed_mcs2_12$highest_household_education)))
#add ridit score to dataset as new variable 
imputed_mcs2_12 = imputed_mcs2_12 %>% mutate(education_ridit = highest_household_education) 
imputed_mcs2_12$education_ridit <- as.numeric(imputed_mcs2_12$education_ridit)
#recode so is replaced with the ridit scores
imputed_mcs2_12$education_ridit[imputed_mcs2_12$education_ridit == 1] <- mcs_education_ridit12[1,]
imputed_mcs2_12$education_ridit[imputed_mcs2_12$education_ridit == 2] <- mcs_education_ridit12[2,]
imputed_mcs2_12$education_ridit[imputed_mcs2_12$education_ridit == 3] <- mcs_education_ridit12[3,]
imputed_mcs2_12$education_ridit[imputed_mcs2_12$education_ridit == 4] <- mcs_education_ridit12[4,]

#imputed dataset 13
#get count data 
#then convert to ridit score with toridit()
mcs_education_ridit13 <- as.matrix(toridit(table(imputed_mcs2_13$highest_household_education)))
#add ridit score to dataset as new variable 
imputed_mcs2_13 = imputed_mcs2_13 %>% mutate(education_ridit = highest_household_education) 
imputed_mcs2_13$education_ridit <- as.numeric(imputed_mcs2_13$education_ridit)
#recode so is replaced with the ridit scores
imputed_mcs2_13$education_ridit[imputed_mcs2_13$education_ridit == 1] <- mcs_education_ridit13[1,]
imputed_mcs2_13$education_ridit[imputed_mcs2_13$education_ridit == 2] <- mcs_education_ridit13[2,]
imputed_mcs2_13$education_ridit[imputed_mcs2_13$education_ridit == 3] <- mcs_education_ridit13[3,]
imputed_mcs2_13$education_ridit[imputed_mcs2_13$education_ridit == 4] <- mcs_education_ridit13[4,]

#imputed dataset 14
#get count data 
#then convert to ridit score with toridit()
mcs_education_ridit14 <- as.matrix(toridit(table(imputed_mcs2_14$highest_household_education)))
#add ridit score to dataset as new variable 
imputed_mcs2_14 = imputed_mcs2_14 %>% mutate(education_ridit = highest_household_education) 
imputed_mcs2_14$education_ridit <- as.numeric(imputed_mcs2_14$education_ridit)
#recode so is replaced with the ridit scores
imputed_mcs2_14$education_ridit[imputed_mcs2_14$education_ridit == 1] <- mcs_education_ridit14[1,]
imputed_mcs2_14$education_ridit[imputed_mcs2_14$education_ridit == 2] <- mcs_education_ridit14[2,]
imputed_mcs2_14$education_ridit[imputed_mcs2_14$education_ridit == 3] <- mcs_education_ridit14[3,]
imputed_mcs2_14$education_ridit[imputed_mcs2_14$education_ridit == 4] <- mcs_education_ridit14[4,]

#imputed dataset 15
#get count data 
#then convert to ridit score with toridit()
mcs_education_ridit15 <- as.matrix(toridit(table(imputed_mcs2_15$highest_household_education)))
#add ridit score to dataset as new variable 
imputed_mcs2_15 = imputed_mcs2_15 %>% mutate(education_ridit = highest_household_education) 
imputed_mcs2_15$education_ridit <- as.numeric(imputed_mcs2_15$education_ridit)
#recode so is replaced with the ridit scores
imputed_mcs2_15$education_ridit[imputed_mcs2_15$education_ridit == 1] <- mcs_education_ridit15[1,]
imputed_mcs2_15$education_ridit[imputed_mcs2_15$education_ridit == 2] <- mcs_education_ridit15[2,]
imputed_mcs2_15$education_ridit[imputed_mcs2_15$education_ridit == 3] <- mcs_education_ridit15[3,]
imputed_mcs2_15$education_ridit[imputed_mcs2_15$education_ridit == 4] <- mcs_education_ridit15[4,]

#imputed dataset 16
#get count data 
#then convert to ridit score with toridit()
mcs_education_ridit16 <- as.matrix(toridit(table(imputed_mcs2_16$highest_household_education)))
#add ridit score to dataset as new variable 
imputed_mcs2_16 = imputed_mcs2_16 %>% mutate(education_ridit = highest_household_education) 
imputed_mcs2_16$education_ridit <- as.numeric(imputed_mcs2_16$education_ridit)
#recode so is replaced with the ridit scores
imputed_mcs2_16$education_ridit[imputed_mcs2_16$education_ridit == 1] <- mcs_education_ridit16[1,]
imputed_mcs2_16$education_ridit[imputed_mcs2_16$education_ridit == 2] <- mcs_education_ridit16[2,]
imputed_mcs2_16$education_ridit[imputed_mcs2_16$education_ridit == 3] <- mcs_education_ridit16[3,]
imputed_mcs2_16$education_ridit[imputed_mcs2_16$education_ridit == 4] <- mcs_education_ridit16[4,]

#imputed dataset 17
#get count data 
#then convert to ridit score with toridit()
mcs_education_ridit17 <- as.matrix(toridit(table(imputed_mcs2_17$highest_household_education)))
#add ridit score to dataset as new variable 
imputed_mcs2_17 = imputed_mcs2_17 %>% mutate(education_ridit = highest_household_education) 
imputed_mcs2_17$education_ridit <- as.numeric(imputed_mcs2_17$education_ridit)
#recode so is replaced with the ridit scores
imputed_mcs2_17$education_ridit[imputed_mcs2_17$education_ridit == 1] <- mcs_education_ridit17[1,]
imputed_mcs2_17$education_ridit[imputed_mcs2_17$education_ridit == 2] <- mcs_education_ridit17[2,]
imputed_mcs2_17$education_ridit[imputed_mcs2_17$education_ridit == 3] <- mcs_education_ridit17[3,]
imputed_mcs2_17$education_ridit[imputed_mcs2_17$education_ridit == 4] <- mcs_education_ridit17[4,]

#imputed dataset 18
#get count data 
#then convert to ridit score with toridit()
mcs_education_ridit18 <- as.matrix(toridit(table(imputed_mcs2_18$highest_household_education)))
#add ridit score to dataset as new variable 
imputed_mcs2_18 = imputed_mcs2_18 %>% mutate(education_ridit = highest_household_education) 
imputed_mcs2_18$education_ridit <- as.numeric(imputed_mcs2_18$education_ridit)
#recode so is replaced with the ridit scores
imputed_mcs2_18$education_ridit[imputed_mcs2_18$education_ridit == 1] <- mcs_education_ridit18[1,]
imputed_mcs2_18$education_ridit[imputed_mcs2_18$education_ridit == 2] <- mcs_education_ridit18[2,]
imputed_mcs2_18$education_ridit[imputed_mcs2_18$education_ridit == 3] <- mcs_education_ridit18[3,]
imputed_mcs2_18$education_ridit[imputed_mcs2_18$education_ridit == 4] <- mcs_education_ridit18[4,]

#imputed dataset 19
#get count data 
#then convert to ridit score with toridit()
mcs_education_ridit19 <- as.matrix(toridit(table(imputed_mcs2_19$highest_household_education)))
#add ridit score to dataset as new variable 
imputed_mcs2_19 = imputed_mcs2_19 %>% mutate(education_ridit = highest_household_education) 
imputed_mcs2_19$education_ridit <- as.numeric(imputed_mcs2_19$education_ridit)
#recode so is replaced with the ridit scores
imputed_mcs2_19$education_ridit[imputed_mcs2_19$education_ridit == 1] <- mcs_education_ridit19[1,]
imputed_mcs2_19$education_ridit[imputed_mcs2_19$education_ridit == 2] <- mcs_education_ridit19[2,]
imputed_mcs2_19$education_ridit[imputed_mcs2_19$education_ridit == 3] <- mcs_education_ridit19[3,]
imputed_mcs2_19$education_ridit[imputed_mcs2_19$education_ridit == 4] <- mcs_education_ridit19[4,]

#imputed dataset 20
#get count data 
#then convert to ridit score with toridit()
mcs_education_ridit20 <- as.matrix(toridit(table(imputed_mcs2_20$highest_household_education)))
#add ridit score to dataset as new variable 
imputed_mcs2_20 = imputed_mcs2_20 %>% mutate(education_ridit = highest_household_education) 
imputed_mcs2_20$education_ridit <- as.numeric(imputed_mcs2_20$education_ridit)
#recode so is replaced with the ridit scores
imputed_mcs2_20$education_ridit[imputed_mcs2_20$education_ridit == 1] <- mcs_education_ridit20[1,]
imputed_mcs2_20$education_ridit[imputed_mcs2_20$education_ridit == 2] <- mcs_education_ridit20[2,]
imputed_mcs2_20$education_ridit[imputed_mcs2_20$education_ridit == 3] <- mcs_education_ridit20[3,]
imputed_mcs2_20$education_ridit[imputed_mcs2_20$education_ridit == 4] <- mcs_education_ridit20[4,]

#imputed dataset 21
#get count data 
#then convert to ridit score with toridit()
mcs_education_ridit21 <- as.matrix(toridit(table(imputed_mcs2_21$highest_household_education)))
#add ridit score to dataset as new variable 
imputed_mcs2_21 = imputed_mcs2_21 %>% mutate(education_ridit = highest_household_education) 
imputed_mcs2_21$education_ridit <- as.numeric(imputed_mcs2_21$education_ridit)
#recode so is replaced with the ridit scores
imputed_mcs2_21$education_ridit[imputed_mcs2_21$education_ridit == 1] <- mcs_education_ridit21[1,]
imputed_mcs2_21$education_ridit[imputed_mcs2_21$education_ridit == 2] <- mcs_education_ridit21[2,]
imputed_mcs2_21$education_ridit[imputed_mcs2_21$education_ridit == 3] <- mcs_education_ridit21[3,]
imputed_mcs2_21$education_ridit[imputed_mcs2_21$education_ridit == 4] <- mcs_education_ridit21[4,]

#imputed dataset 22
#get count data 
#then convert to ridit score with toridit()
mcs_education_ridit22 <- as.matrix(toridit(table(imputed_mcs2_22$highest_household_education)))
#add ridit score to dataset as new variable 
imputed_mcs2_22 = imputed_mcs2_22 %>% mutate(education_ridit = highest_household_education) 
imputed_mcs2_22$education_ridit <- as.numeric(imputed_mcs2_22$education_ridit)
#recode so is replaced with the ridit scores
imputed_mcs2_22$education_ridit[imputed_mcs2_22$education_ridit == 1] <- mcs_education_ridit22[1,]
imputed_mcs2_22$education_ridit[imputed_mcs2_22$education_ridit == 2] <- mcs_education_ridit22[2,]
imputed_mcs2_22$education_ridit[imputed_mcs2_22$education_ridit == 3] <- mcs_education_ridit22[3,]
imputed_mcs2_22$education_ridit[imputed_mcs2_22$education_ridit == 4] <- mcs_education_ridit22[4,]

#imputed dataset 23
#get count data 
#then convert to ridit score with toridit()
mcs_education_ridit23 <- as.matrix(toridit(table(imputed_mcs2_23$highest_household_education)))
#add ridit score to dataset as new variable 
imputed_mcs2_23 = imputed_mcs2_23 %>% mutate(education_ridit = highest_household_education) 
imputed_mcs2_23$education_ridit <- as.numeric(imputed_mcs2_23$education_ridit)
#recode so is replaced with the ridit scores
imputed_mcs2_23$education_ridit[imputed_mcs2_23$education_ridit == 1] <- mcs_education_ridit23[1,]
imputed_mcs2_23$education_ridit[imputed_mcs2_23$education_ridit == 2] <- mcs_education_ridit23[2,]
imputed_mcs2_23$education_ridit[imputed_mcs2_23$education_ridit == 3] <- mcs_education_ridit23[3,]
imputed_mcs2_23$education_ridit[imputed_mcs2_23$education_ridit == 4] <- mcs_education_ridit23[4,]

#imputed dataset 24
#get count data 
#then convert to ridit score with toridit()
mcs_education_ridit24 <- as.matrix(toridit(table(imputed_mcs2_24$highest_household_education)))
#add ridit score to dataset as new variable 
imputed_mcs2_24 = imputed_mcs2_24 %>% mutate(education_ridit = highest_household_education) 
imputed_mcs2_24$education_ridit <- as.numeric(imputed_mcs2_24$education_ridit)
#recode so is replaced with the ridit scores
imputed_mcs2_24$education_ridit[imputed_mcs2_24$education_ridit == 1] <- mcs_education_ridit24[1,]
imputed_mcs2_24$education_ridit[imputed_mcs2_24$education_ridit == 2] <- mcs_education_ridit24[2,]
imputed_mcs2_24$education_ridit[imputed_mcs2_24$education_ridit == 3] <- mcs_education_ridit24[3,]
imputed_mcs2_24$education_ridit[imputed_mcs2_24$education_ridit == 4] <- mcs_education_ridit24[4,]

#imputed dataset 25
#get count data 
#then convert to ridit score with toridit()
mcs_education_ridit25 <- as.matrix(toridit(table(imputed_mcs2_25$highest_household_education)))
#add ridit score to dataset as new variable 
imputed_mcs2_25 = imputed_mcs2_25 %>% mutate(education_ridit = highest_household_education) 
imputed_mcs2_25$education_ridit <- as.numeric(imputed_mcs2_25$education_ridit)
#recode so is replaced with the ridit scores
imputed_mcs2_25$education_ridit[imputed_mcs2_25$education_ridit == 1] <- mcs_education_ridit25[1,]
imputed_mcs2_25$education_ridit[imputed_mcs2_25$education_ridit == 2] <- mcs_education_ridit25[2,]
imputed_mcs2_25$education_ridit[imputed_mcs2_25$education_ridit == 3] <- mcs_education_ridit25[3,]
imputed_mcs2_25$education_ridit[imputed_mcs2_25$education_ridit == 4] <- mcs_education_ridit25[4,]


#education regression models with education ridit as predictor variable. ####
#early childhood imputed datasets 1-25
#late childhood imputed datasets 1-25
#adolescent imputed datasets 1-25

#imputed dataset 1
mcs_early_edRidit1 <- lm(standardised_vocab5 ~ gender + ethnicity +  EAL + standardised_age5+  education_ridit, weights = weight, data=imputed_mcs2_1)
#imputed dataset 2
mcs_early_edRidit2 <- lm(standardised_vocab5 ~ gender + ethnicity +  EAL + standardised_age5+  education_ridit, weights = weight, data=imputed_mcs2_2)
#imputed dataset 3
mcs_early_edRidit3 <- lm(standardised_vocab5 ~ gender + ethnicity +  EAL + standardised_age5+  education_ridit, weights = weight, data=imputed_mcs2_3)
#imputed dataset 4
mcs_early_edRidit4 <- lm(standardised_vocab5 ~ gender + ethnicity +  EAL + standardised_age5+  education_ridit, weights = weight, data=imputed_mcs2_4)
#imputed dataset 5
mcs_early_edRidit5 <- lm(standardised_vocab5 ~ gender + ethnicity +  EAL + standardised_age5+  education_ridit, weights = weight, data=imputed_mcs2_5)
#imputed dataset 6
mcs_early_edRidit6 <- lm(standardised_vocab5 ~ gender + ethnicity +  EAL + standardised_age5+  education_ridit, weights = weight, data=imputed_mcs2_6)
#imputed dataset 7
mcs_early_edRidit7 <- lm(standardised_vocab5 ~ gender + ethnicity +  EAL + standardised_age5+  education_ridit, weights = weight, data=imputed_mcs2_7)
#imputed dataset 8
mcs_early_edRidit8 <- lm(standardised_vocab5 ~ gender + ethnicity +  EAL + standardised_age5+  education_ridit, weights = weight, data=imputed_mcs2_8)
#imputed dataset 9
mcs_early_edRidit9 <- lm(standardised_vocab5 ~ gender + ethnicity +  EAL + standardised_age5+  education_ridit, weights = weight, data=imputed_mcs2_9)
#imputed dataset 10
mcs_early_edRidit10 <- lm(standardised_vocab5 ~ gender + ethnicity +  EAL + standardised_age5+  education_ridit, weights = weight, data=imputed_mcs2_10)
#imputed dataset 11
mcs_early_edRidit11 <- lm(standardised_vocab5 ~ gender + ethnicity +  EAL + standardised_age5+  education_ridit, weights = weight, data=imputed_mcs2_11)
#imputed dataset 12
mcs_early_edRidit12 <- lm(standardised_vocab5 ~ gender + ethnicity +  EAL + standardised_age5+  education_ridit, weights = weight, data=imputed_mcs2_12)
#imputed dataset 13
mcs_early_edRidit13 <- lm(standardised_vocab5 ~ gender + ethnicity +  EAL + standardised_age5+  education_ridit, weights = weight, data=imputed_mcs2_13)
#imputed dataset 14
mcs_early_edRidit14 <- lm(standardised_vocab5 ~ gender + ethnicity +  EAL + standardised_age5+  education_ridit, weights = weight, data=imputed_mcs2_14)
#imputed dataset 15
mcs_early_edRidit15 <- lm(standardised_vocab5 ~ gender + ethnicity +  EAL + standardised_age5+  education_ridit, weights = weight, data=imputed_mcs2_15)
#imputed dataset 16
mcs_early_edRidit16 <- lm(standardised_vocab5 ~ gender + ethnicity +  EAL + standardised_age5+  education_ridit, weights = weight, data=imputed_mcs2_16)
#imputed dataset 17
mcs_early_edRidit17 <- lm(standardised_vocab5 ~ gender + ethnicity +  EAL + standardised_age5+  education_ridit, weights = weight, data=imputed_mcs2_17)
#imputed dataset 18
mcs_early_edRidit18 <- lm(standardised_vocab5 ~ gender + ethnicity +  EAL + standardised_age5+  education_ridit, weights = weight, data=imputed_mcs2_18)
#imputed dataset 19
mcs_early_edRidit19 <- lm(standardised_vocab5 ~ gender + ethnicity +  EAL + standardised_age5+  education_ridit, weights = weight, data=imputed_mcs2_19)
#imputed dataset 20
mcs_early_edRidit20 <- lm(standardised_vocab5 ~ gender + ethnicity +  EAL + standardised_age5+  education_ridit, weights = weight, data=imputed_mcs2_20)
#imputed dataset 21
mcs_early_edRidit21 <- lm(standardised_vocab5 ~ gender + ethnicity +  EAL + standardised_age5+  education_ridit, weights = weight, data=imputed_mcs2_21)
#imputed dataset 22
mcs_early_edRidit22 <- lm(standardised_vocab5 ~ gender + ethnicity +  EAL + standardised_age5+  education_ridit, weights = weight, data=imputed_mcs2_22)
#imputed dataset 23
mcs_early_edRidit23 <- lm(standardised_vocab5 ~ gender + ethnicity +  EAL + standardised_age5+  education_ridit, weights = weight, data=imputed_mcs2_23)
#imputed dataset 24
mcs_early_edRidit24 <- lm(standardised_vocab5 ~ gender + ethnicity +  EAL + standardised_age5+  education_ridit, weights = weight, data=imputed_mcs2_24)
#imputed dataset 25
mcs_early_edRidit25 <- lm(standardised_vocab5 ~ gender + ethnicity +  EAL + standardised_age5+  education_ridit, weights = weight, data=imputed_mcs2_25)


#late childhood education
#imputed dataset 1
mcs_late_edRidit1 <- lm(standardised_vocab11 ~ gender + ethnicity +  EAL + standardised_age11+  education_ridit, weights = weight, data=imputed_mcs2_1)
#imputed dataset 2
mcs_late_edRidit2 <- lm(standardised_vocab11 ~ gender + ethnicity +  EAL + standardised_age11+  education_ridit, weights = weight, data=imputed_mcs2_2)
#imputed dataset 3
mcs_late_edRidit3 <- lm(standardised_vocab11 ~ gender + ethnicity +  EAL + standardised_age11+  education_ridit, weights = weight, data=imputed_mcs2_3)
#imputed dataset 4
mcs_late_edRidit4 <- lm(standardised_vocab11 ~ gender + ethnicity +  EAL + standardised_age11+  education_ridit, weights = weight, data=imputed_mcs2_4)
#imputed dataset 5
mcs_late_edRidit5 <- lm(standardised_vocab11 ~ gender + ethnicity +  EAL + standardised_age11+  education_ridit, weights = weight, data=imputed_mcs2_5)
#imputed dataset 6
mcs_late_edRidit6 <- lm(standardised_vocab11 ~ gender + ethnicity +  EAL + standardised_age11+  education_ridit, weights = weight, data=imputed_mcs2_6)
#imputed dataset 7
mcs_late_edRidit7 <- lm(standardised_vocab11 ~ gender + ethnicity +  EAL + standardised_age11+  education_ridit, weights = weight, data=imputed_mcs2_7)
#imputed dataset 8
mcs_late_edRidit8 <- lm(standardised_vocab11 ~ gender + ethnicity +  EAL + standardised_age11+  education_ridit, weights = weight, data=imputed_mcs2_8)
#imputed dataset 9
mcs_late_edRidit9 <- lm(standardised_vocab11 ~ gender + ethnicity +  EAL + standardised_age11+  education_ridit, weights = weight, data=imputed_mcs2_9)
#imputed dataset 10
mcs_late_edRidit10 <- lm(standardised_vocab11 ~ gender + ethnicity +  EAL + standardised_age11+  education_ridit, weights = weight, data=imputed_mcs2_10)
#imputed dataset 11
mcs_late_edRidit11 <- lm(standardised_vocab11 ~ gender + ethnicity +  EAL + standardised_age11+  education_ridit, weights = weight, data=imputed_mcs2_11)
#imputed dataset 12
mcs_late_edRidit12 <- lm(standardised_vocab11 ~ gender + ethnicity +  EAL + standardised_age11+  education_ridit, weights = weight, data=imputed_mcs2_12)
#imputed dataset 13
mcs_late_edRidit13 <- lm(standardised_vocab11 ~ gender + ethnicity +  EAL + standardised_age11+  education_ridit, weights = weight, data=imputed_mcs2_13)
#imputed dataset 14
mcs_late_edRidit14 <- lm(standardised_vocab11 ~ gender + ethnicity +  EAL + standardised_age11+  education_ridit, weights = weight, data=imputed_mcs2_14)
#imputed dataset 15
mcs_late_edRidit15 <- lm(standardised_vocab11 ~ gender + ethnicity +  EAL + standardised_age11+  education_ridit, weights = weight, data=imputed_mcs2_15)
#imputed dataset 16
mcs_late_edRidit16 <- lm(standardised_vocab11 ~ gender + ethnicity +  EAL + standardised_age11+  education_ridit, weights = weight, data=imputed_mcs2_16)
#imputed dataset 17
mcs_late_edRidit17 <- lm(standardised_vocab11 ~ gender + ethnicity +  EAL + standardised_age11+  education_ridit, weights = weight, data=imputed_mcs2_17)
#imputed dataset 18
mcs_late_edRidit18 <- lm(standardised_vocab11 ~ gender + ethnicity +  EAL + standardised_age11+  education_ridit, weights = weight, data=imputed_mcs2_18)
#imputed dataset 19
mcs_late_edRidit19 <- lm(standardised_vocab11 ~ gender + ethnicity +  EAL + standardised_age11+  education_ridit, weights = weight, data=imputed_mcs2_19)
#imputed dataset 20
mcs_late_edRidit20 <- lm(standardised_vocab11 ~ gender + ethnicity +  EAL + standardised_age11+  education_ridit, weights = weight, data=imputed_mcs2_20)
#imputed dataset 21
mcs_late_edRidit21 <- lm(standardised_vocab11 ~ gender + ethnicity +  EAL + standardised_age11+  education_ridit, weights = weight, data=imputed_mcs2_21)
#imputed dataset 22
mcs_late_edRidit22 <- lm(standardised_vocab11 ~ gender + ethnicity +  EAL + standardised_age11+  education_ridit, weights = weight, data=imputed_mcs2_22)
#imputed dataset 23
mcs_late_edRidit23 <- lm(standardised_vocab11 ~ gender + ethnicity +  EAL + standardised_age11+  education_ridit, weights = weight, data=imputed_mcs2_23)
#imputed dataset 24
mcs_late_edRidit24 <- lm(standardised_vocab11 ~ gender + ethnicity +  EAL + standardised_age11+  education_ridit, weights = weight, data=imputed_mcs2_24)
#imputed dataset 25
mcs_late_edRidit25 <- lm(standardised_vocab11 ~ gender + ethnicity +  EAL + standardised_age11+  education_ridit, weights = weight, data=imputed_mcs2_25)


#adolescent education 

#imputed dataset 1
mcs_adolescent_edRidit1 <- lm(standardised_vocab14 ~ gender + ethnicity +  EAL + standardised_age14+  education_ridit, weights = weight, data=imputed_mcs2_1)
#imputed dataset 2
mcs_adolescent_edRidit2 <- lm(standardised_vocab14 ~ gender + ethnicity +  EAL + standardised_age14+  education_ridit, weights = weight, data=imputed_mcs2_2)
#imputed dataset 3
mcs_adolescent_edRidit3 <- lm(standardised_vocab14 ~ gender + ethnicity +  EAL + standardised_age14+  education_ridit, weights = weight, data=imputed_mcs2_3)
#imputed dataset 4
mcs_adolescent_edRidit4 <- lm(standardised_vocab14 ~ gender + ethnicity +  EAL + standardised_age14+  education_ridit, weights = weight, data=imputed_mcs2_4)
#imputed dataset 5
mcs_adolescent_edRidit5 <- lm(standardised_vocab14 ~ gender + ethnicity +  EAL + standardised_age14+  education_ridit, weights = weight, data=imputed_mcs2_5)
#imputed dataset 6
mcs_adolescent_edRidit6 <- lm(standardised_vocab14 ~ gender + ethnicity +  EAL + standardised_age14+  education_ridit, weights = weight, data=imputed_mcs2_6)
#imputed dataset 7
mcs_adolescent_edRidit7 <- lm(standardised_vocab14 ~ gender + ethnicity +  EAL + standardised_age14+  education_ridit, weights = weight, data=imputed_mcs2_7)
#imputed dataset 8
mcs_adolescent_edRidit8 <- lm(standardised_vocab14 ~ gender + ethnicity +  EAL + standardised_age14+  education_ridit, weights = weight, data=imputed_mcs2_8)
#imputed dataset 9
mcs_adolescent_edRidit9 <- lm(standardised_vocab14 ~ gender + ethnicity +  EAL + standardised_age14+  education_ridit, weights = weight, data=imputed_mcs2_9)
#imputed dataset 10
mcs_adolescent_edRidit10 <- lm(standardised_vocab14 ~ gender + ethnicity +  EAL + standardised_age14+  education_ridit, weights = weight, data=imputed_mcs2_10)
#imputed dataset 11
mcs_adolescent_edRidit11 <- lm(standardised_vocab14 ~ gender + ethnicity +  EAL + standardised_age14+  education_ridit, weights = weight, data=imputed_mcs2_11)
#imputed dataset 12
mcs_adolescent_edRidit12 <- lm(standardised_vocab14 ~ gender + ethnicity +  EAL + standardised_age14+  education_ridit, weights = weight, data=imputed_mcs2_12)
#imputed dataset 13
mcs_adolescent_edRidit13 <- lm(standardised_vocab14 ~ gender + ethnicity +  EAL + standardised_age14+  education_ridit, weights = weight, data=imputed_mcs2_13)
#imputed dataset 14
mcs_adolescent_edRidit14 <- lm(standardised_vocab14 ~ gender + ethnicity +  EAL + standardised_age14+  education_ridit, weights = weight, data=imputed_mcs2_14)
#imputed dataset 15
mcs_adolescent_edRidit15 <- lm(standardised_vocab14 ~ gender + ethnicity +  EAL + standardised_age14+  education_ridit, weights = weight, data=imputed_mcs2_15)
#imputed dataset 16
mcs_adolescent_edRidit16 <- lm(standardised_vocab14 ~ gender + ethnicity +  EAL + standardised_age14+  education_ridit, weights = weight, data=imputed_mcs2_16)
#imputed dataset 17
mcs_adolescent_edRidit17 <- lm(standardised_vocab14 ~ gender + ethnicity +  EAL + standardised_age14+  education_ridit, weights = weight, data=imputed_mcs2_17)
#imputed dataset 18
mcs_adolescent_edRidit18 <- lm(standardised_vocab14 ~ gender + ethnicity +  EAL + standardised_age14+  education_ridit, weights = weight, data=imputed_mcs2_18)
#imputed dataset 19
mcs_adolescent_edRidit19 <- lm(standardised_vocab14 ~ gender + ethnicity +  EAL + standardised_age14+  education_ridit, weights = weight, data=imputed_mcs2_19)
#imputed dataset 20
mcs_adolescent_edRidit20 <- lm(standardised_vocab14 ~ gender + ethnicity +  EAL + standardised_age14+  education_ridit, weights = weight, data=imputed_mcs2_20)
#imputed dataset 21
mcs_adolescent_edRidit21 <- lm(standardised_vocab14 ~ gender + ethnicity +  EAL + standardised_age14+  education_ridit, weights = weight, data=imputed_mcs2_21)
#imputed dataset 22
mcs_adolescent_edRidit22 <- lm(standardised_vocab14 ~ gender + ethnicity +  EAL + standardised_age14+  education_ridit, weights = weight, data=imputed_mcs2_22)
#imputed dataset 23
mcs_adolescent_edRidit23 <- lm(standardised_vocab14 ~ gender + ethnicity +  EAL + standardised_age14+  education_ridit, weights = weight, data=imputed_mcs2_23)
#imputed dataset 24
mcs_adolescent_edRidit24 <- lm(standardised_vocab14 ~ gender + ethnicity +  EAL + standardised_age14+  education_ridit, weights = weight, data=imputed_mcs2_24)
#imputed dataset 25
mcs_adolescent_edRidit25 <- lm(standardised_vocab14 ~ gender + ethnicity +  EAL + standardised_age14+  education_ridit, weights = weight, data=imputed_mcs2_25)


#pool results ####

#early language
early_education_ridit <- summary(pool(as.mira(list(mcs_early_edRidit1,mcs_early_edRidit2, mcs_early_edRidit3, mcs_early_edRidit4, mcs_early_edRidit5, 
                                                  mcs_early_edRidit6, mcs_early_edRidit7, mcs_early_edRidit8, mcs_early_edRidit9, mcs_early_edRidit10, 
                                                  mcs_early_edRidit11, mcs_early_edRidit12, mcs_early_edRidit13, mcs_early_edRidit14, mcs_early_edRidit15, 
                                                  mcs_early_edRidit16, mcs_early_edRidit17, mcs_early_edRidit18, mcs_early_edRidit19, mcs_early_edRidit20, 
                                                  mcs_early_edRidit21, mcs_early_edRidit22, mcs_early_edRidit23, mcs_early_edRidit24, mcs_early_edRidit25))))
round(early_education_ridit,2)
#age 3 confidence intervals 
lower_early_education_ridit = summary(pool(as.mira(list(mcs_early_edRidit1,mcs_early_edRidit2, mcs_early_edRidit3, mcs_early_edRidit4, mcs_early_edRidit5, 
                                                       mcs_early_edRidit6, mcs_early_edRidit7, mcs_early_edRidit8, mcs_early_edRidit9, mcs_early_edRidit10, 
                                                       mcs_early_edRidit11, mcs_early_edRidit12, mcs_early_edRidit13, mcs_early_edRidit14, mcs_early_edRidit15, 
                                                       mcs_early_edRidit16, mcs_early_edRidit17, mcs_early_edRidit18, mcs_early_edRidit19, mcs_early_edRidit20, 
                                                       mcs_early_edRidit21, mcs_early_edRidit22, mcs_early_edRidit23, mcs_early_edRidit24, mcs_early_edRidit25))))$estimate-(
                                                         summary(pool(as.mira(list(mcs_early_edRidit1,mcs_early_edRidit2, mcs_early_edRidit3, mcs_early_edRidit4, mcs_early_edRidit5, 
                                                                                   mcs_early_edRidit6, mcs_early_edRidit7, mcs_early_edRidit8, mcs_early_edRidit9, mcs_early_edRidit10, 
                                                                                   mcs_early_edRidit11, mcs_early_edRidit12, mcs_early_edRidit13, mcs_early_edRidit14, mcs_early_edRidit15, 
                                                                                   mcs_early_edRidit16, mcs_early_edRidit17, mcs_early_edRidit18, mcs_early_edRidit19, mcs_early_edRidit20, 
                                                                                   mcs_early_edRidit21, mcs_early_edRidit22, mcs_early_edRidit23, mcs_early_edRidit24, mcs_early_edRidit25))))$std.error*1.96)
upper_early_education_ridit = summary(pool(as.mira(list(mcs_early_edRidit1,mcs_early_edRidit2, mcs_early_edRidit3, mcs_early_edRidit4, mcs_early_edRidit5, 
                                                       mcs_early_edRidit6, mcs_early_edRidit7, mcs_early_edRidit8, mcs_early_edRidit9, mcs_early_edRidit10, 
                                                       mcs_early_edRidit11, mcs_early_edRidit12, mcs_early_edRidit13, mcs_early_edRidit14, mcs_early_edRidit15, 
                                                       mcs_early_edRidit16, mcs_early_edRidit17, mcs_early_edRidit18, mcs_early_edRidit19, mcs_early_edRidit20, 
                                                       mcs_early_edRidit21, mcs_early_edRidit22, mcs_early_edRidit23, mcs_early_edRidit24, mcs_early_edRidit25))))$estimate+(
                                                         summary(pool(as.mira(list(mcs_early_edRidit1,mcs_early_edRidit2, mcs_early_edRidit3, mcs_early_edRidit4, mcs_early_edRidit5, 
                                                                                   mcs_early_edRidit6, mcs_early_edRidit7, mcs_early_edRidit8, mcs_early_edRidit9, mcs_early_edRidit10, 
                                                                                   mcs_early_edRidit11, mcs_early_edRidit12, mcs_early_edRidit13, mcs_early_edRidit14, mcs_early_edRidit15, 
                                                                                   mcs_early_edRidit16, mcs_early_edRidit17, mcs_early_edRidit18, mcs_early_edRidit19, mcs_early_edRidit20, 
                                                                                   mcs_early_edRidit21, mcs_early_edRidit22, mcs_early_edRidit23, mcs_early_edRidit24, mcs_early_edRidit25))))$std.error*1.96)
round(lower_early_education_ridit,2)
round(upper_early_education_ridit,2)
early_education_r2 = as.data.frame(pool.r.squared(as.mira(list(mcs_early_edRidit1,mcs_early_edRidit2, mcs_early_edRidit3, mcs_early_edRidit4, mcs_early_edRidit5, 
                                    mcs_early_edRidit6, mcs_early_edRidit7, mcs_early_edRidit8, mcs_early_edRidit9, mcs_early_edRidit10, 
                                    mcs_early_edRidit11, mcs_early_edRidit12, mcs_early_edRidit13, mcs_early_edRidit14, mcs_early_edRidit15, 
                                    mcs_early_edRidit16, mcs_early_edRidit17, mcs_early_edRidit18, mcs_early_edRidit19, mcs_early_edRidit20,
                                    mcs_early_edRidit21, mcs_early_edRidit22, mcs_early_edRidit23, mcs_early_edRidit24, mcs_early_edRidit25))))


#late language
late_education_ridit <- summary(pool(as.mira(list(mcs_late_edRidit1,mcs_late_edRidit2, mcs_late_edRidit3, mcs_late_edRidit4, mcs_late_edRidit5, 
                                                  mcs_late_edRidit6, mcs_late_edRidit7, mcs_late_edRidit8, mcs_late_edRidit9, mcs_late_edRidit10, 
                                                  mcs_late_edRidit11, mcs_late_edRidit12, mcs_late_edRidit13, mcs_late_edRidit14, mcs_late_edRidit15, 
                                                  mcs_late_edRidit16, mcs_late_edRidit17, mcs_late_edRidit18, mcs_late_edRidit19, mcs_late_edRidit20, 
                                                  mcs_late_edRidit21, mcs_late_edRidit22, mcs_late_edRidit23, mcs_late_edRidit24, mcs_late_edRidit25))))
round(late_education_ridit,2)
#age 3 confidence intervals 
lower_late_education_ridit = summary(pool(as.mira(list(mcs_late_edRidit1,mcs_late_edRidit2, mcs_late_edRidit3, mcs_late_edRidit4, mcs_late_edRidit5, 
                                                       mcs_late_edRidit6, mcs_late_edRidit7, mcs_late_edRidit8, mcs_late_edRidit9, mcs_late_edRidit10, 
                                                       mcs_late_edRidit11, mcs_late_edRidit12, mcs_late_edRidit13, mcs_late_edRidit14, mcs_late_edRidit15, 
                                                       mcs_late_edRidit16, mcs_late_edRidit17, mcs_late_edRidit18, mcs_late_edRidit19, mcs_late_edRidit20, 
                                                       mcs_late_edRidit21, mcs_late_edRidit22, mcs_late_edRidit23, mcs_late_edRidit24, mcs_late_edRidit25))))$estimate-(
                                                         summary(pool(as.mira(list(mcs_late_edRidit1,mcs_late_edRidit2, mcs_late_edRidit3, mcs_late_edRidit4, mcs_late_edRidit5, 
                                                                                   mcs_late_edRidit6, mcs_late_edRidit7, mcs_late_edRidit8, mcs_late_edRidit9, mcs_late_edRidit10, 
                                                                                   mcs_late_edRidit11, mcs_late_edRidit12, mcs_late_edRidit13, mcs_late_edRidit14, mcs_late_edRidit15, 
                                                                                   mcs_late_edRidit16, mcs_late_edRidit17, mcs_late_edRidit18, mcs_late_edRidit19, mcs_late_edRidit20, 
                                                                                   mcs_late_edRidit21, mcs_late_edRidit22, mcs_late_edRidit23, mcs_late_edRidit24, mcs_late_edRidit25))))$std.error*1.96)
upper_late_education_ridit = summary(pool(as.mira(list(mcs_late_edRidit1,mcs_late_edRidit2, mcs_late_edRidit3, mcs_late_edRidit4, mcs_late_edRidit5, 
                                                       mcs_late_edRidit6, mcs_late_edRidit7, mcs_late_edRidit8, mcs_late_edRidit9, mcs_late_edRidit10, 
                                                       mcs_late_edRidit11, mcs_late_edRidit12, mcs_late_edRidit13, mcs_late_edRidit14, mcs_late_edRidit15, 
                                                       mcs_late_edRidit16, mcs_late_edRidit17, mcs_late_edRidit18, mcs_late_edRidit19, mcs_late_edRidit20, 
                                                       mcs_late_edRidit21, mcs_late_edRidit22, mcs_late_edRidit23, mcs_late_edRidit24, mcs_late_edRidit25))))$estimate+(
                                                         summary(pool(as.mira(list(mcs_late_edRidit1,mcs_late_edRidit2, mcs_late_edRidit3, mcs_late_edRidit4, mcs_late_edRidit5, 
                                                                                   mcs_late_edRidit6, mcs_late_edRidit7, mcs_late_edRidit8, mcs_late_edRidit9, mcs_late_edRidit10, 
                                                                                   mcs_late_edRidit11, mcs_late_edRidit12, mcs_late_edRidit13, mcs_late_edRidit14, mcs_late_edRidit15, 
                                                                                   mcs_late_edRidit16, mcs_late_edRidit17, mcs_late_edRidit18, mcs_late_edRidit19, mcs_late_edRidit20, 
                                                                                   mcs_late_edRidit21, mcs_late_edRidit22, mcs_late_edRidit23, mcs_late_edRidit24, mcs_late_edRidit25))))$std.error*1.96)
round(lower_late_education_ridit,2)
round(upper_late_education_ridit,2)
late_education_r2 = as.data.frame(pool.r.squared(as.mira(list(mcs_late_edRidit1,mcs_late_edRidit2, mcs_late_edRidit3, mcs_late_edRidit4, mcs_late_edRidit5, 
                                    mcs_late_edRidit6, mcs_late_edRidit7, mcs_late_edRidit8, mcs_late_edRidit9, mcs_late_edRidit10, 
                                    mcs_late_edRidit11, mcs_late_edRidit12, mcs_late_edRidit13, mcs_late_edRidit14, mcs_late_edRidit15, 
                                    mcs_late_edRidit16, mcs_late_edRidit17, mcs_late_edRidit18, mcs_late_edRidit19, mcs_late_edRidit20,
                                    mcs_late_edRidit21, mcs_late_edRidit22, mcs_late_edRidit23, mcs_late_edRidit24, mcs_late_edRidit25))))

#adolescent language
adolescent_education_ridit <- summary(pool(as.mira(list(mcs_adolescent_edRidit1,mcs_adolescent_edRidit2, mcs_adolescent_edRidit3, mcs_adolescent_edRidit4, mcs_adolescent_edRidit5, 
                                                        mcs_adolescent_edRidit6, mcs_adolescent_edRidit7, mcs_adolescent_edRidit8, mcs_adolescent_edRidit9, mcs_adolescent_edRidit10, 
                                                        mcs_adolescent_edRidit11, mcs_adolescent_edRidit12, mcs_adolescent_edRidit13, mcs_adolescent_edRidit14, mcs_adolescent_edRidit15, 
                                                        mcs_adolescent_edRidit16, mcs_adolescent_edRidit17, mcs_adolescent_edRidit18, mcs_adolescent_edRidit19, mcs_adolescent_edRidit20, 
                                                        mcs_adolescent_edRidit21, mcs_adolescent_edRidit22, mcs_adolescent_edRidit23, mcs_adolescent_edRidit24, mcs_adolescent_edRidit25))))
round(adolescent_education_ridit,2)
#age 3 confidence intervals 
lower_adolescent_education_ridit = summary(pool(as.mira(list(mcs_adolescent_edRidit1,mcs_adolescent_edRidit2, mcs_adolescent_edRidit3, mcs_adolescent_edRidit4, mcs_adolescent_edRidit5, 
                                                             mcs_adolescent_edRidit6, mcs_adolescent_edRidit7, mcs_adolescent_edRidit8, mcs_adolescent_edRidit9, mcs_adolescent_edRidit10, 
                                                             mcs_adolescent_edRidit11, mcs_adolescent_edRidit12, mcs_adolescent_edRidit13, mcs_adolescent_edRidit14, mcs_adolescent_edRidit15, 
                                                             mcs_adolescent_edRidit16, mcs_adolescent_edRidit17, mcs_adolescent_edRidit18, mcs_adolescent_edRidit19, mcs_adolescent_edRidit20, 
                                                             mcs_adolescent_edRidit21, mcs_adolescent_edRidit22, mcs_adolescent_edRidit23, mcs_adolescent_edRidit24, mcs_adolescent_edRidit25))))$estimate-(
                                                               summary(pool(as.mira(list(mcs_adolescent_edRidit1,mcs_adolescent_edRidit2, mcs_adolescent_edRidit3, mcs_adolescent_edRidit4, mcs_adolescent_edRidit5, 
                                                                                         mcs_adolescent_edRidit6, mcs_adolescent_edRidit7, mcs_adolescent_edRidit8, mcs_adolescent_edRidit9, mcs_adolescent_edRidit10, 
                                                                                         mcs_adolescent_edRidit11, mcs_adolescent_edRidit12, mcs_adolescent_edRidit13, mcs_adolescent_edRidit14, mcs_adolescent_edRidit15, 
                                                                                         mcs_adolescent_edRidit16, mcs_adolescent_edRidit17, mcs_adolescent_edRidit18, mcs_adolescent_edRidit19, mcs_adolescent_edRidit20, 
                                                                                         mcs_adolescent_edRidit21, mcs_adolescent_edRidit22, mcs_adolescent_edRidit23, mcs_adolescent_edRidit24, mcs_adolescent_edRidit25))))$std.error*1.96)
upper_adolescent_education_ridit = summary(pool(as.mira(list(mcs_adolescent_edRidit1,mcs_adolescent_edRidit2, mcs_adolescent_edRidit3, mcs_adolescent_edRidit4, mcs_adolescent_edRidit5, 
                                                             mcs_adolescent_edRidit6, mcs_adolescent_edRidit7, mcs_adolescent_edRidit8, mcs_adolescent_edRidit9, mcs_adolescent_edRidit10, 
                                                             mcs_adolescent_edRidit11, mcs_adolescent_edRidit12, mcs_adolescent_edRidit13, mcs_adolescent_edRidit14, mcs_adolescent_edRidit15, 
                                                             mcs_adolescent_edRidit16, mcs_adolescent_edRidit17, mcs_adolescent_edRidit18, mcs_adolescent_edRidit19, mcs_adolescent_edRidit20, 
                                                             mcs_adolescent_edRidit21, mcs_adolescent_edRidit22, mcs_adolescent_edRidit23, mcs_adolescent_edRidit24, mcs_adolescent_edRidit25))))$estimate+(
                                                               summary(pool(as.mira(list(mcs_adolescent_edRidit1,mcs_adolescent_edRidit2, mcs_adolescent_edRidit3, mcs_adolescent_edRidit4, mcs_adolescent_edRidit5, 
                                                                                         mcs_adolescent_edRidit6, mcs_adolescent_edRidit7, mcs_adolescent_edRidit8, mcs_adolescent_edRidit9, mcs_adolescent_edRidit10, 
                                                                                         mcs_adolescent_edRidit11, mcs_adolescent_edRidit12, mcs_adolescent_edRidit13, mcs_adolescent_edRidit14, mcs_adolescent_edRidit15, 
                                                                                         mcs_adolescent_edRidit16, mcs_adolescent_edRidit17, mcs_adolescent_edRidit18, mcs_adolescent_edRidit19, mcs_adolescent_edRidit20, 
                                                                                         mcs_adolescent_edRidit21, mcs_adolescent_edRidit22, mcs_adolescent_edRidit23, mcs_adolescent_edRidit24, mcs_adolescent_edRidit25))))$std.error*1.96)
round(lower_adolescent_education_ridit,2)
round(upper_adolescent_education_ridit,2)
adolescent_education_r2 = as.data.frame(pool.r.squared(as.mira(list(mcs_adolescent_edRidit1,mcs_adolescent_edRidit2, mcs_adolescent_edRidit3, mcs_adolescent_edRidit4, mcs_adolescent_edRidit5, 
                                    mcs_adolescent_edRidit6, mcs_adolescent_edRidit7, mcs_adolescent_edRidit8, mcs_adolescent_edRidit9, mcs_adolescent_edRidit10, 
                                    mcs_adolescent_edRidit11, mcs_adolescent_edRidit12, mcs_adolescent_edRidit13, mcs_adolescent_edRidit14, mcs_adolescent_edRidit15, 
                                    mcs_adolescent_edRidit16, mcs_adolescent_edRidit17, mcs_adolescent_edRidit18, mcs_adolescent_edRidit19, mcs_adolescent_edRidit20,
                                    mcs_adolescent_edRidit21, mcs_adolescent_edRidit22, mcs_adolescent_edRidit23, mcs_adolescent_edRidit24, mcs_adolescent_edRidit25))))






#OCCUPATIONAL STATUS ####
#make sure have recoded this variable so that 1=unemployed so can remove everyone from this! 
#imputed dataset 1
#remove unemployed from analysis (occupational_status=1) - want to compare routine with profressional. 
imputed_mcs2_1 <- imputed_mcs2_1 %>%  filter(occupational_status !=1)
#get count data 
#then convert to ridit score with toridit()
mcs_occupation_ridit1 <- as.matrix(toridit(table(imputed_mcs2_1$occupational_status)))
#add ridit score to dataset as new variable 
imputed_mcs2_1 = imputed_mcs2_1 %>% mutate(occupation_ridit = occupational_status) 
imputed_mcs2_1$occupation_ridit <- as.numeric(imputed_mcs2_1$occupation_ridit)
#recode so is replaced with the ridit scores
imputed_mcs2_1$occupation_ridit[imputed_mcs2_1$occupation_ridit == 2] <- mcs_occupation_ridit1[2,]
imputed_mcs2_1$occupation_ridit[imputed_mcs2_1$occupation_ridit == 3] <- mcs_occupation_ridit1[3,]
imputed_mcs2_1$occupation_ridit[imputed_mcs2_1$occupation_ridit == 4] <- mcs_occupation_ridit1[4,]

#imputed dataset 2
#remove unemployed from analysis (occupational_status=1) - want to compare routine with profressional. 
imputed_mcs2_2 <- imputed_mcs2_2 %>%  filter(occupational_status !=1)
#get count data 
#then convert to ridit score with toridit()
mcs_occupation_ridit2 <- as.matrix(toridit(table(imputed_mcs2_2$occupational_status)))
#add ridit score to dataset as new variable 
imputed_mcs2_2 = imputed_mcs2_2 %>% mutate(occupation_ridit = occupational_status) 
imputed_mcs2_2$occupation_ridit <- as.numeric(imputed_mcs2_2$occupation_ridit)
#recode so is replaced with the ridit scores
imputed_mcs2_2$occupation_ridit[imputed_mcs2_2$occupation_ridit == 2] <- mcs_occupation_ridit2[2,]
imputed_mcs2_2$occupation_ridit[imputed_mcs2_2$occupation_ridit == 3] <- mcs_occupation_ridit2[3,]
imputed_mcs2_2$occupation_ridit[imputed_mcs2_2$occupation_ridit == 4] <- mcs_occupation_ridit2[4,]

#imputed dataset 3
#remove unemployed from analysis (occupational_status=1) - want to compare routine with profressional. 
imputed_mcs2_3 <- imputed_mcs2_3 %>%  filter(occupational_status !=1)
#get count data 
#then convert to ridit score with toridit()
mcs_occupation_ridit3 <- as.matrix(toridit(table(imputed_mcs2_3$occupational_status)))
#add ridit score to dataset as new variable 
imputed_mcs2_3 = imputed_mcs2_3 %>% mutate(occupation_ridit = occupational_status) 
imputed_mcs2_3$occupation_ridit <- as.numeric(imputed_mcs2_3$occupation_ridit)
#recode so is replaced with the ridit scores
imputed_mcs2_3$occupation_ridit[imputed_mcs2_3$occupation_ridit == 2] <- mcs_occupation_ridit3[2,]
imputed_mcs2_3$occupation_ridit[imputed_mcs2_3$occupation_ridit == 3] <- mcs_occupation_ridit3[3,]
imputed_mcs2_3$occupation_ridit[imputed_mcs2_3$occupation_ridit == 4] <- mcs_occupation_ridit3[4,]

#imputed dataset 4
#remove unemployed from analysis (occupational_status=1) - want to compare routine with profressional. 
imputed_mcs2_4 <- imputed_mcs2_4 %>%  filter(occupational_status !=1)
#get count data 
#then convert to ridit score with toridit()
mcs_occupation_ridit4 <- as.matrix(toridit(table(imputed_mcs2_4$occupational_status)))
#add ridit score to dataset as new variable 
imputed_mcs2_4 = imputed_mcs2_4 %>% mutate(occupation_ridit = occupational_status) 
imputed_mcs2_4$occupation_ridit <- as.numeric(imputed_mcs2_4$occupation_ridit)
#recode so is replaced with the ridit scores
imputed_mcs2_4$occupation_ridit[imputed_mcs2_4$occupation_ridit == 2] <- mcs_occupation_ridit4[2,]
imputed_mcs2_4$occupation_ridit[imputed_mcs2_4$occupation_ridit == 3] <- mcs_occupation_ridit4[3,]
imputed_mcs2_4$occupation_ridit[imputed_mcs2_4$occupation_ridit == 4] <- mcs_occupation_ridit4[4,]


#imputed dataset 5
#remove unemployed from analysis (occupational_status=1) - want to compare routine with profressional. 
imputed_mcs2_5 <- imputed_mcs2_5 %>%  filter(occupational_status !=1)
#get count data 
#then convert to ridit score with toridit()
mcs_occupation_ridit5 <- as.matrix(toridit(table(imputed_mcs2_5$occupational_status)))
#add ridit score to dataset as new variable 
imputed_mcs2_5 = imputed_mcs2_5 %>% mutate(occupation_ridit = occupational_status) 
imputed_mcs2_5$occupation_ridit <- as.numeric(imputed_mcs2_5$occupation_ridit)
#recode so is replaced with the ridit scores
imputed_mcs2_5$occupation_ridit[imputed_mcs2_5$occupation_ridit == 2] <- mcs_occupation_ridit5[2,]
imputed_mcs2_5$occupation_ridit[imputed_mcs2_5$occupation_ridit == 3] <- mcs_occupation_ridit5[3,]
imputed_mcs2_5$occupation_ridit[imputed_mcs2_5$occupation_ridit == 4] <- mcs_occupation_ridit5[4,]

#imputed dataset 6
#remove unemployed from analysis (occupational_status=1) - want to compare routine with profressional. 
imputed_mcs2_6 <- imputed_mcs2_6 %>%  filter(occupational_status !=1)
#get count data 
#then convert to ridit score with toridit()
mcs_occupation_ridit6 <- as.matrix(toridit(table(imputed_mcs2_6$occupational_status)))
#add ridit score to dataset as new variable 
imputed_mcs2_6 = imputed_mcs2_6 %>% mutate(occupation_ridit = occupational_status) 
imputed_mcs2_6$occupation_ridit <- as.numeric(imputed_mcs2_6$occupation_ridit)
#recode so is replaced with the ridit scores
imputed_mcs2_6$occupation_ridit[imputed_mcs2_6$occupation_ridit == 2] <- mcs_occupation_ridit6[2,]
imputed_mcs2_6$occupation_ridit[imputed_mcs2_6$occupation_ridit == 3] <- mcs_occupation_ridit6[3,]
imputed_mcs2_6$occupation_ridit[imputed_mcs2_6$occupation_ridit == 4] <- mcs_occupation_ridit6[4,]


#imputed dataset 7
#remove unemployed from analysis (occupational_status=1) - want to compare routine with profressional. 
imputed_mcs2_7 <- imputed_mcs2_7 %>%  filter(occupational_status !=1)
#get count data 
#then convert to ridit score with toridit()
mcs_occupation_ridit7 <- as.matrix(toridit(table(imputed_mcs2_7$occupational_status)))
#add ridit score to dataset as new variable 
imputed_mcs2_7 = imputed_mcs2_7 %>% mutate(occupation_ridit = occupational_status) 
imputed_mcs2_7$occupation_ridit <- as.numeric(imputed_mcs2_7$occupation_ridit)
#recode so is replaced with the ridit scores
imputed_mcs2_7$occupation_ridit[imputed_mcs2_7$occupation_ridit == 2] <- mcs_occupation_ridit7[2,]
imputed_mcs2_7$occupation_ridit[imputed_mcs2_7$occupation_ridit == 3] <- mcs_occupation_ridit7[3,]
imputed_mcs2_7$occupation_ridit[imputed_mcs2_7$occupation_ridit == 4] <- mcs_occupation_ridit7[4,]

#imputed dataset 8
#remove unemployed from analysis (occupational_status=1) - want to compare routine with profressional. 
imputed_mcs2_8 <- imputed_mcs2_8 %>%  filter(occupational_status !=1)
#get count data 
#then convert to ridit score with toridit()
mcs_occupation_ridit8 <- as.matrix(toridit(table(imputed_mcs2_8$occupational_status)))
#add ridit score to dataset as new variable 
imputed_mcs2_8 = imputed_mcs2_8 %>% mutate(occupation_ridit = occupational_status) 
imputed_mcs2_8$occupation_ridit <- as.numeric(imputed_mcs2_8$occupation_ridit)
#recode so is replaced with the ridit scores
imputed_mcs2_8$occupation_ridit[imputed_mcs2_8$occupation_ridit == 2] <- mcs_occupation_ridit8[2,]
imputed_mcs2_8$occupation_ridit[imputed_mcs2_8$occupation_ridit == 3] <- mcs_occupation_ridit8[3,]
imputed_mcs2_8$occupation_ridit[imputed_mcs2_8$occupation_ridit == 4] <- mcs_occupation_ridit8[4,]

#imputed dataset 9
#remove unemployed from analysis (occupational_status=1) - want to compare routine with profressional. 
imputed_mcs2_9 <- imputed_mcs2_9 %>%  filter(occupational_status !=1)
#get count data 
#then convert to ridit score with toridit()
mcs_occupation_ridit9 <- as.matrix(toridit(table(imputed_mcs2_9$occupational_status)))
#add ridit score to dataset as new variable 
imputed_mcs2_9 = imputed_mcs2_9 %>% mutate(occupation_ridit = occupational_status) 
imputed_mcs2_9$occupation_ridit <- as.numeric(imputed_mcs2_9$occupation_ridit)
#recode so is replaced with the ridit scores
imputed_mcs2_9$occupation_ridit[imputed_mcs2_9$occupation_ridit == 2] <- mcs_occupation_ridit9[2,]
imputed_mcs2_9$occupation_ridit[imputed_mcs2_9$occupation_ridit == 3] <- mcs_occupation_ridit9[3,]
imputed_mcs2_9$occupation_ridit[imputed_mcs2_9$occupation_ridit == 4] <- mcs_occupation_ridit9[4,]


#imputed dataset 10
#remove unemployed from analysis (occupational_status=1) - want to compare routine with profressional. 
imputed_mcs2_10 <- imputed_mcs2_10 %>%  filter(occupational_status !=1)
#get count data 
#then convert to ridit score with toridit()
mcs_occupation_ridit10 <- as.matrix(toridit(table(imputed_mcs2_10$occupational_status)))
#add ridit score to dataset as new variable 
imputed_mcs2_10 = imputed_mcs2_10 %>% mutate(occupation_ridit = occupational_status) 
imputed_mcs2_10$occupation_ridit <- as.numeric(imputed_mcs2_10$occupation_ridit)
#recode so is replaced with the ridit scores
imputed_mcs2_10$occupation_ridit[imputed_mcs2_10$occupation_ridit == 2] <- mcs_occupation_ridit10[2,]
imputed_mcs2_10$occupation_ridit[imputed_mcs2_10$occupation_ridit == 3] <- mcs_occupation_ridit10[3,]
imputed_mcs2_10$occupation_ridit[imputed_mcs2_10$occupation_ridit == 4] <- mcs_occupation_ridit10[4,]

#imputed dataset 11
#remove unemployed from analysis (occupational_status=1) - want to compare routine with profressional. 
imputed_mcs2_11 <- imputed_mcs2_11 %>%  filter(occupational_status !=1)
#get count data 
#then convert to ridit score with toridit()
mcs_occupation_ridit11 <- as.matrix(toridit(table(imputed_mcs2_11$occupational_status)))
#add ridit score to dataset as new variable 
imputed_mcs2_11 = imputed_mcs2_11 %>% mutate(occupation_ridit = occupational_status) 
imputed_mcs2_11$occupation_ridit <- as.numeric(imputed_mcs2_11$occupation_ridit)
#recode so is replaced with the ridit scores
imputed_mcs2_11$occupation_ridit[imputed_mcs2_11$occupation_ridit == 2] <- mcs_occupation_ridit11[2,]
imputed_mcs2_11$occupation_ridit[imputed_mcs2_11$occupation_ridit == 3] <- mcs_occupation_ridit11[3,]
imputed_mcs2_11$occupation_ridit[imputed_mcs2_11$occupation_ridit == 4] <- mcs_occupation_ridit11[4,]


#imputed dataset 12
#remove unemployed from analysis (occupational_status=1) - want to compare routine with profressional. 
imputed_mcs2_12 <- imputed_mcs2_12 %>%  filter(occupational_status !=1)
#get count data 
#then convert to ridit score with toridit()
mcs_occupation_ridit12 <- as.matrix(toridit(table(imputed_mcs2_12$occupational_status)))
#add ridit score to dataset as new variable 
imputed_mcs2_12 = imputed_mcs2_12 %>% mutate(occupation_ridit = occupational_status) 
imputed_mcs2_12$occupation_ridit <- as.numeric(imputed_mcs2_12$occupation_ridit)
#recode so is replaced with the ridit scores
imputed_mcs2_12$occupation_ridit[imputed_mcs2_12$occupation_ridit == 2] <- mcs_occupation_ridit12[2,]
imputed_mcs2_12$occupation_ridit[imputed_mcs2_12$occupation_ridit == 3] <- mcs_occupation_ridit12[3,]
imputed_mcs2_12$occupation_ridit[imputed_mcs2_12$occupation_ridit == 4] <- mcs_occupation_ridit12[4,]

#imputed dataset 13
#remove unemployed from analysis (occupational_status=1) - want to compare routine with profressional. 
imputed_mcs2_13 <- imputed_mcs2_13 %>%  filter(occupational_status !=1)
#get count data 
#then convert to ridit score with toridit()
mcs_occupation_ridit13 <- as.matrix(toridit(table(imputed_mcs2_13$occupational_status)))
#add ridit score to dataset as new variable 
imputed_mcs2_13 = imputed_mcs2_13 %>% mutate(occupation_ridit = occupational_status) 
imputed_mcs2_13$occupation_ridit <- as.numeric(imputed_mcs2_13$occupation_ridit)
#recode so is replaced with the ridit scores
imputed_mcs2_13$occupation_ridit[imputed_mcs2_13$occupation_ridit == 2] <- mcs_occupation_ridit13[2,]
imputed_mcs2_13$occupation_ridit[imputed_mcs2_13$occupation_ridit == 3] <- mcs_occupation_ridit13[3,]
imputed_mcs2_13$occupation_ridit[imputed_mcs2_13$occupation_ridit == 4] <- mcs_occupation_ridit13[4,]

#imputed dataset 14
#remove unemployed from analysis (occupational_status=1) - want to compare routine with profressional. 
imputed_mcs2_14 <- imputed_mcs2_14 %>%  filter(occupational_status !=1)
#get count data 
#then convert to ridit score with toridit()
mcs_occupation_ridit14 <- as.matrix(toridit(table(imputed_mcs2_14$occupational_status)))
#add ridit score to dataset as new variable 
imputed_mcs2_14 = imputed_mcs2_14 %>% mutate(occupation_ridit = occupational_status) 
imputed_mcs2_14$occupation_ridit <- as.numeric(imputed_mcs2_14$occupation_ridit)
#recode so is replaced with the ridit scores
imputed_mcs2_14$occupation_ridit[imputed_mcs2_14$occupation_ridit == 2] <- mcs_occupation_ridit14[2,]
imputed_mcs2_14$occupation_ridit[imputed_mcs2_14$occupation_ridit == 3] <- mcs_occupation_ridit14[3,]
imputed_mcs2_14$occupation_ridit[imputed_mcs2_14$occupation_ridit == 4] <- mcs_occupation_ridit14[4,]

#imputed dataset 15
#remove unemployed from analysis (occupational_status=1) - want to compare routine with profressional. 
imputed_mcs2_15 <- imputed_mcs2_15 %>%  filter(occupational_status !=1)
#get count data 
#then convert to ridit score with toridit()
mcs_occupation_ridit15 <- as.matrix(toridit(table(imputed_mcs2_15$occupational_status)))
#add ridit score to dataset as new variable 
imputed_mcs2_15 = imputed_mcs2_15 %>% mutate(occupation_ridit = occupational_status) 
imputed_mcs2_15$occupation_ridit <- as.numeric(imputed_mcs2_15$occupation_ridit)
#recode so is replaced with the ridit scores
imputed_mcs2_15$occupation_ridit[imputed_mcs2_15$occupation_ridit == 2] <- mcs_occupation_ridit15[2,]
imputed_mcs2_15$occupation_ridit[imputed_mcs2_15$occupation_ridit == 3] <- mcs_occupation_ridit15[3,]
imputed_mcs2_15$occupation_ridit[imputed_mcs2_15$occupation_ridit == 4] <- mcs_occupation_ridit15[4,]


#imputed dataset 16
#remove unemployed from analysis (occupational_status=1) - want to compare routine with profressional. 
imputed_mcs2_16 <- imputed_mcs2_16 %>%  filter(occupational_status !=1)
#get count data 
#then convert to ridit score with toridit()
mcs_occupation_ridit16 <- as.matrix(toridit(table(imputed_mcs2_16$occupational_status)))
#add ridit score to dataset as new variable 
imputed_mcs2_16 = imputed_mcs2_16 %>% mutate(occupation_ridit = occupational_status) 
imputed_mcs2_16$occupation_ridit <- as.numeric(imputed_mcs2_16$occupation_ridit)
#recode so is replaced with the ridit scores
imputed_mcs2_16$occupation_ridit[imputed_mcs2_16$occupation_ridit == 2] <- mcs_occupation_ridit16[2,]
imputed_mcs2_16$occupation_ridit[imputed_mcs2_16$occupation_ridit == 3] <- mcs_occupation_ridit16[3,]
imputed_mcs2_16$occupation_ridit[imputed_mcs2_16$occupation_ridit == 4] <- mcs_occupation_ridit16[4,]

#imputed dataset 17
#remove unemployed from analysis (occupational_status=1) - want to compare routine with profressional. 
imputed_mcs2_17 <- imputed_mcs2_17 %>%  filter(occupational_status !=1)
#get count data 
#then convert to ridit score with toridit()
mcs_occupation_ridit17 <- as.matrix(toridit(table(imputed_mcs2_17$occupational_status)))
#add ridit score to dataset as new variable 
imputed_mcs2_17 = imputed_mcs2_17 %>% mutate(occupation_ridit = occupational_status) 
imputed_mcs2_17$occupation_ridit <- as.numeric(imputed_mcs2_17$occupation_ridit)
#recode so is replaced with the ridit scores
imputed_mcs2_17$occupation_ridit[imputed_mcs2_17$occupation_ridit == 2] <- mcs_occupation_ridit17[2,]
imputed_mcs2_17$occupation_ridit[imputed_mcs2_17$occupation_ridit == 3] <- mcs_occupation_ridit17[3,]
imputed_mcs2_17$occupation_ridit[imputed_mcs2_17$occupation_ridit == 4] <- mcs_occupation_ridit17[4,]

#imputed dataset 18
#remove unemployed from analysis (occupational_status=1) - want to compare routine with profressional. 
imputed_mcs2_18 <- imputed_mcs2_18 %>%  filter(occupational_status !=1)
#get count data 
#then convert to ridit score with toridit()
mcs_occupation_ridit18 <- as.matrix(toridit(table(imputed_mcs2_18$occupational_status)))
#add ridit score to dataset as new variable 
imputed_mcs2_18 = imputed_mcs2_18 %>% mutate(occupation_ridit = occupational_status) 
imputed_mcs2_18$occupation_ridit <- as.numeric(imputed_mcs2_18$occupation_ridit)
#recode so is replaced with the ridit scores
imputed_mcs2_18$occupation_ridit[imputed_mcs2_18$occupation_ridit == 2] <- mcs_occupation_ridit18[2,]
imputed_mcs2_18$occupation_ridit[imputed_mcs2_18$occupation_ridit == 3] <- mcs_occupation_ridit18[3,]
imputed_mcs2_18$occupation_ridit[imputed_mcs2_18$occupation_ridit == 4] <- mcs_occupation_ridit18[4,]

#imputed dataset 19
#remove unemployed from analysis (occupational_status=1) - want to compare routine with profressional. 
imputed_mcs2_19 <- imputed_mcs2_19 %>%  filter(occupational_status !=1)
#get count data 
#then convert to ridit score with toridit()
mcs_occupation_ridit19 <- as.matrix(toridit(table(imputed_mcs2_19$occupational_status)))
#add ridit score to dataset as new variable 
imputed_mcs2_19 = imputed_mcs2_19 %>% mutate(occupation_ridit = occupational_status) 
imputed_mcs2_19$occupation_ridit <- as.numeric(imputed_mcs2_19$occupation_ridit)
#recode so is replaced with the ridit scores
imputed_mcs2_19$occupation_ridit[imputed_mcs2_19$occupation_ridit == 2] <- mcs_occupation_ridit19[2,]
imputed_mcs2_19$occupation_ridit[imputed_mcs2_19$occupation_ridit == 3] <- mcs_occupation_ridit19[3,]
imputed_mcs2_19$occupation_ridit[imputed_mcs2_19$occupation_ridit == 4] <- mcs_occupation_ridit19[4,]

#imputed dataset 20
#remove unemployed from analysis (occupational_status=1) - want to compare routine with profressional. 
imputed_mcs2_20 <- imputed_mcs2_20 %>%  filter(occupational_status !=1)
#get count data 
#then convert to ridit score with toridit()
mcs_occupation_ridit20 <- as.matrix(toridit(table(imputed_mcs2_20$occupational_status)))
#add ridit score to dataset as new variable 
imputed_mcs2_20 = imputed_mcs2_20 %>% mutate(occupation_ridit = occupational_status) 
imputed_mcs2_20$occupation_ridit <- as.numeric(imputed_mcs2_20$occupation_ridit)
#recode so is replaced with the ridit scores
imputed_mcs2_20$occupation_ridit[imputed_mcs2_20$occupation_ridit == 2] <- mcs_occupation_ridit20[2,]
imputed_mcs2_20$occupation_ridit[imputed_mcs2_20$occupation_ridit == 3] <- mcs_occupation_ridit20[3,]
imputed_mcs2_20$occupation_ridit[imputed_mcs2_20$occupation_ridit == 4] <- mcs_occupation_ridit20[4,]

#imputed dataset 21
#remove unemployed from analysis (occupational_status=1) - want to compare routine with profressional. 
imputed_mcs2_21 <- imputed_mcs2_21 %>%  filter(occupational_status !=1)
#get count data 
#then convert to ridit score with toridit()
mcs_occupation_ridit21 <- as.matrix(toridit(table(imputed_mcs2_21$occupational_status)))
#add ridit score to dataset as new variable 
imputed_mcs2_21 = imputed_mcs2_21 %>% mutate(occupation_ridit = occupational_status) 
imputed_mcs2_21$occupation_ridit <- as.numeric(imputed_mcs2_21$occupation_ridit)
#recode so is replaced with the ridit scores
imputed_mcs2_21$occupation_ridit[imputed_mcs2_21$occupation_ridit == 2] <- mcs_occupation_ridit21[2,]
imputed_mcs2_21$occupation_ridit[imputed_mcs2_21$occupation_ridit == 3] <- mcs_occupation_ridit21[3,]
imputed_mcs2_21$occupation_ridit[imputed_mcs2_21$occupation_ridit == 4] <- mcs_occupation_ridit21[4,]

#imputed dataset 22
#remove unemployed from analysis (occupational_status=1) - want to compare routine with profressional. 
imputed_mcs2_22 <- imputed_mcs2_22 %>%  filter(occupational_status !=1)
#get count data 
#then convert to ridit score with toridit()
mcs_occupation_ridit22 <- as.matrix(toridit(table(imputed_mcs2_22$occupational_status)))
#add ridit score to dataset as new variable 
imputed_mcs2_22 = imputed_mcs2_22 %>% mutate(occupation_ridit = occupational_status) 
imputed_mcs2_22$occupation_ridit <- as.numeric(imputed_mcs2_22$occupation_ridit)
#recode so is replaced with the ridit scores
imputed_mcs2_22$occupation_ridit[imputed_mcs2_22$occupation_ridit == 2] <- mcs_occupation_ridit22[2,]
imputed_mcs2_22$occupation_ridit[imputed_mcs2_22$occupation_ridit == 3] <- mcs_occupation_ridit22[3,]
imputed_mcs2_22$occupation_ridit[imputed_mcs2_22$occupation_ridit == 4] <- mcs_occupation_ridit22[4,]


#imputed dataset 23
#remove unemployed from analysis (occupational_status=1) - want to compare routine with profressional. 
imputed_mcs2_23 <- imputed_mcs2_23 %>%  filter(occupational_status !=1)
#get count data 
#then convert to ridit score with toridit()
mcs_occupation_ridit23 <- as.matrix(toridit(table(imputed_mcs2_23$occupational_status)))
#add ridit score to dataset as new variable 
imputed_mcs2_23 = imputed_mcs2_23 %>% mutate(occupation_ridit = occupational_status) 
imputed_mcs2_23$occupation_ridit <- as.numeric(imputed_mcs2_23$occupation_ridit)
#recode so is replaced with the ridit scores
imputed_mcs2_23$occupation_ridit[imputed_mcs2_23$occupation_ridit == 2] <- mcs_occupation_ridit23[2,]
imputed_mcs2_23$occupation_ridit[imputed_mcs2_23$occupation_ridit == 3] <- mcs_occupation_ridit23[3,]
imputed_mcs2_23$occupation_ridit[imputed_mcs2_23$occupation_ridit == 4] <- mcs_occupation_ridit23[4,]

#imputed dataset 24
#remove unemployed from analysis (occupational_status=1) - want to compare routine with profressional. 
imputed_mcs2_24 <- imputed_mcs2_24 %>%  filter(occupational_status !=1)
#get count data 
#then convert to ridit score with toridit()
mcs_occupation_ridit24 <- as.matrix(toridit(table(imputed_mcs2_24$occupational_status)))
#add ridit score to dataset as new variable 
imputed_mcs2_24 = imputed_mcs2_24 %>% mutate(occupation_ridit = occupational_status) 
imputed_mcs2_24$occupation_ridit <- as.numeric(imputed_mcs2_24$occupation_ridit)
#recode so is replaced with the ridit scores
imputed_mcs2_24$occupation_ridit[imputed_mcs2_24$occupation_ridit == 2] <- mcs_occupation_ridit24[2,]
imputed_mcs2_24$occupation_ridit[imputed_mcs2_24$occupation_ridit == 3] <- mcs_occupation_ridit24[3,]
imputed_mcs2_24$occupation_ridit[imputed_mcs2_24$occupation_ridit == 4] <- mcs_occupation_ridit24[4,]

#imputed dataset 25
#remove unemployed from analysis (occupational_status=1) - want to compare routine with profressional. 
imputed_mcs2_25 <- imputed_mcs2_25 %>%  filter(occupational_status !=1)
#get count data 
#then convert to ridit score with toridit()
mcs_occupation_ridit25 <- as.matrix(toridit(table(imputed_mcs2_25$occupational_status)))
#add ridit score to dataset as new variable 
imputed_mcs2_25 = imputed_mcs2_25 %>% mutate(occupation_ridit = occupational_status) 
imputed_mcs2_25$occupation_ridit <- as.numeric(imputed_mcs2_25$occupation_ridit)
#recode so is replaced with the ridit scores
imputed_mcs2_25$occupation_ridit[imputed_mcs2_25$occupation_ridit == 2] <- mcs_occupation_ridit25[2,]
imputed_mcs2_25$occupation_ridit[imputed_mcs2_25$occupation_ridit == 3] <- mcs_occupation_ridit25[3,]
imputed_mcs2_25$occupation_ridit[imputed_mcs2_25$occupation_ridit == 4] <- mcs_occupation_ridit25[4,]



#occupation regression models with occupation ridit as predictor variable. ####
#early childhood imputed datasets 1-25
#late childhood imputed datasets 1-25
#adolescent imputed datasets 1-25

#imputed dataset 1
mcs_early_ocRidit1 <- lm(standardised_vocab5 ~ gender + ethnicity +  EAL + standardised_age5+  occupation_ridit, weights = weight, data=imputed_mcs2_1)
#imputed dataset 2
mcs_early_ocRidit2 <- lm(standardised_vocab5 ~ gender + ethnicity +  EAL + standardised_age5+  occupation_ridit, weights = weight, data=imputed_mcs2_2)
#imputed dataset 3
mcs_early_ocRidit3 <- lm(standardised_vocab5 ~ gender + ethnicity +  EAL + standardised_age5+  occupation_ridit, weights = weight, data=imputed_mcs2_3)
#imputed dataset 4
mcs_early_ocRidit4 <- lm(standardised_vocab5 ~ gender + ethnicity +  EAL + standardised_age5+  occupation_ridit, weights = weight, data=imputed_mcs2_4)
#imputed dataset 5
mcs_early_ocRidit5 <- lm(standardised_vocab5 ~ gender + ethnicity +  EAL + standardised_age5+  occupation_ridit, weights = weight, data=imputed_mcs2_5)
#imputed dataset 6
mcs_early_ocRidit6 <- lm(standardised_vocab5 ~ gender + ethnicity +  EAL + standardised_age5+  occupation_ridit, weights = weight, data=imputed_mcs2_6)
#imputed dataset 7
mcs_early_ocRidit7 <- lm(standardised_vocab5 ~ gender + ethnicity +  EAL + standardised_age5+  occupation_ridit, weights = weight, data=imputed_mcs2_7)
#imputed dataset 8
mcs_early_ocRidit8 <- lm(standardised_vocab5 ~ gender + ethnicity +  EAL + standardised_age5+  occupation_ridit, weights = weight, data=imputed_mcs2_8)
#imputed dataset 9
mcs_early_ocRidit9 <- lm(standardised_vocab5 ~ gender + ethnicity +  EAL + standardised_age5+  occupation_ridit, weights = weight, data=imputed_mcs2_9)
#imputed dataset 10
mcs_early_ocRidit10 <- lm(standardised_vocab5 ~ gender + ethnicity +  EAL + standardised_age5+  occupation_ridit, weights = weight, data=imputed_mcs2_10)
#imputed dataset 11
mcs_early_ocRidit11 <- lm(standardised_vocab5 ~ gender + ethnicity +  EAL + standardised_age5+  occupation_ridit, weights = weight, data=imputed_mcs2_11)
#imputed dataset 12
mcs_early_ocRidit12 <- lm(standardised_vocab5 ~ gender + ethnicity +  EAL + standardised_age5+  occupation_ridit, weights = weight, data=imputed_mcs2_12)
#imputed dataset 13
mcs_early_ocRidit13 <- lm(standardised_vocab5 ~ gender + ethnicity +  EAL + standardised_age5+  occupation_ridit, weights = weight, data=imputed_mcs2_13)
#imputed dataset 14
mcs_early_ocRidit14 <- lm(standardised_vocab5 ~ gender + ethnicity +  EAL + standardised_age5+  occupation_ridit, weights = weight, data=imputed_mcs2_14)
#imputed dataset 15
mcs_early_ocRidit15 <- lm(standardised_vocab5 ~ gender + ethnicity +  EAL + standardised_age5+  occupation_ridit, weights = weight, data=imputed_mcs2_15)
#imputed dataset 16
mcs_early_ocRidit16 <- lm(standardised_vocab5 ~ gender + ethnicity +  EAL + standardised_age5+  occupation_ridit, weights = weight, data=imputed_mcs2_16)
#imputed dataset 17
mcs_early_ocRidit17 <- lm(standardised_vocab5 ~ gender + ethnicity +  EAL + standardised_age5+  occupation_ridit, weights = weight, data=imputed_mcs2_17)
#imputed dataset 18
mcs_early_ocRidit18 <- lm(standardised_vocab5 ~ gender + ethnicity +  EAL + standardised_age5+  occupation_ridit, weights = weight, data=imputed_mcs2_18)
#imputed dataset 19
mcs_early_ocRidit19 <- lm(standardised_vocab5 ~ gender + ethnicity +  EAL + standardised_age5+  occupation_ridit, weights = weight, data=imputed_mcs2_19)
#imputed dataset 20
mcs_early_ocRidit20 <- lm(standardised_vocab5 ~ gender + ethnicity +  EAL + standardised_age5+  occupation_ridit, weights = weight, data=imputed_mcs2_20)
#imputed dataset 21
mcs_early_ocRidit21 <- lm(standardised_vocab5 ~ gender + ethnicity +  EAL + standardised_age5+  occupation_ridit, weights = weight, data=imputed_mcs2_21)
#imputed dataset 22
mcs_early_ocRidit22 <- lm(standardised_vocab5 ~ gender + ethnicity +  EAL + standardised_age5+  occupation_ridit, weights = weight, data=imputed_mcs2_22)
#imputed dataset 23
mcs_early_ocRidit23 <- lm(standardised_vocab5 ~ gender + ethnicity +  EAL + standardised_age5+  occupation_ridit, weights = weight, data=imputed_mcs2_23)
#imputed dataset 24
mcs_early_ocRidit24 <- lm(standardised_vocab5 ~ gender + ethnicity +  EAL + standardised_age5+  occupation_ridit, weights = weight, data=imputed_mcs2_24)
#imputed dataset 25
mcs_early_ocRidit25 <- lm(standardised_vocab5 ~ gender + ethnicity +  EAL + standardised_age5+  occupation_ridit, weights = weight, data=imputed_mcs2_25)


#late childhood education
#imputed dataset 1
mcs_late_ocRidit1 <- lm(standardised_vocab11 ~ gender + ethnicity +  EAL + standardised_age11+  occupation_ridit, weights = weight, data=imputed_mcs2_1)
#imputed dataset 2
mcs_late_ocRidit2 <- lm(standardised_vocab11 ~ gender + ethnicity +  EAL + standardised_age11+  occupation_ridit, weights = weight, data=imputed_mcs2_2)
#imputed dataset 3
mcs_late_ocRidit3 <- lm(standardised_vocab11 ~ gender + ethnicity +  EAL + standardised_age11+  occupation_ridit, weights = weight, data=imputed_mcs2_3)
#imputed dataset 4
mcs_late_ocRidit4 <- lm(standardised_vocab11 ~ gender + ethnicity +  EAL + standardised_age11+  occupation_ridit, weights = weight, data=imputed_mcs2_4)
#imputed dataset 5
mcs_late_ocRidit5 <- lm(standardised_vocab11 ~ gender + ethnicity +  EAL + standardised_age11+  occupation_ridit, weights = weight, data=imputed_mcs2_5)
#imputed dataset 6
mcs_late_ocRidit6 <- lm(standardised_vocab11 ~ gender + ethnicity +  EAL + standardised_age11+  occupation_ridit, weights = weight, data=imputed_mcs2_6)
#imputed dataset 7
mcs_late_ocRidit7 <- lm(standardised_vocab11 ~ gender + ethnicity +  EAL + standardised_age11+  occupation_ridit, weights = weight, data=imputed_mcs2_7)
#imputed dataset 8
mcs_late_ocRidit8 <- lm(standardised_vocab11 ~ gender + ethnicity +  EAL + standardised_age11+  occupation_ridit, weights = weight, data=imputed_mcs2_8)
#imputed dataset 9
mcs_late_ocRidit9 <- lm(standardised_vocab11 ~ gender + ethnicity +  EAL + standardised_age11+  occupation_ridit, weights = weight, data=imputed_mcs2_9)
#imputed dataset 10
mcs_late_ocRidit10 <- lm(standardised_vocab11 ~ gender + ethnicity +  EAL + standardised_age11+  occupation_ridit, weights = weight, data=imputed_mcs2_10)
#imputed dataset 11
mcs_late_ocRidit11 <- lm(standardised_vocab11 ~ gender + ethnicity +  EAL + standardised_age11+  occupation_ridit, weights = weight, data=imputed_mcs2_11)
#imputed dataset 12
mcs_late_ocRidit12 <- lm(standardised_vocab11 ~ gender + ethnicity +  EAL + standardised_age11+  occupation_ridit, weights = weight, data=imputed_mcs2_12)
#imputed dataset 13
mcs_late_ocRidit13 <- lm(standardised_vocab11 ~ gender + ethnicity +  EAL + standardised_age11+  occupation_ridit, weights = weight, data=imputed_mcs2_13)
#imputed dataset 14
mcs_late_ocRidit14 <- lm(standardised_vocab11 ~ gender + ethnicity +  EAL + standardised_age11+  occupation_ridit, weights = weight, data=imputed_mcs2_14)
#imputed dataset 15
mcs_late_ocRidit15 <- lm(standardised_vocab11 ~ gender + ethnicity +  EAL + standardised_age11+  occupation_ridit, weights = weight, data=imputed_mcs2_15)
#imputed dataset 16
mcs_late_ocRidit16 <- lm(standardised_vocab11 ~ gender + ethnicity +  EAL + standardised_age11+  occupation_ridit, weights = weight, data=imputed_mcs2_16)
#imputed dataset 17
mcs_late_ocRidit17 <- lm(standardised_vocab11 ~ gender + ethnicity +  EAL + standardised_age11+  occupation_ridit, weights = weight, data=imputed_mcs2_17)
#imputed dataset 18
mcs_late_ocRidit18 <- lm(standardised_vocab11 ~ gender + ethnicity +  EAL + standardised_age11+  occupation_ridit, weights = weight, data=imputed_mcs2_18)
#imputed dataset 19
mcs_late_ocRidit19 <- lm(standardised_vocab11 ~ gender + ethnicity +  EAL + standardised_age11+  occupation_ridit, weights = weight, data=imputed_mcs2_19)
#imputed dataset 20
mcs_late_ocRidit20 <- lm(standardised_vocab11 ~ gender + ethnicity +  EAL + standardised_age11+  occupation_ridit, weights = weight, data=imputed_mcs2_20)
#imputed dataset 21
mcs_late_ocRidit21 <- lm(standardised_vocab11 ~ gender + ethnicity +  EAL + standardised_age11+  occupation_ridit, weights = weight, data=imputed_mcs2_21)
#imputed dataset 22
mcs_late_ocRidit22 <- lm(standardised_vocab11 ~ gender + ethnicity +  EAL + standardised_age11+  occupation_ridit, weights = weight, data=imputed_mcs2_22)
#imputed dataset 23
mcs_late_ocRidit23 <- lm(standardised_vocab11 ~ gender + ethnicity +  EAL + standardised_age11+  occupation_ridit, weights = weight, data=imputed_mcs2_23)
#imputed dataset 24
mcs_late_ocRidit24 <- lm(standardised_vocab11 ~ gender + ethnicity +  EAL + standardised_age11+  occupation_ridit, weights = weight, data=imputed_mcs2_24)
#imputed dataset 25
mcs_late_ocRidit25 <- lm(standardised_vocab11 ~ gender + ethnicity +  EAL + standardised_age11+  occupation_ridit, weights = weight, data=imputed_mcs2_25)


#adolescent education 

#imputed dataset 1
mcs_adolescent_ocRidit1 <- lm(standardised_vocab14 ~ gender + ethnicity +  EAL + standardised_age14+  occupation_ridit, weights = weight, data=imputed_mcs2_1)
#imputed dataset 2
mcs_adolescent_ocRidit2 <- lm(standardised_vocab14 ~ gender + ethnicity +  EAL + standardised_age14+  occupation_ridit, weights = weight, data=imputed_mcs2_2)
#imputed dataset 3
mcs_adolescent_ocRidit3 <- lm(standardised_vocab14 ~ gender + ethnicity +  EAL + standardised_age14+  occupation_ridit, weights = weight, data=imputed_mcs2_3)
#imputed dataset 4
mcs_adolescent_ocRidit4 <- lm(standardised_vocab14 ~ gender + ethnicity +  EAL + standardised_age14+  occupation_ridit, weights = weight, data=imputed_mcs2_4)
#imputed dataset 5
mcs_adolescent_ocRidit5 <- lm(standardised_vocab14 ~ gender + ethnicity +  EAL + standardised_age14+  occupation_ridit, weights = weight, data=imputed_mcs2_5)
#imputed dataset 6
mcs_adolescent_ocRidit6 <- lm(standardised_vocab14 ~ gender + ethnicity +  EAL + standardised_age14+  occupation_ridit, weights = weight, data=imputed_mcs2_6)
#imputed dataset 7
mcs_adolescent_ocRidit7 <- lm(standardised_vocab14 ~ gender + ethnicity +  EAL + standardised_age14+  occupation_ridit, weights = weight, data=imputed_mcs2_7)
#imputed dataset 8
mcs_adolescent_ocRidit8 <- lm(standardised_vocab14 ~ gender + ethnicity +  EAL + standardised_age14+  occupation_ridit, weights = weight, data=imputed_mcs2_8)
#imputed dataset 9
mcs_adolescent_ocRidit9 <- lm(standardised_vocab14 ~ gender + ethnicity +  EAL + standardised_age14+  occupation_ridit, weights = weight, data=imputed_mcs2_9)
#imputed dataset 10
mcs_adolescent_ocRidit10 <- lm(standardised_vocab14 ~ gender + ethnicity +  EAL + standardised_age14+  occupation_ridit, weights = weight, data=imputed_mcs2_10)
#imputed dataset 11
mcs_adolescent_ocRidit11 <- lm(standardised_vocab14 ~ gender + ethnicity +  EAL + standardised_age14+  occupation_ridit, weights = weight, data=imputed_mcs2_11)
#imputed dataset 12
mcs_adolescent_ocRidit12 <- lm(standardised_vocab14 ~ gender + ethnicity +  EAL + standardised_age14+  occupation_ridit, weights = weight, data=imputed_mcs2_12)
#imputed dataset 13
mcs_adolescent_ocRidit13 <- lm(standardised_vocab14 ~ gender + ethnicity +  EAL + standardised_age14+  occupation_ridit, weights = weight, data=imputed_mcs2_13)
#imputed dataset 14
mcs_adolescent_ocRidit14 <- lm(standardised_vocab14 ~ gender + ethnicity +  EAL + standardised_age14+  occupation_ridit, weights = weight, data=imputed_mcs2_14)
#imputed dataset 15
mcs_adolescent_ocRidit15 <- lm(standardised_vocab14 ~ gender + ethnicity +  EAL + standardised_age14+  occupation_ridit, weights = weight, data=imputed_mcs2_15)
#imputed dataset 16
mcs_adolescent_ocRidit16 <- lm(standardised_vocab14 ~ gender + ethnicity +  EAL + standardised_age14+  occupation_ridit, weights = weight, data=imputed_mcs2_16)
#imputed dataset 17
mcs_adolescent_ocRidit17 <- lm(standardised_vocab14 ~ gender + ethnicity +  EAL + standardised_age14+  occupation_ridit, weights = weight, data=imputed_mcs2_17)
#imputed dataset 18
mcs_adolescent_ocRidit18 <- lm(standardised_vocab14 ~ gender + ethnicity +  EAL + standardised_age14+  occupation_ridit, weights = weight, data=imputed_mcs2_18)
#imputed dataset 19
mcs_adolescent_ocRidit19 <- lm(standardised_vocab14 ~ gender + ethnicity +  EAL + standardised_age14+  occupation_ridit, weights = weight, data=imputed_mcs2_19)
#imputed dataset 20
mcs_adolescent_ocRidit20 <- lm(standardised_vocab14 ~ gender + ethnicity +  EAL + standardised_age14+  occupation_ridit, weights = weight, data=imputed_mcs2_20)
#imputed dataset 21
mcs_adolescent_ocRidit21 <- lm(standardised_vocab14 ~ gender + ethnicity +  EAL + standardised_age14+  occupation_ridit, weights = weight, data=imputed_mcs2_21)
#imputed dataset 22
mcs_adolescent_ocRidit22 <- lm(standardised_vocab14 ~ gender + ethnicity +  EAL + standardised_age14+  occupation_ridit, weights = weight, data=imputed_mcs2_22)
#imputed dataset 23
mcs_adolescent_ocRidit23 <- lm(standardised_vocab14 ~ gender + ethnicity +  EAL + standardised_age14+  occupation_ridit, weights = weight, data=imputed_mcs2_23)
#imputed dataset 24
mcs_adolescent_ocRidit24 <- lm(standardised_vocab14 ~ gender + ethnicity +  EAL + standardised_age14+  occupation_ridit, weights = weight, data=imputed_mcs2_24)
#imputed dataset 25
mcs_adolescent_ocRidit25 <- lm(standardised_vocab14 ~ gender + ethnicity +  EAL + standardised_age14+  occupation_ridit, weights = weight, data=imputed_mcs2_25)


#pool results ####

#early language
early_occupation_ridit <- summary(pool(as.mira(list(mcs_early_ocRidit1,mcs_early_ocRidit2, mcs_early_ocRidit3, mcs_early_ocRidit4, mcs_early_ocRidit5, 
                                                    mcs_early_ocRidit6, mcs_early_ocRidit7, mcs_early_ocRidit8, mcs_early_ocRidit9, mcs_early_ocRidit10, 
                                                    mcs_early_ocRidit11, mcs_early_ocRidit12, mcs_early_ocRidit13, mcs_early_ocRidit14, mcs_early_ocRidit15, 
                                                    mcs_early_ocRidit16, mcs_early_ocRidit17, mcs_early_ocRidit18, mcs_early_ocRidit19, mcs_early_ocRidit20, 
                                                    mcs_early_ocRidit21, mcs_early_ocRidit22, mcs_early_ocRidit23, mcs_early_ocRidit24, mcs_early_ocRidit25))))
round(early_occupation_ridit,2)
#age 3 confidence intervals 
lower_early_occupation_ridit = summary(pool(as.mira(list(mcs_early_ocRidit1,mcs_early_ocRidit2, mcs_early_ocRidit3, mcs_early_ocRidit4, mcs_early_ocRidit5, 
                                                         mcs_early_ocRidit6, mcs_early_ocRidit7, mcs_early_ocRidit8, mcs_early_ocRidit9, mcs_early_ocRidit10, 
                                                         mcs_early_ocRidit11, mcs_early_ocRidit12, mcs_early_ocRidit13, mcs_early_ocRidit14, mcs_early_ocRidit15, 
                                                         mcs_early_ocRidit16, mcs_early_ocRidit17, mcs_early_ocRidit18, mcs_early_ocRidit19, mcs_early_ocRidit20, 
                                                         mcs_early_ocRidit21, mcs_early_ocRidit22, mcs_early_ocRidit23, mcs_early_ocRidit24, mcs_early_ocRidit25))))$estimate-(
                                                           summary(pool(as.mira(list(mcs_early_ocRidit1,mcs_early_ocRidit2, mcs_early_ocRidit3, mcs_early_ocRidit4, mcs_early_ocRidit5, 
                                                                                     mcs_early_ocRidit6, mcs_early_ocRidit7, mcs_early_ocRidit8, mcs_early_ocRidit9, mcs_early_ocRidit10, 
                                                                                     mcs_early_ocRidit11, mcs_early_ocRidit12, mcs_early_ocRidit13, mcs_early_ocRidit14, mcs_early_ocRidit15, 
                                                                                     mcs_early_ocRidit16, mcs_early_ocRidit17, mcs_early_ocRidit18, mcs_early_ocRidit19, mcs_early_ocRidit20, 
                                                                                     mcs_early_ocRidit21, mcs_early_ocRidit22, mcs_early_ocRidit23, mcs_early_ocRidit24, mcs_early_ocRidit25))))$std.error*1.96)
upper_early_occupation_ridit = summary(pool(as.mira(list(mcs_early_ocRidit1,mcs_early_ocRidit2, mcs_early_ocRidit3, mcs_early_ocRidit4, mcs_early_ocRidit5, 
                                                         mcs_early_ocRidit6, mcs_early_ocRidit7, mcs_early_ocRidit8, mcs_early_ocRidit9, mcs_early_ocRidit10, 
                                                         mcs_early_ocRidit11, mcs_early_ocRidit12, mcs_early_ocRidit13, mcs_early_ocRidit14, mcs_early_ocRidit15, 
                                                         mcs_early_ocRidit16, mcs_early_ocRidit17, mcs_early_ocRidit18, mcs_early_ocRidit19, mcs_early_ocRidit20, 
                                                         mcs_early_ocRidit21, mcs_early_ocRidit22, mcs_early_ocRidit23, mcs_early_ocRidit24, mcs_early_ocRidit25))))$estimate+(
                                                           summary(pool(as.mira(list(mcs_early_ocRidit1,mcs_early_ocRidit2, mcs_early_ocRidit3, mcs_early_ocRidit4, mcs_early_ocRidit5, 
                                                                                     mcs_early_ocRidit6, mcs_early_ocRidit7, mcs_early_ocRidit8, mcs_early_ocRidit9, mcs_early_ocRidit10, 
                                                                                     mcs_early_ocRidit11, mcs_early_ocRidit12, mcs_early_ocRidit13, mcs_early_ocRidit14, mcs_early_ocRidit15, 
                                                                                     mcs_early_ocRidit16, mcs_early_ocRidit17, mcs_early_ocRidit18, mcs_early_ocRidit19, mcs_early_ocRidit20, 
                                                                                     mcs_early_ocRidit21, mcs_early_ocRidit22, mcs_early_ocRidit23, mcs_early_ocRidit24, mcs_early_ocRidit25))))$std.error*1.96)
round(lower_early_occupation_ridit,2)
round(upper_early_occupation_ridit,2)
early_occupation_r2 = as.data.frame(pool.r.squared(as.mira(list(mcs_early_ocRidit1,mcs_early_ocRidit2, mcs_early_ocRidit3, mcs_early_ocRidit4, mcs_early_ocRidit5, 
                                    mcs_early_ocRidit6, mcs_early_ocRidit7, mcs_early_ocRidit8, mcs_early_ocRidit9, mcs_early_ocRidit10, 
                                    mcs_early_ocRidit11, mcs_early_ocRidit12, mcs_early_ocRidit13, mcs_early_ocRidit14, mcs_early_ocRidit15, 
                                    mcs_early_ocRidit16, mcs_early_ocRidit17, mcs_early_ocRidit18, mcs_early_ocRidit19, mcs_early_ocRidit20,
                                    mcs_early_ocRidit21, mcs_early_ocRidit22, mcs_early_ocRidit23, mcs_early_ocRidit24, mcs_early_ocRidit25))))


#late language
late_occupation_ridit <- summary(pool(as.mira(list(mcs_late_ocRidit1,mcs_late_ocRidit2, mcs_late_ocRidit3, mcs_late_ocRidit4, mcs_late_ocRidit5, 
                                                   mcs_late_ocRidit6, mcs_late_ocRidit7, mcs_late_ocRidit8, mcs_late_ocRidit9, mcs_late_ocRidit10, 
                                                   mcs_late_ocRidit11, mcs_late_ocRidit12, mcs_late_ocRidit13, mcs_late_ocRidit14, mcs_late_ocRidit15, 
                                                   mcs_late_ocRidit16, mcs_late_ocRidit17, mcs_late_ocRidit18, mcs_late_ocRidit19, mcs_late_ocRidit20, 
                                                   mcs_late_ocRidit21, mcs_late_ocRidit22, mcs_late_ocRidit23, mcs_late_ocRidit24, mcs_late_ocRidit25))))
round(late_occupation_ridit,2)
#age 3 confidence intervals 
lower_late_occupation_ridit = summary(pool(as.mira(list(mcs_late_ocRidit1,mcs_late_ocRidit2, mcs_late_ocRidit3, mcs_late_ocRidit4, mcs_late_ocRidit5, 
                                                        mcs_late_ocRidit6, mcs_late_ocRidit7, mcs_late_ocRidit8, mcs_late_ocRidit9, mcs_late_ocRidit10, 
                                                        mcs_late_ocRidit11, mcs_late_ocRidit12, mcs_late_ocRidit13, mcs_late_ocRidit14, mcs_late_ocRidit15, 
                                                        mcs_late_ocRidit16, mcs_late_ocRidit17, mcs_late_ocRidit18, mcs_late_ocRidit19, mcs_late_ocRidit20, 
                                                        mcs_late_ocRidit21, mcs_late_ocRidit22, mcs_late_ocRidit23, mcs_late_ocRidit24, mcs_late_ocRidit25))))$estimate-(
                                                          summary(pool(as.mira(list(mcs_late_ocRidit1,mcs_late_ocRidit2, mcs_late_ocRidit3, mcs_late_ocRidit4, mcs_late_ocRidit5, 
                                                                                    mcs_late_ocRidit6, mcs_late_ocRidit7, mcs_late_ocRidit8, mcs_late_ocRidit9, mcs_late_ocRidit10, 
                                                                                    mcs_late_ocRidit11, mcs_late_ocRidit12, mcs_late_ocRidit13, mcs_late_ocRidit14, mcs_late_ocRidit15, 
                                                                                    mcs_late_ocRidit16, mcs_late_ocRidit17, mcs_late_ocRidit18, mcs_late_ocRidit19, mcs_late_ocRidit20, 
                                                                                    mcs_late_ocRidit21, mcs_late_ocRidit22, mcs_late_ocRidit23, mcs_late_ocRidit24, mcs_late_ocRidit25))))$std.error*1.96)
upper_late_occupation_ridit = summary(pool(as.mira(list(mcs_late_ocRidit1,mcs_late_ocRidit2, mcs_late_ocRidit3, mcs_late_ocRidit4, mcs_late_ocRidit5, 
                                                        mcs_late_ocRidit6, mcs_late_ocRidit7, mcs_late_ocRidit8, mcs_late_ocRidit9, mcs_late_ocRidit10, 
                                                        mcs_late_ocRidit11, mcs_late_ocRidit12, mcs_late_ocRidit13, mcs_late_ocRidit14, mcs_late_ocRidit15, 
                                                        mcs_late_ocRidit16, mcs_late_ocRidit17, mcs_late_ocRidit18, mcs_late_ocRidit19, mcs_late_ocRidit20, 
                                                        mcs_late_ocRidit21, mcs_late_ocRidit22, mcs_late_ocRidit23, mcs_late_ocRidit24, mcs_late_ocRidit25))))$estimate+(
                                                          summary(pool(as.mira(list(mcs_late_ocRidit1,mcs_late_ocRidit2, mcs_late_ocRidit3, mcs_late_ocRidit4, mcs_late_ocRidit5, 
                                                                                    mcs_late_ocRidit6, mcs_late_ocRidit7, mcs_late_ocRidit8, mcs_late_ocRidit9, mcs_late_ocRidit10, 
                                                                                    mcs_late_ocRidit11, mcs_late_ocRidit12, mcs_late_ocRidit13, mcs_late_ocRidit14, mcs_late_ocRidit15, 
                                                                                    mcs_late_ocRidit16, mcs_late_ocRidit17, mcs_late_ocRidit18, mcs_late_ocRidit19, mcs_late_ocRidit20, 
                                                                                    mcs_late_ocRidit21, mcs_late_ocRidit22, mcs_late_ocRidit23, mcs_late_ocRidit24, mcs_late_ocRidit25))))$std.error*1.96)
round(lower_late_occupation_ridit,2)
round(upper_late_occupation_ridit,2)
late_occupation_r2 = as.data.frame(pool.r.squared(as.mira(list(mcs_late_ocRidit1,mcs_late_ocRidit2, mcs_late_ocRidit3, mcs_late_ocRidit4, mcs_late_ocRidit5, 
                                    mcs_late_ocRidit6, mcs_late_ocRidit7, mcs_late_ocRidit8, mcs_late_ocRidit9, mcs_late_ocRidit10, 
                                    mcs_late_ocRidit11, mcs_late_ocRidit12, mcs_late_ocRidit13, mcs_late_ocRidit14, mcs_late_ocRidit15, 
                                    mcs_late_ocRidit16, mcs_late_ocRidit17, mcs_late_ocRidit18, mcs_late_ocRidit19, mcs_late_ocRidit20,
                                    mcs_late_ocRidit21, mcs_late_ocRidit22, mcs_late_ocRidit23, mcs_late_ocRidit24, mcs_late_ocRidit25))))

#adolescent language
adolescent_occupation_ridit <- summary(pool(as.mira(list(mcs_adolescent_ocRidit1,mcs_adolescent_ocRidit2, mcs_adolescent_ocRidit3, mcs_adolescent_ocRidit4, mcs_adolescent_ocRidit5, 
                                                         mcs_adolescent_ocRidit6, mcs_adolescent_ocRidit7, mcs_adolescent_ocRidit8, mcs_adolescent_ocRidit9, mcs_adolescent_ocRidit10, 
                                                         mcs_adolescent_ocRidit11, mcs_adolescent_ocRidit12, mcs_adolescent_ocRidit13, mcs_adolescent_ocRidit14, mcs_adolescent_ocRidit15, 
                                                         mcs_adolescent_ocRidit16, mcs_adolescent_ocRidit17, mcs_adolescent_ocRidit18, mcs_adolescent_ocRidit19, mcs_adolescent_ocRidit20, 
                                                         mcs_adolescent_ocRidit21, mcs_adolescent_ocRidit22, mcs_adolescent_ocRidit23, mcs_adolescent_ocRidit24, mcs_adolescent_ocRidit25))))
round(adolescent_occupation_ridit,2)
#age 3 confidence intervals 
lower_adolescent_occupation_ridit = summary(pool(as.mira(list(mcs_adolescent_ocRidit1,mcs_adolescent_ocRidit2, mcs_adolescent_ocRidit3, mcs_adolescent_ocRidit4, mcs_adolescent_ocRidit5, 
                                                              mcs_adolescent_ocRidit6, mcs_adolescent_ocRidit7, mcs_adolescent_ocRidit8, mcs_adolescent_ocRidit9, mcs_adolescent_ocRidit10, 
                                                              mcs_adolescent_ocRidit11, mcs_adolescent_ocRidit12, mcs_adolescent_ocRidit13, mcs_adolescent_ocRidit14, mcs_adolescent_ocRidit15, 
                                                              mcs_adolescent_ocRidit16, mcs_adolescent_ocRidit17, mcs_adolescent_ocRidit18, mcs_adolescent_ocRidit19, mcs_adolescent_ocRidit20, 
                                                              mcs_adolescent_ocRidit21, mcs_adolescent_ocRidit22, mcs_adolescent_ocRidit23, mcs_adolescent_ocRidit24, mcs_adolescent_ocRidit25))))$estimate-(
                                                                summary(pool(as.mira(list(mcs_adolescent_ocRidit1,mcs_adolescent_ocRidit2, mcs_adolescent_ocRidit3, mcs_adolescent_ocRidit4, mcs_adolescent_ocRidit5, 
                                                                                          mcs_adolescent_ocRidit6, mcs_adolescent_ocRidit7, mcs_adolescent_ocRidit8, mcs_adolescent_ocRidit9, mcs_adolescent_ocRidit10, 
                                                                                          mcs_adolescent_ocRidit11, mcs_adolescent_ocRidit12, mcs_adolescent_ocRidit13, mcs_adolescent_ocRidit14, mcs_adolescent_ocRidit15, 
                                                                                          mcs_adolescent_ocRidit16, mcs_adolescent_ocRidit17, mcs_adolescent_ocRidit18, mcs_adolescent_ocRidit19, mcs_adolescent_ocRidit20, 
                                                                                          mcs_adolescent_ocRidit21, mcs_adolescent_ocRidit22, mcs_adolescent_ocRidit23, mcs_adolescent_ocRidit24, mcs_adolescent_ocRidit25))))$std.error*1.96)
upper_adolescent_occupation_ridit = summary(pool(as.mira(list(mcs_adolescent_ocRidit1,mcs_adolescent_ocRidit2, mcs_adolescent_ocRidit3, mcs_adolescent_ocRidit4, mcs_adolescent_ocRidit5, 
                                                              mcs_adolescent_ocRidit6, mcs_adolescent_ocRidit7, mcs_adolescent_ocRidit8, mcs_adolescent_ocRidit9, mcs_adolescent_ocRidit10, 
                                                              mcs_adolescent_ocRidit11, mcs_adolescent_ocRidit12, mcs_adolescent_ocRidit13, mcs_adolescent_ocRidit14, mcs_adolescent_ocRidit15, 
                                                              mcs_adolescent_ocRidit16, mcs_adolescent_ocRidit17, mcs_adolescent_ocRidit18, mcs_adolescent_ocRidit19, mcs_adolescent_ocRidit20, 
                                                              mcs_adolescent_ocRidit21, mcs_adolescent_ocRidit22, mcs_adolescent_ocRidit23, mcs_adolescent_ocRidit24, mcs_adolescent_ocRidit25))))$estimate+(
                                                                summary(pool(as.mira(list(mcs_adolescent_ocRidit1,mcs_adolescent_ocRidit2, mcs_adolescent_ocRidit3, mcs_adolescent_ocRidit4, mcs_adolescent_ocRidit5, 
                                                                                          mcs_adolescent_ocRidit6, mcs_adolescent_ocRidit7, mcs_adolescent_ocRidit8, mcs_adolescent_ocRidit9, mcs_adolescent_ocRidit10, 
                                                                                          mcs_adolescent_ocRidit11, mcs_adolescent_ocRidit12, mcs_adolescent_ocRidit13, mcs_adolescent_ocRidit14, mcs_adolescent_ocRidit15, 
                                                                                          mcs_adolescent_ocRidit16, mcs_adolescent_ocRidit17, mcs_adolescent_ocRidit18, mcs_adolescent_ocRidit19, mcs_adolescent_ocRidit20, 
                                                                                          mcs_adolescent_ocRidit21, mcs_adolescent_ocRidit22, mcs_adolescent_ocRidit23, mcs_adolescent_ocRidit24, mcs_adolescent_ocRidit25))))$std.error*1.96)
round(lower_adolescent_occupation_ridit,2)
round(upper_adolescent_occupation_ridit,2)
adolescent_occupation_r2 = as.data.frame(pool.r.squared(as.mira(list(mcs_adolescent_ocRidit1,mcs_adolescent_ocRidit2, mcs_adolescent_ocRidit3, mcs_adolescent_ocRidit4, mcs_adolescent_ocRidit5, 
                                    mcs_adolescent_ocRidit6, mcs_adolescent_ocRidit7, mcs_adolescent_ocRidit8, mcs_adolescent_ocRidit9, mcs_adolescent_ocRidit10, 
                                    mcs_adolescent_ocRidit11, mcs_adolescent_ocRidit12, mcs_adolescent_ocRidit13, mcs_adolescent_ocRidit14, mcs_adolescent_ocRidit15, 
                                    mcs_adolescent_ocRidit16, mcs_adolescent_ocRidit17, mcs_adolescent_ocRidit18, mcs_adolescent_ocRidit19, mcs_adolescent_ocRidit20,
                                    mcs_adolescent_ocRidit21, mcs_adolescent_ocRidit22, mcs_adolescent_ocRidit23, mcs_adolescent_ocRidit24, mcs_adolescent_ocRidit25))))






#bcs
#remove unemployed from analysis (occupational_status=1) - want to compare routine with profressional. 
#imputed_bcs2_1 <- imputed_bcs2_1 %>%  filter(occupational_status !=1)
#create vector of counts for ridit score
#bcs_occupation1 <- table(imputed_bcs2_1$occupational_status)
#bcs_occupation_ridit1 <-toridit(bcs_occupation1)
#imputed_bcs2_1 = imputed_bcs2_1 %>% mutate(occupation_ridit = occupational_status) %>%
#  mutate(rec(occupation_ridit, rec = "2= 0.0797914; 3=0.4348544; 4=  0.8550630  ", as.num = TRUE,
#             var.label = occupation_ridit, val.labels = NULL,
 #            append = FALSE, suffix = "_r")) %>% 
  #rename(occupationRiditScore = `rec(...)`)

#mcs_early_occRidit <- lm(standardised_vocab5 ~ gender + ethnicity +  EAL + standardised_age5+  occupationRiditScore, weights = weight, data=imputed_mcs2_1)
#bcs_early_occRidit <- lm(standardised_vocab5 ~ gender + ethnicity +  EAL + standardised_age5+  occupationRiditScore, weights = weight, data=imputed_bcs2_1)

#summary(mcs_early_occRidit, conf.int =TRUE)
#summary(bcs_early_occRidit, conf.int =TRUE)

#mcs_late_occRidit <- lm(standardised_vocab11 ~ gender + ethnicity +  EAL + standardised_age5+  occupationRiditScore, weights = weight, data=imputed_mcs2_1)
#bcs_late_occRidit <- lm(standardised_vocab10 ~ gender + ethnicity +  EAL + standardised_age5+  occupationRiditScore, weights = weight, data=imputed_bcs2_1)

#summary(mcs_late_occRidit, conf.int =TRUE)
#summary(bcs_late_occRidit, conf.int =TRUE)

#mcs_adolescent_occRidit <- lm(standardised_vocab14 ~ gender + ethnicity +  EAL + standardised_age5+  occupationRiditScore, weights = weight, data=imputed_mcs2_1)
#bcs_adolescent_occRidit <- lm(standardised_vocab16 ~ gender + ethnicity +  EAL + standardised_age5+  occupationRiditScore, weights = weight, data=imputed_bcs2_1)

#summary(mcs_adolescent_occRidit, conf.int =TRUE)
#summary(bcs_adolescent_occRidit, conf.int =TRUE)

#confounders only for partial r squareds

#imputed dataset 1
mcs_early_confounders1 <- lm(standardised_vocab5 ~ gender + ethnicity +  EAL + standardised_age5, weights = weight, data=imputed_mcs2_1)
#imputed dataset 2
mcs_early_confounders2 <- lm(standardised_vocab5 ~ gender + ethnicity +  EAL + standardised_age5, weights = weight, data=imputed_mcs2_2)
#imputed dataset 3
mcs_early_confounders3 <- lm(standardised_vocab5 ~ gender + ethnicity +  EAL + standardised_age5, weights = weight, data=imputed_mcs2_3)
#imputed dataset 4
mcs_early_confounders4 <- lm(standardised_vocab5 ~ gender + ethnicity +  EAL + standardised_age5, weights = weight, data=imputed_mcs2_4)
#imputed dataset 5
mcs_early_confounders5 <- lm(standardised_vocab5 ~ gender + ethnicity +  EAL + standardised_age5, weights = weight, data=imputed_mcs2_5)
#imputed dataset 6
mcs_early_confounders6 <- lm(standardised_vocab5 ~ gender + ethnicity +  EAL + standardised_age5, weights = weight, data=imputed_mcs2_6)
#imputed dataset 7
mcs_early_confounders7 <- lm(standardised_vocab5 ~ gender + ethnicity +  EAL + standardised_age5, weights = weight, data=imputed_mcs2_7)
#imputed dataset 8
mcs_early_confounders8 <- lm(standardised_vocab5 ~ gender + ethnicity +  EAL + standardised_age5, weights = weight, data=imputed_mcs2_8)
#imputed dataset 9
mcs_early_confounders9 <- lm(standardised_vocab5 ~ gender + ethnicity +  EAL + standardised_age5, weights = weight, data=imputed_mcs2_9)
#imputed dataset 10
mcs_early_confounders10 <- lm(standardised_vocab5 ~ gender + ethnicity +  EAL + standardised_age5, weights = weight, data=imputed_mcs2_10)
#imputed dataset 11
mcs_early_confounders11 <- lm(standardised_vocab5 ~ gender + ethnicity +  EAL + standardised_age5, weights = weight, data=imputed_mcs2_11)
#imputed dataset 12
mcs_early_confounders12 <- lm(standardised_vocab5 ~ gender + ethnicity +  EAL + standardised_age5, weights = weight, data=imputed_mcs2_12)
#imputed dataset 13
mcs_early_confounders13 <- lm(standardised_vocab5 ~ gender + ethnicity +  EAL + standardised_age5, weights = weight, data=imputed_mcs2_13)
#imputed dataset 14
mcs_early_confounders14 <- lm(standardised_vocab5 ~ gender + ethnicity +  EAL + standardised_age5, weights = weight, data=imputed_mcs2_14)
#imputed dataset 15
mcs_early_confounders15 <- lm(standardised_vocab5 ~ gender + ethnicity +  EAL + standardised_age5, weights = weight, data=imputed_mcs2_15)
#imputed dataset 16
mcs_early_confounders16 <- lm(standardised_vocab5 ~ gender + ethnicity +  EAL + standardised_age5, weights = weight, data=imputed_mcs2_16)
#imputed dataset 17
mcs_early_confounders17 <- lm(standardised_vocab5 ~ gender + ethnicity +  EAL + standardised_age5, weights = weight, data=imputed_mcs2_17)
#imputed dataset 18
mcs_early_confounders18 <- lm(standardised_vocab5 ~ gender + ethnicity +  EAL + standardised_age5, weights = weight, data=imputed_mcs2_18)
#imputed dataset 19
mcs_early_confounders19 <- lm(standardised_vocab5 ~ gender + ethnicity +  EAL + standardised_age5, weights = weight, data=imputed_mcs2_19)
#imputed dataset 20
mcs_early_confounders20 <- lm(standardised_vocab5 ~ gender + ethnicity +  EAL + standardised_age5, weights = weight, data=imputed_mcs2_20)
#imputed dataset 21
mcs_early_confounders21 <- lm(standardised_vocab5 ~ gender + ethnicity +  EAL + standardised_age5, weights = weight, data=imputed_mcs2_21)
#imputed dataset 22
mcs_early_confounders22 <- lm(standardised_vocab5 ~ gender + ethnicity +  EAL + standardised_age5, weights = weight, data=imputed_mcs2_22)
#imputed dataset 23
mcs_early_confounders23 <- lm(standardised_vocab5 ~ gender + ethnicity +  EAL + standardised_age5, weights = weight, data=imputed_mcs2_23)
#imputed dataset 24
mcs_early_confounders24 <- lm(standardised_vocab5 ~ gender + ethnicity +  EAL + standardised_age5, weights = weight, data=imputed_mcs2_24)
#imputed dataset 25
mcs_early_confounders25 <- lm(standardised_vocab5 ~ gender + ethnicity +  EAL + standardised_age5, weights = weight, data=imputed_mcs2_25)


#late childhood education
#imputed dataset 1
mcs_late_confounders1 <- lm(standardised_vocab11 ~ gender + ethnicity +  EAL + standardised_age11, weights = weight, data=imputed_mcs2_1)
#imputed dataset 2
mcs_late_confounders2 <- lm(standardised_vocab11 ~ gender + ethnicity +  EAL + standardised_age11, weights = weight, data=imputed_mcs2_2)
#imputed dataset 3
mcs_late_confounders3 <- lm(standardised_vocab11 ~ gender + ethnicity +  EAL + standardised_age11, weights = weight, data=imputed_mcs2_3)
#imputed dataset 4
mcs_late_confounders4 <- lm(standardised_vocab11 ~ gender + ethnicity +  EAL + standardised_age11, weights = weight, data=imputed_mcs2_4)
#imputed dataset 5
mcs_late_confounders5 <- lm(standardised_vocab11 ~ gender + ethnicity +  EAL + standardised_age11, weights = weight, data=imputed_mcs2_5)
#imputed dataset 6
mcs_late_confounders6 <- lm(standardised_vocab11 ~ gender + ethnicity +  EAL + standardised_age11, weights = weight, data=imputed_mcs2_6)
#imputed dataset 7
mcs_late_confounders7 <- lm(standardised_vocab11 ~ gender + ethnicity +  EAL + standardised_age11, weights = weight, data=imputed_mcs2_7)
#imputed dataset 8
mcs_late_confounders8 <- lm(standardised_vocab11 ~ gender + ethnicity +  EAL + standardised_age11, weights = weight, data=imputed_mcs2_8)
#imputed dataset 9
mcs_late_confounders9 <- lm(standardised_vocab11 ~ gender + ethnicity +  EAL + standardised_age11, weights = weight, data=imputed_mcs2_9)
#imputed dataset 10
mcs_late_confounders10 <- lm(standardised_vocab11 ~ gender + ethnicity +  EAL + standardised_age11, weights = weight, data=imputed_mcs2_10)
#imputed dataset 11
mcs_late_confounders11 <- lm(standardised_vocab11 ~ gender + ethnicity +  EAL + standardised_age11, weights = weight, data=imputed_mcs2_11)
#imputed dataset 12
mcs_late_confounders12 <- lm(standardised_vocab11 ~ gender + ethnicity +  EAL + standardised_age11, weights = weight, data=imputed_mcs2_12)
#imputed dataset 13
mcs_late_confounders13 <- lm(standardised_vocab11 ~ gender + ethnicity +  EAL + standardised_age11, weights = weight, data=imputed_mcs2_13)
#imputed dataset 14
mcs_late_confounders14 <- lm(standardised_vocab11 ~ gender + ethnicity +  EAL + standardised_age11, weights = weight, data=imputed_mcs2_14)
#imputed dataset 15
mcs_late_confounders15 <- lm(standardised_vocab11 ~ gender + ethnicity +  EAL + standardised_age11, weights = weight, data=imputed_mcs2_15)
#imputed dataset 16
mcs_late_confounders16 <- lm(standardised_vocab11 ~ gender + ethnicity +  EAL + standardised_age11, weights = weight, data=imputed_mcs2_16)
#imputed dataset 17
mcs_late_confounders17 <- lm(standardised_vocab11 ~ gender + ethnicity +  EAL + standardised_age11, weights = weight, data=imputed_mcs2_17)
#imputed dataset 18
mcs_late_confounders18 <- lm(standardised_vocab11 ~ gender + ethnicity +  EAL + standardised_age11, weights = weight, data=imputed_mcs2_18)
#imputed dataset 19
mcs_late_confounders19 <- lm(standardised_vocab11 ~ gender + ethnicity +  EAL + standardised_age11, weights = weight, data=imputed_mcs2_19)
#imputed dataset 20
mcs_late_confounders20 <- lm(standardised_vocab11 ~ gender + ethnicity +  EAL + standardised_age11, weights = weight, data=imputed_mcs2_20)
#imputed dataset 21
mcs_late_confounders21 <- lm(standardised_vocab11 ~ gender + ethnicity +  EAL + standardised_age11, weights = weight, data=imputed_mcs2_21)
#imputed dataset 22
mcs_late_confounders22 <- lm(standardised_vocab11 ~ gender + ethnicity +  EAL + standardised_age11, weights = weight, data=imputed_mcs2_22)
#imputed dataset 23
mcs_late_confounders23 <- lm(standardised_vocab11 ~ gender + ethnicity +  EAL + standardised_age11, weights = weight, data=imputed_mcs2_23)
#imputed dataset 24
mcs_late_confounders24 <- lm(standardised_vocab11 ~ gender + ethnicity +  EAL + standardised_age11, weights = weight, data=imputed_mcs2_24)
#imputed dataset 25
mcs_late_confounders25 <- lm(standardised_vocab11 ~ gender + ethnicity +  EAL + standardised_age11, weights = weight, data=imputed_mcs2_25)


#adolescent education 

#imputed dataset 1
mcs_adolescent_confounders1 <- lm(standardised_vocab14 ~ gender + ethnicity +  EAL + standardised_age14, weights = weight, data=imputed_mcs2_1)
#imputed dataset 2
mcs_adolescent_confounders2 <- lm(standardised_vocab14 ~ gender + ethnicity +  EAL + standardised_age14, weights = weight, data=imputed_mcs2_2)
#imputed dataset 3
mcs_adolescent_confounders3 <- lm(standardised_vocab14 ~ gender + ethnicity +  EAL + standardised_age14, weights = weight, data=imputed_mcs2_3)
#imputed dataset 4
mcs_adolescent_confounders4 <- lm(standardised_vocab14 ~ gender + ethnicity +  EAL + standardised_age14, weights = weight, data=imputed_mcs2_4)
#imputed dataset 5
mcs_adolescent_confounders5 <- lm(standardised_vocab14 ~ gender + ethnicity +  EAL + standardised_age14, weights = weight, data=imputed_mcs2_5)
#imputed dataset 6
mcs_adolescent_confounders6 <- lm(standardised_vocab14 ~ gender + ethnicity +  EAL + standardised_age14, weights = weight, data=imputed_mcs2_6)
#imputed dataset 7
mcs_adolescent_confounders7 <- lm(standardised_vocab14 ~ gender + ethnicity +  EAL + standardised_age14, weights = weight, data=imputed_mcs2_7)
#imputed dataset 8
mcs_adolescent_confounders8 <- lm(standardised_vocab14 ~ gender + ethnicity +  EAL + standardised_age14, weights = weight, data=imputed_mcs2_8)
#imputed dataset 9
mcs_adolescent_confounders9 <- lm(standardised_vocab14 ~ gender + ethnicity +  EAL + standardised_age14, weights = weight, data=imputed_mcs2_9)
#imputed dataset 10
mcs_adolescent_confounders10 <- lm(standardised_vocab14 ~ gender + ethnicity +  EAL + standardised_age14, weights = weight, data=imputed_mcs2_10)
#imputed dataset 11
mcs_adolescent_confounders11 <- lm(standardised_vocab14 ~ gender + ethnicity +  EAL + standardised_age14, weights = weight, data=imputed_mcs2_11)
#imputed dataset 12
mcs_adolescent_confounders12 <- lm(standardised_vocab14 ~ gender + ethnicity +  EAL + standardised_age14, weights = weight, data=imputed_mcs2_12)
#imputed dataset 13
mcs_adolescent_confounders13 <- lm(standardised_vocab14 ~ gender + ethnicity +  EAL + standardised_age14, weights = weight, data=imputed_mcs2_13)
#imputed dataset 14
mcs_adolescent_confounders14 <- lm(standardised_vocab14 ~ gender + ethnicity +  EAL + standardised_age14, weights = weight, data=imputed_mcs2_14)
#imputed dataset 15
mcs_adolescent_confounders15 <- lm(standardised_vocab14 ~ gender + ethnicity +  EAL + standardised_age14, weights = weight, data=imputed_mcs2_15)
#imputed dataset 16
mcs_adolescent_confounders16 <- lm(standardised_vocab14 ~ gender + ethnicity +  EAL + standardised_age14, weights = weight, data=imputed_mcs2_16)
#imputed dataset 17
mcs_adolescent_confounders17 <- lm(standardised_vocab14 ~ gender + ethnicity +  EAL + standardised_age14, weights = weight, data=imputed_mcs2_17)
#imputed dataset 18
mcs_adolescent_confounders18 <- lm(standardised_vocab14 ~ gender + ethnicity +  EAL + standardised_age14, weights = weight, data=imputed_mcs2_18)
#imputed dataset 19
mcs_adolescent_confounders19 <- lm(standardised_vocab14 ~ gender + ethnicity +  EAL + standardised_age14, weights = weight, data=imputed_mcs2_19)
#imputed dataset 20
mcs_adolescent_confounders20 <- lm(standardised_vocab14 ~ gender + ethnicity +  EAL + standardised_age14, weights = weight, data=imputed_mcs2_20)
#imputed dataset 21
mcs_adolescent_confounders21 <- lm(standardised_vocab14 ~ gender + ethnicity +  EAL + standardised_age14, weights = weight, data=imputed_mcs2_21)
#imputed dataset 22
mcs_adolescent_confounders22 <- lm(standardised_vocab14 ~ gender + ethnicity +  EAL + standardised_age14, weights = weight, data=imputed_mcs2_22)
#imputed dataset 23
mcs_adolescent_confounders23 <- lm(standardised_vocab14 ~ gender + ethnicity +  EAL + standardised_age14, weights = weight, data=imputed_mcs2_23)
#imputed dataset 24
mcs_adolescent_confounders24 <- lm(standardised_vocab14 ~ gender + ethnicity +  EAL + standardised_age14, weights = weight, data=imputed_mcs2_24)
#imputed dataset 25
mcs_adolescent_confounders25 <- lm(standardised_vocab14 ~ gender + ethnicity +  EAL + standardised_age14, weights = weight, data=imputed_mcs2_25)


#r squared values 

early_confounders = as.data.frame(pool.r.squared(as.mira(list(mcs_early_confounders1,mcs_early_confounders2, mcs_early_confounders3, mcs_early_confounders4, mcs_early_confounders5, 
                                                              mcs_early_confounders6, mcs_early_confounders7, mcs_early_confounders8, mcs_early_confounders9, mcs_early_confounders10, 
                                                              mcs_early_confounders11, mcs_early_confounders12, mcs_early_confounders13, mcs_early_confounders14, mcs_early_confounders15, 
                                                              mcs_early_confounders16, mcs_early_confounders17, mcs_early_confounders18, mcs_early_confounders19, mcs_early_confounders20,
                                                              mcs_early_confounders21, mcs_early_confounders22, mcs_early_confounders23, mcs_early_confounders24, mcs_early_confounders25))))




late_confounders = as.data.frame(pool.r.squared(as.mira(list(mcs_late_confounders1,mcs_late_confounders2, mcs_late_confounders3, mcs_late_confounders4, mcs_late_confounders5, 
                                                             mcs_late_confounders6, mcs_late_confounders7, mcs_late_confounders8, mcs_late_confounders9, mcs_late_confounders10, 
                                                             mcs_late_confounders11, mcs_late_confounders12, mcs_late_confounders13, mcs_late_confounders14, mcs_late_confounders15, 
                                                             mcs_late_confounders16, mcs_late_confounders17, mcs_late_confounders18, mcs_late_confounders19, mcs_late_confounders20,
                                                             mcs_late_confounders21, mcs_late_confounders22, mcs_late_confounders23, mcs_late_confounders24, mcs_late_confounders25))))



adolescent_confounders =as.data.frame(pool.r.squared(as.mira(list(mcs_adolescent_confounders1,mcs_adolescent_confounders2, mcs_adolescent_confounders3, mcs_adolescent_confounders4, mcs_adolescent_confounders5, 
                                                                  mcs_adolescent_confounders6, mcs_adolescent_confounders7, mcs_adolescent_confounders8, mcs_adolescent_confounders9, mcs_adolescent_confounders10, 
                                                                  mcs_adolescent_confounders11, mcs_adolescent_confounders12, mcs_adolescent_confounders13, mcs_adolescent_confounders14, mcs_adolescent_confounders15, 
                                                                  mcs_adolescent_confounders16, mcs_adolescent_confounders17, mcs_adolescent_confounders18, mcs_adolescent_confounders19, mcs_adolescent_confounders20,
                                                                  mcs_adolescent_confounders21, mcs_adolescent_confounders22, mcs_adolescent_confounders23, mcs_adolescent_confounders24, mcs_adolescent_confounders25))))



#partial r2s
#education
early_childhood_ed_partial = early_education_r2 - early_confounders
early_childhood_ed_partial*100

late_childhood_ed_partial = late_education_r2 - late_confounders
late_childhood_ed_partial*100

adolescent_ed_partial = adolescent_education_r2 - adolescent_confounders
adolescent_ed_partial*100

#occupation
early_childhood_oc_partial = early_occupation_r2 - early_confounders
early_childhood_oc_partial*100

late_childhood_oc_partial = late_occupation_r2 - late_confounders
late_childhood_oc_partial*100

adolescent_oc_partial = adolescent_occupation_r2 - adolescent_confounders
adolescent_oc_partial*100



