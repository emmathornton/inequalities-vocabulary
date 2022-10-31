#mcs ridit score analyses
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
library(dummies)

#change order of occupaiton variable back to unemployed, routine (???)
#Load in data and make long format
load("~/Documents/updated MCS datasets/inequalities datasets /2022-10-26_mcs_ses_crossCohort/2022-10-26_mcs_ses_crossCohort.Rdata")
mcs_imputation = mi.res
long_format_mcs <- mice::complete(mcs_imputation, "long", include=TRUE)
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

#calculate ridit scores. EDUCATION  ####
#imputed dataset 1
#get count data 
#then convert to ridit score with toridit()
mcs_education_ridit1 <- as.matrix(toridit(table(imputed_mcs2_1$highested1)))
#add ridit score to dataset as new variable 
imputed_mcs2_1 = imputed_mcs2_1 %>% mutate(education_ridit = highested1) 
imputed_mcs2_1$education_ridit <- as.numeric(imputed_mcs2_1$education_ridit)
#recode so is replaced with the ridit scores
imputed_mcs2_1$education_ridit[imputed_mcs2_1$education_ridit == 1] <- mcs_education_ridit1[1,]
imputed_mcs2_1$education_ridit[imputed_mcs2_1$education_ridit == 2] <- mcs_education_ridit1[2,]
imputed_mcs2_1$education_ridit[imputed_mcs2_1$education_ridit == 3] <- mcs_education_ridit1[3,]
imputed_mcs2_1$education_ridit[imputed_mcs2_1$education_ridit == 4] <- mcs_education_ridit1[4,]

#imputed dataset 2
#get count data 
#then convert to ridit score with toridit()
mcs_education_ridit2 <- as.matrix(toridit(table(imputed_mcs2_2$highested1)))
#add ridit score to dataset as new variable 
imputed_mcs2_2 = imputed_mcs2_2 %>% mutate(education_ridit = highested1) 
imputed_mcs2_2$education_ridit <- as.numeric(imputed_mcs2_2$education_ridit)
#recode so is replaced with the ridit scores
imputed_mcs2_2$education_ridit[imputed_mcs2_2$education_ridit == 1] <- mcs_education_ridit2[1,]
imputed_mcs2_2$education_ridit[imputed_mcs2_2$education_ridit == 2] <- mcs_education_ridit2[2,]
imputed_mcs2_2$education_ridit[imputed_mcs2_2$education_ridit == 3] <- mcs_education_ridit2[3,]
imputed_mcs2_2$education_ridit[imputed_mcs2_2$education_ridit == 4] <- mcs_education_ridit2[4,]

#imputed dataset 3
#get count data 
#then convert to ridit score with toridit()
mcs_education_ridit3 <- as.matrix(toridit(table(imputed_mcs2_3$highested1)))
#add ridit score to dataset as new variable 
imputed_mcs2_3 = imputed_mcs2_3 %>% mutate(education_ridit = highested1) 
imputed_mcs2_3$education_ridit <- as.numeric(imputed_mcs2_3$education_ridit)
#recode so is replaced with the ridit scores
imputed_mcs2_3$education_ridit[imputed_mcs2_3$education_ridit == 1] <- mcs_education_ridit3[1,]
imputed_mcs2_3$education_ridit[imputed_mcs2_3$education_ridit == 2] <- mcs_education_ridit3[2,]
imputed_mcs2_3$education_ridit[imputed_mcs2_3$education_ridit == 3] <- mcs_education_ridit3[3,]
imputed_mcs2_3$education_ridit[imputed_mcs2_3$education_ridit == 4] <- mcs_education_ridit3[4,]


#imputed dataset 4
#get count data 
#then convert to ridit score with toridit()
mcs_education_ridit4 <- as.matrix(toridit(table(imputed_mcs2_4$highested1)))
#add ridit score to dataset as new variable 
imputed_mcs2_4 = imputed_mcs2_4 %>% mutate(education_ridit = highested1) 
imputed_mcs2_4$education_ridit <- as.numeric(imputed_mcs2_4$education_ridit)
#recode so is replaced with the ridit scores
imputed_mcs2_4$education_ridit[imputed_mcs2_4$education_ridit == 1] <- mcs_education_ridit4[1,]
imputed_mcs2_4$education_ridit[imputed_mcs2_4$education_ridit == 2] <- mcs_education_ridit4[2,]
imputed_mcs2_4$education_ridit[imputed_mcs2_4$education_ridit == 3] <- mcs_education_ridit4[3,]
imputed_mcs2_4$education_ridit[imputed_mcs2_4$education_ridit == 4] <- mcs_education_ridit4[4,]

#imputed dataset 5
#get count data 
#then convert to ridit score with toridit()
mcs_education_ridit5 <- as.matrix(toridit(table(imputed_mcs2_5$highested1)))
#add ridit score to dataset as new variable 
imputed_mcs2_5 = imputed_mcs2_5 %>% mutate(education_ridit = highested1) 
imputed_mcs2_5$education_ridit <- as.numeric(imputed_mcs2_5$education_ridit)
#recode so is replaced with the ridit scores
imputed_mcs2_5$education_ridit[imputed_mcs2_5$education_ridit == 1] <- mcs_education_ridit5[1,]
imputed_mcs2_5$education_ridit[imputed_mcs2_5$education_ridit == 2] <- mcs_education_ridit5[2,]
imputed_mcs2_5$education_ridit[imputed_mcs2_5$education_ridit == 3] <- mcs_education_ridit5[3,]
imputed_mcs2_5$education_ridit[imputed_mcs2_5$education_ridit == 4] <- mcs_education_ridit5[4,]

#imputed dataset 6
#get count data 
#then convert to ridit score with toridit()
mcs_education_ridit6 <- as.matrix(toridit(table(imputed_mcs2_6$highested1)))
#add ridit score to dataset as new variable 
imputed_mcs2_6 = imputed_mcs2_6 %>% mutate(education_ridit = highested1) 
imputed_mcs2_6$education_ridit <- as.numeric(imputed_mcs2_6$education_ridit)
#recode so is replaced with the ridit scores
imputed_mcs2_6$education_ridit[imputed_mcs2_6$education_ridit == 1] <- mcs_education_ridit6[1,]
imputed_mcs2_6$education_ridit[imputed_mcs2_6$education_ridit == 2] <- mcs_education_ridit6[2,]
imputed_mcs2_6$education_ridit[imputed_mcs2_6$education_ridit == 3] <- mcs_education_ridit6[3,]
imputed_mcs2_6$education_ridit[imputed_mcs2_6$education_ridit == 4] <- mcs_education_ridit6[4,]

#imputed dataset 7
#get count data 
#then convert to ridit score with toridit()
mcs_education_ridit7 <- as.matrix(toridit(table(imputed_mcs2_7$highested1)))
#add ridit score to dataset as new variable 
imputed_mcs2_7 = imputed_mcs2_7 %>% mutate(education_ridit = highested1) 
imputed_mcs2_7$education_ridit <- as.numeric(imputed_mcs2_7$education_ridit)
#recode so is replaced with the ridit scores
imputed_mcs2_7$education_ridit[imputed_mcs2_7$education_ridit == 1] <- mcs_education_ridit7[1,]
imputed_mcs2_7$education_ridit[imputed_mcs2_7$education_ridit == 2] <- mcs_education_ridit7[2,]
imputed_mcs2_7$education_ridit[imputed_mcs2_7$education_ridit == 3] <- mcs_education_ridit7[3,]
imputed_mcs2_7$education_ridit[imputed_mcs2_7$education_ridit == 4] <- mcs_education_ridit7[4,]


#imputed dataset 8
#get count data 
#then convert to ridit score with toridit()
mcs_education_ridit8 <- as.matrix(toridit(table(imputed_mcs2_8$highested1)))
#add ridit score to dataset as new variable 
imputed_mcs2_8 = imputed_mcs2_8 %>% mutate(education_ridit = highested1) 
imputed_mcs2_8$education_ridit <- as.numeric(imputed_mcs2_8$education_ridit)
#recode so is replaced with the ridit scores
imputed_mcs2_8$education_ridit[imputed_mcs2_8$education_ridit == 1] <- mcs_education_ridit8[1,]
imputed_mcs2_8$education_ridit[imputed_mcs2_8$education_ridit == 2] <- mcs_education_ridit8[2,]
imputed_mcs2_8$education_ridit[imputed_mcs2_8$education_ridit == 3] <- mcs_education_ridit8[3,]
imputed_mcs2_8$education_ridit[imputed_mcs2_8$education_ridit == 4] <- mcs_education_ridit8[4,]

#imputed dataset 9
#get count data 
#then convert to ridit score with toridit()
mcs_education_ridit9 <- as.matrix(toridit(table(imputed_mcs2_9$highested1)))
#add ridit score to dataset as new variable 
imputed_mcs2_9 = imputed_mcs2_9 %>% mutate(education_ridit = highested1) 
imputed_mcs2_9$education_ridit <- as.numeric(imputed_mcs2_9$education_ridit)
#recode so is replaced with the ridit scores
imputed_mcs2_9$education_ridit[imputed_mcs2_9$education_ridit == 1] <- mcs_education_ridit9[1,]
imputed_mcs2_9$education_ridit[imputed_mcs2_9$education_ridit == 2] <- mcs_education_ridit9[2,]
imputed_mcs2_9$education_ridit[imputed_mcs2_9$education_ridit == 3] <- mcs_education_ridit9[3,]
imputed_mcs2_9$education_ridit[imputed_mcs2_9$education_ridit == 4] <- mcs_education_ridit9[4,]

#imputed dataset 10
#get count data 
#then convert to ridit score with toridit()
mcs_education_ridit10 <- as.matrix(toridit(table(imputed_mcs2_10$highested1)))
#add ridit score to dataset as new variable 
imputed_mcs2_10 = imputed_mcs2_10 %>% mutate(education_ridit = highested1) 
imputed_mcs2_10$education_ridit <- as.numeric(imputed_mcs2_10$education_ridit)
#recode so is replaced with the ridit scores
imputed_mcs2_10$education_ridit[imputed_mcs2_10$education_ridit == 1] <- mcs_education_ridit10[1,]
imputed_mcs2_10$education_ridit[imputed_mcs2_10$education_ridit == 2] <- mcs_education_ridit10[2,]
imputed_mcs2_10$education_ridit[imputed_mcs2_10$education_ridit == 3] <- mcs_education_ridit10[3,]
imputed_mcs2_10$education_ridit[imputed_mcs2_10$education_ridit == 4] <- mcs_education_ridit10[4,]

#imputed dataset 11
#get count data 
#then convert to ridit score with toridit()
mcs_education_ridit11 <- as.matrix(toridit(table(imputed_mcs2_11$highested1)))
#add ridit score to dataset as new variable 
imputed_mcs2_11 = imputed_mcs2_11 %>% mutate(education_ridit = highested1) 
imputed_mcs2_11$education_ridit <- as.numeric(imputed_mcs2_11$education_ridit)
#recode so is replaced with the ridit scores
imputed_mcs2_11$education_ridit[imputed_mcs2_11$education_ridit == 1] <- mcs_education_ridit11[1,]
imputed_mcs2_11$education_ridit[imputed_mcs2_11$education_ridit == 2] <- mcs_education_ridit11[2,]
imputed_mcs2_11$education_ridit[imputed_mcs2_11$education_ridit == 3] <- mcs_education_ridit11[3,]
imputed_mcs2_11$education_ridit[imputed_mcs2_11$education_ridit == 4] <- mcs_education_ridit11[4,]


#imputed dataset 12
#get count data 
#then convert to ridit score with toridit()
mcs_education_ridit12 <- as.matrix(toridit(table(imputed_mcs2_12$highested1)))
#add ridit score to dataset as new variable 
imputed_mcs2_12 = imputed_mcs2_12 %>% mutate(education_ridit = highested1) 
imputed_mcs2_12$education_ridit <- as.numeric(imputed_mcs2_12$education_ridit)
#recode so is replaced with the ridit scores
imputed_mcs2_12$education_ridit[imputed_mcs2_12$education_ridit == 1] <- mcs_education_ridit12[1,]
imputed_mcs2_12$education_ridit[imputed_mcs2_12$education_ridit == 2] <- mcs_education_ridit12[2,]
imputed_mcs2_12$education_ridit[imputed_mcs2_12$education_ridit == 3] <- mcs_education_ridit12[3,]
imputed_mcs2_12$education_ridit[imputed_mcs2_12$education_ridit == 4] <- mcs_education_ridit12[4,]

#imputed dataset 13
#get count data 
#then convert to ridit score with toridit()
mcs_education_ridit13 <- as.matrix(toridit(table(imputed_mcs2_13$highested1)))
#add ridit score to dataset as new variable 
imputed_mcs2_13 = imputed_mcs2_13 %>% mutate(education_ridit = highested1) 
imputed_mcs2_13$education_ridit <- as.numeric(imputed_mcs2_13$education_ridit)
#recode so is replaced with the ridit scores
imputed_mcs2_13$education_ridit[imputed_mcs2_13$education_ridit == 1] <- mcs_education_ridit13[1,]
imputed_mcs2_13$education_ridit[imputed_mcs2_13$education_ridit == 2] <- mcs_education_ridit13[2,]
imputed_mcs2_13$education_ridit[imputed_mcs2_13$education_ridit == 3] <- mcs_education_ridit13[3,]
imputed_mcs2_13$education_ridit[imputed_mcs2_13$education_ridit == 4] <- mcs_education_ridit13[4,]

#imputed dataset 14
#get count data 
#then convert to ridit score with toridit()
mcs_education_ridit14 <- as.matrix(toridit(table(imputed_mcs2_14$highested1)))
#add ridit score to dataset as new variable 
imputed_mcs2_14 = imputed_mcs2_14 %>% mutate(education_ridit = highested1) 
imputed_mcs2_14$education_ridit <- as.numeric(imputed_mcs2_14$education_ridit)
#recode so is replaced with the ridit scores
imputed_mcs2_14$education_ridit[imputed_mcs2_14$education_ridit == 1] <- mcs_education_ridit14[1,]
imputed_mcs2_14$education_ridit[imputed_mcs2_14$education_ridit == 2] <- mcs_education_ridit14[2,]
imputed_mcs2_14$education_ridit[imputed_mcs2_14$education_ridit == 3] <- mcs_education_ridit14[3,]
imputed_mcs2_14$education_ridit[imputed_mcs2_14$education_ridit == 4] <- mcs_education_ridit14[4,]

#imputed dataset 15
#get count data 
#then convert to ridit score with toridit()
mcs_education_ridit15 <- as.matrix(toridit(table(imputed_mcs2_15$highested1)))
#add ridit score to dataset as new variable 
imputed_mcs2_15 = imputed_mcs2_15 %>% mutate(education_ridit = highested1) 
imputed_mcs2_15$education_ridit <- as.numeric(imputed_mcs2_15$education_ridit)
#recode so is replaced with the ridit scores
imputed_mcs2_15$education_ridit[imputed_mcs2_15$education_ridit == 1] <- mcs_education_ridit15[1,]
imputed_mcs2_15$education_ridit[imputed_mcs2_15$education_ridit == 2] <- mcs_education_ridit15[2,]
imputed_mcs2_15$education_ridit[imputed_mcs2_15$education_ridit == 3] <- mcs_education_ridit15[3,]
imputed_mcs2_15$education_ridit[imputed_mcs2_15$education_ridit == 4] <- mcs_education_ridit15[4,]

#imputed dataset 16
#get count data 
#then convert to ridit score with toridit()
mcs_education_ridit16 <- as.matrix(toridit(table(imputed_mcs2_16$highested1)))
#add ridit score to dataset as new variable 
imputed_mcs2_16 = imputed_mcs2_16 %>% mutate(education_ridit = highested1) 
imputed_mcs2_16$education_ridit <- as.numeric(imputed_mcs2_16$education_ridit)
#recode so is replaced with the ridit scores
imputed_mcs2_16$education_ridit[imputed_mcs2_16$education_ridit == 1] <- mcs_education_ridit16[1,]
imputed_mcs2_16$education_ridit[imputed_mcs2_16$education_ridit == 2] <- mcs_education_ridit16[2,]
imputed_mcs2_16$education_ridit[imputed_mcs2_16$education_ridit == 3] <- mcs_education_ridit16[3,]
imputed_mcs2_16$education_ridit[imputed_mcs2_16$education_ridit == 4] <- mcs_education_ridit16[4,]

#imputed dataset 17
#get count data 
#then convert to ridit score with toridit()
mcs_education_ridit17 <- as.matrix(toridit(table(imputed_mcs2_17$highested1)))
#add ridit score to dataset as new variable 
imputed_mcs2_17 = imputed_mcs2_17 %>% mutate(education_ridit = highested1) 
imputed_mcs2_17$education_ridit <- as.numeric(imputed_mcs2_17$education_ridit)
#recode so is replaced with the ridit scores
imputed_mcs2_17$education_ridit[imputed_mcs2_17$education_ridit == 1] <- mcs_education_ridit17[1,]
imputed_mcs2_17$education_ridit[imputed_mcs2_17$education_ridit == 2] <- mcs_education_ridit17[2,]
imputed_mcs2_17$education_ridit[imputed_mcs2_17$education_ridit == 3] <- mcs_education_ridit17[3,]
imputed_mcs2_17$education_ridit[imputed_mcs2_17$education_ridit == 4] <- mcs_education_ridit17[4,]

#imputed dataset 18
#get count data 
#then convert to ridit score with toridit()
mcs_education_ridit18 <- as.matrix(toridit(table(imputed_mcs2_18$highested1)))
#add ridit score to dataset as new variable 
imputed_mcs2_18 = imputed_mcs2_18 %>% mutate(education_ridit = highested1) 
imputed_mcs2_18$education_ridit <- as.numeric(imputed_mcs2_18$education_ridit)
#recode so is replaced with the ridit scores
imputed_mcs2_18$education_ridit[imputed_mcs2_18$education_ridit == 1] <- mcs_education_ridit18[1,]
imputed_mcs2_18$education_ridit[imputed_mcs2_18$education_ridit == 2] <- mcs_education_ridit18[2,]
imputed_mcs2_18$education_ridit[imputed_mcs2_18$education_ridit == 3] <- mcs_education_ridit18[3,]
imputed_mcs2_18$education_ridit[imputed_mcs2_18$education_ridit == 4] <- mcs_education_ridit18[4,]

#imputed dataset 19
#get count data 
#then convert to ridit score with toridit()
mcs_education_ridit19 <- as.matrix(toridit(table(imputed_mcs2_19$highested1)))
#add ridit score to dataset as new variable 
imputed_mcs2_19 = imputed_mcs2_19 %>% mutate(education_ridit = highested1) 
imputed_mcs2_19$education_ridit <- as.numeric(imputed_mcs2_19$education_ridit)
#recode so is replaced with the ridit scores
imputed_mcs2_19$education_ridit[imputed_mcs2_19$education_ridit == 1] <- mcs_education_ridit19[1,]
imputed_mcs2_19$education_ridit[imputed_mcs2_19$education_ridit == 2] <- mcs_education_ridit19[2,]
imputed_mcs2_19$education_ridit[imputed_mcs2_19$education_ridit == 3] <- mcs_education_ridit19[3,]
imputed_mcs2_19$education_ridit[imputed_mcs2_19$education_ridit == 4] <- mcs_education_ridit19[4,]

#imputed dataset 20
#get count data 
#then convert to ridit score with toridit()
mcs_education_ridit20 <- as.matrix(toridit(table(imputed_mcs2_20$highested1)))
#add ridit score to dataset as new variable 
imputed_mcs2_20 = imputed_mcs2_20 %>% mutate(education_ridit = highested1) 
imputed_mcs2_20$education_ridit <- as.numeric(imputed_mcs2_20$education_ridit)
#recode so is replaced with the ridit scores
imputed_mcs2_20$education_ridit[imputed_mcs2_20$education_ridit == 1] <- mcs_education_ridit20[1,]
imputed_mcs2_20$education_ridit[imputed_mcs2_20$education_ridit == 2] <- mcs_education_ridit20[2,]
imputed_mcs2_20$education_ridit[imputed_mcs2_20$education_ridit == 3] <- mcs_education_ridit20[3,]
imputed_mcs2_20$education_ridit[imputed_mcs2_20$education_ridit == 4] <- mcs_education_ridit20[4,]

#imputed dataset 21
#get count data 
#then convert to ridit score with toridit()
mcs_education_ridit21 <- as.matrix(toridit(table(imputed_mcs2_21$highested1)))
#add ridit score to dataset as new variable 
imputed_mcs2_21 = imputed_mcs2_21 %>% mutate(education_ridit = highested1) 
imputed_mcs2_21$education_ridit <- as.numeric(imputed_mcs2_21$education_ridit)
#recode so is replaced with the ridit scores
imputed_mcs2_21$education_ridit[imputed_mcs2_21$education_ridit == 1] <- mcs_education_ridit21[1,]
imputed_mcs2_21$education_ridit[imputed_mcs2_21$education_ridit == 2] <- mcs_education_ridit21[2,]
imputed_mcs2_21$education_ridit[imputed_mcs2_21$education_ridit == 3] <- mcs_education_ridit21[3,]
imputed_mcs2_21$education_ridit[imputed_mcs2_21$education_ridit == 4] <- mcs_education_ridit21[4,]

#imputed dataset 22
#get count data 
#then convert to ridit score with toridit()
mcs_education_ridit22 <- as.matrix(toridit(table(imputed_mcs2_22$highested1)))
#add ridit score to dataset as new variable 
imputed_mcs2_22 = imputed_mcs2_22 %>% mutate(education_ridit = highested1) 
imputed_mcs2_22$education_ridit <- as.numeric(imputed_mcs2_22$education_ridit)
#recode so is replaced with the ridit scores
imputed_mcs2_22$education_ridit[imputed_mcs2_22$education_ridit == 1] <- mcs_education_ridit22[1,]
imputed_mcs2_22$education_ridit[imputed_mcs2_22$education_ridit == 2] <- mcs_education_ridit22[2,]
imputed_mcs2_22$education_ridit[imputed_mcs2_22$education_ridit == 3] <- mcs_education_ridit22[3,]
imputed_mcs2_22$education_ridit[imputed_mcs2_22$education_ridit == 4] <- mcs_education_ridit22[4,]

#imputed dataset 23
#get count data 
#then convert to ridit score with toridit()
mcs_education_ridit23 <- as.matrix(toridit(table(imputed_mcs2_23$highested1)))
#add ridit score to dataset as new variable 
imputed_mcs2_23 = imputed_mcs2_23 %>% mutate(education_ridit = highested1) 
imputed_mcs2_23$education_ridit <- as.numeric(imputed_mcs2_23$education_ridit)
#recode so is replaced with the ridit scores
imputed_mcs2_23$education_ridit[imputed_mcs2_23$education_ridit == 1] <- mcs_education_ridit23[1,]
imputed_mcs2_23$education_ridit[imputed_mcs2_23$education_ridit == 2] <- mcs_education_ridit23[2,]
imputed_mcs2_23$education_ridit[imputed_mcs2_23$education_ridit == 3] <- mcs_education_ridit23[3,]
imputed_mcs2_23$education_ridit[imputed_mcs2_23$education_ridit == 4] <- mcs_education_ridit23[4,]

#imputed dataset 24
#get count data 
#then convert to ridit score with toridit()
mcs_education_ridit24 <- as.matrix(toridit(table(imputed_mcs2_24$highested1)))
#add ridit score to dataset as new variable 
imputed_mcs2_24 = imputed_mcs2_24 %>% mutate(education_ridit = highested1) 
imputed_mcs2_24$education_ridit <- as.numeric(imputed_mcs2_24$education_ridit)
#recode so is replaced with the ridit scores
imputed_mcs2_24$education_ridit[imputed_mcs2_24$education_ridit == 1] <- mcs_education_ridit24[1,]
imputed_mcs2_24$education_ridit[imputed_mcs2_24$education_ridit == 2] <- mcs_education_ridit24[2,]
imputed_mcs2_24$education_ridit[imputed_mcs2_24$education_ridit == 3] <- mcs_education_ridit24[3,]
imputed_mcs2_24$education_ridit[imputed_mcs2_24$education_ridit == 4] <- mcs_education_ridit24[4,]

#imputed dataset 25
#get count data 
#then convert to ridit score with toridit()
mcs_education_ridit25 <- as.matrix(toridit(table(imputed_mcs2_25$highested1)))
#add ridit score to dataset as new variable 
imputed_mcs2_25 = imputed_mcs2_25 %>% mutate(education_ridit = highested1) 
imputed_mcs2_25$education_ridit <- as.numeric(imputed_mcs2_25$education_ridit)
#recode so is replaced with the ridit scores
imputed_mcs2_25$education_ridit[imputed_mcs2_25$education_ridit == 1] <- mcs_education_ridit25[1,]
imputed_mcs2_25$education_ridit[imputed_mcs2_25$education_ridit == 2] <- mcs_education_ridit25[2,]
imputed_mcs2_25$education_ridit[imputed_mcs2_25$education_ridit == 3] <- mcs_education_ridit25[3,]
imputed_mcs2_25$education_ridit[imputed_mcs2_25$education_ridit == 4] <- mcs_education_ridit25[4,]


#education regression models with education ridit as predictor variable.
#early childhood imputed datasets 1-25
#late childhood imputed datasets 1-25
#adolescent imputed datasets 1-25

#education regression models with education ridit as predictor variable.
#Regression models over each imputed dataset 
age5_educationModel <- function(df) {
  fit <- lm(
    standardised_vocab5 ~ sex + ethnicity +  EAL + standardised_age5+  education_ridit, weight = weight2, data=df)
  return(fit)
}

age11_educationModel <- function(df) {
  fit <- lm(
    standardised_vocab11 ~ sex + ethnicity +  EAL + standardised_age11+  education_ridit, weight = weight2, data=df)
  return(fit)
}

age14_educationModel <- function(df) {
  fit <- lm(
    standardised_vocab14 ~ sex + ethnicity +  EAL + standardised_age14 +  education_ridit, weight = weight2, data=df)
  return(fit)
}



age5educationResults <- lapply(list(imputed_mcs2_1, imputed_mcs2_2, imputed_mcs2_3, imputed_mcs2_4, imputed_mcs2_5, 
                                    imputed_mcs2_6, imputed_mcs2_7, imputed_mcs2_8, imputed_mcs2_9, imputed_mcs2_10, 
                                    imputed_mcs2_11, imputed_mcs2_12, imputed_mcs2_13, imputed_mcs2_14, imputed_mcs2_15,
                                    imputed_mcs2_16, imputed_mcs2_17, imputed_mcs2_18, imputed_mcs2_19, imputed_mcs2_20, 
                                    imputed_mcs2_21, imputed_mcs2_22, imputed_mcs2_23, imputed_mcs2_24, imputed_mcs2_25),
                               age5_educationModel)

age10educationResults <- lapply(list(imputed_mcs2_1, imputed_mcs2_2, imputed_mcs2_3, imputed_mcs2_4, imputed_mcs2_5, 
                                     imputed_mcs2_6, imputed_mcs2_7, imputed_mcs2_8, imputed_mcs2_9, imputed_mcs2_10, 
                                     imputed_mcs2_11, imputed_mcs2_12, imputed_mcs2_13, imputed_mcs2_14, imputed_mcs2_15,
                                     imputed_mcs2_16, imputed_mcs2_17, imputed_mcs2_18, imputed_mcs2_19, imputed_mcs2_20, 
                                     imputed_mcs2_21, imputed_mcs2_22, imputed_mcs2_23, imputed_mcs2_24, imputed_mcs2_25),
                                age11_educationModel)

age16educationResults <- lapply(list(imputed_mcs2_1, imputed_mcs2_2, imputed_mcs2_3, imputed_mcs2_4, imputed_mcs2_5, 
                                     imputed_mcs2_6, imputed_mcs2_7, imputed_mcs2_8, imputed_mcs2_9, imputed_mcs2_10, 
                                     imputed_mcs2_11, imputed_mcs2_12, imputed_mcs2_13, imputed_mcs2_14, imputed_mcs2_15,
                                     imputed_mcs2_16, imputed_mcs2_17, imputed_mcs2_18, imputed_mcs2_19, imputed_mcs2_20, 
                                     imputed_mcs2_21, imputed_mcs2_22, imputed_mcs2_23, imputed_mcs2_24, imputed_mcs2_25),
                                age14_educationModel)

#Pool Results 

age5_educationResults <- summary(pool(as.mira(age5educationResults)),conf.int = TRUE, conf.level = 0.95) 
age11_educationResults <- summary(pool(as.mira(age10educationResults)),conf.int = TRUE, conf.level = 0.95) 
age14_educationResults <- summary(pool(as.mira(age16educationResults)),conf.int = TRUE, conf.level = 0.95) 





#OCCUPATIONAL STATUS ####
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

#occupation regression models with occupation ridit as predictor variable.
#Regression models over each imputed dataset 
age5_occupationModel <- function(df) {
  fit <- lm(
    standardised_vocab5 ~ sex + ethnicity +  EAL + standardised_age5+  occupation_ridit, weight = weight2, data=df)
  return(fit)
}

age11_occupationModel <- function(df) {
  fit <- lm(
    standardised_vocab11 ~ sex + ethnicity +  EAL + standardised_age11+  occupation_ridit, weight = weight2, data=df)
  return(fit)
}

age14_occupationModel <- function(df) {
  fit <- lm(
    standardised_vocab14 ~ sex + ethnicity +  EAL + standardised_age14 +  occupation_ridit, weight = weight2, data=df)
  return(fit)
}



age5occupationResults <- lapply(list(imputed_mcs2_1, imputed_mcs2_2, imputed_mcs2_3, imputed_mcs2_4, imputed_mcs2_5, 
                                     imputed_mcs2_6, imputed_mcs2_7, imputed_mcs2_8, imputed_mcs2_9, imputed_mcs2_10, 
                                     imputed_mcs2_11, imputed_mcs2_12, imputed_mcs2_13, imputed_mcs2_14, imputed_mcs2_15,
                                     imputed_mcs2_16, imputed_mcs2_17, imputed_mcs2_18, imputed_mcs2_19, imputed_mcs2_20, 
                                     imputed_mcs2_21, imputed_mcs2_22, imputed_mcs2_23, imputed_mcs2_24, imputed_mcs2_25),
                                age5_occupationModel)

age10occupationResults <- lapply(list(imputed_mcs2_1, imputed_mcs2_2, imputed_mcs2_3, imputed_mcs2_4, imputed_mcs2_5, 
                                      imputed_mcs2_6, imputed_mcs2_7, imputed_mcs2_8, imputed_mcs2_9, imputed_mcs2_10, 
                                      imputed_mcs2_11, imputed_mcs2_12, imputed_mcs2_13, imputed_mcs2_14, imputed_mcs2_15,
                                      imputed_mcs2_16, imputed_mcs2_17, imputed_mcs2_18, imputed_mcs2_19, imputed_mcs2_20, 
                                      imputed_mcs2_21, imputed_mcs2_22, imputed_mcs2_23, imputed_mcs2_24, imputed_mcs2_25),
                                 age11_occupationModel)

age16occupationResults <- lapply(list(imputed_mcs2_1, imputed_mcs2_2, imputed_mcs2_3, imputed_mcs2_4, imputed_mcs2_5, 
                                      imputed_mcs2_6, imputed_mcs2_7, imputed_mcs2_8, imputed_mcs2_9, imputed_mcs2_10, 
                                      imputed_mcs2_11, imputed_mcs2_12, imputed_mcs2_13, imputed_mcs2_14, imputed_mcs2_15,
                                      imputed_mcs2_16, imputed_mcs2_17, imputed_mcs2_18, imputed_mcs2_19, imputed_mcs2_20, 
                                      imputed_mcs2_21, imputed_mcs2_22, imputed_mcs2_23, imputed_mcs2_24, imputed_mcs2_25),
                                 age14_occupationModel)

#Pool Results 

age5_occupationResults <- summary(pool(as.mira(age5occupationResults)),conf.int = TRUE, conf.level = 0.95) 
age11_occupationResults <- summary(pool(as.mira(age10occupationResults)),conf.int = TRUE, conf.level = 0.95) 
age14_occupationResults <- summary(pool(as.mira(age16occupationResults)),conf.int = TRUE, conf.level = 0.95) 





#calculate ridit scores. income  ####
#imputed dataset 1
#get count data 
#then convert to ridit score with toridit()
mcs_income_ridit1 <- as.matrix(toridit(table(imputed_mcs2_1$income_quintiles)))
#add ridit score to dataset as new variable 
imputed_mcs2_1 = imputed_mcs2_1 %>% mutate(income_ridit = income_quintiles) 
imputed_mcs2_1$income_ridit <- as.numeric(imputed_mcs2_1$income_ridit)
#recode so is replaced with the ridit scores
imputed_mcs2_1$income_ridit[imputed_mcs2_1$income_ridit == 1] <- mcs_income_ridit1[1,]
imputed_mcs2_1$income_ridit[imputed_mcs2_1$income_ridit == 2] <- mcs_income_ridit1[2,]
imputed_mcs2_1$income_ridit[imputed_mcs2_1$income_ridit == 3] <- mcs_income_ridit1[3,]
imputed_mcs2_1$income_ridit[imputed_mcs2_1$income_ridit == 4] <- mcs_income_ridit1[4,]

#imputed dataset 2
#get count data 
#then convert to ridit score with toridit()
mcs_income_ridit2 <- as.matrix(toridit(table(imputed_mcs2_2$income_quintiles)))
#add ridit score to dataset as new variable 
imputed_mcs2_2 = imputed_mcs2_2 %>% mutate(income_ridit = income_quintiles) 
imputed_mcs2_2$income_ridit <- as.numeric(imputed_mcs2_2$income_ridit)
#recode so is replaced with the ridit scores
imputed_mcs2_2$income_ridit[imputed_mcs2_2$income_ridit == 1] <- mcs_income_ridit2[1,]
imputed_mcs2_2$income_ridit[imputed_mcs2_2$income_ridit == 2] <- mcs_income_ridit2[2,]
imputed_mcs2_2$income_ridit[imputed_mcs2_2$income_ridit == 3] <- mcs_income_ridit2[3,]
imputed_mcs2_2$income_ridit[imputed_mcs2_2$income_ridit == 4] <- mcs_income_ridit2[4,]

#imputed dataset 3
#get count data 
#then convert to ridit score with toridit()
mcs_income_ridit3 <- as.matrix(toridit(table(imputed_mcs2_3$income_quintiles)))
#add ridit score to dataset as new variable 
imputed_mcs2_3 = imputed_mcs2_3 %>% mutate(income_ridit = income_quintiles) 
imputed_mcs2_3$income_ridit <- as.numeric(imputed_mcs2_3$income_ridit)
#recode so is replaced with the ridit scores
imputed_mcs2_3$income_ridit[imputed_mcs2_3$income_ridit == 1] <- mcs_income_ridit3[1,]
imputed_mcs2_3$income_ridit[imputed_mcs2_3$income_ridit == 2] <- mcs_income_ridit3[2,]
imputed_mcs2_3$income_ridit[imputed_mcs2_3$income_ridit == 3] <- mcs_income_ridit3[3,]
imputed_mcs2_3$income_ridit[imputed_mcs2_3$income_ridit == 4] <- mcs_income_ridit3[4,]


#imputed dataset 4
#get count data 
#then convert to ridit score with toridit()
mcs_income_ridit4 <- as.matrix(toridit(table(imputed_mcs2_4$income_quintiles)))
#add ridit score to dataset as new variable 
imputed_mcs2_4 = imputed_mcs2_4 %>% mutate(income_ridit = income_quintiles) 
imputed_mcs2_4$income_ridit <- as.numeric(imputed_mcs2_4$income_ridit)
#recode so is replaced with the ridit scores
imputed_mcs2_4$income_ridit[imputed_mcs2_4$income_ridit == 1] <- mcs_income_ridit4[1,]
imputed_mcs2_4$income_ridit[imputed_mcs2_4$income_ridit == 2] <- mcs_income_ridit4[2,]
imputed_mcs2_4$income_ridit[imputed_mcs2_4$income_ridit == 3] <- mcs_income_ridit4[3,]
imputed_mcs2_4$income_ridit[imputed_mcs2_4$income_ridit == 4] <- mcs_income_ridit4[4,]

#imputed dataset 5
#get count data 
#then convert to ridit score with toridit()
mcs_income_ridit5 <- as.matrix(toridit(table(imputed_mcs2_5$income_quintiles)))
#add ridit score to dataset as new variable 
imputed_mcs2_5 = imputed_mcs2_5 %>% mutate(income_ridit = income_quintiles) 
imputed_mcs2_5$income_ridit <- as.numeric(imputed_mcs2_5$income_ridit)
#recode so is replaced with the ridit scores
imputed_mcs2_5$income_ridit[imputed_mcs2_5$income_ridit == 1] <- mcs_income_ridit5[1,]
imputed_mcs2_5$income_ridit[imputed_mcs2_5$income_ridit == 2] <- mcs_income_ridit5[2,]
imputed_mcs2_5$income_ridit[imputed_mcs2_5$income_ridit == 3] <- mcs_income_ridit5[3,]
imputed_mcs2_5$income_ridit[imputed_mcs2_5$income_ridit == 4] <- mcs_income_ridit5[4,]

#imputed dataset 6
#get count data 
#then convert to ridit score with toridit()
mcs_income_ridit6 <- as.matrix(toridit(table(imputed_mcs2_6$income_quintiles)))
#add ridit score to dataset as new variable 
imputed_mcs2_6 = imputed_mcs2_6 %>% mutate(income_ridit = income_quintiles) 
imputed_mcs2_6$income_ridit <- as.numeric(imputed_mcs2_6$income_ridit)
#recode so is replaced with the ridit scores
imputed_mcs2_6$income_ridit[imputed_mcs2_6$income_ridit == 1] <- mcs_income_ridit6[1,]
imputed_mcs2_6$income_ridit[imputed_mcs2_6$income_ridit == 2] <- mcs_income_ridit6[2,]
imputed_mcs2_6$income_ridit[imputed_mcs2_6$income_ridit == 3] <- mcs_income_ridit6[3,]
imputed_mcs2_6$income_ridit[imputed_mcs2_6$income_ridit == 4] <- mcs_income_ridit6[4,]

#imputed dataset 7
#get count data 
#then convert to ridit score with toridit()
mcs_income_ridit7 <- as.matrix(toridit(table(imputed_mcs2_7$income_quintiles)))
#add ridit score to dataset as new variable 
imputed_mcs2_7 = imputed_mcs2_7 %>% mutate(income_ridit = income_quintiles) 
imputed_mcs2_7$income_ridit <- as.numeric(imputed_mcs2_7$income_ridit)
#recode so is replaced with the ridit scores
imputed_mcs2_7$income_ridit[imputed_mcs2_7$income_ridit == 1] <- mcs_income_ridit7[1,]
imputed_mcs2_7$income_ridit[imputed_mcs2_7$income_ridit == 2] <- mcs_income_ridit7[2,]
imputed_mcs2_7$income_ridit[imputed_mcs2_7$income_ridit == 3] <- mcs_income_ridit7[3,]
imputed_mcs2_7$income_ridit[imputed_mcs2_7$income_ridit == 4] <- mcs_income_ridit7[4,]


#imputed dataset 8
#get count data 
#then convert to ridit score with toridit()
mcs_income_ridit8 <- as.matrix(toridit(table(imputed_mcs2_8$income_quintiles)))
#add ridit score to dataset as new variable 
imputed_mcs2_8 = imputed_mcs2_8 %>% mutate(income_ridit = income_quintiles) 
imputed_mcs2_8$income_ridit <- as.numeric(imputed_mcs2_8$income_ridit)
#recode so is replaced with the ridit scores
imputed_mcs2_8$income_ridit[imputed_mcs2_8$income_ridit == 1] <- mcs_income_ridit8[1,]
imputed_mcs2_8$income_ridit[imputed_mcs2_8$income_ridit == 2] <- mcs_income_ridit8[2,]
imputed_mcs2_8$income_ridit[imputed_mcs2_8$income_ridit == 3] <- mcs_income_ridit8[3,]
imputed_mcs2_8$income_ridit[imputed_mcs2_8$income_ridit == 4] <- mcs_income_ridit8[4,]

#imputed dataset 9
#get count data 
#then convert to ridit score with toridit()
mcs_income_ridit9 <- as.matrix(toridit(table(imputed_mcs2_9$income_quintiles)))
#add ridit score to dataset as new variable 
imputed_mcs2_9 = imputed_mcs2_9 %>% mutate(income_ridit = income_quintiles) 
imputed_mcs2_9$income_ridit <- as.numeric(imputed_mcs2_9$income_ridit)
#recode so is replaced with the ridit scores
imputed_mcs2_9$income_ridit[imputed_mcs2_9$income_ridit == 1] <- mcs_income_ridit9[1,]
imputed_mcs2_9$income_ridit[imputed_mcs2_9$income_ridit == 2] <- mcs_income_ridit9[2,]
imputed_mcs2_9$income_ridit[imputed_mcs2_9$income_ridit == 3] <- mcs_income_ridit9[3,]
imputed_mcs2_9$income_ridit[imputed_mcs2_9$income_ridit == 4] <- mcs_income_ridit9[4,]

#imputed dataset 10
#get count data 
#then convert to ridit score with toridit()
mcs_income_ridit10 <- as.matrix(toridit(table(imputed_mcs2_10$income_quintiles)))
#add ridit score to dataset as new variable 
imputed_mcs2_10 = imputed_mcs2_10 %>% mutate(income_ridit = income_quintiles) 
imputed_mcs2_10$income_ridit <- as.numeric(imputed_mcs2_10$income_ridit)
#recode so is replaced with the ridit scores
imputed_mcs2_10$income_ridit[imputed_mcs2_10$income_ridit == 1] <- mcs_income_ridit10[1,]
imputed_mcs2_10$income_ridit[imputed_mcs2_10$income_ridit == 2] <- mcs_income_ridit10[2,]
imputed_mcs2_10$income_ridit[imputed_mcs2_10$income_ridit == 3] <- mcs_income_ridit10[3,]
imputed_mcs2_10$income_ridit[imputed_mcs2_10$income_ridit == 4] <- mcs_income_ridit10[4,]

#imputed dataset 11
#get count data 
#then convert to ridit score with toridit()
mcs_income_ridit11 <- as.matrix(toridit(table(imputed_mcs2_11$income_quintiles)))
#add ridit score to dataset as new variable 
imputed_mcs2_11 = imputed_mcs2_11 %>% mutate(income_ridit = income_quintiles) 
imputed_mcs2_11$income_ridit <- as.numeric(imputed_mcs2_11$income_ridit)
#recode so is replaced with the ridit scores
imputed_mcs2_11$income_ridit[imputed_mcs2_11$income_ridit == 1] <- mcs_income_ridit11[1,]
imputed_mcs2_11$income_ridit[imputed_mcs2_11$income_ridit == 2] <- mcs_income_ridit11[2,]
imputed_mcs2_11$income_ridit[imputed_mcs2_11$income_ridit == 3] <- mcs_income_ridit11[3,]
imputed_mcs2_11$income_ridit[imputed_mcs2_11$income_ridit == 4] <- mcs_income_ridit11[4,]


#imputed dataset 12
#get count data 
#then convert to ridit score with toridit()
mcs_income_ridit12 <- as.matrix(toridit(table(imputed_mcs2_12$income_quintiles)))
#add ridit score to dataset as new variable 
imputed_mcs2_12 = imputed_mcs2_12 %>% mutate(income_ridit = income_quintiles) 
imputed_mcs2_12$income_ridit <- as.numeric(imputed_mcs2_12$income_ridit)
#recode so is replaced with the ridit scores
imputed_mcs2_12$income_ridit[imputed_mcs2_12$income_ridit == 1] <- mcs_income_ridit12[1,]
imputed_mcs2_12$income_ridit[imputed_mcs2_12$income_ridit == 2] <- mcs_income_ridit12[2,]
imputed_mcs2_12$income_ridit[imputed_mcs2_12$income_ridit == 3] <- mcs_income_ridit12[3,]
imputed_mcs2_12$income_ridit[imputed_mcs2_12$income_ridit == 4] <- mcs_income_ridit12[4,]

#imputed dataset 13
#get count data 
#then convert to ridit score with toridit()
mcs_income_ridit13 <- as.matrix(toridit(table(imputed_mcs2_13$income_quintiles)))
#add ridit score to dataset as new variable 
imputed_mcs2_13 = imputed_mcs2_13 %>% mutate(income_ridit = income_quintiles) 
imputed_mcs2_13$income_ridit <- as.numeric(imputed_mcs2_13$income_ridit)
#recode so is replaced with the ridit scores
imputed_mcs2_13$income_ridit[imputed_mcs2_13$income_ridit == 1] <- mcs_income_ridit13[1,]
imputed_mcs2_13$income_ridit[imputed_mcs2_13$income_ridit == 2] <- mcs_income_ridit13[2,]
imputed_mcs2_13$income_ridit[imputed_mcs2_13$income_ridit == 3] <- mcs_income_ridit13[3,]
imputed_mcs2_13$income_ridit[imputed_mcs2_13$income_ridit == 4] <- mcs_income_ridit13[4,]

#imputed dataset 14
#get count data 
#then convert to ridit score with toridit()
mcs_income_ridit14 <- as.matrix(toridit(table(imputed_mcs2_14$income_quintiles)))
#add ridit score to dataset as new variable 
imputed_mcs2_14 = imputed_mcs2_14 %>% mutate(income_ridit = income_quintiles) 
imputed_mcs2_14$income_ridit <- as.numeric(imputed_mcs2_14$income_ridit)
#recode so is replaced with the ridit scores
imputed_mcs2_14$income_ridit[imputed_mcs2_14$income_ridit == 1] <- mcs_income_ridit14[1,]
imputed_mcs2_14$income_ridit[imputed_mcs2_14$income_ridit == 2] <- mcs_income_ridit14[2,]
imputed_mcs2_14$income_ridit[imputed_mcs2_14$income_ridit == 3] <- mcs_income_ridit14[3,]
imputed_mcs2_14$income_ridit[imputed_mcs2_14$income_ridit == 4] <- mcs_income_ridit14[4,]

#imputed dataset 15
#get count data 
#then convert to ridit score with toridit()
mcs_income_ridit15 <- as.matrix(toridit(table(imputed_mcs2_15$income_quintiles)))
#add ridit score to dataset as new variable 
imputed_mcs2_15 = imputed_mcs2_15 %>% mutate(income_ridit = income_quintiles) 
imputed_mcs2_15$income_ridit <- as.numeric(imputed_mcs2_15$income_ridit)
#recode so is replaced with the ridit scores
imputed_mcs2_15$income_ridit[imputed_mcs2_15$income_ridit == 1] <- mcs_income_ridit15[1,]
imputed_mcs2_15$income_ridit[imputed_mcs2_15$income_ridit == 2] <- mcs_income_ridit15[2,]
imputed_mcs2_15$income_ridit[imputed_mcs2_15$income_ridit == 3] <- mcs_income_ridit15[3,]
imputed_mcs2_15$income_ridit[imputed_mcs2_15$income_ridit == 4] <- mcs_income_ridit15[4,]

#imputed dataset 16
#get count data 
#then convert to ridit score with toridit()
mcs_income_ridit16 <- as.matrix(toridit(table(imputed_mcs2_16$income_quintiles)))
#add ridit score to dataset as new variable 
imputed_mcs2_16 = imputed_mcs2_16 %>% mutate(income_ridit = income_quintiles) 
imputed_mcs2_16$income_ridit <- as.numeric(imputed_mcs2_16$income_ridit)
#recode so is replaced with the ridit scores
imputed_mcs2_16$income_ridit[imputed_mcs2_16$income_ridit == 1] <- mcs_income_ridit16[1,]
imputed_mcs2_16$income_ridit[imputed_mcs2_16$income_ridit == 2] <- mcs_income_ridit16[2,]
imputed_mcs2_16$income_ridit[imputed_mcs2_16$income_ridit == 3] <- mcs_income_ridit16[3,]
imputed_mcs2_16$income_ridit[imputed_mcs2_16$income_ridit == 4] <- mcs_income_ridit16[4,]

#imputed dataset 17
#get count data 
#then convert to ridit score with toridit()
mcs_income_ridit17 <- as.matrix(toridit(table(imputed_mcs2_17$income_quintiles)))
#add ridit score to dataset as new variable 
imputed_mcs2_17 = imputed_mcs2_17 %>% mutate(income_ridit = income_quintiles) 
imputed_mcs2_17$income_ridit <- as.numeric(imputed_mcs2_17$income_ridit)
#recode so is replaced with the ridit scores
imputed_mcs2_17$income_ridit[imputed_mcs2_17$income_ridit == 1] <- mcs_income_ridit17[1,]
imputed_mcs2_17$income_ridit[imputed_mcs2_17$income_ridit == 2] <- mcs_income_ridit17[2,]
imputed_mcs2_17$income_ridit[imputed_mcs2_17$income_ridit == 3] <- mcs_income_ridit17[3,]
imputed_mcs2_17$income_ridit[imputed_mcs2_17$income_ridit == 4] <- mcs_income_ridit17[4,]

#imputed dataset 18
#get count data 
#then convert to ridit score with toridit()
mcs_income_ridit18 <- as.matrix(toridit(table(imputed_mcs2_18$income_quintiles)))
#add ridit score to dataset as new variable 
imputed_mcs2_18 = imputed_mcs2_18 %>% mutate(income_ridit = income_quintiles) 
imputed_mcs2_18$income_ridit <- as.numeric(imputed_mcs2_18$income_ridit)
#recode so is replaced with the ridit scores
imputed_mcs2_18$income_ridit[imputed_mcs2_18$income_ridit == 1] <- mcs_income_ridit18[1,]
imputed_mcs2_18$income_ridit[imputed_mcs2_18$income_ridit == 2] <- mcs_income_ridit18[2,]
imputed_mcs2_18$income_ridit[imputed_mcs2_18$income_ridit == 3] <- mcs_income_ridit18[3,]
imputed_mcs2_18$income_ridit[imputed_mcs2_18$income_ridit == 4] <- mcs_income_ridit18[4,]

#imputed dataset 19
#get count data 
#then convert to ridit score with toridit()
mcs_income_ridit19 <- as.matrix(toridit(table(imputed_mcs2_19$income_quintiles)))
#add ridit score to dataset as new variable 
imputed_mcs2_19 = imputed_mcs2_19 %>% mutate(income_ridit = income_quintiles) 
imputed_mcs2_19$income_ridit <- as.numeric(imputed_mcs2_19$income_ridit)
#recode so is replaced with the ridit scores
imputed_mcs2_19$income_ridit[imputed_mcs2_19$income_ridit == 1] <- mcs_income_ridit19[1,]
imputed_mcs2_19$income_ridit[imputed_mcs2_19$income_ridit == 2] <- mcs_income_ridit19[2,]
imputed_mcs2_19$income_ridit[imputed_mcs2_19$income_ridit == 3] <- mcs_income_ridit19[3,]
imputed_mcs2_19$income_ridit[imputed_mcs2_19$income_ridit == 4] <- mcs_income_ridit19[4,]

#imputed dataset 20
#get count data 
#then convert to ridit score with toridit()
mcs_income_ridit20 <- as.matrix(toridit(table(imputed_mcs2_20$income_quintiles)))
#add ridit score to dataset as new variable 
imputed_mcs2_20 = imputed_mcs2_20 %>% mutate(income_ridit = income_quintiles) 
imputed_mcs2_20$income_ridit <- as.numeric(imputed_mcs2_20$income_ridit)
#recode so is replaced with the ridit scores
imputed_mcs2_20$income_ridit[imputed_mcs2_20$income_ridit == 1] <- mcs_income_ridit20[1,]
imputed_mcs2_20$income_ridit[imputed_mcs2_20$income_ridit == 2] <- mcs_income_ridit20[2,]
imputed_mcs2_20$income_ridit[imputed_mcs2_20$income_ridit == 3] <- mcs_income_ridit20[3,]
imputed_mcs2_20$income_ridit[imputed_mcs2_20$income_ridit == 4] <- mcs_income_ridit20[4,]

#imputed dataset 21
#get count data 
#then convert to ridit score with toridit()
mcs_income_ridit21 <- as.matrix(toridit(table(imputed_mcs2_21$income_quintiles)))
#add ridit score to dataset as new variable 
imputed_mcs2_21 = imputed_mcs2_21 %>% mutate(income_ridit = income_quintiles) 
imputed_mcs2_21$income_ridit <- as.numeric(imputed_mcs2_21$income_ridit)
#recode so is replaced with the ridit scores
imputed_mcs2_21$income_ridit[imputed_mcs2_21$income_ridit == 1] <- mcs_income_ridit21[1,]
imputed_mcs2_21$income_ridit[imputed_mcs2_21$income_ridit == 2] <- mcs_income_ridit21[2,]
imputed_mcs2_21$income_ridit[imputed_mcs2_21$income_ridit == 3] <- mcs_income_ridit21[3,]
imputed_mcs2_21$income_ridit[imputed_mcs2_21$income_ridit == 4] <- mcs_income_ridit21[4,]

#imputed dataset 22
#get count data 
#then convert to ridit score with toridit()
mcs_income_ridit22 <- as.matrix(toridit(table(imputed_mcs2_22$income_quintiles)))
#add ridit score to dataset as new variable 
imputed_mcs2_22 = imputed_mcs2_22 %>% mutate(income_ridit = income_quintiles) 
imputed_mcs2_22$income_ridit <- as.numeric(imputed_mcs2_22$income_ridit)
#recode so is replaced with the ridit scores
imputed_mcs2_22$income_ridit[imputed_mcs2_22$income_ridit == 1] <- mcs_income_ridit22[1,]
imputed_mcs2_22$income_ridit[imputed_mcs2_22$income_ridit == 2] <- mcs_income_ridit22[2,]
imputed_mcs2_22$income_ridit[imputed_mcs2_22$income_ridit == 3] <- mcs_income_ridit22[3,]
imputed_mcs2_22$income_ridit[imputed_mcs2_22$income_ridit == 4] <- mcs_income_ridit22[4,]

#imputed dataset 23
#get count data 
#then convert to ridit score with toridit()
mcs_income_ridit23 <- as.matrix(toridit(table(imputed_mcs2_23$income_quintiles)))
#add ridit score to dataset as new variable 
imputed_mcs2_23 = imputed_mcs2_23 %>% mutate(income_ridit = income_quintiles) 
imputed_mcs2_23$income_ridit <- as.numeric(imputed_mcs2_23$income_ridit)
#recode so is replaced with the ridit scores
imputed_mcs2_23$income_ridit[imputed_mcs2_23$income_ridit == 1] <- mcs_income_ridit23[1,]
imputed_mcs2_23$income_ridit[imputed_mcs2_23$income_ridit == 2] <- mcs_income_ridit23[2,]
imputed_mcs2_23$income_ridit[imputed_mcs2_23$income_ridit == 3] <- mcs_income_ridit23[3,]
imputed_mcs2_23$income_ridit[imputed_mcs2_23$income_ridit == 4] <- mcs_income_ridit23[4,]

#imputed dataset 24
#get count data 
#then convert to ridit score with toridit()
mcs_income_ridit24 <- as.matrix(toridit(table(imputed_mcs2_24$income_quintiles)))
#add ridit score to dataset as new variable 
imputed_mcs2_24 = imputed_mcs2_24 %>% mutate(income_ridit = income_quintiles) 
imputed_mcs2_24$income_ridit <- as.numeric(imputed_mcs2_24$income_ridit)
#recode so is replaced with the ridit scores
imputed_mcs2_24$income_ridit[imputed_mcs2_24$income_ridit == 1] <- mcs_income_ridit24[1,]
imputed_mcs2_24$income_ridit[imputed_mcs2_24$income_ridit == 2] <- mcs_income_ridit24[2,]
imputed_mcs2_24$income_ridit[imputed_mcs2_24$income_ridit == 3] <- mcs_income_ridit24[3,]
imputed_mcs2_24$income_ridit[imputed_mcs2_24$income_ridit == 4] <- mcs_income_ridit24[4,]

#imputed dataset 25
#get count data 
#then convert to ridit score with toridit()
mcs_income_ridit25 <- as.matrix(toridit(table(imputed_mcs2_25$income_quintiles)))
#add ridit score to dataset as new variable 
imputed_mcs2_25 = imputed_mcs2_25 %>% mutate(income_ridit = income_quintiles) 
imputed_mcs2_25$income_ridit <- as.numeric(imputed_mcs2_25$income_ridit)
#recode so is replaced with the ridit scores
imputed_mcs2_25$income_ridit[imputed_mcs2_25$income_ridit == 1] <- mcs_income_ridit25[1,]
imputed_mcs2_25$income_ridit[imputed_mcs2_25$income_ridit == 2] <- mcs_income_ridit25[2,]
imputed_mcs2_25$income_ridit[imputed_mcs2_25$income_ridit == 3] <- mcs_income_ridit25[3,]
imputed_mcs2_25$income_ridit[imputed_mcs2_25$income_ridit == 4] <- mcs_income_ridit25[4,]


#income regression models with income ridit as predictor variable.
#Regression models over each imputed dataset 
age5_IncomeModel <- function(df) {
  fit <- lm(
    standardised_vocab5 ~ sex + ethnicity +  EAL + standardised_age5+  income_ridit, weight = weight2, data=df)
  return(fit)
}

age11_IncomeModel <- function(df) {
  fit <- lm(
    standardised_vocab11 ~ sex + ethnicity +  EAL + standardised_age11+  income_ridit, weight = weight2, data=df)
  return(fit)
}

age14_IncomeModel <- function(df) {
  fit <- lm(
    standardised_vocab14 ~ sex + ethnicity +  EAL + standardised_age14 +  income_ridit, weight = weight2, data=df)
  return(fit)
}



age5incomeResults <- lapply(list(imputed_mcs2_1, imputed_mcs2_2, imputed_mcs2_3, imputed_mcs2_4, imputed_mcs2_5, 
                                 imputed_mcs2_6, imputed_mcs2_7, imputed_mcs2_8, imputed_mcs2_9, imputed_mcs2_10, 
                                 imputed_mcs2_11, imputed_mcs2_12, imputed_mcs2_13, imputed_mcs2_14, imputed_mcs2_15,
                                 imputed_mcs2_16, imputed_mcs2_17, imputed_mcs2_18, imputed_mcs2_19, imputed_mcs2_20, 
                                 imputed_mcs2_21, imputed_mcs2_22, imputed_mcs2_23, imputed_mcs2_24, imputed_mcs2_25),
                            age5_IncomeModel)

age10incomeResults <- lapply(list(imputed_mcs2_1, imputed_mcs2_2, imputed_mcs2_3, imputed_mcs2_4, imputed_mcs2_5, 
                                  imputed_mcs2_6, imputed_mcs2_7, imputed_mcs2_8, imputed_mcs2_9, imputed_mcs2_10, 
                                  imputed_mcs2_11, imputed_mcs2_12, imputed_mcs2_13, imputed_mcs2_14, imputed_mcs2_15,
                                  imputed_mcs2_16, imputed_mcs2_17, imputed_mcs2_18, imputed_mcs2_19, imputed_mcs2_20, 
                                  imputed_mcs2_21, imputed_mcs2_22, imputed_mcs2_23, imputed_mcs2_24, imputed_mcs2_25),
                             age11_IncomeModel)

age16incomeResults <- lapply(list(imputed_mcs2_1, imputed_mcs2_2, imputed_mcs2_3, imputed_mcs2_4, imputed_mcs2_5, 
                                  imputed_mcs2_6, imputed_mcs2_7, imputed_mcs2_8, imputed_mcs2_9, imputed_mcs2_10, 
                                  imputed_mcs2_11, imputed_mcs2_12, imputed_mcs2_13, imputed_mcs2_14, imputed_mcs2_15,
                                  imputed_mcs2_16, imputed_mcs2_17, imputed_mcs2_18, imputed_mcs2_19, imputed_mcs2_20, 
                                  imputed_mcs2_21, imputed_mcs2_22, imputed_mcs2_23, imputed_mcs2_24, imputed_mcs2_25),
                             age14_IncomeModel)

#Pool Results 

age5_incomeResults <- summary(pool(as.mira(age5incomeResults)),conf.int = TRUE, conf.level = 0.95) 
age11_incomeResults <- summary(pool(as.mira(age10incomeResults)),conf.int = TRUE, conf.level = 0.95) 
age14_incomeResults <- summary(pool(as.mira(age16incomeResults)),conf.int = TRUE, conf.level = 0.95) 


#confounders only models to get Partial R2 for ridits. ####

#confounders regression models with confounders ridit as predictor variable.
#Regression models over each imputed dataset 
age5_confoundersModel <- function(df) {
  fit <- lm(
    standardised_vocab5 ~ sex + ethnicity +  EAL + standardised_age5  , weight = weight2, data=df)
  return(fit)
}

age11_confoundersModel <- function(df) {
  fit <- lm(
    standardised_vocab11 ~ sex + ethnicity +  EAL + standardised_age11, weight = weight2, data=df)
  return(fit)
}

age14_confoundersModel <- function(df) {
  fit <- lm(
    standardised_vocab14 ~ sex + ethnicity +  EAL + standardised_age14, weight = weight2, data=df)
  return(fit)
}



age5confoundersResults <- lapply(list(imputed_mcs2_1, imputed_mcs2_2, imputed_mcs2_3, imputed_mcs2_4, imputed_mcs2_5, 
                                      imputed_mcs2_6, imputed_mcs2_7, imputed_mcs2_8, imputed_mcs2_9, imputed_mcs2_10, 
                                      imputed_mcs2_11, imputed_mcs2_12, imputed_mcs2_13, imputed_mcs2_14, imputed_mcs2_15,
                                      imputed_mcs2_16, imputed_mcs2_17, imputed_mcs2_18, imputed_mcs2_19, imputed_mcs2_20, 
                                      imputed_mcs2_21, imputed_mcs2_22, imputed_mcs2_23, imputed_mcs2_24, imputed_mcs2_25),
                                 age5_confoundersModel)

age10confoundersResults <- lapply(list(imputed_mcs2_1, imputed_mcs2_2, imputed_mcs2_3, imputed_mcs2_4, imputed_mcs2_5, 
                                       imputed_mcs2_6, imputed_mcs2_7, imputed_mcs2_8, imputed_mcs2_9, imputed_mcs2_10, 
                                       imputed_mcs2_11, imputed_mcs2_12, imputed_mcs2_13, imputed_mcs2_14, imputed_mcs2_15,
                                       imputed_mcs2_16, imputed_mcs2_17, imputed_mcs2_18, imputed_mcs2_19, imputed_mcs2_20, 
                                       imputed_mcs2_21, imputed_mcs2_22, imputed_mcs2_23, imputed_mcs2_24, imputed_mcs2_25),
                                  age11_confoundersModel)

age16confoundersResults <- lapply(list(imputed_mcs2_1, imputed_mcs2_2, imputed_mcs2_3, imputed_mcs2_4, imputed_mcs2_5, 
                                       imputed_mcs2_6, imputed_mcs2_7, imputed_mcs2_8, imputed_mcs2_9, imputed_mcs2_10, 
                                       imputed_mcs2_11, imputed_mcs2_12, imputed_mcs2_13, imputed_mcs2_14, imputed_mcs2_15,
                                       imputed_mcs2_16, imputed_mcs2_17, imputed_mcs2_18, imputed_mcs2_19, imputed_mcs2_20, 
                                       imputed_mcs2_21, imputed_mcs2_22, imputed_mcs2_23, imputed_mcs2_24, imputed_mcs2_25),
                                  age14_confoundersModel)

#Pool Results 

age5_confoundersResults <- summary(pool(as.mira(age5confoundersResults)),conf.int = TRUE, conf.level = 0.95) 
age11_confoundersResults <- summary(pool(as.mira(age10confoundersResults)),conf.int = TRUE, conf.level = 0.95) 
age14_confoundersResults <- summary(pool(as.mira(age16confoundersResults)),conf.int = TRUE, conf.level = 0.95) 


#R2 for ridits
#Education 
early_education_r2 = as.data.frame(pool.r.squared(as.mira(age5educationResults)))*100
late_education_r2 = as.data.frame(pool.r.squared(as.mira(age10educationResults)))*100
adolescent_education_r2 = as.data.frame(pool.r.squared(as.mira(age16educationResults)))*100

#Occupation 
early_occupation_r2 = as.data.frame(pool.r.squared(as.mira(age5occupationResults)))*100
late_occupation_r2 = as.data.frame(pool.r.squared(as.mira(age10occupationResults)))*100
adolescent_occupation_r2 = as.data.frame(pool.r.squared(as.mira(age16occupationResults)))*100

#Income
early_income_r2 = as.data.frame(pool.r.squared(as.mira(age5incomeResults)))*100
late_income_r2 = as.data.frame(pool.r.squared(as.mira(age10incomeResults)))*100
adolescent_income_r2 = as.data.frame(pool.r.squared(as.mira(age16incomeResults)))*100

#Confounders
early_confounders = as.data.frame(pool.r.squared(as.mira(age5confoundersResults)))*100
late_confounders = as.data.frame(pool.r.squared(as.mira(age10confoundersResults)))*100
adolescent_confounders = as.data.frame(pool.r.squared(as.mira(age16confoundersResults)))*100

#partial r2s
#education
early_childhood_ed_partial = early_education_r2 - early_confounders
early_childhood_ed_partial

late_childhood_ed_partial = late_education_r2 - late_confounders
late_childhood_ed_partial

adolescent_ed_partial = adolescent_education_r2 - adolescent_confounders
adolescent_ed_partial

#occupation
early_childhood_oc_partial = early_occupation_r2 - early_confounders
early_childhood_oc_partial

late_childhood_oc_partial = late_occupation_r2 - late_confounders
late_childhood_oc_partial

adolescent_oc_partial = adolescent_occupation_r2 - adolescent_confounders
adolescent_oc_partial

#income
early_childhood_income_partial = early_income_r2 - early_confounders
early_childhood_income_partial

late_childhood_income_partial = late_income_r2 - late_confounders
late_childhood_income_partial

adolescent_income_partial = adolescent_income_r2 - adolescent_confounders
adolescent_income_partial
