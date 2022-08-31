#MCS cross cohort comparison data
#load in required packages####
library(haven)
require(swfscMisc)
require(sjmisc)
require(Hmisc)
require(psych)
library(tidyverse)
library(lubridate)
#load in data####
mcs3_child_assessment <- read_sav("mcs3_child_assessment_data.sav")
mcs2_child_assessment <- read_sav("mcs2_child_assessment_data.sav")
mcs1_hh <- read_sav("mcs1_hhgrid.sav")
mcs_family <- read_sav("mcs_longitudinal_family_file.sav")
mcs1_parent <- read_sav("mcs1_parent_interview.sav")
mcs2_parent <- read_sav("mcs2_parent_interview.sav")
mcs2_derived <- read_sav("mcs2_derived_variables.sav")
mcs3_derived <- read_sav("mcs3_derived_variables.sav")
mcs3_parent <- read_sav("mcs3_parent_interview.sav")
mcs1_derived <- read_sav("mcs1_derived_variables.sav")
mcs5_child_assessment <- read_sav("mcs5_cm_assessment.sav")
mcs6_child_assessment<- read_sav("mcs6_cm_assessment.sav")
mcs6_cm_derived <- read_sav("mcs6_cm_derived.sav")
#convert all to lowercase####
names(mcs3_child_assessment) <- tolower(names(mcs3_child_assessment))
names(mcs2_child_assessment) <- tolower(names(mcs2_child_assessment))
names(mcs5_child_assessment) <- tolower(names(mcs5_child_assessment))
names(mcs6_child_assessment) <- tolower(names(mcs6_child_assessment))
names(mcs_family) <- tolower(names(mcs_family))
names(mcs1_parent) <- tolower(names(mcs1_parent))
names(mcs2_parent) <- tolower(names(mcs2_parent))
names(mcs3_parent) <- tolower(names(mcs3_parent))
names(mcs2_derived) <- tolower(names(mcs2_derived))
names(mcs1_derived) <- tolower(names(mcs1_derived))
names(mcs1_hh) <- tolower(names(mcs1_hh))
names(mcs3_derived) <- tolower(names(mcs3_derived))
names(mcs6_cm_derived) <- tolower(names(mcs6_cm_derived))
#variable to identify new families in sweep2####
sweep_entry <- c("mcsid", "sentry")
sweep_entry <- mcs_family[sweep_entry]
sweep_entry$sentry = as.character(sweep_entry$sentry)

#weight variable####
#attrition and sample weight age 5 sweep 
mcs3_weight <- c("mcsid", "covwt2")
mcs3_weight <- mcs_family[mcs3_weight]
mcs3_weight [mcs3_weight  ==-1] <- NA

#attrition and sample weight age 11 sweep 
mcs5_weight <- c("mcsid", "eovwt2")
mcs5_weight <- mcs_family[mcs5_weight]
mcs5_weight [mcs5_weight  ==-1] <- NA

weight <- merge(all=TRUE, mcs3_weight, mcs5_weight,by="mcsid")
weight$weight1 <- ifelse(!is.na(weight$covwt2), weight$covwt2, weight$eovwt2)
mcs_weight <- c("mcsid", "weight1")
mcs_weight <- weight[mcs_weight]


#attrition and sample  weight age 11 sweep 
mcs6_weight <- c("mcsid", "fovwt2")
mcs6_weight <- mcs_family[mcs6_weight]
mcs6_weight [mcs6_weight  ==-1] <- NA

weight2 <- merge(all=TRUE, mcs_weight, mcs6_weight,by="mcsid")
weight2$weight2 <- ifelse(!is.na(weight2$weight1), weight2$weight1, weight2$fovwt2)
mcs_weight2 <- c("mcsid", "weight2")
mcs_weight2 <- weight2[mcs_weight2]

#COHORT MEMBER NUMBERS####
mcsid_number_age9mo <- c("mcsid", "ahcnuma0")
mcsid_number_age9mo <- mcs1_parent[mcsid_number_age9mo]
mcsid_number_age9mo$ahcnuma0 = as.character(mcsid_number_age9mo$ahcnuma0)
mcsid_number_age3 <- c("mcsid", "bhcnuma0")
mcsid_number_age3 <- mcs2_parent[mcsid_number_age3]
mcsid_number_age3$bhcnuma0 = as.character (mcsid_number_age3$bhcnuma0)
mcsid_number_age5 <- c("mcsid", "chcnum00")
mcsid_number_age5 <- mcs3_child_assessment[mcsid_number_age5]
mcsid_number_age5$chcnum00 = as.character (mcsid_number_age5$chcnum00 )

#language ability variables/age at time of language test####
#language ability age 5. ability score and adjust for age. 
#AGE 5 naming vocabulary ability score for single births in MCS1
NVability1_new <- c("mcsid",  "cdnvabil")
NVability1_new <-mcs3_child_assessment[NVability1_new]
NVability1_new[ NVability1_new== -1:-9] <-NA
naming_vocab_age5<- NVability1_new[which(mcsid_number_age5$chcnum00=="1"),]
new_naming_vocab5 <- merge (all=TRUE, naming_vocab_age5, sweep_entry, by="mcsid")
naming_vocab5_sweep1<- new_naming_vocab5[which(new_naming_vocab5$sentry == "1"),]
#BAS standardised score for single births who were new families in sweep 2
NVability2_new <- c("mcsid",  "cdnvabil")
NVability2_new <-mcs3_child_assessment[NVability2_new]
NVability2_new[ NVability2_new== -1:-9] <-NA
naming_vocab_age5_2<- NVability2_new[which(mcsid_number_age5$chcnum00=="1"),]
new_NVability2 <- merge (all=TRUE, naming_vocab_age5_2, sweep_entry, by="mcsid")
NVability_second2<- new_NVability2[which(new_NVability2$sentry == "2"),]
#combine single births MCS1 and single births MCS2 new entry
new_NVability2 <- merge(all=TRUE, naming_vocab5_sweep1, NVability_second2, by="mcsid")
#merge together so that only one value for standardised score
new_NVabilitycombine <- ifelse(!is.na(new_NVability2$cdnvabil.x), new_NVability2$cdnvabil.x, new_NVability2$cdnvabil.y)
#create dataframe so also have mcsid 
NVability_score <- data.frame(new_NVabilitycombine, new_NVability2)
#subset data so just have 1 standard score and mcsid for the variable
final_NVability <- c("mcsid", "new_NVabilitycombine")
age5_language<- NVability_score[final_NVability]

#age when CM took language test in age 5 sweep

#age at test time (in days, converted to years)
age5_days <- c("mcsid", "chcage00")
age5_days <- mcs3_child_assessment[age5_days]
age5_days[age5_days ==-8] <- NA
age5_days[age5_days ==-1] <- NA
age5_days[age5_days ==-9] <- NA
age5_days[age5_days ==98] <- NA
age5_days[age5_days ==99] <- NA
age5_days_test<- age5_days[which(mcsid_number_age5$chcnum00=="1"),]
age5_days_test$age_days = days(age5_days_test$chcage00)
age5_days_test$age5_years = as.period(age5_days_test$age_days,unit="days")/years(1)
new_age1 <- merge (all=TRUE, age5_days_test, sweep_entry, by="mcsid")
age_second1<- new_age1[which(new_age1$sentry == "1"),]
#second sweep new entry families 
age5_days2 <- c("mcsid", "chcage00")
age5_days2 <- mcs3_child_assessment[age5_days2]
age5_days2[age5_days2 ==-8] <- NA
age5_days2[age5_days2 ==-1] <- NA
age5_days2[age5_days2 ==-9] <- NA
age5_days2[age5_days2 ==98] <- NA
age5_days2[age5_days2 ==99] <- NA
age5_days_test2<- age5_days2[which(mcsid_number_age5$chcnum00=="1"),]
age5_days_test2$age_days2 = days(age5_days_test2$chcage00)
age5_days_test2$age5_years2 = as.period(age5_days_test2$age_days2,unit="days")/years(1)
new_age2 <- merge (all=TRUE, age5_days_test2, sweep_entry, by="mcsid")
age_second2<- new_age2[which(new_age2$sentry == "2"),]
#combine single births MCS1 and single births MCS2 new entry
new_age_2 <- merge(all=TRUE, age_second1, age_second2, by="mcsid")
#merge together so that only one value for standardised score
new_agecombine <- ifelse(!is.na(new_age_2$age5_years), new_age_2$age5_years, new_age_2$age5_years2)
#create dataframe so also have mcsid 
cm_age <- data.frame(new_agecombine, new_age_2)
#subset data so just have 1 standard score and mcsid for the variable
final_age <- c("mcsid", "new_agecombine")
final_cm_age5 <-cm_age[final_age]
final_cm_age5$new_agecombine = round(final_cm_age5$new_agecombine, 2)
names(final_cm_age5) <- c("mcsid", "cm_age5")


#AGE 11 LANGUAGE SCORE 
mcsid_number_age11 <- c("mcsid", "ecnum00")
mcsid_number_age11 <- mcs5_child_assessment[mcsid_number_age11]
mcsid_number_age11$ecnum00 = as.character(mcsid_number_age11$ecnum00)
verbal_similarities <- c("mcsid",  "evsabil")
verbal_similarities <-mcs5_child_assessment[verbal_similarities]
verbal_similarities[ verbal_similarities== -1:-2] <-NA
verbal_similarities1<- verbal_similarities[which(mcsid_number_age11$ecnum00=="1"),]
new_verbal_sims_sweep1 <- merge (all=TRUE, verbal_similarities1, sweep_entry, by="mcsid")
verbal_sims_sweep1<- new_verbal_sims_sweep1[which(new_verbal_sims_sweep1$sentry == "1"),]
#verbal similarities for  new families in sweep 2
verbal_similarities_2 <- c("mcsid",  "evsabil")
verbal_similarities_2 <-mcs5_child_assessment[verbal_similarities_2]
verbal_similarities_2[verbal_similarities_2== -1:-2] <-NA
verbal_similarities2<- verbal_similarities_2[which(mcsid_number_age11$ecnum00=="1"),]
new_verbal_sims <- merge (all=TRUE, verbal_similarities2, sweep_entry, by="mcsid")
verbal_sims_sweep2<- new_verbal_sims[which(new_verbal_sims$sentry == "2"),]
#combine single births MCS1 and single births MCS2 new entry
verbal_sims_age11 <- merge(all=TRUE, verbal_sims_sweep1, verbal_sims_sweep2, by="mcsid")
#merge together so that only one value for standardised score
verbal_similarities_age11 <- ifelse(!is.na(verbal_sims_age11$evsabil.x), verbal_sims_age11$evsabil.x, verbal_sims_age11$evsabil.y)
#create dataframe so also have mcsid 
vocab_age11 <- data.frame(verbal_sims_age11, verbal_similarities_age11)
#subset data so just have 1 standard score and mcsid for the variable
new_vocab_age11 <- c("mcsid", "verbal_similarities_age11")
age11_language <- vocab_age11[new_vocab_age11]
#age11_lang_number <- merge(all=TRUE, new_vocab_age11, mcsid_number_age11, by="mcsid")

#age at time of test age 11
cm_age11 <- c("mcsid", "age")
cm_age11<- mcs5_child_assessment[cm_age11]
cm_age11$age[cm_age11$age==-1]<- NA
cm_age11<- cm_age11[which(mcsid_number_age11$ecnum00=="1"),]
new_age11 <- merge (all=TRUE, cm_age11, sweep_entry, by="mcsid")
age11_new<- new_age11[which(new_age11$sentry == "1"),]
age11_new2 <- new_age11[which(new_age11$sentry =="2"),]
#combine single births MCS1 and single births MCS2 new entry
age11 <- merge(all=TRUE, age11_new, age11_new2, by="mcsid")
#merge together so that only one value for standardised score
age11$age11_combined <- ifelse(!is.na(age11$age.x), age11$age.x, age11$age.y)
#pull out id and age
cm_age11 <- age11 %>% select(mcsid, age11_combined)
names(cm_age11) <- c("mcsid", "cm_age11")

#AGE 14 language - word activity test; scores out of 20. 
mcsid_number_age14 <- c("mcsid", "fcnum00")
mcsid_number_age14 <- mcs6_child_assessment[mcsid_number_age14]
mcsid_number_age14$fcnum00=as.character(mcsid_number_age14$fcnum00)
word_activity <- c("mcsid",  "fcwrdsc")
word_activity<-mcs6_child_assessment[word_activity]
word_activity[ word_activity== -1:-3] <-NA
word_activity1 <- word_activity[mcsid_number_age14$fcnum00 == "1", ]
new_word_act_sweep1 <- merge (all=TRUE, word_activity1, sweep_entry, by="mcsid")
word_act_sweep1<- new_word_act_sweep1[which(new_word_act_sweep1$sentry == "1"),]
#verbal similarities for  new families in sweep 2
word_activity_2 <- c("mcsid",  "fcwrdsc")
word_activity_2 <-mcs6_child_assessment[word_activity_2]
word_activity_2[word_activity_2== -1:-3] <-NA
word_activity_2<- word_activity_2[mcsid_number_age14$fcnum00 == "1", ]
new_word_act <- merge (all=TRUE, word_activity_2, sweep_entry, by="mcsid")
word_act_sweep2<- new_word_act[which(new_word_act$sentry == "2"),]
#combine single births MCS1 and single births MCS2 new entry
word_activity_age14 <- merge(all=TRUE,word_act_sweep1, word_act_sweep2, by="mcsid")
#merge together so that only one value for standardised score
word_act_age14 <- ifelse(!is.na(word_activity_age14$fcwrdsc.x), word_activity_age14$fcwrdsc.x, word_activity_age14$fcwrdsc.y)
#create dataframe so also have mcsid 
vocab_age14 <- data.frame(word_activity_age14, word_act_age14)
#subset data so just have 1 standard score and mcsid for the variable
new_vocab_age14 <- c("mcsid", "word_act_age14")
age14_language <- vocab_age14[new_vocab_age14]


#age at time of test age 14
cm_age14 <- c("mcsid", "fcmcs6ag")
cm_age14<- mcs6_cm_derived[cm_age14]
cm_age14$fcmcs6ag[cm_age14$fcmcs6ag==-1]<- NA
cm_age14 <- cm_age14[mcsid_number_age14$fcnum00 == "1", ]
new_age14 <- merge (all=TRUE, cm_age14, sweep_entry, by="mcsid")
age14_new<- new_age14[which(new_age14$sentry == "1"),]
age14_new2 <- new_age14[which(new_age14$sentry =="2"),]
#combine single births MCS1 and single births MCS2 new entry
age14 <- merge(all=TRUE, age14_new, age14_new2, by="mcsid")
#merge together so that only one value for standardised score
age14$age14_combined <- ifelse(!is.na(age14$fcmcs6ag.x), age14$fcmcs6ag.x, age14$fcmcs6ag.y)
#pull out id and age
cm_age14 <- age14 %>% select(mcsid, age14_combined)
names(cm_age14) <- c("mcsid", "cm_age14")


#potential confounding variables####
#language spoken at home####
language_home1<- c("mcsid", "bhhlan00")
language_home1 <- mcs2_parent[language_home1]
language_home1[language_home1==-1]<-NA
language_home<- language_home1[which(mcsid_number_age3$bhcnuma0=="1"),]
language_home_1 <- merge (all=TRUE, language_home , sweep_entry, by="mcsid")
new_language_home<- language_home_1[which(language_home_1$sentry == "1"),]
EAL_sweep1 <- c("mcsid", "ahlang00")
EAL_sweep1 <- mcs1_parent[EAL_sweep1]
EAL_sweep1[EAL_sweep1==-9:-1] <- NA
EAL_home<- EAL_sweep1[which(mcsid_number_age9mo$ahcnuma0=="1"),]
EAL_sentry1 <- merge(all=TRUE, new_language_home, EAL_home, by="mcsid")
EAL_sentry1$EAL <- ifelse(!is.na(EAL_sentry1$bhhlan00), EAL_sentry1$bhhlan00 ,EAL_sentry1$ahlang00)

language_home2<- c("mcsid", "bhhlan00")
language_home2 <- mcs2_parent[language_home2]
language_home2[language_home2==-1]<-NA
language_home22<- language_home2[which(mcsid_number_age3$bhcnuma0=="1"),]
new_lang2 <- merge (all=TRUE, language_home22 , sweep_entry, by="mcsid")
lang2<- new_lang2[which(new_lang2$sentry == "2"),]
new_language <- merge(all=TRUE, EAL_sentry1, lang2,by="mcsid")
#language_combine <- c("mcsid", "bhhlan00.x", "bhhlan00.y" )
#language_combine <- new_language[language_combine]
#merge together so that only one value 
new_language$new_langcombine<- ifelse(!is.na(new_language$EAL), new_language$EAL ,new_language$bhhlan00.y)
#create dataframe so also have mcsid 
#lang_home <- data.frame(language_combine, new_langcombine)
#subset data so just have 1 score and mcsid for the variable. may need recoding as all 0s seem to be 1 and all 1s coming up as 2?
language_used <- c("mcsid", "new_langcombine" )
language_used <- new_language[language_used]
#home_lang_number <- merge(all=TRUE, language_used, mcsid_number_age3, by="mcsid")

language_used[language_used == 1] <- 0
language_used[language_used == 2] <- 1
language_used[language_used == 3] <- 1
#home_language <- c("mcsid", "new_langcombine")
#home_language <- language_used_home[home_language]

#ethnicity####
#ethnicity single births mcs1
ethnicity1 <- c("mcsid", "adc06ea0")
ethnicity1 <- mcs1_derived[ethnicity1]
ethnicity1[ethnicity1==-1:-9] <- NA
ethnicity<- ethnicity1[which(mcsid_number_age9mo$ahcnuma0=="1"),]
#new_ethnicity1 <- merge (all=TRUE, ethnicity , sweep_entry, by="mcsid")
#ethnic1<- new_ethnicity1[which(new_ethnicity1$sentry == "1"),]

#ethnicity sweep 2 new families
ethnicity2 <- c("mcsid", "bdc06ea0")
ethnicity2 <- mcs2_derived[ethnicity2]
ethnicity2[ethnicity2==-1:-9] <- NA
ethnicity_2<- ethnicity2[which(mcsid_number_age3$bhcnuma0=="1"),]
new_ethnicity2 <- merge (all=TRUE, ethnicity_2 , sweep_entry, by="mcsid")
ethnic2<- new_ethnicity2[which(new_ethnicity2$sentry == "2"),]
#combine
new_ethnicity <- merge(all=TRUE, ethnicity, ethnic2,by="mcsid")
#merge together so that only one value for standardised score
ethnic_combine<- ifelse(!is.na(new_ethnicity$adc06ea0), new_ethnicity$adc06ea0,new_ethnicity$bdc06ea0)
#create dataframe so also have mcsid 
new_ethnic<- data.frame(ethnic_combine, new_ethnicity)
#subset data so just have 1 score and mcsid for the variable
cm_ethnicity <- c("mcsid", "ethnic_combine" )
cm_ethnicity <- new_ethnic[cm_ethnicity]
#ethnicity_number <- merge(all=TRUE, cm_ethnicity, mcsid_number_age9mo, by="mcsid")

cm_ethnicity[cm_ethnicity == 1] <- 0
cm_ethnicity[cm_ethnicity == 2] <- 1
cm_ethnicity[cm_ethnicity == 3] <- 1
cm_ethnicity[cm_ethnicity == 4] <- 1
cm_ethnicity[cm_ethnicity == 5] <- 1
cm_ethnicity[cm_ethnicity == 6] <- 1
cm_ethnicity[cm_ethnicity == 7] <- 1

#ethnicity_cm <- c("mcsid", "ethnic_combine")
#ethnicity_cm <- cm_ethnicity1[ethnicity_cm]

#gender####
sex1 <- c("mcsid", "ahcsexa0")
sex1 <- mcs1_parent[sex1]
sex1[sex1==-1] <- NA
sex_1<- sex1[which(mcsid_number_age9mo$ahcnuma0=="1"),]
new_sex1 <- merge (all=TRUE, sex_1 , sweep_entry, by="mcsid")
sex_1<- new_sex1[which(new_sex1$sentry == "1"),]

sex2 <- c("mcsid", "bhcsexa0")
sex2 <- mcs2_parent[sex2]
sex2[sex2==-1] <- NA
sex_2<- sex2[which(mcsid_number_age9mo$ahcnuma0=="1"),]
new_sex2 <- merge (all=TRUE, sex_2 , sweep_entry, by="mcsid")
sex_2<- new_sex2[which(new_sex2$sentry == "2"),]
#combine
new_sex <- merge(all=TRUE, sex_1, sex_2,by="mcsid")
#merge together so that only one value for standardised score
sex_combine<- ifelse(!is.na(new_sex$ahcsexa0), new_sex$ahcsexa0,new_sex$bhcsexa0)
#create dataframe so also have mcsid 
new_sex1<- data.frame(sex_combine, new_sex)
#subset data so just have 1 score and mcsid for the variable
cm_sex<- c("mcsid", "sex_combine" )
cm_sex <- new_sex1[cm_sex]
#sex_number <- merge(all=TRUE, cm_sex, mcsid_number_age9mo, by="mcsid")

#ses variables####
#PARENT EDUCATION - ACADEMIC QUALIFICATIONS ####
#parent education at age 5. highest household level. 

#MAIN RESPONDENT new quals age 3
MAINany_new<- c("mcsid", "bmedus00")
MAINany_new <- mcs2_parent[MAINany_new]
MAINany_new$bmedus00 = as.character(MAINany_new$bmedus00)

MAINnew_qualifications <- c("mcsid", "bmnacq0a", "bmnacq0b", "bmnacq0c", "bmnacq0d", "bmnacq0e", "bmnacq0f", "bmnacq0g", "bmnacq0h")
MAINnew_qualifications<-mcs2_parent[MAINnew_qualifications]
MAINnew_qualifications[MAINnew_qualifications==-1] <- NA


MAINnew_quals <- merge(all=TRUE, MAINany_new, MAINnew_qualifications)
#only those who have additional qualifications
MAINadditional_quals<- MAINnew_quals[which(MAINnew_quals$bmedus00 == "1"),]
#recode multipart questions to match coding for sweep 1 
MAINadditional_quals$bmnacq0a[MAINadditional_quals$bmnacq0a==1] <- 1
MAINadditional_quals$bmnacq0a[MAINadditional_quals$bmnacq0a==0] <- NA
MAINadditional_quals$bmnacq0b[MAINadditional_quals$bmnacq0b==1] <- 2
MAINadditional_quals$bmnacq0b[MAINadditional_quals$bmnacq0b==0] <- NA
MAINadditional_quals$bmnacq0c[MAINadditional_quals$bmnacq0c==1] <- 3
MAINadditional_quals$bmnacq0c[MAINadditional_quals$bmnacq0c==0] <- NA
MAINadditional_quals$bmnacq0d[MAINadditional_quals$bmnacq0d==1] <- 4
MAINadditional_quals$bmnacq0d[MAINadditional_quals$bmnacq0d==0] <- NA
MAINadditional_quals$bmnacq0e[MAINadditional_quals$bmnacq0e==1] <- 5
MAINadditional_quals$bmnacq0e[MAINadditional_quals$bmnacq0e==0] <- NA
MAINadditional_quals$bmnacq0f[MAINadditional_quals$bmnacq0f==1] <- 6
MAINadditional_quals$bmnacq0f[MAINadditional_quals$bmnacq0f==0] <- NA
MAINadditional_quals$bmnacq0g[MAINadditional_quals$bmnacq0g==1] <- 7
MAINadditional_quals$bmnacq0g[MAINadditional_quals$bmnacq0g==0] <- NA
MAINadditional_quals$bmnacq0h[MAINadditional_quals$bmnacq0h==1] <- 8
MAINadditional_quals$bmnacq0h[MAINadditional_quals$bmnacq0h==0] <- NA
#combine together to one variable 
MAINadditional_quals <- transform(MAINadditional_quals, MAINnew_academic_quals = pmax(bmnacq0a,bmnacq0b, bmnacq0c, bmnacq0d, bmnacq0e, bmnacq0f, bmnacq0g, bmnacq0h,  na.rm = TRUE))
#extract only new variable with additional qualifications
MAINnew_academic <- c("mcsid", "MAINnew_academic_quals")
MAINnew_academic<- MAINadditional_quals[MAINnew_academic]

#combine with sweep 1 variable to get updated overall measure of highest academic qualification at sweep2
#sweep 1 variable 
MAINhighest_qual <- c("mcsid", "amacqu00")
MAINhighest_qual <- mcs1_parent[MAINhighest_qual]
MAINhighest_qual$amacqu00[ MAINhighest_qual$amacqu00== 96] <-8
MAINhighest_qual$amacqu00[ MAINhighest_qual$amacqu00== 95] <-7
MAINhighest_qual[ MAINhighest_qual== -9:-1] <-NA

#combine with any new qualifications
MAINadditional <- merge(all=TRUE, MAINhighest_qual, MAINany_new,by="mcsid")
#MAINadditional$amacqu00[MAINadditional$bmedus00==1] <- NA #if the response to any new quals at age 3 is yes, make the qualifications in age 9 months NA, so can fill this in with updated quals.
#combine with created multipart response
MAINdefined_quals <- merge(all=TRUE, MAINadditional, MAINnew_academic, by="mcsid")
MAINdefined_quals$amacqu00=as.character(MAINdefined_quals$amacqu00)
MAINdefined_quals$MAINnew_academic_quals=as.character(MAINdefined_quals$MAINnew_academic_quals)
MAINdefined_quals <- transform(MAINdefined_quals, MAINupdated_quals = pmin(amacqu00, MAINnew_academic_quals,  na.rm = TRUE))
#create dataframe so have mcsid
#MAIN_quals <- data.frame(MAINdefined_quals, MAINupdated_quals)
#subset data so just have 1 response and mcsid for the variable
MAIN_academic_quals <- c("mcsid", "MAINupdated_quals")
MAIN_academic_quals <- MAINdefined_quals[MAIN_academic_quals]


#PARTNER RESPONDENT
#PARTNER RESPONDENT 
PARTNERany_new<- c("mcsid", "bpedus00")
PARTNERany_new <- mcs2_parent[PARTNERany_new]
PARTNERany_new$bpedus00 = as.character(PARTNERany_new$bpedus00)

PARTNERnew_qualifications <- c("mcsid", "bpnacq0a", "bpnacq0b", "bpnacq0c", "bpnacq0d", "bpnacq0e", "bpnacq0f", "bpnacq0g", "bpnacq0h")
PARTNERnew_qualifications<-mcs2_parent[PARTNERnew_qualifications]
PARTNERnew_qualifications[PARTNERnew_qualifications==-1] <- NA

PARTNERnew_quals <- merge(all=TRUE, PARTNERany_new, PARTNERnew_qualifications)
#only those who have additional qualifications
PARTNERadditional_quals<- PARTNERnew_quals[which(PARTNERnew_quals$bpedus00 == "1"),]
#recode multipart questions to match coding for sweep 1 
PARTNERadditional_quals$bpnacq0a[PARTNERadditional_quals$bpnacq0a==1] <- 1
PARTNERadditional_quals$bpnacq0a[PARTNERadditional_quals$bpnacq0a==0] <- NA
PARTNERadditional_quals$bpnacq0b[PARTNERadditional_quals$bpnacq0b==1] <- 2
PARTNERadditional_quals$bpnacq0b[PARTNERadditional_quals$bpnacq0b==0] <- NA
PARTNERadditional_quals$bpnacq0c[PARTNERadditional_quals$bpnacq0c==1] <- 3
PARTNERadditional_quals$bpnacq0c[PARTNERadditional_quals$bpnacq0c==0] <- NA
PARTNERadditional_quals$bpnacq0d[PARTNERadditional_quals$bpnacq0d==1] <- 4
PARTNERadditional_quals$bpnacq0d[PARTNERadditional_quals$bpnacq0d==0] <- NA
PARTNERadditional_quals$bpnacq0e[PARTNERadditional_quals$bpnacq0e==1] <- 5
PARTNERadditional_quals$bpnacq0e[PARTNERadditional_quals$bpnacq0e==0] <- NA
PARTNERadditional_quals$bpnacq0f[PARTNERadditional_quals$bpnacq0f==1] <- 6
PARTNERadditional_quals$bpnacq0f[PARTNERadditional_quals$bpnacq0f==0] <- NA
PARTNERadditional_quals$bpnacq0g[PARTNERadditional_quals$bpnacq0g==1] <- 7
PARTNERadditional_quals$bpnacq0g[PARTNERadditional_quals$bpnacq0g==0] <- NA
PARTNERadditional_quals$bpnacq0h[PARTNERadditional_quals$bpnacq0h==1] <- 8
PARTNERadditional_quals$bpnacq0h[PARTNERadditional_quals$bpnacq0h==0] <- NA
#combine together to one variable 
PARTNERadditional_quals <- transform(PARTNERadditional_quals, PARTNERnew_academic_quals = pmax(bpnacq0a,bpnacq0b, bpnacq0c, bpnacq0d, bpnacq0e, bpnacq0f, bpnacq0g, bpnacq0h,  na.rm = TRUE))
#extract only new variable with additional qualifications
PARTNERnew_academic <- c("mcsid", "PARTNERnew_academic_quals")
PARTNERnew_academic<- PARTNERadditional_quals[PARTNERnew_academic]

#combine with sweep 1 variable to get updated overall measure of highest academic qualification at sweep2
#sweep 1 variable 
PARTNERhighest_qual <- c("mcsid", "apacqu00")
PARTNERhighest_qual <- mcs1_parent[PARTNERhighest_qual]
PARTNERhighest_qual$apacqu00[ PARTNERhighest_qual$apacqu00== 96] <-8
PARTNERhighest_qual$apacqu00[ PARTNERhighest_qual$apacqu00== 95] <-7
PARTNERhighest_qual[ PARTNERhighest_qual== -9:-1] <-NA

#combine with any new qualifications
PARTNERadditional <- merge(all=TRUE, PARTNERhighest_qual, PARTNERany_new,by="mcsid")
#PARTNERadditional$apacqu00[PARTNERadditional$bpedus00==1] <- NA
#combine with created multipart response
PARTNERdefined_quals <- merge(all=TRUE, PARTNERadditional, PARTNERnew_academic, by="mcsid")
PARTNERdefined_quals$apacqu00 = as.character(PARTNERdefined_quals$apacqu00)
PARTNERdefined_quals$PARTNERnew_academic_quals = as.character(PARTNERdefined_quals$PARTNERnew_academic_quals)
PARTNERdefined_quals <- transform(PARTNERdefined_quals, PARTNERupdated_quals = pmin(apacqu00, PARTNERnew_academic_quals,  na.rm = TRUE))
#PARTNERupdated_quals <- ifelse(!is.na(PARTNERdefined_quals$apacqu00), PARTNERdefined_quals$apacqu00, PARTNERdefined_quals$PARTNERnew_academic_quals)
#create dataframe so have mcsid
#PARTNER_quals <- data.frame(PARTNERdefined_quals, PARTNERupdated_quals)
#subset data so just have 1 response and mcsid for the variable
PARTNER_academic_quals <- c("mcsid", "PARTNERupdated_quals")
PARTNER_academic_quals <- PARTNERdefined_quals[PARTNER_academic_quals]

#now merge with updated quals at age 5 for those who joined in sweep 1. 

#families that entered in sweep 1
MAINany_new_age5<- c("mcsid", "cmedus00")
MAINany_new_age5 <- mcs3_parent[MAINany_new_age5]
MAINany_new_age5$cmedus00 = as.character(MAINany_new_age5$cmedus00)
MAINany_new_age5 <- merge (all=TRUE, MAINany_new_age5 , sweep_entry, by="mcsid")
MAINany_new_age5<- MAINany_new_age5[which(MAINany_new_age5$sentry == "1"),]

MAINnew_qualifications_age5 <- c("mcsid", "cmacqu0a", "cmacqu0b", "cmacqu0c", "cmacqu0d", "cmacqu0e", "cmacqu0f", "cmacqu0g")
MAINnew_qualifications_age5<-mcs3_parent[MAINnew_qualifications_age5]
MMAINnew_qualifications_age5 <- merge (all=TRUE, MAINnew_qualifications_age5 , sweep_entry, by="mcsid")
MAINnew_qualifications_age5<- MMAINnew_qualifications_age5[which(MMAINnew_qualifications_age5$sentry == "1"),]

MAINnew_qualifications_age5[MAINnew_qualifications_age5==-1] <- NA
MAINnew_qualifications_age5[MAINnew_qualifications_age5==-8] <- NA
MAINnew_qualifications_age5[MAINnew_qualifications_age5==-9] <- NA
MAINnew_qualifications_age5[MAINnew_qualifications_age5==98] <- NA
MAINnew_qualifications_age5[MAINnew_qualifications_age5==99] <- NA
MAINnew_qualifications_age5[MAINnew_qualifications_age5==96] <- 8
MAINnew_qualifications_age5[MAINnew_qualifications_age5==3] <- 1
MAINnew_qualifications_age5[MAINnew_qualifications_age5==4] <- 3
MAINnew_qualifications_age5[MAINnew_qualifications_age5==5] <- 3
MAINnew_qualifications_age5[MAINnew_qualifications_age5==6] <- 4
MAINnew_qualifications_age5[MAINnew_qualifications_age5==7] <-5
MAINnew_qualifications_age5[MAINnew_qualifications_age5==8] <- 6
MAINnew_qualifications_age5[MAINnew_qualifications_age5==95] <- 7

MAINnew_quals_age5 <- merge(all=TRUE, MAINany_new_age5, MAINnew_qualifications_age5)
#only those who have additional qualifications
MAINadditional_quals_age5<- MAINnew_quals_age5[which(MAINnew_quals_age5$cmedus00 == "1"),]

#combine together to one variable 
MAINadditional_quals_age5 <- transform(MAINadditional_quals_age5, MAINnew_academic_quals_age5 = pmin(cmacqu0a, cmacqu0b, cmacqu0c, cmacqu0d, cmacqu0e, cmacqu0f, cmacqu0g,  na.rm = TRUE))
#extract only new variable with additional qualifications
MAINnew_academic_age5 <- c("mcsid", "MAINnew_academic_quals_age5")
MAINnew_academic_age5<- MAINadditional_quals_age5[MAINnew_academic_age5]

#combine to MAIN_academic_quals (sweeps 1 & 2 combined) to get updated main qualifications by age 5. for first entry families.
MAINupdated_age5 <- merge(all=TRUE, MAINnew_academic_age5, MAIN_academic_quals, by="mcsid")
MAINupdated_age5$MAINnew_academic_quals_age5 = as.character(MAINupdated_age5$MAINnew_academic_quals_age5)
MAINupdated_age5$MAINupdated_quals = as.character(MAINupdated_age5$MAINupdated_quals)
#if new quals at age 5, use the highest qualification out of the two variables. so the minimum response. 
MAIN_highest_qual_age5 <- transform(MAINupdated_age5, MAIN_highest_qual_age5 = pmin(MAINnew_academic_quals_age5, MAINupdated_quals, na.rm=TRUE))
#extract so just have mcsid and highest qual at age 5
MAINhighest_qualification_age5 <- c("mcsid", "MAIN_highest_qual_age5")
MAINhighest_qualification_age5 <- MAIN_highest_qual_age5[MAINhighest_qualification_age5]


#partner

#PARTNER RESPONDENT 
PARTNERany_new_age5<- c("mcsid", "cpedus00")
PARTNERany_new_age5 <- mcs3_parent[PARTNERany_new_age5]
PARTNERany_new_age5$cpedus00 = as.character(PARTNERany_new_age5$cpedus00)
PARTNERany_new_age5 <- merge (all=TRUE, PARTNERany_new_age5 , sweep_entry, by="mcsid")
PARTNERany_new_age5<- PARTNERany_new_age5[which(PARTNERany_new_age5$sentry == "1"),]

PARTNERnew_qualifications_age5 <- c("mcsid", "cpacqu0a", "cpacqu0b", "cpacqu0c", "cpacqu0d", "cpacqu0e", "cpacqu0f", "cpacqu0g")
PARTNERnew_qualifications_age5<-mcs3_parent[PARTNERnew_qualifications_age5]
MPARTNERnew_qualifications_age5 <- merge (all=TRUE, PARTNERnew_qualifications_age5 , sweep_entry, by="mcsid")
PARTNERnew_qualifications_age5<- MPARTNERnew_qualifications_age5[which(MPARTNERnew_qualifications_age5$sentry == "1"),]
PARTNERnew_qualifications_age5[PARTNERnew_qualifications_age5==-1] <- NA
PARTNERnew_qualifications_age5[PARTNERnew_qualifications_age5==-8] <- NA
PARTNERnew_qualifications_age5[PARTNERnew_qualifications_age5==-9] <- NA
PARTNERnew_qualifications_age5[PARTNERnew_qualifications_age5==98] <- NA
PARTNERnew_qualifications_age5[PARTNERnew_qualifications_age5==99] <- NA
PARTNERnew_qualifications_age5[PARTNERnew_qualifications_age5==96] <- 8
PARTNERnew_qualifications_age5[PARTNERnew_qualifications_age5==3] <- 1
PARTNERnew_qualifications_age5[PARTNERnew_qualifications_age5==4] <- 3
PARTNERnew_qualifications_age5[PARTNERnew_qualifications_age5==5] <- 3
PARTNERnew_qualifications_age5[PARTNERnew_qualifications_age5==6] <- 4
PARTNERnew_qualifications_age5[PARTNERnew_qualifications_age5==7] <-5
PARTNERnew_qualifications_age5[PARTNERnew_qualifications_age5==8] <- 6
PARTNERnew_qualifications_age5[PARTNERnew_qualifications_age5==95] <- 7

PARTNERnew_quals_age5 <- merge(all=TRUE, PARTNERany_new_age5, PARTNERnew_qualifications_age5)
#only those who have additional qualifications
PARTNERadditional_quals_age5<- PARTNERnew_quals_age5[which(PARTNERnew_quals_age5$cpedus00 == "1"),]

#combine together to one variable 
PARTNERadditional_quals_age5 <- transform(PARTNERadditional_quals_age5, PARTNERnew_academic_quals_age5 = pmin(cpacqu0a, cpacqu0b, cpacqu0c, cpacqu0d, cpacqu0e, cpacqu0f, cpacqu0g,  na.rm = TRUE))
#extract only new variable with additional qualifications
PARTNERnew_academic_age5 <- c("mcsid", "PARTNERnew_academic_quals_age5")
PARTNERnew_academic_age5<- PARTNERadditional_quals_age5[PARTNERnew_academic_age5]

#combine to PARTNER_academic_quals (sweeps 1 & 2 combined) to get updated partner qualifications by age 5. for first entry families.
PARTNERupdated_age5 <- merge(all=TRUE, PARTNERnew_academic_age5, PARTNER_academic_quals, by="mcsid")
PARTNERupdated_age5$PARTNERnew_academic_quals_age5 = as.character(PARTNERupdated_age5$PARTNERnew_academic_quals_age5)
PARTNERupdated_age5$PARTNERupdated_quals = as.character(PARTNERupdated_age5$PARTNERupdated_quals)
#if new quals at age 5, use the highest qualification out of the two variables. so the minimum response. 
PARTNER_highest_qual_age5 <- transform(PARTNERupdated_age5, PARTNER_highest_qual_age5 = pmin(PARTNERnew_academic_quals_age5, PARTNERupdated_quals, na.rm=TRUE))
#extract so just have mcsid and highest qual at age 5
PARTNERhighest_qualification_age5 <- c("mcsid", "PARTNER_highest_qual_age5")
PARTNERhighest_qualification_age5 <- PARTNER_highest_qual_age5[PARTNERhighest_qualification_age5]



#creating maternal and paternal highest education 

#RESPONDENT VARIABLE
#sweep 1 entry families at 9 months
MAINrespondent <- c("mcsid", "amdres00")
MAINrespondent<-mcs1_derived[MAINrespondent]
PARTNERrespondent <- c("mcsid", "apdres00")
PARTNERrespondent<-mcs1_derived[PARTNERrespondent]
#mother 
#mother as main respondent
mother_main <- MAINrespondent[MAINrespondent$amdres00 == 1 | MAINrespondent$amdres00== 3 | MAINrespondent$amdres00 == 5 | MAINrespondent$amdres00 == 7 |MAINrespondent$amdres00 == 9 |MAINrespondent$amdres00 == 11 | MAINrespondent$amdres00 == 13|MAINrespondent$amdres00 == 15 ,]
mother_partner <- PARTNERrespondent[PARTNERrespondent$apdres00 == 1 | PARTNERrespondent$apdres00== 3 | PARTNERrespondent$apdres00 == 5 | PARTNERrespondent$apdres00 == 7 |PARTNERrespondent$apdres00 == 11 | PARTNERrespondent$apdres00 == 13| PARTNERrespondent$apdres00 == 15| PARTNERrespondent$apdres00 == 21,]
mother_partner <- mother_partner[!is.na(mother_partner$mcsid),] 
mother_main_sweep1 <- merge (all=TRUE, mother_main, sweep_entry, by="mcsid")
mother_main_sweep1_1<- mother_main_sweep1[which(mother_main_sweep1$sentry == "1"),]
sweep1_main_mother <- c("mcsid", "amdres00")
sweep1_main_mother <-mother_main_sweep1_1[sweep1_main_mother]
sweep1_main_mother  <- sweep1_main_mother[!is.na(sweep1_main_mother$amdres00),] 
mother_partner_sweep1 <- merge (all=TRUE, mother_partner, sweep_entry, by="mcsid")
mother_partner_sweep1_1<- mother_partner_sweep1[which(mother_partner_sweep1$sentry == "1"),]
sweep1_partner_mother <- c("mcsid", "apdres00")
sweep1_partner_mother <-mother_partner_sweep1_1[sweep1_partner_mother]
sweep1_partner_mother  <- sweep1_partner_mother[!is.na(sweep1_partner_mother$apdres00),] 

father_main <- MAINrespondent[MAINrespondent$amdres00 == 2 | MAINrespondent$amdres00== 4 | MAINrespondent$amdres00 == 6 | MAINrespondent$amdres00 == 8 |MAINrespondent$amdres00 == 12 | MAINrespondent$amdres00 == 14 ,]
father_partner <- PARTNERrespondent[PARTNERrespondent$apdres00 == 2 | PARTNERrespondent$apdres00== 4 | PARTNERrespondent$apdres00 == 6 | PARTNERrespondent$apdres00 == 8 |PARTNERrespondent$apdres00 == 10 |PARTNERrespondent$apdres00 == 12 | PARTNERrespondent$apdres00 == 14 | PARTNERrespondent$apdres00 == 16 | PARTNERrespondent$apdres00 == 22 | PARTNERrespondent$apdres00 == 24 ,]
father_partner <- father_partner[!is.na(father_partner$mcsid),] 

father_main_sweep1 <- merge (all=TRUE, father_main, sweep_entry, by="mcsid")
father_main_sweep1_1<- father_main_sweep1[which(father_main_sweep1$sentry == "1"),]
sweep1_main_father <- c("mcsid", "amdres00")
sweep1_main_father <-father_main_sweep1_1[sweep1_main_father]
sweep1_main_father  <- sweep1_main_father[!is.na(sweep1_main_father$amdres00),] 
father_partner_sweep1 <- merge (all=TRUE, father_partner, sweep_entry, by="mcsid")
father_partner_sweep1_1<- father_partner_sweep1[which(father_partner_sweep1$sentry == "1"),]
sweep1_partner_father <- c("mcsid", "apdres00")
sweep1_partner_father <-father_partner_sweep1_1[sweep1_partner_father]
sweep1_partner_father  <- sweep1_partner_father[!is.na(sweep1_partner_father$apdres00),] 


MAINrespondent2 <- c("mcsid", "bmdres00")
MAINrespondent2<-mcs2_derived[MAINrespondent2]
PARTNERrespondent2 <- c("mcsid", "bpdres00")
PARTNERrespondent2<-mcs2_derived[PARTNERrespondent2]
#mother sweep 2
mother_main2 <- MAINrespondent2[MAINrespondent2$bmdres00 == 1 | MAINrespondent2$bmdres00== 3 | MAINrespondent2$bmdres00 == 5 | MAINrespondent2$bmdres00 == 7 |MAINrespondent2$bmdres00 == 9 |MAINrespondent2$bmdres00 == 11 | MAINrespondent2$bmdres00 == 13| MAINrespondent2$bmdres00 == 15 ,]
mother_partner2 <- PARTNERrespondent2[PARTNERrespondent2$bpdres00 == 1 | PARTNERrespondent2$bpdres00== 3 | PARTNERrespondent2$bpdres00 == 5 | PARTNERrespondent2$bpdres00 == 7 |PARTNERrespondent2$bpdres00 == 11 | PARTNERrespondent2$bpdres00 == 13| PARTNERrespondent2$bpdres00 == 15 | PARTNERrespondent2$bpdres00 == 21,]
mother_partner2 <- mother_partner2[!is.na(mother_partner2$mcsid),] 
mum_main_sweep2_1st <- merge (all=TRUE, mother_main2, sweep_entry, by="mcsid")
mum_main_sweep2_1<- mum_main_sweep2_1st[which(mum_main_sweep2_1st$sentry == "1"),]
sweep2_main_mum1 <- c("mcsid", "bmdres00")
sweep2_main_mum1 <- mum_main_sweep2_1[sweep2_main_mum1]
sweep2_main_mum1 <- sweep2_main_mum1[!is.na(sweep2_main_mum1$bmdres00),] 
mum_partner_sweep2_1st <- merge (all=TRUE, mother_partner2, sweep_entry, by="mcsid")
mum_partner_sweep2_1<- mum_partner_sweep2_1st[which(mum_partner_sweep2_1st$sentry == "1"),]
sweep2_partner_mum1 <- c("mcsid", "bpdres00")
sweep2_partner_mum1 <- mum_partner_sweep2_1[sweep2_partner_mum1]
sweep2_partner_mum1 <- sweep2_partner_mum1[!is.na(sweep2_partner_mum1$bpdres00),] 


#father sweep 2
father_main2 <- MAINrespondent2[MAINrespondent2$bmdres00 == 2 | MAINrespondent2$bmdres00== 4 | MAINrespondent2$bmdres00 == 6 | MAINrespondent2$bmdres00 == 8 |MAINrespondent2$bmdres00 == 12 | MAINrespondent2$bmdres00 == 14 | MAINrespondent2$bmdres00 == 16 ,]
father_partner2 <- PARTNERrespondent2[PARTNERrespondent2$bpdres00 == 2 | PARTNERrespondent2$bpdres00== 4 | PARTNERrespondent2$bpdres00 == 6 | PARTNERrespondent2$bpdres00 == 8 |PARTNERrespondent2$bpdres00 == 10 |PARTNERrespondent2$bpdres00 == 12 | PARTNERrespondent2$bpdres00 == 14 | PARTNERrespondent2$bpdres00 == 16| PARTNERrespondent2$bpdres00 == 18| PARTNERrespondent2$bpdres00 == 22,]
father_partner2 <- father_partner2[!is.na(father_partner2$mcsid),]
dad_main_sweep2_1st <- merge (all=TRUE,father_main2, sweep_entry, by="mcsid")
dad_main_sweep2_1<- dad_main_sweep2_1st[which(dad_main_sweep2_1st$sentry == "1"),]
sweep2_main_dad1 <- c("mcsid", "bmdres00")
sweep2_main_dad1 <- dad_main_sweep2_1[sweep2_main_dad1]
sweep2_main_dad1 <- sweep2_main_dad1[!is.na(sweep2_main_dad1$bmdres00),] 
dad_partner_sweep2_1st <- merge (all=TRUE, father_partner2, sweep_entry, by="mcsid")
dad_partner_sweep2_1<- dad_partner_sweep2_1st[which(dad_partner_sweep2_1st$sentry == "1"),]
sweep2_partner_dad1 <- c("mcsid", "bpdres00")
sweep2_partner_dad1 <- dad_partner_sweep2_1[sweep2_partner_dad1]
sweep2_partner_dad1 <- sweep2_partner_dad1[!is.na(sweep2_partner_dad1$bpdres00),] 

#second sweep entry families respondent - mother
mum_main_sweep2 <- merge (all=TRUE, mother_main2, sweep_entry, by="mcsid")
mum_main_sweep2<- mum_main_sweep2[which(mum_main_sweep2$sentry == "2"),]
sweep2_main_mum<- c("mcsid", "bmdres00")
sweep2_main_mum <- mum_main_sweep2[sweep2_main_mum]
sweep2_main_mum <- sweep2_main_mum[!is.na(sweep2_main_mum$bmdres00),] 
mum_partner_sweep2 <- merge (all=TRUE, mother_partner2, sweep_entry, by="mcsid")
mum_partner_sweep2<- mum_partner_sweep2[which(mum_partner_sweep2$sentry == "2"),]
sweep2_partner_mum <- c("mcsid", "bpdres00")
sweep2_partner_mum <- mum_partner_sweep2[sweep2_partner_mum]
sweep2_partner_mum<- sweep2_partner_mum[!is.na(sweep2_partner_mum$bpdres00),] 
#second sweep entry families respondent - father
dad_main_sweep2 <- merge (all=TRUE,father_main2, sweep_entry, by="mcsid")
dad_main_sweep2<- dad_main_sweep2[which(dad_main_sweep2$sentry == "2"),]
sweep2_main_dad <- c("mcsid", "bmdres00")
sweep2_main_dad <- dad_main_sweep2[sweep2_main_dad]
sweep2_main_dad <- sweep2_main_dad[!is.na(sweep2_main_dad$bmdres00),] 
dad_partner_sweep2 <- merge (all=TRUE, father_partner2, sweep_entry, by="mcsid")
dad_partner_sweep2<- dad_partner_sweep2[which(dad_partner_sweep2$sentry == "2"),]
sweep2_partner_dad <- c("mcsid", "bpdres00")
sweep2_partner_dad <- dad_partner_sweep2[sweep2_partner_dad]
sweep2_partner_dad <- sweep2_partner_dad[!is.na(sweep2_partner_dad$bpdres00),] 



#creating mother and father highest education out of highest qual at age 5
#mother
#mother
MAIN_mother_ed_sweep1family<- MAINhighest_qualification_age5[MAINhighest_qualification_age5$mcsid %in% sweep1_main_mother$mcsid,]
PARTNER_mother_ed_sweep1family<- PARTNERhighest_qualification_age5[PARTNERhighest_qualification_age5$mcsid %in% sweep1_partner_mother$mcsid,]
maternal_ed_sweep1family <- merge(all=TRUE,MAIN_mother_ed_sweep1family, PARTNER_mother_ed_sweep1family, by="mcsid")
mum_ed_sweep1family <- ifelse(!is.na(maternal_ed_sweep1family$MAIN_highest_qual_age5), maternal_ed_sweep1family$MAIN_highest_qual_age5, maternal_ed_sweep1family$PARTNER_highest_qual_age5)
mum_ed1_sweep1family <- data.frame(maternal_ed_sweep1family, mum_ed_sweep1family)
MATERNAL_EDUCATIONsweep1<- c("mcsid", "mum_ed_sweep1family")
MATERNAL_EDUCATIONsweep1 <- mum_ed1_sweep1family[MATERNAL_EDUCATIONsweep1]



#father
MAIN_father_ed_sweep1family<- MAINhighest_qualification_age5[MAINhighest_qualification_age5$mcsid %in% sweep1_main_father$mcsid,]
PARTNER_father_ed_sweep1family<- PARTNERhighest_qualification_age5[PARTNERhighest_qualification_age5$mcsid %in% sweep1_partner_father$mcsid,]
paternal_ed_sweep1family <- merge(all=TRUE,MAIN_father_ed_sweep1family, PARTNER_father_ed_sweep1family, by="mcsid")
dad_ed_sweep1family <- ifelse(!is.na(paternal_ed_sweep1family$MAIN_highest_qual_age5), paternal_ed_sweep1family$MAIN_highest_qual_age5, paternal_ed_sweep1family$PARTNER_highest_qual_age5)
dad_ed1_sweep1family <- data.frame(paternal_ed_sweep1family, dad_ed_sweep1family)
PATERNAL_EDUCATIONsweep1<- c("mcsid", "dad_ed_sweep1family")
PATERNAL_EDUCATIONsweep1 <- dad_ed1_sweep1family[PATERNAL_EDUCATIONsweep1]





#academic qualifications for parents who joined MCS in sweep 2. 'new families'. combine this with sentry=2 updated quals age 5 before split into mother and father 
#create mother and father variables out of respondent


#EDUCATION sweep2 
main_education21 <- c("mcsid", "bmacqu00")
main_education21 <- mcs2_parent[main_education21]
main_education21[main_education21==-1] <- NA
main_education2_1 <- merge(all=TRUE, main_education21, sweep_entry, by="mcsid")
main_education2 <- main_education2_1[which(main_education2_1$sentry == "2"),]

partner_education21 <- c("mcsid", "bpacqu00")
partner_education21<- mcs2_parent[partner_education21]
partner_education21[partner_education21==-1] <- NA
partner_education2_1 <- merge(all=TRUE, partner_education21, sweep_entry, by="mcsid")
partner_education2 <- partner_education2_1[which(partner_education2_1$sentry == "2"),]



#adding in sweep 2 families updated quals by age 5

#second entry families main respondent
MAINany_new_age52<- c("mcsid", "cmedus00")
MAINany_new_age52 <- mcs3_parent[MAINany_new_age52]
MAINany_new_age52$cmedus00 = as.character(MAINany_new_age52$cmedus00)
MAINany_new_age52 <- merge (all=TRUE, MAINany_new_age52 , sweep_entry, by="mcsid")
MAINany_new_age52<- MAINany_new_age52[which(MAINany_new_age52$sentry == "2"),]

MAINnew_qualifications_age52 <- c("mcsid", "cmacqu0a", "cmacqu0b", "cmacqu0c", "cmacqu0d", "cmacqu0e", "cmacqu0f", "cmacqu0g")
MAINnew_qualifications_age52<-mcs3_parent[MAINnew_qualifications_age52]
MMAINnew_qualifications_age52 <- merge (all=TRUE, MAINnew_qualifications_age52 , sweep_entry, by="mcsid")
MAINnew_qualifications_age52<- MMAINnew_qualifications_age52[which(MMAINnew_qualifications_age52$sentry == "2"),]

MAINnew_qualifications_age52[MAINnew_qualifications_age52==-1] <- NA
MAINnew_qualifications_age52[MAINnew_qualifications_age52==-8] <- NA
MAINnew_qualifications_age52[MAINnew_qualifications_age52==-9] <- NA
MAINnew_qualifications_age52[MAINnew_qualifications_age52==98] <- NA
MAINnew_qualifications_age52[MAINnew_qualifications_age52==99] <- NA
MAINnew_qualifications_age52[MAINnew_qualifications_age52==96] <- 8
MAINnew_qualifications_age52[MAINnew_qualifications_age52==3] <- 1
MAINnew_qualifications_age52[MAINnew_qualifications_age52==4] <- 3
MAINnew_qualifications_age52[MAINnew_qualifications_age52==5] <- 3
MAINnew_qualifications_age52[MAINnew_qualifications_age52==6] <- 4
MAINnew_qualifications_age52[MAINnew_qualifications_age52==7] <-5
MAINnew_qualifications_age52[MAINnew_qualifications_age52==8] <- 6
MAINnew_qualifications_age52[MAINnew_qualifications_age52==95] <- 7

MAINnew_quals_age52 <- merge(all=TRUE, MAINany_new_age52, MAINnew_qualifications_age52)
#only those who have additional qualifications
MAINadditional_quals_age52<- MAINnew_quals_age52[which(MAINnew_quals_age52$cmedus00 == "1"),]

#combine together to one variable 
MAINadditional_quals_age52 <- transform(MAINadditional_quals_age52, MAINnew_academic_quals_age52 = pmin(cmacqu0a, cmacqu0b, cmacqu0c, cmacqu0d, cmacqu0e, cmacqu0f, cmacqu0g,  na.rm = TRUE))
#extract only new variable with additional qualifications
MAINnew_academic_age52 <- c("mcsid", "MAINnew_academic_quals_age52")
MAINnew_academic_age52<- MAINadditional_quals_age52[MAINnew_academic_age52] #merge this with the second entry families education variable (main_education2)

#merge with highest qualification in sweep 2
#combine to MAIN_academic_quals (sweeps 2 & 3 combined) to get updated main qualifications by age 5. for second entry families.

#if new quals at age 5, use the  new qualification
#combine with any new qualifications
MAINadditional_sweep2 <- merge(all=TRUE,main_education2, MAINany_new_age52,by="mcsid")
#MAINadditional_sweep2$bmacqu00[MAINadditional_sweep2$cmedus00==1] <- NA
#combine with created multipart response
MAINdefined_quals_sweep2 <- merge(all=TRUE, MAINadditional_sweep2, MAINnew_academic_age52, by="mcsid")
#MAINdefined_quals_sweep2$MAINhighest_qualsweep2 <- ifelse(!is.na(MAINdefined_quals_sweep2$bmacqu00), MAINdefined_quals_sweep2$bmacqu00, MAINdefined_quals_sweep2$MAINnew_academic_quals_age52)
MAINdefined_quals_sweep2$bmacqu00 = as.character(MAINdefined_quals_sweep2$bmacqu00)
MAINdefined_quals_sweep2$MAINnew_academic_quals_age52=as.character(MAINdefined_quals_sweep2$MAINnew_academic_quals_age52)
MAINdefined_quals_sweep2 <- transform(MAINdefined_quals_sweep2, MAINhighest_qualsweep2 = pmin(bmacqu00, MAINnew_academic_quals_age52,  na.rm = TRUE))

#extract so just have mcsid and highest qual at age 5
MAINhighest_qualification_age5_sweep2 <- c("mcsid", "MAINhighest_qualsweep2")
MAINhighest_qualification_age5_sweep2 <- MAINdefined_quals_sweep2[MAINhighest_qualification_age5_sweep2]



#second entry partner respondent
PARTNERany_new_age52<- c("mcsid", "cmedus00")
PARTNERany_new_age52 <- mcs3_parent[PARTNERany_new_age52]
PARTNERany_new_age52$cmedus00 = as.character(PARTNERany_new_age52$cmedus00)
PARTNERany_new_age52 <- merge (all=TRUE, PARTNERany_new_age52 , sweep_entry, by="mcsid")
PARTNERany_new_age52<- PARTNERany_new_age52[which(PARTNERany_new_age52$sentry == "2"),]

PARTNERnew_qualifications_age52 <- c("mcsid", "cmacqu0a", "cmacqu0b", "cmacqu0c", "cmacqu0d", "cmacqu0e", "cmacqu0f", "cmacqu0g")
PARTNERnew_qualifications_age52<-mcs3_parent[PARTNERnew_qualifications_age52]
MPARTNERnew_qualifications_age52 <- merge (all=TRUE, PARTNERnew_qualifications_age52 , sweep_entry, by="mcsid")
PARTNERnew_qualifications_age52<- MPARTNERnew_qualifications_age52[which(MPARTNERnew_qualifications_age52$sentry == "2"),]

PARTNERnew_qualifications_age52[PARTNERnew_qualifications_age52==-1] <- NA
PARTNERnew_qualifications_age52[PARTNERnew_qualifications_age52==-8] <- NA
PARTNERnew_qualifications_age52[PARTNERnew_qualifications_age52==-9] <- NA
PARTNERnew_qualifications_age52[PARTNERnew_qualifications_age52==98] <- NA
PARTNERnew_qualifications_age52[PARTNERnew_qualifications_age52==99] <- NA
PARTNERnew_qualifications_age52[PARTNERnew_qualifications_age52==96] <- 8
PARTNERnew_qualifications_age52[PARTNERnew_qualifications_age52==3] <- 1
PARTNERnew_qualifications_age52[PARTNERnew_qualifications_age52==4] <- 3
PARTNERnew_qualifications_age52[PARTNERnew_qualifications_age52==5] <- 3
PARTNERnew_qualifications_age52[PARTNERnew_qualifications_age52==6] <- 4
PARTNERnew_qualifications_age52[PARTNERnew_qualifications_age52==7] <-5
PARTNERnew_qualifications_age52[PARTNERnew_qualifications_age52==8] <- 6
PARTNERnew_qualifications_age52[PARTNERnew_qualifications_age52==95] <- 7

PARTNERnew_quals_age52 <- merge(all=TRUE, PARTNERany_new_age52, PARTNERnew_qualifications_age52)
#only those who have additional qualifications
PARTNERadditional_quals_age52<- PARTNERnew_quals_age52[which(PARTNERnew_quals_age52$cmedus00 == "1"),]

#combine together to one variable 
PARTNERadditional_quals_age52 <- transform(PARTNERadditional_quals_age52, PARTNERnew_academic_quals_age52 = pmin(cmacqu0a, cmacqu0b, cmacqu0c, cmacqu0d, cmacqu0e, cmacqu0f, cmacqu0g,  na.rm = TRUE))
#extract only new variable with additional qualifications
PARTNERnew_academic_age52 <- c("mcsid", "PARTNERnew_academic_quals_age52")
PARTNERnew_academic_age52<- PARTNERadditional_quals_age52[PARTNERnew_academic_age52] #merge this with the second entry families education variable (PARTNER_education2)


#if new quals at age 5, use the  new qualification
#combine with any new qualifications
PARTNERadditional_sweep2 <- merge(all=TRUE,partner_education2, PARTNERany_new_age52,by="mcsid")
#PARTNERadditional_sweep2$bpacqu00[PARTNERadditional_sweep2$cpedus00==1] <- NA
#combine with created multipart response
PARTNERdefined_quals_sweep2 <- merge(all=TRUE, PARTNERadditional_sweep2, PARTNERnew_academic_age52, by="mcsid")
#PARTNERdefined_quals_sweep2$PARTNERhighest_qualsweep2 <- ifelse(!is.na(PARTNERdefined_quals_sweep2$bpacqu00),PARTNERdefined_quals_sweep2$bpacqu00, PARTNERdefined_quals_sweep2$PARTNERnew_academic_quals_age52)
PARTNERdefined_quals_sweep2$bpacqu00 = as.character(PARTNERdefined_quals_sweep2$bpacqu00)
PARTNERdefined_quals_sweep2$PARTNERnew_academic_quals_age52 = as.character(PARTNERdefined_quals_sweep2$PARTNERnew_academic_quals_age52)
PARTNERdefined_quals_sweep2 <- transform(PARTNERdefined_quals_sweep2, PARTNERhighest_qualsweep2 = pmin(bpacqu00, PARTNERnew_academic_quals_age52,  na.rm = TRUE))


#extract so just have mcsid and highest qual at age 5
PARTNERhighest_qualification_age5_sweep2 <- c("mcsid", "PARTNERhighest_qualsweep2")
PARTNERhighest_qualification_age5_sweep2 <- PARTNERdefined_quals_sweep2[PARTNERhighest_qualification_age5_sweep2]

#creating mother and father highest education
#mother
MAIN_mother_ed2<- MAINhighest_qualification_age5_sweep2[MAINhighest_qualification_age5_sweep2$mcsid %in% sweep2_main_mum$mcsid,]
PARTNER_mother_ed2<- PARTNERhighest_qualification_age5_sweep2[PARTNERhighest_qualification_age5_sweep2$mcsid %in% sweep2_partner_mum$mcsid,]
maternal_ed2 <- merge(all=TRUE,MAIN_mother_ed2, PARTNER_mother_ed2, by="mcsid")
mum_ed2 <- ifelse(!is.na(maternal_ed2$MAINhighest_qualsweep2), maternal_ed2$MAINhighest_qualsweep2, maternal_ed2$PARTNERhighest_qualsweep2)
mum_ed_2 <- data.frame(maternal_ed2, mum_ed2)
MATERNAL_EDUCATIONsweep2<- c("mcsid", "mum_ed2")
MATERNAL_EDUCATIONsweep2 <- mum_ed_2[MATERNAL_EDUCATIONsweep2]


#father
MAIN_father_ed2<- MAINhighest_qualification_age5_sweep2[MAINhighest_qualification_age5_sweep2$mcsid %in% sweep2_main_mum$mcsid,]
PARTNER_father_ed2<- PARTNERhighest_qualification_age5_sweep2[PARTNERhighest_qualification_age5_sweep2$mcsid %in% sweep2_partner_dad$mcsid,]
paternal_ed2 <- merge(all=TRUE,MAIN_father_ed2, PARTNER_father_ed2, by="mcsid")
dad_ed2 <- ifelse(!is.na(paternal_ed2$MAINhighest_qualsweep2), paternal_ed2$MAINhighest_qualsweep2, paternal_ed2$PARTNERhighest_qualsweep2)
dad_ed_2 <- data.frame(paternal_ed2, dad_ed2)
PATERNAL_EDUCATIONsweep2<- c("mcsid", "dad_ed2")
PATERNAL_EDUCATIONsweep2 <- dad_ed_2[PATERNAL_EDUCATIONsweep2]


#combining sweep 1 and sweep 2 families - parent education variable
#mother
maternal_education_combine <- merge (all=TRUE, MATERNAL_EDUCATIONsweep1, MATERNAL_EDUCATIONsweep2,by="mcsid")
mother_ed_combine <- ifelse(!is.na(maternal_education_combine$mum_ed_sweep1family), maternal_education_combine$mum_ed_sweep1family, maternal_education_combine$mum_ed2)
mum_combine <- data.frame(maternal_education_combine, mother_ed_combine)
mother_academic_qualification <- c("mcsid", "mother_ed_combine")
mother_academic_qualification <- mum_combine[mother_academic_qualification]

#father
paternal_education_combine <- merge (all=TRUE, PATERNAL_EDUCATIONsweep1, PATERNAL_EDUCATIONsweep2,by="mcsid")
father_ed_combine <- ifelse(!is.na(paternal_education_combine$dad_ed_sweep1family), paternal_education_combine$dad_ed_sweep1family, paternal_education_combine$dad_ed2)
dad_combine <- data.frame(paternal_education_combine, father_ed_combine)
father_academic_qualification <- c("mcsid", "father_ed_combine")
father_academic_qualification <- dad_combine[father_academic_qualification]


#both_parents
parent_education <- merge(all=TRUE,mother_academic_qualification, father_academic_qualification,by="mcsid")
#parent_academic_quals<- parent_education[which(mcsid_number_age5$chcnum00=="1"),]
#to get highest household level 
#parent_highest_quals <- merge(all=TRUE,parent_education, parent_academic_quals, by="mcsid")
highest_academic_qual <- transform(parent_education, highested = pmin(mother_ed_combine, father_ed_combine, na.rm=TRUE))
parent_quals <- data.frame(highest_academic_qual, parent_education)
highest_parent_qualification <- c("mcsid", "highested")
highest_parent_qualification <- parent_quals[highest_parent_qualification]
#highest_parent_qualification<- highest_parent_qualification[which(mcsid_number_age5$chcnum00=="1"),]

#collapse into 4 category measure for cross cohort comparison
#1 =no quals/low level quals. 2 = o levels/gcses grades a*-c. 3= post 16 education. 4=university level qualifications
#highest_parent_qualification$highested1 <- rec(highest_parent_qualification$highested,  rec = "1=4; 2=4; 3=3; 4=3; 5=2; 6=1; 7=1; 8=1", as.num = TRUE, var.label = NULL, val.labels = NULL, append = FALSE, suffix = "_r")
highest_parent_qualification$highested1 <- rec(highest_parent_qualification$highested,  rec = "1, 2=4; 3,4=3;  5=2; 6,7,8=1", as.num = TRUE, var.label = NULL, val.labels = NULL, append = FALSE, suffix = "_r")
academic_qualification <-c("mcsid", "highested1")
academic_qualification<- highest_parent_qualification[academic_qualification]


#OCCUPATIONAL STATUS ####
#HIGHEST HOUSEHOLD LEVEL AT AGE 5. IF NA, HIGHEST HOUSEHOLD LEVEL AT AGE 3. IF NA, HIGHEST HOUSEHOLD LEVEL AT AGE 9 MONTHS.

#AGE 5

#ns-sec at sweep 3 (AGE 5)
nssec_age5_1 <- c("mcsid", "cmd05s00", "cpd05s00")
nssec_age5_1 <- mcs3_derived[nssec_age5_1]
nssec_age5_1<-nssec_age5_1[which(mcsid_number_age5$chcnum00=="1"),]
new_nssec_age5_1 <- merge (all=TRUE, nssec_age5_1, sweep_entry, by="mcsid")
nssec_age51<- new_nssec_age5_1[which(new_nssec_age5_1$sentry == "1"),]
nssec_age51[nssec_age51==-9] <- NA
nssec_age51[nssec_age51==-1] <- NA
nssec_age51[nssec_age51==-8] <- NA
nssec_age51[nssec_age51==1] <- 1
nssec_age51[nssec_age51==2] <- 2
nssec_age51[nssec_age51==3] <- 2
nssec_age51[nssec_age51==4] <- 3
nssec_age51[nssec_age51==5] <- 3

nssec_age5_2<- c("mcsid", "cmd05s00", "cpd05s00")
nssec_age5_2<-mcs3_derived[nssec_age5_2]
nssec_age5_2<-nssec_age5_2[which(mcsid_number_age5$chcnum00=="1"),]
new_nssec_age5_2 <- merge (all=TRUE, nssec_age5_2, sweep_entry, by="mcsid")
nssec_age52<- new_nssec_age5_2[which(new_nssec_age5_2$sentry == "2"),]
nssec_age52[nssec_age52==-9] <- NA
nssec_age52[nssec_age52==-1] <-NA
nssec_age52[nssec_age52==-8] <- NA
nssec_age52[nssec_age52==1] <- 1
nssec_age52[nssec_age52==2] <- 2
nssec_age52[nssec_age52==3] <- 2
nssec_age52[nssec_age52==4] <- 3
nssec_age52[nssec_age52==5] <- 3

nssec3_sweep3 <- merge(all=TRUE, nssec_age51, nssec_age52, by="mcsid")
cmsec3 <- ifelse(!is.na(nssec3_sweep3$cmd05s00.x), nssec3_sweep3$cmd05s00.x, nssec3_sweep3$cmd05s00.y)
cpsec3 <- ifelse(!is.na(nssec3_sweep3$cpd05s00.x), nssec3_sweep3$cpd05s00.x, nssec3_sweep3$cpd05s00.y)
new_sec33<- data.frame(nssec3_sweep3, cmsec3 , cpsec3 )
csec3_age5 <- c("mcsid", "cmsec3", "cpsec3")
csec3_age5 <- new_sec33[csec3_age5]


#age3 

#ns-sec at sweep 2
nssec_age3_1 <- c("mcsid", "bmd05s00", "bpd05s00")
nssec_age3_1 <- mcs2_derived[nssec_age3_1]
nssec_age3_1<-nssec_age3_1[which(mcsid_number_age3$bhcnuma0=="1"),]
new_nssec_age3_1 <- merge (all=TRUE, nssec_age3_1, sweep_entry, by="mcsid")
nssec_age31<- new_nssec_age3_1[which(new_nssec_age3_1$sentry == "1"),]
nssec_age31[nssec_age31==-9] <- NA
nssec_age31[nssec_age31==-1] <- NA
nssec_age31[nssec_age31==-8] <- NA
nssec_age31[nssec_age31==1] <- 1
nssec_age31[nssec_age31==2] <- 2
nssec_age31[nssec_age31==3] <- 2
nssec_age31[nssec_age31==4] <- 3
nssec_age31[nssec_age31==5] <- 3

nssec_age3_2<- c("mcsid", "bmd05s00", "bpd05s00")
nssec_age3_2<-mcs2_derived[nssec_age3_2]
nssec_age3_2<-nssec_age3_2[which(mcsid_number_age3$bhcnuma0=="1"),]
new_nssec_age3_2 <- merge (all=TRUE, nssec_age3_2, sweep_entry, by="mcsid")
nssec_age32<- new_nssec_age3_2[which(new_nssec_age3_2$sentry == "2"),]
nssec_age32[nssec_age32==-9] <- NA
nssec_age32[nssec_age32==-1] <-NA
nssec_age32[nssec_age32==-8] <- NA
nssec_age32[nssec_age32==1] <- 1
nssec_age32[nssec_age32==2] <- 2
nssec_age32[nssec_age32==3] <- 2
nssec_age32[nssec_age32==4] <- 3
nssec_age32[nssec_age32==5] <- 3

nssec3_sweep2 <- merge(all=TRUE, nssec_age31, nssec_age32, by="mcsid")
bmsec3 <- ifelse(!is.na(nssec3_sweep2$bmd05s00.x), nssec3_sweep2$bmd05s00.x, nssec3_sweep2$bmd05s00.y)
bpsec3 <- ifelse(!is.na(nssec3_sweep2$bpd05s00.x), nssec3_sweep2$bpd05s00.x, nssec3_sweep2$bpd05s00.y)
new_sec23<- data.frame(nssec3_sweep2, bmsec3 , bpsec3 )
bsec3_age3 <- c("mcsid", "bmsec3", "bpsec3")
bsec3_age3 <- new_sec23[bsec3_age3]



#sweep 1
#create NS-SEC 3 categories
nssec3_1 <- c("mcsid", "amd05s00", "apd05s00")
nssec3_1 <-mcs1_derived[nssec3_1]
asec3<-nssec3_1[which(mcsid_number_age9mo$ahcnuma0=="1"),]
asec3$amd05s00[asec3$amd05s00==-9] <- NA
asec3$amd05s00[asec3$amd05s00==-1] <- NA
asec3$amd05s00[asec3$amd05s00==-8] <- NA
asec3$amd05s00[asec3$amd05s00==1] <- 1
asec3$amd05s00[asec3$amd05s00==2] <- 2
asec3$amd05s00[asec3$amd05s00==3] <- 2
asec3$amd05s00[asec3$amd05s00==4] <- 3
asec3$amd05s00[asec3$amd05s00==5] <- 3

asec3$apd05s00[asec3$apd05s00==-9] <- NA
asec3$apd05s00[asec3$apd05s00==-1] <- NA
asec3$apd05s00[asec3$apd05s00==-8] <- NA
asec3$apd05s00[asec3$apd05s00==1] <- 1
asec3$apd05s00[asec3$apd05s00==2] <- 2
asec3$apd05s00[asec3$apd05s00==3] <- 2
asec3$apd05s00[asec3$apd05s00==4] <- 3
asec3$apd05s00[asec3$apd05s00==5] <- 3

#get highest value for asec/bsec/csec (minimum value is the highest)
ahighest <- transform(asec3, ahighest = pmin(amd05s00, apd05s00, na.rm=TRUE))
bhighest <- transform(bsec3_age3, bhighest = pmin(bmsec3, bpsec3, na.rm=TRUE))
chighest <- transform(csec3_age5, chighest = pmin(cmsec3, cpsec3, na.rm=TRUE))
sec3 <- merge(all=TRUE, ahighest, bhighest, by="mcsid")
sec3 <- merge(all=TRUE, sec3, chighest, by="mcsid")
highsec3<- c("mcsid", "ahighest", "bhighest", "chighest")
highsec3<- sec3[highsec3]

HighestSEC3 <- ifelse(!is.na(highsec3$chighest), highsec3$chighest, highsec3$bhighest)
newhighsec3 <- data.frame(HighestSEC3, highsec3)
HighSEC3 <- ifelse(!is.na(newhighsec3$HighestSEC3), newhighsec3$HighestSEC3, newhighsec3$ahighest)
high_nssec3 <- data.frame(HighSEC3, highsec3)
nssec_3classes <- c("mcsid", "HighSEC3")
nssec_3classes <- high_nssec3[nssec_3classes]



#unemployment at age 5 sweep 1 families
employment_status1_age5 <- c("mcsid", "cdcwrk00")
employment_status1_age5 <- mcs3_derived[employment_status1_age5]
employment_status1_age5<-employment_status1_age5[which(mcsid_number_age5$chcnum00=="1"),]
new_employment_status1_age5 <- merge (all=TRUE, employment_status1_age5, sweep_entry, by="mcsid")
employment_status1_age51<- new_employment_status1_age5[which(new_employment_status1_age5$sentry == "1"),]
#unemployment at age 5 sweep 2 families
employment_status2_age5 <- c("mcsid", "cdcwrk00")
employment_status2_age5 <- mcs3_derived[employment_status2_age5]
employment_status2_age5<-employment_status2_age5[which(mcsid_number_age5$chcnum00=="1"),]
new_employment_status2_age5 <- merge (all=TRUE, employment_status2_age5, sweep_entry, by="mcsid")
employment_status2_age51<- new_employment_status2_age5[which(new_employment_status2_age5$sentry == "2"),]
#employment status age 5
employment_age5 <- merge(all=TRUE, employment_status2_age51, employment_status1_age51,by="mcsid")
employment_age5_combined <- ifelse(!is.na(employment_age5$cdcwrk00.x), employment_age5$cdcwrk00.x, employment_age5$cdcwrk00.y)
age5_employment <- data.frame(employment_age5, employment_age5_combined)
employment_5 <- c("mcsid", "employment_age5_combined")
employment_5 <- age5_employment[employment_5]

#unemployment at age 3 sweep 1 families
employment_status1_age3 <- c("mcsid", "bdcwrk00")
employment_status1_age3 <- mcs2_derived[employment_status1_age3]
employment_status1_age3<-employment_status1_age3[which(mcsid_number_age3$bhcnuma0=="1"),]
new_employment_status1_age3 <- merge (all=TRUE, employment_status1_age3, sweep_entry, by="mcsid")
employment_status1_age31<- new_employment_status1_age3[which(new_employment_status1_age3$sentry == "1"),]
#unemployment at age 3 sweep 2 families
employment_status2_age3 <- c("mcsid", "bdcwrk00")
employment_status2_age3 <- mcs2_derived[employment_status2_age3]
employment_status2_age3<-employment_status2_age3[which(mcsid_number_age3$bhcnuma0=="1"),]
new_employment_status2_age3 <- merge (all=TRUE, employment_status2_age3, sweep_entry, by="mcsid")
employment_status2_age31<- new_employment_status2_age3[which(new_employment_status2_age3$sentry == "2"),]
#employment status age 3
employment_age3 <- merge(all=TRUE, employment_status2_age31, employment_status1_age31,by="mcsid")
employment_age3_combined <- ifelse(!is.na(employment_age3$bdcwrk00.x), employment_age3$bdcwrk00.x, employment_age3$bdcwrk00.y)
age3_employment <- data.frame(employment_age3, employment_age3_combined)
employment_3 <- c("mcsid", "employment_age3_combined")
employment_3 <- age3_employment[employment_3]

#sweep1 labour market status 
labour1<- c("mcsid", "adcwrk00")
labour1<- mcs1_derived[labour1]
labour1<-labour1[which(mcsid_number_age9mo$ahcnuma0=="1"),]
labour1[labour1==-9] <- NA
labour1[labour1==-8] <- NA
labour1[labour1==-1] <- NA


#merge together labour market at 3 sweeps
labour_market <- merge(all=TRUE, labour1, employment_3, by="mcsid")
labour_market1 <- merge(all=TRUE, labour_market, employment_5, by="mcsid")
labour_market_all <- ifelse(!is.na(labour_market1$employment_age5_combined), labour_market1$employment_age5_combined, labour_market1$employment_age3_combined)
new_labour_market3<- data.frame(labour_market_all, labour_market)
labour_market_combined <- ifelse(!is.na(new_labour_market3$labour_market_all), new_labour_market3$labour_market_all, new_labour_market3$adcwrk00)
LABOUR <- data.frame(labour_market1, labour_market_combined)
LABOUR_MARKET<- c("mcsid", "labour_market_combined")
LABOUR_MARKET <- LABOUR[LABOUR_MARKET]
#adding 4th category of unemployed
cat4sec <- merge(all=TRUE,LABOUR_MARKET, nssec_3classes, by="mcsid")
cat4sec[is.na(cat4sec$HighSEC3) & cat4sec$labour_market_combined %in% c(2, 3, 4,6,10),]$HighSEC3 = 4

highest_SES <- c("mcsid", "HighSEC3")
highest_SES <- cat4sec[highest_SES]

highest_SES$highest_occupation <- rec(highest_SES$HighSEC3,  rec = "1=4; 2=3; 3=2; 4=1", as.num = TRUE, var.label = NULL, val.labels = NULL, append = FALSE, suffix = "_r")
highest_occupation<- c("mcsid", "highest_occupation")
highest_occupation <- highest_SES[highest_occupation]



#auxiliary variables for imputation####
#housing tenure at age 5####
housing_tenure <- c("mcsid", "cdroow00")
housing_tenure <- mcs3_derived[housing_tenure]
housing_tenure[housing_tenure==-9] <- NA
housing_tenure[housing_tenure==-8] <- NA
housing_tenure[housing_tenure==-1] <- NA
tenure<-housing_tenure[which(mcsid_number_age5$chcnum00=="1"),]
new_tenure1<- merge (all=TRUE, tenure, sweep_entry, by="mcsid")
tenure1<- new_tenure1[which(new_tenure1$sentry == "1"),]
# new families
housing_tenure2 <- c("mcsid", "cdroow00")
housing_tenure2 <- mcs3_derived[housing_tenure2]
housing_tenure2[housing_tenure2==-9] <- NA
housing_tenure2[housing_tenure2==-8] <- NA
housing_tenure2[housing_tenure2==-1] <- NA
tenure2<-housing_tenure2[which(mcsid_number_age5$chcnum00=="1"),]
new_tenure2<- merge (all=TRUE, tenure2, sweep_entry, by="mcsid")
tenure_2<- new_tenure2[which(new_tenure2$sentry == "2"),]
house_tenure <- merge(all=TRUE, tenure1, tenure_2,by="mcsid")
house_tenure$tenure_combine <- ifelse(!is.na(house_tenure$cdroow00.x), house_tenure$cdroow00.x, house_tenure$cdroow00.y)
#subset data so just have 1 entry and mcsid for the variable
tenure_house <- c("mcsid", "tenure_combine")
tenure_house<- house_tenure[tenure_house]

#tenure at sweep 2 to fill in missing values
housing_tenure_sweep2 <- c("mcsid", "bdroow00")
housing_tenure_sweep2 <- mcs2_derived[housing_tenure_sweep2]
housing_tenure_sweep2[housing_tenure_sweep2==-9] <- NA
housing_tenure_sweep2[housing_tenure_sweep2==-8] <- NA
housing_tenure_sweep2[housing_tenure_sweep2==-1] <- NA
tenure_sweep2<-housing_tenure_sweep2[which(mcsid_number_age3$bhcnuma0=="1"),]
new_tenure1_sweep2<- merge (all=TRUE, tenure_sweep2, sweep_entry, by="mcsid")
tenure1_sweep2<- new_tenure1_sweep2[which(new_tenure1_sweep2$sentry == "1"),]
# new families
housing_tenure2_sweep2 <- c("mcsid", "bdroow00")
housing_tenure2_sweep2 <- mcs2_derived[housing_tenure2_sweep2]
housing_tenure2_sweep2[housing_tenure2_sweep2==-9] <- NA
housing_tenure2_sweep2[housing_tenure2_sweep2==-8] <- NA
housing_tenure2_sweep2[housing_tenure2_sweep2==-1] <- NA
tenure2_sweep2<-housing_tenure2_sweep2[which(mcsid_number_age3$bhcnuma0=="1"),]
new_tenure2_sweep2<- merge (all=TRUE, tenure2_sweep2, sweep_entry, by="mcsid")
tenure_2_sweep2<- new_tenure2_sweep2[which(new_tenure2_sweep2$sentry == "2"),]

house_tenure_sweep2 <- merge(all=TRUE, tenure1_sweep2, tenure_2_sweep2,by="mcsid")
house_tenure_sweep2$tenure_combine_sweep2 <- ifelse(!is.na(house_tenure_sweep2$bdroow00.x), house_tenure_sweep2$bdroow00.x, house_tenure_sweep2$bdroow00.y)

#subset data so just have 1 entry and mcsid for the variable
tenure_house_sweep2 <- c("mcsid", "tenure_combine_sweep2")
tenure_house_sweep2<- house_tenure_sweep2[tenure_house_sweep2]
houses_tenure_sweep2 <- merge(all=TRUE, tenure_house, tenure_house_sweep2, by="mcsid")
houses_tenure_sweep2$housing_tenure_combined <- ifelse(!is.na(houses_tenure_sweep2$tenure_combine), houses_tenure_sweep2$tenure_combine,  houses_tenure_sweep2$tenure_combine_sweep2)
TENURE_1 <- c("mcsid", "housing_tenure_combined")
TENURE_1<- houses_tenure_sweep2[TENURE_1]

#sweep 1 - replace remaining NA values. 
housing_tenure_sweep1 <- c("mcsid", "adroow00")
housing_tenure_sweep1 <- mcs1_derived[housing_tenure_sweep1]
housing_tenure_sweep1[housing_tenure_sweep1==-9] <- NA
housing_tenure_sweep1[housing_tenure_sweep1==-8] <- NA
housing_tenure_sweep1[housing_tenure_sweep1==-1] <- NA
tenure_sweep1<-housing_tenure_sweep1[which(mcsid_number_age9mo$ahcnuma0=="1"),]

tenure_1 <- merge(all=TRUE, TENURE_1,tenure_sweep1, by="mcsid")
tenure_1$tenure_type_combined <- ifelse(!is.na(tenure_1$housing_tenure_combined), tenure_1$housing_tenure_combined, tenure_1$adroow00)
TENURE <- c("mcsid", "tenure_type_combined")
TENURE<- tenure_1[TENURE]

TENURE[TENURE==1] <-1
TENURE[TENURE==2] <-1
TENURE[TENURE==3] <-2
TENURE[TENURE==4] <-2
TENURE[TENURE==5] <-2
TENURE[TENURE==6] <-2
TENURE[TENURE==7] <-3
TENURE[TENURE==8] <-4
TENURE[TENURE==9] <-5
TENURE[TENURE==10] <-5


#accommodation type####
#accommodation type at age 5 (sweep 3)
accommodation <-c ("mcsid", "cmmoty00")
accommodation <- mcs3_parent[accommodation]
accommodation[accommodation ==-1:-9] <- NA
accommodation[accommodation == 95] <- NA
accommodation <- accommodation[which(mcsid_number_age5$chcnum00=="1"),]
new_accommodation <- merge (all=TRUE,accommodation, sweep_entry, by="mcsid")
new_accommodation1<- new_accommodation[which(new_accommodation$sentry == "1"),]
#new families
accommodation2 <-c ("mcsid", "cmmoty00")
accommodation2 <- mcs3_parent[accommodation2]
accommodation2[accommodation2 ==-1:-9] <- NA
accommodation2[accommodation2 == 95] <- NA
accommodation2 <- accommodation2[which(mcsid_number_age5$chcnum00=="1"),] 
new_accommodation2 <- merge (all=TRUE,accommodation2, sweep_entry, by="mcsid")
new_accommodation2<- new_accommodation2[which(new_accommodation2$sentry == "2"),]
accomm <- merge(all=TRUE, new_accommodation1, new_accommodation2,by="mcsid")
accomm_combine <- ifelse(!is.na(accomm$cmmoty00.x), accomm$cmmoty00.x, accomm$cmmoty00.y)
#create dataframe so also have mcsid 
new_accommodation<- data.frame(accomm_combine, accomm)
#subset data so just have 1 standard score and mcsid for the variable
accommodation_type <- c("mcsid", "accomm_combine")
accommodation_type <- new_accommodation[accommodation_type ]

#accommodation type at sweep 2 to fill in missing values
accommodation_sweep2 <-c ("mcsid", "bmmoty00")
accommodation_sweep2 <- mcs2_parent[accommodation_sweep2]
accommodation_sweep2[accommodation_sweep2 ==-1:-9] <- NA
accommodation_sweep2[accommodation_sweep2 == 98] <- NA
accommodation_sweep2[accommodation_sweep2 == 99] <- NA
accommodation_sweep2[accommodation_sweep2== 95] <- NA
accommodation_sweep2[accommodation_sweep2 == 85] <- NA
accommodation_sweep2[accommodation_sweep2 == 86] <- NA
accommodation_sweep2 <- accommodation_sweep2[which(mcsid_number_age3$bhcnuma0=="1"),]
new_accommodation_sweep2 <- merge (all=TRUE,accommodation_sweep2, sweep_entry, by="mcsid")
new_accommodation_sweep2_1<- new_accommodation_sweep2[which(new_accommodation_sweep2$sentry == "1"),]
#new families
accommodation2_sweeep2 <-c ("mcsid", "bmmoty00")
accommodation2_sweeep2 <- mcs2_parent[accommodation2_sweeep2]
accommodation2_sweeep2[accommodation2_sweeep2 ==-1:-9] <- NA
accommodation2_sweeep2[accommodation2_sweeep2 == 95] <- NA
accommodation2_sweeep2 <- accommodation2_sweeep2[which(mcsid_number_age3$bhcnuma0=="1"),] 
new_accommodation2_sweeep2 <- merge (all=TRUE,accommodation2_sweeep2, sweep_entry, by="mcsid")
new_accommodation_sweeep2<- new_accommodation2_sweeep2[which(new_accommodation2_sweeep2$sentry == "2"),]
accomm_sweep2 <- merge(all=TRUE, new_accommodation_sweep2_1, new_accommodation_sweeep2,by="mcsid")

accomm_combine_sweep2 <- ifelse(!is.na(accomm_sweep2$bmmoty00.x), accomm_sweep2$bmmoty00.x, accomm_sweep2$bmmoty00.y)
#create dataframe so also have mcsid 
new_accommodation_sweep2<- data.frame(accomm_combine_sweep2, accomm_sweep2)
#subset data so just have 1 standard score and mcsid for the variable
accommodation_type_sweep2 <- c("mcsid", "accomm_combine_sweep2")
accommodation_type_sweep2 <- new_accommodation_sweep2[accommodation_type_sweep2 ]
accomm_type <- merge(all=TRUE, accommodation_type, accommodation_sweep2, by="mcsid")
accomm_type$accommodation_type_combined1 <- ifelse(!is.na(accomm_type$accomm_combine), accomm_type$accomm_combine, accomm_type$bmmoty00)
ACCOMMODATION_1 <- c("mcsid", "accommodation_type_combined1")
ACCOMMODATION_1<- accomm_type[ACCOMMODATION_1]

#accommodation type at sweep 1 to fill in remaining missing values
accommodation_sweep1 <-c ("mcsid", "ammoty00")
accommodation_sweep1 <- mcs1_parent[accommodation_sweep1]
accommodation_sweep1[accommodation_sweep1 ==-1:-9] <- NA
accommodation_sweep1[accommodation_sweep1 == 98] <- NA
accommodation_sweep1[accommodation_sweep1 == 99] <- NA
accommodation_sweep1[accommodation_sweep1 == 95] <- NA
accommodation_sweep1[accommodation_sweep1 == 85] <- NA
accommodation_sweep1[accommodation_sweep1 == 86] <- NA
accommodation_sweep1 <- accommodation_sweep1[which(mcsid_number_age9mo$ahcnuma0=="1"),]

accomm_type_2 <- merge(all=TRUE, ACCOMMODATION_1, accommodation_sweep1, by="mcsid")
accomm_type$accommodation_type_combined <- ifelse(!is.na(accomm_type_2$accommodation_type_combined1), accomm_type_2$accommodation_type_combined1, accomm_type_2$ammoty00)
ACCOMMODATION <- c("mcsid", "accommodation_type_combined")
ACCOMMODATION<- accomm_type[ACCOMMODATION]

ACCOMMODATION[ACCOMMODATION==1] <-1
ACCOMMODATION[ACCOMMODATION==2] <-2
ACCOMMODATION[ACCOMMODATION==3] <-2
ACCOMMODATION[ACCOMMODATION==4] <-3

#whether CM breastfed####
breastfed <-c("mcsid", "ambfeva0")
breastfed <- mcs1_parent[breastfed]
breastfed[breastfed == -1:-9]<- NA
breastfed <- breastfed[which(mcsid_number_age9mo$ahcnuma0=="1"),]

#parents in household####
#number of parents present in household at age 5
household <-c ("mcsid", "cdhtys00")
household <- mcs3_derived[household]
household[household ==-1:-9] <- NA
household <-household[which(mcsid_number_age5$chcnum00=="1"),]
new_household <- merge (all=TRUE,household, sweep_entry, by="mcsid")
new_household1<- new_household[which(new_household$sentry == "1"),]
household2 <-c ("mcsid", "cdhtys00")
household2 <- mcs3_derived[household2]
household2[household2 ==-1:-9] <- NA
household2 <-household2[which(mcsid_number_age5$chcnum00=="1"),]
new_household2 <- merge (all=TRUE,household2, sweep_entry, by="mcsid")
new_household_2<- new_household2[which(new_household2$sentry == "2"),]
house <- merge(all=TRUE, new_household1, new_household_2,by="mcsid")
house_combine <- ifelse(!is.na(house$cdhtys00.x), house$cdhtys00.x, house$cdhtys00.y)
#create dataframe so also have mcsid 
new_household<- data.frame(house_combine,house)
#subset data so just have 1 standard score and mcsid for the variable
parents_in_hh <- c("mcsid", "house_combine")
parents_in_hh <- new_household[parents_in_hh ]

#sweep 2 to fill in NA
household_sweep2 <-c ("mcsid", "bdhtys00")
household_sweep2 <- mcs2_derived[household_sweep2]
household_sweep2[household_sweep2 ==-1:-9] <- NA
household_sweep2<-household_sweep2[which(mcsid_number_age3$bhcnuma0=="1"),]
new_household_sweep2 <- merge (all=TRUE,household_sweep2, sweep_entry, by="mcsid")
new_household1_sweep2<- new_household_sweep2[which(new_household_sweep2$sentry == "1"),]
#new families
household2_sweep2 <-c ("mcsid", "bdhtys00")
household2_sweep2 <- mcs2_derived[household2_sweep2]
household2_sweep2[household2_sweep2 ==-1:-9] <- NA
household2_sweep2 <-household2_sweep2[which(mcsid_number_age3$bhcnuma0=="1"),]
new_household2_sweep2 <- merge (all=TRUE,household2_sweep2, sweep_entry, by="mcsid")
new_household_2_sweep2<- new_household2_sweep2[which(new_household2_sweep2$sentry == "2"),]
house_sweep2 <- merge(all=TRUE, new_household1_sweep2, new_household_2_sweep2,by="mcsid")
house_combine_sweep2 <- ifelse(!is.na(house_sweep2$bdhtys00.x), house_sweep2$bdhtys00.x, house_sweep2$bdhtys00.y)
#create dataframe so also have mcsid 
new_household_sweep2<- data.frame(house_combine_sweep2,house_sweep2)
#subset data so just have 1 standard score and mcsid for the variable
parents_in_hh_sweep2 <- c("mcsid", "house_combine_sweep2")
parents_in_hh_sweep2 <- new_household_sweep2[parents_in_hh_sweep2 ]

parents_2 <- merge(all=TRUE, parents_in_hh, parents_in_hh_sweep2, by="mcsid")
parents_2$parents_present_combined <- ifelse(!is.na(parents_2$house_combine), parents_2$house_combine, parents_2$house_combine_sweep2)
parents_there <- c("mcsid", "parents_present_combined")
parents_there<- parents_2[parents_there]
#replace remaining NA with sweep1 responses
household_sweep1 <-c ("mcsid", "adhtys00")
household_sweep1 <- mcs1_derived[household_sweep1]
household_sweep1[household_sweep1 ==-1:-9] <- NA
household_sweep1<-household_sweep1[which(mcsid_number_age9mo$ahcnuma0=="1"),]
parents_home <- merge(all=TRUE, household_sweep1, parents_there,by="mcsid")
parents_home$parents_in_household <- ifelse(!is.na(parents_home$parents_present_combined), parents_home$parents_present_combined, parents_home$adhtys00)
PARENTS_IN_HH <- c("mcsid", "parents_in_household")
PARENTS_IN_HH <- parents_home[PARENTS_IN_HH]

#mother's age at birth of CM####
#sweep 1
mother_main_birth <- MAINrespondent[MAINrespondent$amdres00 == 1 | MAINrespondent$amdres00 == 11 | MAINrespondent$amdres00 == 15 ,]
mother_partner_birth <- PARTNERrespondent[PARTNERrespondent$apdres00 == 1 |PARTNERrespondent$apdres00 == 11| PARTNERrespondent$apdres00 == 15,]
mother_partner_birth <- mother_partner_birth[!is.na(mother_partner_birth$mcsid),] 
mother_main_sweep1_birth <- merge (all=TRUE, mother_main_birth, sweep_entry, by="mcsid")
mother_main_sweep1_1_birth<- mother_main_sweep1_birth[which(mother_main_sweep1_birth$sentry == "1"),]
sweep1_main_mother_birth <- c("mcsid", "amdres00")
sweep1_main_mother_birth <-mother_main_sweep1_1_birth[sweep1_main_mother_birth]
sweep1_main_mother_birth  <- sweep1_main_mother_birth[!is.na(sweep1_main_mother_birth$amdres00),] 
mother_partner_sweep1_birth <- merge (all=TRUE, mother_partner_birth, sweep_entry, by="mcsid")
mother_partner_sweep1_1_birth<- mother_partner_sweep1_birth[which(mother_partner_sweep1_birth$sentry == "1"),]
sweep1_partner_mother_birth <- c("mcsid", "apdres00")
sweep1_partner_mother_birth <-mother_partner_sweep1_1_birth[sweep1_partner_mother_birth]
sweep1_partner_mother_birth  <- sweep1_partner_mother_birth[!is.na(sweep1_partner_mother_birth$apdres00),] 

#mother sweep 2
mother_main2_birth <- MAINrespondent2[MAINrespondent2$bmdres00 == 1 | MAINrespondent2$bmdres00 == 11 | MAINrespondent2$bmdres00 == 15 ,]
mother_partner2_birth <- PARTNERrespondent2[PARTNERrespondent2$bpdres00 == 1 | PARTNERrespondent2$bpdres00 == 11 | PARTNERrespondent2$bpdres00 == 15 ,]
mother_partner2_birth <- mother_partner2_birth[!is.na(mother_partner2_birth$mcsid),] 
mum_main_sweep2_1st_birth <- merge (all=TRUE, mother_main2_birth, sweep_entry, by="mcsid")
mum_main_sweep2_1_birth<- mum_main_sweep2_1st_birth[which(mum_main_sweep2_1st_birth$sentry == "1"),]
sweep2_main_mum1_birth <- c("mcsid", "bmdres00")
sweep2_main_mum1_birth <- mum_main_sweep2_1_birth[sweep2_main_mum1_birth]
sweep2_main_mum1_birth <- sweep2_main_mum1_birth[!is.na(sweep2_main_mum1_birth$bmdres00),] 
mum_partner_sweep2_1st_birth <- merge (all=TRUE, mother_partner2_birth, sweep_entry, by="mcsid")
mum_partner_sweep2_1_birth<- mum_partner_sweep2_1st_birth[which(mum_partner_sweep2_1st_birth$sentry == "1"),]
sweep2_partner_mum1_birth <- c("mcsid", "bpdres00")
sweep2_partner_mum1_birth <- mum_partner_sweep2_1_birth[sweep2_partner_mum1_birth]
sweep2_partner_mum1_birth <- sweep2_partner_mum1_birth[!is.na(sweep2_partner_mum1_birth$bpdres00),] 


#auxiliary variables for imputation
#mother's age at birth of CM
#sweep 1
MAINbirth_age <- c("mcsid", "amdagb00")
MAINbirth_age <- mcs1_derived[MAINbirth_age]
MAINbirth_age[MAINbirth_age==-2:-1] <- NA
#MAINbirth_age1<-MAINbirth_age[which(mcsid_number_age9mo$ahcnuma0=="1"),]

PARTNERbirth_age <- c("mcsid", "apdagb00")
PARTNERbirth_age <- mcs1_derived[PARTNERbirth_age]
PARTNERbirth_age[PARTNERbirth_age==-2:-1] <- NA
#PARTNERbirth_age1 <- PARTNERbirth_age[which(mcsid_number_age9mo$ahcnuma0=="1"),]

MAIN_mother_age<- MAINbirth_age[MAINbirth_age$mcsid %in% sweep1_main_mother_birth$mcsid,]
PARTNER_mother_age<- PARTNERbirth_age[PARTNERbirth_age$mcsid %in% sweep1_partner_mother_birth$mcsid,]

maternal_age<- merge(all=TRUE,MAIN_mother_age, PARTNER_mother_age, by="mcsid")
mum_age <- ifelse(!is.na(maternal_age$amdagb00), maternal_age$amdagb00, maternal_age$apdagb00)
mum_age1 <- data.frame(maternal_age, mum_age)
MATERNAL_AGEsweep1<- c("mcsid", "mum_age")
MATERNAL_AGEsweep1 <- mum_age1[MATERNAL_AGEsweep1]

#sweep 2 - new families
MAINbirth_age2 <- c("mcsid", "bmdagb00")
MAINbirth_age2 <- mcs2_derived[MAINbirth_age2]
MAINbirth_age2[MAINbirth_age2==-2:-1] <- NA
#MAINbirth_age_2<-MAINbirth_age2[which(mcsid_number_age3$bhcnuma0=="1"),]

PARTNERbirth_age2 <- c("mcsid", "bpdagb00")
PARTNERbirth_age2 <- mcs2_derived[PARTNERbirth_age2]
PARTNERbirth_age2[PARTNERbirth_age2==-2:-1] <- NA
#PARTNERbirth_age_2 <- PARTNERbirth_age2[which(mcsid_number_age3$bhcnuma0=="1"),]

MAIN_mother_age2<- MAINbirth_age2[MAINbirth_age2$mcsid %in% sweep2_main_mum1_birth$mcsid,]
PARTNER_mother_age2<- PARTNERbirth_age2[PARTNERbirth_age2$mcsid %in% sweep2_partner_mum1_birth$mcsid,]

maternal_age2<- merge(all=TRUE,MAIN_mother_age2, PARTNER_mother_age2, by="mcsid")
mum_age2 <- ifelse(!is.na(maternal_age2$bmdagb00), maternal_age2$bmdagb00, maternal_age2$bpdagb00)
mum_age_2 <- data.frame(maternal_age2, mum_age2)
MATERNAL_AGEsweep2<- c("mcsid", "mum_age2")
MATERNAL_AGEsweep2 <- mum_age_2[MATERNAL_AGEsweep2]

mother_age <- merge(all=TRUE, MATERNAL_AGEsweep1, MATERNAL_AGEsweep2, by="mcsid")
mother_age$mothers_age <- ifelse(!is.na(mother_age$mum_age), mother_age$mum_age, mother_age$mum_age2)
mother_birth_age <- c("mcsid", "mothers_age")
mother_birth_age<- mother_age[mother_birth_age]


#combine variables into analysis data ####

analysis_data <- merge(all=TRUE, cm_sex, cm_ethnicity,by="mcsid")
nrow(analysis_data)
analysis_data <- merge(all=TRUE, analysis_data, language_used,by="mcsid")
nrow(analysis_data)
analysis_data <- merge(all=TRUE, analysis_data, mother_birth_age,by="mcsid")
nrow(analysis_data)
analysis_data <- merge(all=TRUE, analysis_data, ACCOMMODATION,by="mcsid")
nrow(analysis_data)
analysis_data <- merge(all=TRUE, analysis_data, academic_qualification,by="mcsid")
nrow(analysis_data)
analysis_data <- merge(all=TRUE, analysis_data, highest_occupation, by="mcsid")
nrow(analysis_data)
analysis_data <- merge(all=TRUE, analysis_data, age5_language, by="mcsid")
nrow(analysis_data)
analysis_data <- merge(all=TRUE, analysis_data,final_cm_age5, by="mcsid")
nrow(analysis_data)
analysis_data <- merge(all=TRUE, analysis_data, age11_language, by="mcsid")
nrow(analysis_data)
analysis_data <- merge(all=TRUE, analysis_data,cm_age11, by="mcsid")
nrow(analysis_data)
analysis_data <- merge(all=TRUE, analysis_data, age14_language, by="mcsid")
nrow(analysis_data)
analysis_data <- merge(all=TRUE, analysis_data,cm_age14, by="mcsid")
nrow(analysis_data)
analysis_data <- merge(all=TRUE, analysis_data,mcs_weight2, by="mcsid")
nrow(analysis_data)

#pull out those with a response to language measure ####
mcs_analysis <- analysis_data[!is.na(analysis_data$new_NVabilitycombine) |!is.na(analysis_data$verbal_similarities_age11) | !is.na(analysis_data$word_act_age14),]

#dummy variable for cohort membership. for MCS membership this will = 1. ####
mcs_analysis$cohort <- rep(1, nrow(mcs_analysis))
#save data as csv file####
write.csv(mcs_analysis, file = "mcs_ses_comparison_data.csv")


#sensitivity check with white only ethnicity - to account for the change ####
#in ethnic composition between cohorts/see if this has affected results. 

white_sensitivity <- mcs_analysis[which(mcs_analysis$ethnic_combine == "0"),] #binary variable where 0=white, 1=minority 
write.csv(white_sensitivity, file = "mcs_crossCohort_whiteSample.csv")




