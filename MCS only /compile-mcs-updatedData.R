#SEC Inequalities in Vocabualry - MCS data compilation code (with new edition data)

#### Load necessary packages ####
library(haven)
require(swfscMisc)
require(sjmisc)
require(Hmisc)
require(psych)
library(dplyr) 
library(tidyr) 
library(tidyverse)
library(fauxnaif)
library(naniar)
library(gtools)

#### Load in data ####
mcs2_child_assessment <- read_sav("mcs2_cm_cognitive_assessment.sav")
mcs3_child_assessment <- read_sav("mcs3_cm_cognitive_assessment.sav")
mcs5_child_assessment <- read_sav("mcs5_cm_cognitive_assessment.sav")
mcs5_child_derived <- read_sav("mcs5_cm_derived.sav")
mcs6_child_assessment<- read_sav("mcs6_cm_cognitive_assessment.sav")
mcs1_hh <- read_sav("mcs1_hhgrid.sav")
mcs2_hh <- read_sav("mcs2_hhgrid.sav")
mcs_family <- read_sav("mcs_longitudinal_family_file.sav")
mcs1_parent <- read_sav("mcs1_parent_interview.sav")
mcs1_derived <- read_sav("mcs1_parent_derived.sav")
mcs2_parent <- read_sav("mcs2_parent_interview.sav")
mcs2_derived <- read_sav("mcs2_parent_derived.sav")
mcs2_derived_family <- read_sav("mcs2_family_derived.sav")
mcs1_derived_family <- read_sav("mcs1_family_derived.sav")
mcs5_parent4thEd<- read_sav("mcs5_parent_interview_4thEd.sav")
mcs5_parent<- read_sav("mcs5_parent_interview.sav")
mcs1_geography <- read_sav("mcs1_geographically_linked_data.sav")
mcs2_geography <- read_sav("mcs2_geographically_linked_data.sav")
mcs5_family <- read_sav("mcs5_family_derived.sav")
mcs1_parent_12thEd <- read_sav("mcs1_parent_interview_12thEd.sav")
mcs2_parent_9thEd <- read_sav("mcs2_parent_interview_9thEd.sav")
mcs1_cm_derived = read_sav("mcs1_cm_derived.sav")
mcs2_cm_derived = read_sav("mcs2_cm_derived.sav")
mcs1_cm_parent = read_sav("mcs1_parent_cm_interview.sav")
mcs2_cm_parent = read_sav("mcs2_parent_cm_interview.sav")

#### Convert all to lowercase ####
names(mcs2_child_assessment) <- tolower(names(mcs2_child_assessment))
names(mcs3_child_assessment) <- tolower(names(mcs3_child_assessment))
names(mcs5_child_assessment) <- tolower(names(mcs5_child_assessment))
names(mcs6_child_assessment) <- tolower(names(mcs6_child_assessment))
names(mcs_family) <- tolower(names(mcs_family))
names(mcs5_parent) <- tolower(names(mcs5_parent))
names(mcs5_parent4thEd) <- tolower(names(mcs5_parent4thEd))
names(mcs5_child_derived) <- tolower(names(mcs5_child_derived))
names(mcs1_parent) <- tolower(names(mcs1_parent))
names(mcs2_parent) <- tolower(names(mcs2_parent))
names(mcs2_derived) <- tolower(names(mcs2_derived))
names(mcs2_derived_family) <- tolower(names(mcs2_derived_family))
names(mcs1_derived) <- tolower(names(mcs1_derived))
names(mcs1_derived_family) <- tolower(names(mcs1_derived_family))
names(mcs1_hh) <- tolower(names(mcs1_hh))
names(mcs2_hh) <- tolower(names(mcs2_hh))
names(mcs5_family) <- tolower(names(mcs5_family))
names(mcs2_geography) <- tolower(names(mcs2_geography))
names(mcs1_geography) <- tolower(names(mcs1_geography))
names(mcs1_parent_12thEd) <- tolower(names(mcs1_parent_12thEd))
names(mcs2_parent_9thEd) <- tolower(names(mcs2_parent_9thEd))
names(mcs1_cm_derived) <- tolower(names(mcs1_cm_derived))
names(mcs2_cm_derived) <- tolower(names(mcs2_cm_derived))
names(mcs1_cm_parent) <- tolower(names(mcs1_cm_parent))
names(mcs2_cm_parent) <- tolower(names(mcs2_cm_parent))

#### Create weight variable####
# Attrition and sample weight age 9 months sweep 
mcs1_weight <- c("mcsid", "aovwt2")
mcs1_weight <- mcs_family[mcs1_weight]
mcs1_weight [mcs1_weight  ==-1] <- NA


# Attrition and sample weight age 3 sweep 
mcs2_weight <- c("mcsid", "bovwt2")
mcs2_weight <- mcs_family[mcs2_weight]
mcs2_weight [mcs2_weight  ==-1] <- NA

weight <- merge(all=TRUE, mcs1_weight, mcs2_weight,by="mcsid")
weight$weight1 <- ifelse(!is.na(weight$bovwt2), weight$bovwt2, weight$aovwt2)
mcs_weight <- c("mcsid", "weight1")
mcs_weight <- weight[mcs_weight]


#### Respondent Key ####
respondent_key = c(`1` = "main",
                   `2` = "partner", 
                   `3` = "partner_proxy",
                   `4` = "not_eligible")

#create sweep entry variable to identify second entry families later####
sweep_entry <- c("mcsid", "sentry")
sweep_entry <- mcs_family[sweep_entry]
sweep_entry$sentry = as.character(sweep_entry$sentry)

#Cohort member number - will select CM number = 1 when they joined the study for analytical sample. 
mcs1 = mcs1_cm_derived %>% select(mcsid, acnum00)
mcs2 = mcs2_child_assessment %>% select(mcsid, bcnum00)  %>% 
  full_join(sweep_entry) %>% 
  filter(sentry == 2)

mcs_cm1 = mcs1 %>% 
  full_join(mcs2) %>% 
  #filter(acnum00 == 1 | bcnum00 == 1) %>% 
  mutate(cm_number = case_when(
    !is.na(acnum00) ~ acnum00, 
    is.na(acnum00) ~ bcnum00
  )) 

#### Vocabulary variables ####

#Age 3 - Naming Vocabulary T-Scores
age3_vocab = mcs2_child_assessment %>% select(mcsid, bdbast00, bcnum00) %>% 
  filter(bcnum00 == 1) %>% 
  select(mcsid, bdbast00) %>% 
  replace_with_na(replace = list(bdbast00 = c(-6, -7, -8))) %>%  #-6 = not administered, -8 = ended early, -7 = not carried out. mark these as NA.
  filter(!is.na(bdbast00)) %>% 
  rename(age3_vocab = bdbast00)

#Age 5 - Naming Vocabulary T-Scores
age5_vocab = mcs3_child_assessment %>% select(mcsid, ccnvtscore, ccnum00, ccstnv00) %>% 
  filter(ccnum00 == 1) %>% 
  filter(!ccstnv00 == 2) %>% #filter out those who didnt start vocab test and were given incorrect score of 20 in data
  select(mcsid, ccnvtscore) %>% 
  filter(!is.na(ccnvtscore)) %>% 
  rename(age5_vocab = ccnvtscore)

#Age 11 - Verbal Similarities T-Scores
age11_vocab = mcs5_child_derived %>% select(mcsid, evstsco, ecnum00) %>% 
  filter(ecnum00 == 1) %>% 
  select(mcsid, evstsco) %>% 
  filter(!is.na(evstsco)) %>% 
  rename(age11_vocab = evstsco)

#Age 14 - Word Activity Scores - create own total score
age14_vocab = mcs6_child_assessment %>% select(mcsid, contains("fccmcog"), fcvtypdo, fcvtcdck, fcnum00) %>% 
  filter(fcnum00==1) %>% 
  mutate(across(c(fccmcogg, fccmcogq, fccmcogr), 
                .fns = ~ case_when(. == 1 ~ 1, 
                                   is.na(.) ~ NA_real_,
                                   .!=1 ~ 0 ))) %>% 
  mutate(across(c(fccmcogc, fccmcogd, fccmcogk, fccmcogl),
                .fns = ~case_when(. == 2 ~ 1, 
                                  is.na(.) ~ NA_real_,
                                  .!=2 ~ 0))) %>% 
  mutate(across(c(fccmcogf, fccmcogo, fccmcogs, fccmcogt),
                .fns = ~case_when(.==3 ~ 1, 
                                  is.na(.) ~ NA_real_,
                                  .!=3 ~ 0))) %>% 
  mutate(across(c(fccmcogj, fccmcogn), 
                .fns = ~case_when(. == 4 ~ 1, 
                                  is.na(.) ~ NA_real_,
                                  .!=4 ~ 0))) %>% 
  mutate(across(c(fccmcoga, fccmcogb, fccmcoge, fccmcogh, fccmcogi, fccmcogm, fccmcogp),
                            .fns = ~ case_when(. == 5 ~ 1,
                                               is.na(.) ~ NA_real_,
                                            . !=5 ~ 0))) %>% 
  filter(!fcvtypdo == 2 ) %>% 
  mutate(age14_vocab = rowSums(.[c("fccmcoga", "fccmcogb", "fccmcogc", "fccmcogd", 
                                   "fccmcoge", "fccmcogf", "fccmcogg", "fccmcogh", 
                                   "fccmcogi", "fccmcogj", "fccmcogk", "fccmcogl",
                                   "fccmcogm", "fccmcogn", "fccmcogo", "fccmcogp",
                                   "fccmcogq", "fccmcogr", "fccmcogs", "fccmcogt")], na.rm = T), .after = 1) %>% 
  select(mcsid, age14_vocab)

#### Socioeconomic Circumstances Variables ####

#1. Occupational Status - 4 classes. ####
#Age 3, 9 Months if Age 3 is NA.


#occupation at age 3

age3_occupation = mcs2_derived %>% select(mcsid, bdd05s00, belig00) %>% 
  mutate(occupation = case_when(bdd05s00 ==1 ~1, #recode to be 3 level variable. 
                                bdd05s00 ==2 ~2,  
                                bdd05s00 ==3 ~2, 
                                bdd05s00 ==4 ~3, 
                                bdd05s00 ==5 ~3))
age3_occupation$belig00 = as.factor(age3_occupation$belig00)
age3_occupation$belig00 = recode(age3_occupation$belig00, !!!respondent_key)

age3_occupation_wide = age3_occupation %>% select(!bdd05s00) %>% 
  group_by(mcsid) %>%
  pivot_wider(names_from = belig00, values_from = occupation) %>% 
  rename("main_occupation" = main, 
         "partner_occupation" = partner, 
         "proxy_partner_occupation" = partner_proxy) %>% 
  mutate(partner_occupation = case_when(!is.na(partner_occupation) ~ partner_occupation, 
                                        is.na(partner_occupation) ~ proxy_partner_occupation)) %>% 
  mutate(highest_occupation = pmin(main_occupation, partner_occupation, na.rm = TRUE)) %>% 
  select(mcsid, highest_occupation)

#add unemployment as 4th category 
age3_employment = mcs2_derived_family %>% select(mcsid, bdcwrk00)
age3_occupation = merge(all=TRUE, age3_occupation_wide, age3_employment, by="mcsid")
age3_occupation[is.na(age3_occupation$highest_occupation) & age3_occupation$bdcwrk00 %in% c(4,6,10),]$highest_occupation= 4

#occupation at 9 months
months9_occupation = mcs1_derived %>% select(mcsid, add05s00, aelig00) %>% 
  mutate(occupation = case_when(add05s00 ==1 ~1, #recode to be 3 level variable. 
                                add05s00 ==2 ~2,  
                                add05s00 ==3 ~2, 
                                add05s00 ==4 ~3, 
                                add05s00 ==5 ~3))
months9_occupation$aelig00 = as.factor(months9_occupation$aelig00)
months9_occupation$aelig00 = recode(months9_occupation$aelig00, !!!respondent_key)

months9_occupation_wide = months9_occupation %>% select(!add05s00) %>% 
  group_by(mcsid) %>%
  pivot_wider(names_from = aelig00, values_from = occupation) %>% 
  rename("main_occupation" = main, 
         "partner_occupation" = partner, 
         "proxy_partner_occupation" = partner_proxy) %>% 
  mutate(partner_occupation = case_when(!is.na(partner_occupation) ~ partner_occupation, 
                                        is.na(partner_occupation) ~ proxy_partner_occupation)) %>% 
  mutate(highest_occupation = pmin(main_occupation, partner_occupation, na.rm = TRUE)) %>% 
  select(mcsid, highest_occupation)

#add unemployment as 4th category 
months9_employment = mcs1_derived_family %>% select(mcsid, adcwrk00)
months9_occupation = merge(all=TRUE, months9_occupation_wide, months9_employment, by="mcsid")
months9_occupation[is.na(months9_occupation$highest_occupation) & months9_occupation$adcwrk00 %in% c(4,6,10),]$highest_occupation= 4
months9_occupation = months9_occupation %>% rename("highest_occupation_s1" = highest_occupation)

#replace NA at age 3 with values from 9 months
occupational_status = merge(all=TRUE, age3_occupation, months9_occupation, by = "mcsid") %>% 
  select(mcsid, highest_occupation, highest_occupation_s1) %>% 
  mutate(highest_household_occupation = case_when(!is.na(highest_occupation) ~ highest_occupation, 
                                                  is.na(highest_occupation) ~ highest_occupation_s1)) %>% 
  select(mcsid, highest_household_occupation) %>% 
  rec(highest_household_occupation,  rec = "1=4; 2=3; 3=2; 4=1", #reverse code variable
      as.num = TRUE, var.label = NULL, 
      val.labels = NULL, append = TRUE, suffix = "_r") %>% 
  select(mcsid, highest_household_occupation_r) %>% 
  rename("occupational_status" = highest_household_occupation_r)

#2. Parent Education ####
#NVQ levels at Age 3, or 9 Months if Age 3 Na
#NVQ education variable
#NVQ qualifications

#RESPONDENT VARIABLE
#parent education variable 
#first need to identify mother and father figures from main and partner respondents
#do this separately for sweep 1 and 2 as can change between sweeps

#sweep 1 respondent identity
respondent_identity_sweep1 = mcs1_derived %>% select(mcsid, addres00, aelig00) 
mother_respondent_main = respondent_identity_sweep1 %>% filter(aelig00 == 1 & (addres00 == 1 | addres00 == 3 |addres00 ==5|
                                                                                 addres00==7 | addres00 == 9 | addres00 == 11 |
                                                                                 addres00 == 13 |addres00 == 15)) %>% 
  select(mcsid, addres00) %>% 
  rename(mother_mainRespondent = "addres00")

mother_respondent_partner = respondent_identity_sweep1 %>% filter((aelig00 == 2 | aelig00 == 3) & (addres00 == 1 | addres00 == 3 |addres00 ==5|
                                                                                                     addres00==7 | addres00 == 9 | addres00 == 11 |
                                                                                                     addres00 == 13 |addres00 == 15 | addres00 == 21)) %>% 
  select(mcsid, addres00) %>% 
  rename(mother_partnerRespondent = "addres00")

father_respondent_main = respondent_identity_sweep1 %>% filter(aelig00 == 1 & (addres00 == 2 | addres00 == 4 |addres00 ==6|
                                                                                 addres00==8 | addres00 == 10 | addres00 == 12 |
                                                                                 addres00 == 14 |addres00 == 16)) %>% 
  select(mcsid, addres00) %>% 
  rename(father_mainRespondent = "addres00")

father_respondent_partner = respondent_identity_sweep1 %>% filter((aelig00 == 2 | aelig00 == 3) & (addres00 == 2 | addres00 == 4 |addres00 ==6|
                                                                                                     addres00==8 | addres00 == 10 | addres00 == 12 |
                                                                                                     addres00 == 14 |addres00 == 16 | addres00 == 22 | addres00 == 24)) %>% 
  select(mcsid, addres00) %>% 
  rename(father_partnerRespondent = "addres00")

#sweep 2 respondent identity

respondent_identity_sweep2  = mcs2_derived %>% select(mcsid, bddres00, belig00)
#split sweep 2 into original families and families that joined in second sweep. 
respondent_sweep2_original = merge(all=TRUE, respondent_identity_sweep2, sweep_entry, by = "mcsid") %>% 
  filter(sentry == 1)
mother_respondent_main_sweep2_original = respondent_sweep2_original %>% filter(belig00 == 1 & (bddres00 == 1 | bddres00 == 3 |bddres00 ==5|
                                                                                                 bddres00==7 | bddres00 == 9 | bddres00 == 11 |
                                                                                                 bddres00 == 13 |bddres00 == 15)) %>% 
  select(mcsid, bddres00) %>% 
  rename(mother_mainRespondent_2 = "bddres00")

mother_respondent_partner_sweep2_original = respondent_sweep2_original %>% filter((belig00 == 2 | belig00 == 3) & (bddres00 == 1 | bddres00 == 3 |bddres00 ==5|
                                                                                                                     bddres00==7 | bddres00 == 9 | bddres00 == 11 |
                                                                                                                     bddres00 == 13 |bddres00 == 15 | bddres00 == 21)) %>% 
  select(mcsid, bddres00) %>% 
  rename(mother_partnerRespondent_2 = "bddres00")

father_respondent_main_sweep2_original = respondent_sweep2_original%>% filter(belig00 == 1  & (bddres00 == 2 | bddres00 == 4 |bddres00 ==6|
                                                                                                 bddres00==8 | bddres00 == 10 | bddres00 == 12 |
                                                                                                 bddres00 == 14 |bddres00 == 16)) %>% 
  select(mcsid, bddres00) %>% 
  rename(father_mainRespondent_2 = "bddres00")

father_respondent_partner_sweep2_original = respondent_sweep2_original %>% filter((belig00 == 2 | belig00 == 3) & bddres00 == 2 | bddres00 == 4 |bddres00 ==6|
                                                                                    bddres00==8 | bddres00 == 10 | bddres00 == 12 |
                                                                                    bddres00 == 14 |bddres00 == 16 |bddres00 == 18 | bddres00 == 22 | bddres00 == 24) %>% 
  select(mcsid, bddres00) %>% 
  rename(father_partnerRespondent_2 = "bddres00")

#families that joined in second sweep - new families
respondent_sweep2_new = merge(all=TRUE, respondent_identity_sweep2, sweep_entry, by = "mcsid") %>% 
  filter(sentry == 2)
mother_respondent_main_sweep2_new = respondent_sweep2_new %>% filter(belig00 == 1 & (bddres00 == 1 | bddres00 == 3 |bddres00 ==5|
                                                                                       bddres00==7 | bddres00 == 9 | bddres00 == 11 |
                                                                                       bddres00 == 13 |bddres00 == 15)) %>% 
  select(mcsid, bddres00) %>% 
  rename(mother_mainRespondent_2_new = "bddres00")

mother_respondent_partner_sweep2_new = respondent_sweep2_new %>% filter((belig00 == 2 | belig00 == 3) & (bddres00 == 1 | bddres00 == 3 |bddres00 ==5|
                                                                                                           bddres00==7 | bddres00 == 9 | bddres00 == 11 |
                                                                                                           bddres00 == 13 |bddres00 == 15 | bddres00 == 21 )) %>% 
  select(mcsid, bddres00) %>% 
  rename(mother_partnerRespondent_2_new = "bddres00")

father_respondent_main_sweep2_new = respondent_sweep2_new%>% filter(belig00 == 1  & (bddres00 == 2 | bddres00 == 4 |bddres00 ==6|
                                                                                       bddres00==8 | bddres00 == 10 | bddres00 == 12 |
                                                                                       bddres00 == 14 |bddres00 == 16)) %>% 
  select(mcsid, bddres00) %>% 
  rename(father_mainRespondent_2_new = "bddres00")

father_respondent_partner_sweep2_new = respondent_sweep2_new %>% filter((belig00 == 2 | belig00 == 3) & (bddres00 == 2 | bddres00 == 4 |bddres00 ==6|
                                                                                                           bddres00==8 | bddres00 == 10 | bddres00 == 12 |
                                                                                                           bddres00 == 14 |bddres00 == 16 | bddres00 == 18 |bddres00 == 22 | bddres00 == 24)) %>% 
  select(mcsid, bddres00) %>% 
  rename(father_partnerRespondent_2_new = "bddres00")


#parent NVQ at age 3 - for original families
#recode education variables
mcs2_derived$bddnvq00[mcs2_derived$bddnvq00==-1] <- NA
mcs2_derived$bddnvq00[mcs2_derived$bddnvq00==-1] <- NA
mcs2_derived$bddnvq00[mcs2_derived$bddnvq00==95] <- 0 #overseas qual only - convert into an NVQ level 0
mcs2_derived$bddnvq00[mcs2_derived$bddnvq00==96] <- 0

#mother
maternal_NVQ_age3_original_main = mcs2_derived %>% select(mcsid, bddnvq00, belig00) %>% 
  filter(belig00 == 1 & (mcsid %in% mother_respondent_main_sweep2_original$mcsid)) %>% 
  select(mcsid, bddnvq00) %>% 
  rename("main_education" = bddnvq00)
maternal_NVQ_age3_original_partner = mcs2_derived %>% select(mcsid, bddnvq00, belig00) %>% 
  filter((belig00 == 2 |belig00 == 3) & (mcsid %in% mother_respondent_partner_sweep2_original$mcsid)) %>% 
  select(mcsid, bddnvq00) %>% 
  rename("partner_education" = bddnvq00)
maternal_NVQ_age3_original = merge(all=TRUE, maternal_NVQ_age3_original_main, maternal_NVQ_age3_original_partner, by="mcsid") %>% 
  mutate(maternal_education_age3 = case_when(!is.na(main_education) ~ main_education, 
                                             is.na(main_education) ~ partner_education)) %>% 
  select(mcsid, maternal_education_age3)

#father
paternal_NVQ_age3_original_main = mcs2_derived %>%  select(mcsid, bddnvq00, belig00) %>% 
  filter(belig00 ==1 & (mcsid %in% father_respondent_main_sweep2_original$mcsid)) %>% 
  select(mcsid, bddnvq00) %>% 
  rename("main_education" = bddnvq00)
paternal_NVQ_age3_original_partner = mcs2_derived %>%  select(mcsid, bddnvq00, belig00) %>% 
  filter((belig00 == 2 |belig00 == 3) & (mcsid %in% father_respondent_partner_sweep2_original$mcsid)) %>% 
  select(mcsid, bddnvq00) %>% 
  rename("partner_education" = bddnvq00)
paternal_NVQ_age3_original = merge(all=TRUE, paternal_NVQ_age3_original_main,paternal_NVQ_age3_original_partner, by = "mcsid") %>% 
  mutate(paternal_education_age3 = case_when(!is.na(partner_education) ~ partner_education, 
                                             is.na(partner_education) ~ main_education)) %>% 
  select(mcsid, paternal_education_age3)

#original families - parent NVQ at 9 months 
mcs1_derived$addnvq00[mcs1_derived$addnvq00==-1] <- NA
mcs1_derived$addnvq00[mcs1_derived$addnvq00==-1] <- NA
mcs1_derived$addnvq00[mcs1_derived$addnvq00==95] <- 0 #overseas qual only - convert into an NVQ level 0
mcs1_derived$addnvq00[mcs1_derived$addnvq00==96] <- 0

#mother
maternal_NVQ_9months_main = mcs1_derived %>% select(mcsid, addnvq00, aelig00) %>% 
  filter(aelig00 == 1 &(mcsid %in% mother_respondent_main$mcsid)) %>% 
  select(mcsid, addnvq00) %>% 
  rename("main_education" = addnvq00)
maternal_NVQ_9months_partner = mcs1_derived %>% select(mcsid, addnvq00, aelig00) %>% 
  filter((aelig00 == 2 | aelig00 == 3) & (mcsid %in% mother_respondent_partner$mcsid)) %>%
  select(mcsid, addnvq00) %>% 
  rename("partner_education" = addnvq00)
maternal_NVQ_9months = merge(all=TRUE, maternal_NVQ_9months_main, maternal_NVQ_9months_partner, by="mcsid") %>% 
  mutate(maternal_education_9months = case_when(!is.na(main_education) ~ main_education, 
                                                is.na(main_education) ~ partner_education)) %>% 
  select(mcsid, maternal_education_9months)
#father
paternal_NVQ_9months_main = mcs1_derived %>% select(mcsid, addnvq00, aelig00) %>% 
  filter(aelig00 == 1 & (mcsid %in% father_respondent_main$mcsid)) %>% 
  select(mcsid, addnvq00) %>% 
  rename("main_education" = addnvq00)
paternal_NVQ_9months_partner = mcs1_derived %>% select(mcsid, addnvq00, aelig00) %>% 
  filter((aelig00 == 2 | aelig00 == 3) & (mcsid %in% father_respondent_partner$mcsid)) %>% 
  select(mcsid, addnvq00) %>% 
  rename("partner_education" = addnvq00)
paternal_NVQ_9months = merge(all=TRUE,paternal_NVQ_9months_main, paternal_NVQ_9months_partner, by="mcsid") %>% 
  mutate(paternal_education_9months = case_when(!is.na(partner_education) ~ partner_education, 
                                                is.na(partner_education) ~ main_education)) %>% 
  select(mcsid, paternal_education_9months)

#merge and replace NA at age 3 with response at 9 months instead
#mother
maternal_original_nvq = merge(all=TRUE, maternal_NVQ_age3_original, maternal_NVQ_9months, by="mcsid") %>% 
  mutate(maternal_nvq = case_when(!is.na(maternal_education_age3) ~ maternal_education_age3, 
                                  is.na(maternal_education_age3) ~ maternal_education_9months)) %>% 
  select(mcsid,maternal_nvq)
#father
paternal_original_nvq = merge(all=TRUE, paternal_NVQ_age3_original, paternal_NVQ_9months, by="mcsid") %>% 
  mutate(paternal_nvq = case_when(!is.na(paternal_education_age3) ~ paternal_education_age3, 
                                  is.na(paternal_education_age3) ~ paternal_education_9months)) %>% 
  select(mcsid,paternal_nvq)

#families who joined in sweep 2 - NVQ for new families

maternal_NVQ_age3_new_main = mcs2_derived %>% select(mcsid, bddnvq00, belig00) %>% 
  filter(belig00 ==1 &(mcsid %in% mother_respondent_main_sweep2_new$mcsid)) %>% 
  select(mcsid, bddnvq00) %>% 
  rename("main_education" = bddnvq00)
maternal_NVQ_age3_partner = mcs2_derived %>% select(mcsid,  bddnvq00, belig00) %>% 
  filter((belig00 == 2 | belig00 == 3) & (mcsid %in% mother_respondent_partner_sweep2_new$mcsid)) %>% 
  select(mcsid, bddnvq00) %>% 
  rename("partner_education" = bddnvq00)
maternal_NVQ_age3_new = merge(all=TRUE, maternal_NVQ_age3_new_main, maternal_NVQ_age3_partner, by="mcsid") %>% 
  mutate(maternal_education_age3_new = case_when(!is.na(main_education) ~ main_education, 
                                                 is.na(main_education) ~ partner_education)) %>% 
  select(mcsid, maternal_education_age3_new)
#father
paternal_NVQ_age3_new_main = mcs2_derived %>%  select(mcsid, bddnvq00, belig00) %>% 
  filter(belig00 ==1 &(mcsid %in% father_respondent_main_sweep2_new$mcsid)) %>% 
  select(mcsid, bddnvq00) %>% 
  rename("main_education" = bddnvq00)
paternal_NVQ_age3_new_partner = mcs2_derived %>%  select(mcsid,bddnvq00, belig00) %>% 
  filter((belig00 == 2 | belig00 == 3) & (mcsid %in% father_respondent_partner_sweep2_new$mcsid))%>% 
  select(mcsid, bddnvq00) %>% 
  rename("partner_education" = bddnvq00)
paternal_NVQ_age3_new = merge(all=TRUE, paternal_NVQ_age3_new_main, paternal_NVQ_age3_new_partner, by="mcsid") %>% 
  mutate(paternal_education_age3_new = case_when(!is.na(partner_education) ~ partner_education, 
                                                 is.na(partner_education) ~ main_education)) %>% 
  select(mcsid, paternal_education_age3_new)

#combine original and new families together to give 1 overall variable 
#mother
maternal_nvq = merge(all=TRUE, maternal_original_nvq, maternal_NVQ_age3_new, by = "mcsid") %>% 
  mutate(maternal_nvq_variable = case_when(!is.na(maternal_nvq) ~ maternal_nvq, 
                                           is.na(maternal_nvq) ~ maternal_education_age3_new)) %>% 
  select(mcsid, maternal_nvq_variable)
#father
paternal_nvq = merge(all=TRUE, paternal_original_nvq, paternal_NVQ_age3_new, by = "mcsid") %>% 
  mutate(paternal_nvq_variable = case_when(!is.na(paternal_nvq) ~ paternal_nvq, 
                                           is.na(paternal_nvq) ~ paternal_education_age3_new)) %>% 
  select(mcsid, paternal_nvq_variable)

#both parents
parent_nvq = merge(all=TRUE, maternal_nvq, paternal_nvq, by="mcsid") %>% 
  mutate(highest_nvq =pmax(maternal_nvq_variable, paternal_nvq_variable, na.rm=TRUE)) %>% 
  select(mcsid, highest_nvq)

#3. Income ####

#INCOME AT AGE 3. OECD weighted quintiles
#Create OECD equivilisation for Age 3

household_grid_age3 = mcs2_hh %>% select(
  mcsid, bpnum00, bcnum00,bhcrel00,
 bhpage00, bhcrel00, bhpres00) %>% 
  group_by(mcsid)
#add "person" to person number column 
household_grid_age3$bpnum00 = paste0("Person_", household_grid_age3$bpnum00)
#note: person number == 100 -> this person is the first (cnum==1) cohort member, make sure dont count them twice! 
#also case for person number ==200 -> This also refers to cohort member (cnum==2)

#add in binary variables (1 = yes; 2 = no) to indicate whether each person is a child (age <=13) and present in household, or adult and present in household - will total these later to give total number of adults and children in household. 
household_grid_age3 = household_grid_age3 %>% 
  mutate(isChild_andPresent = case_when(bhpage00 <=13 & (bhpres00 == 1 ) ~ 1,
                                        bhpage00 >13 ~ 0, 
                                        is.na(bhpage00) ~ NA_real_, 
                                        TRUE ~ 0), .after = "bhpage00") %>% #think this includes CM too?
  mutate(isAdult_andPresent = case_when(bhpage00 >13 & (bhpres00 == 1 ) ~ 1,
                                        bhpage00 <=13 ~ 0, 
                                        is.na(bhpage00) ~ NA_real_, 
                                        TRUE ~ 0), .after = "bhpage00") 
#select mcsid, person number, and created binary variavles 
household_grid_age3 = household_grid_age3 %>% select(mcsid, bpnum00,  isChild_andPresent, isAdult_andPresent)

#convert household grid variables to wide format- 1 row per mcsid, so that we can use rowSums to create total number of adults and children variables 
household_gridWide_age3 = household_grid_age3 %>% 
  group_by(mcsid, bpnum00) %>% 
  mutate(row=row_number()) %>% 
  pivot_wider(names_from = bpnum00, values_from = c("isChild_andPresent", "isAdult_andPresent")) %>% 
  select(-row) 

#create total number of children and total number of adults in household
household_gridWide_age3 = household_gridWide_age3 %>% 
  ungroup() %>% 
  mutate(number_ofChildren =  rowSums(.[c("isChild_andPresent_Person_1","isChild_andPresent_Person_2" , "isChild_andPresent_Person_3" , "isChild_andPresent_Person_4" , "isChild_andPresent_Person_5", "isChild_andPresent_Person_6" , "isChild_andPresent_Person_7" , "isChild_andPresent_Person_8" , "isChild_andPresent_Person_9" ,"isChild_andPresent_Person_10", "isChild_andPresent_Person_11" ,"isChild_andPresent_Person_12", "isChild_andPresent_Person_13", "isChild_andPresent_Person_14" ,"isChild_andPresent_Person_15", "isChild_andPresent_Person_16", "isChild_andPresent_Person_17","isChild_andPresent_Person_100", "isChild_andPresent_Person_200" )], #person 100 and person 200 refer to cohort members.
                                      na.rm = TRUE), .after ="mcsid") %>% 
  mutate(number_ofAdults =  rowSums(.[c("isAdult_andPresent_Person_1","isAdult_andPresent_Person_2" , "isAdult_andPresent_Person_3" , "isAdult_andPresent_Person_4" , "isAdult_andPresent_Person_5", "isAdult_andPresent_Person_6" , "isAdult_andPresent_Person_7" , "isAdult_andPresent_Person_8" , "isAdult_andPresent_Person_9" ,"isAdult_andPresent_Person_10", "isAdult_andPresent_Person_11" ,"isAdult_andPresent_Person_12", "isAdult_andPresent_Person_13", "isAdult_andPresent_Person_14" ,"isAdult_andPresent_Person_15", "isAdult_andPresent_Person_16", "isAdult_andPresent_Person_17")],
                                    na.rm = TRUE), .after ="mcsid")  %>% 
  mutate(childrenEquivalence = number_ofChildren*0.3, .after = "number_ofAdults") %>% #add equivalence values - each child is 0.3
  mutate(adultEquivalence = case_when(number_ofAdults == 1 ~ 1, #equivalence values for total number of adults
                                      number_ofAdults == 2 ~ 1.5, 
                                      number_ofAdults == 3 ~ 2, 
                                      number_ofAdults == 4 ~ 2.5,
                                      number_ofAdults == 5 ~ 3, 
                                      number_ofAdults == 6 ~ 3.5, 
                                      number_ofAdults == 7 ~ 4, 
                                      number_ofAdults == 8 ~ 4.5, 
                                      number_ofAdults == 9 ~ 5, 
                                      number_ofAdults == 10 ~ 5.5, 
                                      is.na(number_ofAdults) ~ NA_real_), .after = "childrenEquivalence") %>% 
  mutate(total_equivalenceAge3 =  rowSums(.[c("childrenEquivalence", "adultEquivalence")], #create total equivalence value by summing child and adult equivalence values 
                                      na.rm = TRUE), .before = "childrenEquivalence") 
age3_equivalence = household_gridWide_age3 %>% select(mcsid, total_equivalenceAge3)

#Income - Age 3
#Take mid-point of each band and multiply this by the equivilisation score. 
age3_income = mcs2_derived_family %>% select(mcsid, bdhinc00) %>% 
  mutate(annual_incomeMedian = case_when(bdhinc00 == 1 ~ 1650,
                                   bdhinc00 == 2 ~ 7150,
                                   bdhinc00 == 3 ~ 16500, 
                                   bdhinc00 == 4 ~ 27500,
                                   bdhinc00 == 5 ~ 44000,
                                   bdhinc00 == 6 ~ 70000, 
                                   is.na(bdhinc00) ~ NA_real_)) %>% 
  inner_join(age3_equivalence) %>% 
  mutate(oecd_adjusted = annual_incomeMedian/total_equivalenceAge3) %>%  #divide income by the total_equivalence to give equivalised income 
  mutate(oecd_quintilesAge3 = quantcut(oecd_adjusted,5)) %>% 
  select(mcsid, oecd_quintilesAge3)

levels(age3_income$oecd_quintilesAge3)[1] = "1"
levels(age3_income$oecd_quintilesAge3)[2] = "2"
levels(age3_income$oecd_quintilesAge3)[3] = "3"
levels(age3_income$oecd_quintilesAge3)[4] = "4"
levels(age3_income$oecd_quintilesAge3)[5] = "5"

#Income 9 months - replace  with 9 Months if Age 3 NA. 

#Create OECD equivilisation for 9 Months
household_grid_9mo = mcs1_hh %>% select(
  mcsid, apnum00, acnum00,
  ahpage00, ahcrel00, ahpres00) %>% 
  group_by(mcsid)
#add "person" to person number column 
household_grid_9mo$apnum00 = paste0("Person_", household_grid_9mo$apnum00)
#note: person number == 100 -> this person is the first (cnum==1) cohort member, make sure dont count them twice! 
#also case for person number ==200 -> This also refers to cohort member (cnum==2)

#add in binary variables (1 = yes; 2 = no) to indicate whether each person is a child (age <=13) and present in household, or adult and present in household - will total these later to give total number of adults and children in household. 
household_grid_9mo = household_grid_9mo %>% 
  mutate(isChild_andPresent = case_when(ahpage00 <=13 & (ahpres00 == 1 ) ~ 1,
                                        ahpage00 >13 ~ 0, 
                                        is.na(ahpage00) ~ NA_real_, 
                                        TRUE ~ 0), .after = "ahpage00") %>% #think this includes CM too?
  mutate(isAdult_andPresent = case_when(ahpage00 >13 & (ahpres00 == 1 ) ~ 1,
                                        ahpage00 <=13 ~ 0, 
                                        is.na(ahpage00) ~ NA_real_, 
                                        TRUE ~ 0), .after = "ahpage00") 
#select mcsid, person number, and created binary variavles 
household_grid_9mo = household_grid_9mo %>% select(mcsid, apnum00,  isChild_andPresent, isAdult_andPresent)

#convert household grid variables to wide format- 1 row per mcsid, so that we can use rowSums to create total number of adults and children variables 
household_gridWide_9mo = household_grid_9mo %>% 
  group_by(mcsid, apnum00) %>% 
  mutate(row=row_number()) %>% 
  pivot_wider(names_from = apnum00, values_from = c("isChild_andPresent", "isAdult_andPresent")) %>% 
  select(-row) 

#create total number of children and total number of adults in household
household_gridWide_9mo = household_gridWide_9mo %>% 
  ungroup() %>% 
  mutate(number_ofChildren =  rowSums(.[c("isChild_andPresent_Person_1","isChild_andPresent_Person_2" , "isChild_andPresent_Person_3" , "isChild_andPresent_Person_4" , "isChild_andPresent_Person_5", "isChild_andPresent_Person_6" , "isChild_andPresent_Person_7" , "isChild_andPresent_Person_8" , "isChild_andPresent_Person_9" ,"isChild_andPresent_Person_10", "isChild_andPresent_Person_11", "isChild_andPresent_Person_100", "isChild_andPresent_Person_200" )], #person 100 and person 200 refer to cohort members.
                                      na.rm = TRUE), .after ="mcsid") %>% 
  mutate(number_ofAdults =  rowSums(.[c("isAdult_andPresent_Person_1","isAdult_andPresent_Person_2" , "isAdult_andPresent_Person_3" , "isAdult_andPresent_Person_4" , "isAdult_andPresent_Person_5", "isAdult_andPresent_Person_6" , "isAdult_andPresent_Person_7" , "isAdult_andPresent_Person_8" , "isAdult_andPresent_Person_9" ,"isAdult_andPresent_Person_10", "isAdult_andPresent_Person_11")],
                                    na.rm = TRUE), .after ="mcsid")  %>% 
  mutate(childrenEquivalence = number_ofChildren*0.3, .after = "number_ofAdults") %>% #add equivalence values - each child is 0.3
  mutate(adultEquivalence = case_when(number_ofAdults == 1 ~ 1, #equivalence values for total number of adults
                                      number_ofAdults == 2 ~ 1.5, 
                                      number_ofAdults == 3 ~ 2, 
                                      number_ofAdults == 4 ~ 2.5,
                                      number_ofAdults == 5 ~ 3, 
                                      number_ofAdults == 6 ~ 3.5, 
                                      number_ofAdults == 7 ~ 4, 
                                      number_ofAdults == 8 ~ 4.5, 
                                      number_ofAdults == 9 ~ 5, 
                                      number_ofAdults == 10 ~ 5.5, 
                                      is.na(number_ofAdults) ~ NA_real_), .after = "childrenEquivalence") %>% 
  mutate(total_equivalence9mo =  rowSums(.[c("childrenEquivalence", "adultEquivalence")], #create total equivalence value by summing child and adult equivalence values 
                                         na.rm = TRUE), .before = "childrenEquivalence") 
equivalence_9mo = household_gridWide_9mo %>% select(mcsid, total_equivalence9mo)

#Income - 9 Months
#Take mid-point of each band and multiply this by the equivilisation score. 
income_9mo = mcs1_derived_family %>% select(mcsid, adhinc00) %>% 
  mutate(annual_incomeMedian = case_when(adhinc00 == 1 ~ 1549.5,
                                         adhinc00 == 2 ~ 6749.5,
                                         adhinc00 == 3 ~ 15599.5, 
                                         adhinc00 == 4 ~ 25999.5,
                                         adhinc00 == 5 ~ 415999,
                                         adhinc00 == 6 ~ 66000, 
                                         adhinc00 == 96 ~ NA_real_,
                                         adhinc00 == 97 ~ NA_real_,
                                         adhinc00 == -6 ~ NA_real_,
                                         adhinc00 == -1 ~ NA_real_,
                                         is.na(adhinc00) ~ NA_real_)) %>% 
  inner_join(equivalence_9mo) %>% 
  mutate(oecd_adjusted9mo = annual_incomeMedian/total_equivalence9mo) %>%  #divide income by the total_equivalence to give equivalised income 
  mutate(oecd_quintiles9mo = quantcut(oecd_adjusted9mo,5)) %>% 
  select(mcsid, oecd_quintiles9mo)

levels(income_9mo$oecd_quintiles9mo)[1] = "1"
levels(income_9mo$oecd_quintiles9mo)[2] = "2"
levels(income_9mo$oecd_quintiles9mo)[3] = "3"
levels(income_9mo$oecd_quintiles9mo)[4] = "4"
levels(income_9mo$oecd_quintiles9mo)[5] = "5"

#create final variable = Age 3 and 9 Months if NA.
income = age3_income %>% left_join(income_9mo) %>% 
  mutate(income_quintiles = case_when(!is.na(oecd_quintilesAge3) ~ oecd_quintilesAge3, 
                                is.na(oecd_quintilesAge3) ~ oecd_quintiles9mo)) %>% 
  select(mcsid, income_quintiles)
  

#4. Wealth ####
wealth_variables <- mcs5_parent %>%  select(mcsid, eresp00, epmopa00, ephval00, epinvt00, epdeba00, eelig00) %>% 
  filter(eelig00 == 1) %>% 
  select(mcsid, epmopa00, ephval00, epinvt00, epdeba00) %>% 
  rename("mortgage" = epmopa00,
         "houseValue" = ephval00, 
         "savings" = epinvt00, 
         "debt" = epdeba00)

tenure = mcs5_family %>% select(mcsid, eroow00)


new_wealth = merge(all=TRUE, wealth_variables, tenure, by = "mcsid")
#if mortgage missing and tenure = not a home owner --> mortgage = 0 

housing_wealthVars = new_wealth %>% 
  mutate(new_mortgage = case_when(
    !is.na(mortgage) ~ as.numeric(mortgage),
    is.na(mortgage) & eroow00 == 2 | eroow00 == 3 ~ NA_real_,
    (eroow00 != 2 | eroow00 != 3) & (is.na(mortgage))   ~ 0 
  )) %>% 
  mutate(new_value = case_when(
    !is.na(houseValue) ~ as.numeric(houseValue), 
    is.na(mortgage) & eroow00 == 1 | eroow00 == 2 | eroow00 == 3 ~ NA_real_,
    (eroow00 != 1 | eroow00 != 2 | eroow00 != 3) & (is.na(mortgage))   ~ 0
  )) %>% 
  select(mcsid, mortgage, new_mortgage, houseValue, new_value)


#savings variables
savings = mcs5_parent %>% select(mcsid, epinvt00, contains("epsavi0"), eelig00) %>% 
  filter(eelig00 == 1) %>% 
  mutate(new_savings = case_when(
    !is.na(epinvt00) ~ as.numeric(epinvt00), 
    is.na(epinvt00) & epsavi0a == 1 |epsavi0b == 1 |
      epsavi0c == 1 | epsavi0d == 1 | epsavi0e == 1 |
      epsavi0f == 1 | epsavi0g == 1 | epsavi0h == 1 ~ NA_real_, 
    (epsavi0a == 0 |epsavi0b == 0 |
       epsavi0c == 0 | epsavi0d == 0 | epsavi0e == 0 |
       epsavi0f == 0 | epsavi0g == 0 | epsavi0h == 0) & is.na(epinvt00)  ~ 0
  )) %>% 
  select(mcsid, epinvt00, new_savings)

#debt variables 
debt = mcs5_parent %>% select(mcsid, epdeba00, contains("epdebt0"), eelig00) %>% 
  filter(eelig00 == 1) %>% 
  select(-eelig00) %>% 
  mutate(new_debt = case_when(
    !is.na(epdeba00) ~ as.numeric(epdeba00), 
    is.na(epdeba00) & epdebt0a == 1 |epdebt0b == 1 |
      epdebt0c == 1 | epdebt0d == 1 | epdebt0e == 1 |
      epdebt0f == 1 | epdebt0g == 1 | epdebt0h == 1 |
      epdebt0i == 1 ~ NA_real_, 
    (epdebt0a == 0 |epdebt0b == 0 |
       epdebt0c == 0 | epdebt0d == 0 | 
       epdebt0e == 0 | epdebt0f == 0 | 
       epdebt0g == 0 | epdebt0h == 0 | epdebt0i == 0) & is.na(epdeba00)  ~ 0
  )) %>% 
  select(mcsid, epdeba00, new_debt)

wealth_vars = merge(all = TRUE, housing_wealthVars, savings, by = "mcsid")
wealth_vars = merge(all= TRUE, wealth_vars, debt, by = "mcsid")

wealth = wealth_vars %>% select(mcsid, new_mortgage, new_value, new_savings, new_debt)

#5. Relative Neighbourhood Deprivation ####
#IMD at age 3, 9 months if missing
imd_sweep2 = mcs2_geography %>% select(mcsid, bimdscoe, biwimdsc, bisimdsc,bimdscon) %>% 
  mutate(imd_sweep2 =  pmax(bimdscoe, biwimdsc, bisimdsc, bimdscon,  na.rm = TRUE))

imd_sweep1 = mcs1_geography %>% select(mcsid, aimdscoe, aiwimdsc, aisimdsc, aimdscon) %>% 
  mutate(imd_sweep1 =  pmax(aimdscoe, aiwimdsc, aisimdsc, aimdscon,  na.rm = TRUE))

#IMD at age 3 and if NA, replace with 9 months.
imd = merge(all=TRUE, imd_sweep2, imd_sweep1, by="mcsid") %>% 
  select(mcsid, imd_sweep2, imd_sweep1) %>% 
  mutate(imd = case_when(!is.na(imd_sweep2) ~ imd_sweep2, 
                         is.na(imd_sweep2) ~imd_sweep1)) %>% 
  select(mcsid, imd)

#### Potential Confounders ####

# 1. EAL status #### 
#University of London. Institute of Education. Centre for Longitudinal Studies. (2017). Millennium Cohort Study: First Survey, 2001-2003. [data collection]. 12th Edition. UK Data Service. SN: 4683, http://doi.org/10.5255/UKDA-SN-4683-4
#University of London. Institute of Education. Centre for Longitudinal Studies. (2017). Millennium Cohort Study: Second Survey, 2003-2005. [data collection]. 9th Edition. UK Data Service. SN: 5350, http://doi.org/10.5255/UKDA-SN-5350-4
EAL_sweep1 = mcs1_parent_12thEd %>% select(mcsid, ahlang00) 

EAL_sweep2_original = mcs2_parent_9thEd %>% select(mcsid, bhhlan00) %>% 
  merge(all =TRUE, sweep_entry, by = "mcsid") %>% 
  filter(sentry == 1) %>% 
  merge(all=TRUE, EAL_sweep1, by ="mcsid") %>% 
  mutate(EAL_sentry1 = case_when(!is.na(bhhlan00) ~ bhhlan00, #language spoken at home at age 3, if missing, at 9 months. 
                                 is.na(bhhlan00) ~ ahlang00)) %>% 
  select(mcsid, EAL_sentry1)

EAL = mcs2_parent_9thEd %>% select(mcsid, bhhlan00) %>% 
  merge(all =TRUE, sweep_entry, by = "mcsid") %>% 
  filter(sentry == 2) %>% 
  merge(all=TRUE, EAL_sweep2_original, by="mcsid") %>% 
  mutate(EAL = case_when(!is.na(EAL_sentry1) ~ EAL_sentry1, 
                         is.na(EAL_sentry1) ~ bhhlan00)) %>% 
  select(mcsid, EAL) 

#2. Ethnicity####
#ethnicity single births mcs1
ethnicity_sweep1 = mcs1_cm_derived %>% select(mcsid, adc06e00, acnum00) %>% 
  filter(acnum00==1) %>% 
  select(mcsid, adc06e00)
ethnicity_sweep2 = mcs2_cm_derived %>% select(mcsid, bdc06e00, bcnum00) %>% 
  filter(bcnum00==1) %>% 
  #merge(all=TRUE,  sweep_entry, by="mcsid") %>% 
  #filter(sentry == 2) %>% #check if want to do this or just replace sweep 1 NA with sweep 2 responses regardless of sweep entry
  select(mcsid, bdc06e00) %>% 
  merge(all= TRUE, ethnicity_sweep1, by = "mcsid") %>% 
  mutate(ethnicity = case_when(!is.na(adc06e00) ~ adc06e00, 
                               is.na(adc06e00) ~ bdc06e00))
ethnicity = ethnicity_sweep2 %>% select(mcsid, ethnicity)


#3. Sex at birth####
sex_sweep1 = mcs1_hh %>% select(mcsid, ahcsex00,acnum00) %>% 
  filter(acnum00 ==1) %>% 
  select(mcsid, ahcsex00)

sex_sweep2 = mcs2_hh %>% select(mcsid, bhcsex00, bcnum00) %>% 
  filter(bcnum00 == 1) %>% 
  select(mcsid, bhcsex00) %>% 
  #merge(all=TRUE, sweep_entry, by="mcsid") %>% 
  #filter(sentry == 2) %>% 
  select(mcsid, bhcsex00) %>% 
  merge(all=TRUE, sex_sweep1, by = "mcsid") %>% 
  mutate(sex = case_when(!is.na(ahcsex00) ~ ahcsex00,
                         is.na(ahcsex00) ~ bhcsex00))

sex = sex_sweep2 %>% select(mcsid, sex)

#### Auxiliary Variables for Imputation ####

#1. Mother's age at birth of CM ####
#sweep 1 - creating mother respondent variables
age_atBirth_sweep2_motherMain = mcs2_derived %>% select(mcsid, bddagb00, bddres00,belig00) %>% 
  filter(belig00 == 1 & (mcsid %in% mother_respondent_main_sweep2_original$mcsid)) %>% 
  select(mcsid, bddagb00) %>% 
  rename("main_birthAge" = bddagb00)


age_atBirth_sweep2_motherPartner = mcs2_derived %>% select(mcsid, bddagb00, bddres00,belig00) %>% 
  filter((belig00 == 2 |belig00 == 3) & (mcsid %in% mother_respondent_partner_sweep2_original$mcsid)) %>% 
  select(mcsid, bddagb00) %>% 
  rename("partner_birthAge" = bddagb00)

age_atBirth_sweep2_original = merge(all=TRUE, age_atBirth_sweep2_motherMain, age_atBirth_sweep2_motherPartner, by="mcsid") %>% 
  mutate(age_atBirth_sweep2 = case_when(!is.na(main_birthAge) ~main_birthAge, 
                                        is.na(main_birthAge) ~ partner_birthAge))

#new families
age_atBirth_sweep2_motherMain_new = mcs2_derived %>% select(mcsid, bddagb00, bddres00,belig00) %>% 
  filter(belig00 == 1 & (mcsid %in% mother_respondent_main_sweep2_new$mcsid)) %>% 
  select(mcsid, bddagb00) %>% 
  rename("main_birthAge_new" = bddagb00)


age_atBirth_sweep2_motherPartner_new = mcs2_derived %>% select(mcsid, bddagb00, bddres00,belig00) %>% 
  filter((belig00 == 2 |belig00 == 3) & (mcsid %in% mother_respondent_partner_sweep2_new$mcsid)) %>% 
  select(mcsid, bddagb00) %>% 
  rename("partner_birthAge_new" = bddagb00)

age_atBirth_sweep2_newFamilies = merge(all=TRUE, age_atBirth_sweep2_motherMain_new, age_atBirth_sweep2_motherPartner_new, by="mcsid") %>% 
  mutate(age_atBirth_sweep2_new = case_when(!is.na(main_birthAge_new) ~main_birthAge_new, 
                                            is.na(main_birthAge_new) ~ partner_birthAge_new))

#combine new entry families with original families
age_atBirth_sweep2 = merge(all=TRUE, age_atBirth_sweep2_original, age_atBirth_sweep2_newFamilies, by="mcsid") %>% 
  mutate(sweep2_birthAge = case_when(!is.na(age_atBirth_sweep2) ~ age_atBirth_sweep2, 
                                     is.na(age_atBirth_sweep2) ~age_atBirth_sweep2_new)) %>% 
  select(mcsid, sweep2_birthAge)



#sweep 1
#main
age_atBirth_main_sweep1 = mcs1_derived %>% select(mcsid, addagb00, addres00, aelig00) %>% 
  filter(aelig00 == 1 & (mcsid %in% mother_respondent_main$mcsid)) %>% 
  select(mcsid, addagb00) %>% 
  rename("main_birthAge" = addagb00)

#partner
age_atBirth_partner_sweep1 = mcs1_derived %>% select(mcsid, addagb00, addres00, aelig00) %>% 
  filter((aelig00 == 2 | aelig00 == 3) & (mcsid %in% mother_respondent_partner$mcsid)) %>% 
  select(mcsid, addagb00) %>% 
  rename("partner_birthAge" = addagb00)
#merge
age_atBirth_sweep1 = merge(all=TRUE, age_atBirth_main_sweep1, age_atBirth_partner_sweep1, by="mcsid") %>% 
  mutate(age_atBirth_sweep1 = case_when(!is.na(main_birthAge) ~ main_birthAge,
                                        is.na(main_birthAge) ~partner_birthAge)) %>% 
  select(mcsid,age_atBirth_sweep1 )

#combine sweeps
age_atBirth = merge(all=TRUE, age_atBirth_sweep1, age_atBirth_sweep2, by="mcsid") %>% 
  mutate(age_atBirth = case_when(!is.na(age_atBirth_sweep1) ~ age_atBirth_sweep1,
                                 is.na(age_atBirth_sweep1) ~ sweep2_birthAge, 
                                 is.na(sweep2_birthAge) ~ age_atBirth_sweep1, 
                                 TRUE ~ NA_real_)) %>% 
  select(mcsid, age_atBirth)



#2. Housing tenure at Age 3 ####
#housing tenure at age 3, replace with 9 months if missing
tenure_sweep1 = mcs1_derived_family %>% select(mcsid, adroow00) 
tenure_sweep2 = mcs2_derived_family %>% select(mcsid, bdroow00) %>% 
  merge(all=TRUE, tenure_sweep1, by="mcsid") %>% 
  mutate(housing_tenure = case_when(!is.na(bdroow00) ~ bdroow00,
                                    is.na(bdroow00) ~ adroow00)) %>% 
  merge(all=TRUE, sweep_entry,by="mcsid") 

tenure = tenure_sweep2 %>% select(mcsid, housing_tenure)

tenure[tenure==1] <-1
tenure[tenure==2] <-1
tenure[tenure==3] <-2
tenure[tenure==4] <-2
tenure[tenure==5] <-2
tenure[tenure==6] <-2
tenure[tenure==7] <-3
tenure[tenure==8] <-4
tenure[tenure==9] <-5
tenure[tenure==10] <-5

#3. Accommodation type at age 3, replace with sweep 1 if NA ####
accommodation_sweep2 = mcs2_parent %>% select(mcsid, bpmotm00, belig00) %>% 
  filter (belig00 == 1)

accommodation_sweep1 = mcs1_parent %>% select(mcsid, apmoty00, aelig00) %>% 
  filter (aelig00 == 1) %>% 
  merge(all=TRUE, accommodation_sweep2, by="mcsid") %>% 
  merge(all=TRUE, sweep_entry, by="mcsid") 
accommodation_sweep1$accommodation_type = 
  ifelse(!is.na(accommodation_sweep1$apmoty00),accommodation_sweep1$apmoty00, accommodation_sweep1$bpmotm00)  

accommodation = accommodation_sweep1  %>% select(mcsid, accommodation_type)
#recode values 
accommodation[accommodation == 85] <- NA
accommodation[accommodation == 86] <- NA  
accommodation[accommodation ==-1:-9] <- NA
accommodation[accommodation == 98] <- NA
accommodation[accommodation == 99] <- NA
accommodation[accommodation == 95] <- NA

accommodation[accommodation==1] <-1
accommodation[accommodation==2] <-2
accommodation[accommodation==3] <-2
accommodation[accommodation==4] <-3

#4. Whether CM breastfed ####
breastfed_sweep1 = mcs1_cm_parent %>% select(mcsid, acbfev00, acbfem00,aelig00, acnum00) %>% 
  filter(acnum00 == 1) %>% 
  filter(aelig00 == 1) %>% 
  merge(all=TRUE, sweep_entry, by="mcsid") %>% 
  filter(sentry ==1) %>% 
  select(mcsid, acbfev00, acbfem00)

breastfed = mcs2_cm_parent %>% select(mcsid, bpbfmt00, belig00, bcnum00) %>% 
  filter(bcnum00 ==1) %>% 
  filter(belig00 ==1) %>% 
  merge(all=TRUE, sweep_entry, by="mcsid") %>% 
  filter(sentry ==2) %>% 
  select(mcsid, bpbfmt00) %>% 
  merge(all=TRUE, breastfed_sweep1, by="mcsid") %>% 
  mutate(cm_breastfed = case_when(acbfev00 == 1 ~ 1, 
                                  acbfev00 == 2 ~ 2, 
                                  bpbfmt00 == 0 ~ 2, 
                                  bpbfmt00 > 0 ~ 1,
                                  is.na(acbfev00)  ~ NA_real_,
                                  is.na(bpbfmt00) ~ NA_real_)) %>% 
  select(mcsid, cm_breastfed)

#ACBFEV00 ==1, breastfed = 1 (yes)
#ACBFEV00 ==2, breastfed = 2 (no)
#bpbfmt00 ==0, breastfed = 2 (no) as month last breastfed = 0
#bpbfmt00 = any other number, breastfed = 1
#bpbfmt00 = NA, breastfed = NA


#5. Number of parents present in household at age 3; replace with 9 months if NA####
carers_in_hh_s1 = mcs1_derived_family %>% select(mcsid,adhtys00)
carers_in_hh = mcs2_derived_family %>% select(mcsid,bdhtys00) %>% 
  merge(all=TRUE, carers_in_hh_s1, by="mcsid") %>% 
  mutate(carers_in_hh = case_when(!is.na(bdhtys00)~bdhtys00,
                                  is.na(bdhtys00)~adhtys00)) %>% 
  select(mcsid, carers_in_hh)

#### Create analysis data ####
analysis_data = merge(all = TRUE, sex, ethnicity, by = "mcsid")
nrow(analysis_data)
analysis_data = merge(all=TRUE, analysis_data, EAL, by = "mcsid")
nrow(analysis_data)
analysis_data = merge(all=TRUE, analysis_data, age_atBirth, by = "mcsid")
nrow(analysis_data)
analysis_data = merge(all=TRUE, analysis_data, tenure, by = "mcsid")
nrow(analysis_data)
analysis_data = merge(all=TRUE, analysis_data, accommodation, by = "mcsid")
nrow(analysis_data)
analysis_data = merge(all=TRUE, analysis_data, parent_nvq, by = "mcsid")
nrow(analysis_data)
analysis_data = merge(all= TRUE, analysis_data, breastfed, by = "mcsid")
nrow(analysis_data)
analysis_data = merge(all=TRUE, analysis_data, carers_in_hh, by = "mcsid")
nrow(analysis_data)
analysis_data = merge(all=TRUE, analysis_data, income, by = "mcsid")
nrow(analysis_data)
analysis_data = merge(all=TRUE, analysis_data, imd, by = "mcsid")
nrow(analysis_data)
analysis_data = merge(all=TRUE, analysis_data, occupational_status, by = "mcsid")
nrow(analysis_data)
analysis_data = merge(all=TRUE, analysis_data, wealth, by = "mcsid")
nrow(analysis_data)
analysis_data = merge(all=TRUE, analysis_data, age3_vocab, by = "mcsid")
nrow(analysis_data)
analysis_data = merge(all=TRUE, analysis_data, age5_vocab, by = "mcsid")
nrow(analysis_data)
analysis_data = merge(all=TRUE, analysis_data, age11_vocab, by="mcsid")
nrow(analysis_data)
analysis_data = merge(all=TRUE, analysis_data, age14_vocab, by = "mcsid")
nrow(analysis_data)
analysis_data = merge(all=TRUE, analysis_data, mcs_weight, by = "mcsid")
nrow(analysis_data)


# Select anyone who has a response on at least one Vocabulary measure
mcs_analysis = analysis_data[!is.na(analysis_data$age3_vocab) | 
!is.na(analysis_data$age5_vocab) | 
  !is.na(analysis_data$age11_vocab) | 
  !is.na(analysis_data$age14_vocab) ,]

# Rename columns 
names(mcs_analysis) <- c("mcsid", "gender", "ethnicity", "language_used_at_home", 
                         "mothers_age", "housing_tenure", "accommodation_type", "highest_NVQ", 
                         "cm_breastfed", "parents_in_house",  "income_quintiles", "imd" , 
                         "occupational_status",   "mortgage", "house_value","savings", "total_debt", 
                          "vocabulary_age3", "vocabulary_age5",  "vocabulary_age11",  
                         "vocabulary_age14", "mcs2_weight")
# Save CSV file 
write.csv(mcs_analysis, file = "SES_data.csv")

#### Sensitivity Checks ####

#1. Vocabulary complete cases for each age ####
age3_complete_cases = analysis_data[!is.na(analysis_data$age3_vocab) ,]
age5_complete_cases = analysis_data[!is.na(analysis_data$age5_vocab) ,]
age11_complete_cases = analysis_data[!is.na(analysis_data$age11_vocab) ,]
age14_complete_cases = analysis_data[!is.na(analysis_data$age14_vocab) ,]


# Save complete cases as csv files ####
write.csv(age3_complete_cases, file = "age3_ses_cc.csv")
write.csv(age5_complete_cases, file = "age5_ses_cc.csv")
write.csv(age11_complete_cases, file = "age11_ses_cc.csv")
write.csv(age14_complete_cases, file = "age14_ses_cc.csv")

#2. Wealth : at least 1 wealth variable present. 
wealth_sensitivity1 <- analysis_data %>% 
  filter(!is.na(new_mortgage) | !is.na(new_value) | !is.na(new_debt) | !is.na(new_savings))

#3. Wealth at least 2 conditions satisfied
#conditions for wealth sensitivity = response to at least 2 (>=2) of wealth variables.
#these are: mortgage, house value, savings, debt. 
wealth_sensitivity2 <- analysis_data %>% 
  filter(as.numeric(!is.na(new_savings)) + as.numeric(!is.na(new_mortgage)) +
           as.numeric(!is.na(new_debt)) + as.numeric(!is.na(new_mortgage)) >=2)

# Save CSV files 
write.csv(wealth_sensitivity1, file = "wealth_sensitivity1.csv")
write.csv(wealth_sensitivity2, file = "wealth_sensitivity2.csv")


