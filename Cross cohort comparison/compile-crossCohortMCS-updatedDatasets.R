#SEC Inequalities in Vocabualry - MCS data compilation code for cross-cohort comparison (with new edition data)

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
library(lubridate)

#### Load in data ####
mcs2_child_assessment <- read_sav("mcs2_cm_cognitive_assessment.sav")
mcs3_child_assessment <- read_sav("mcs3_cm_cognitive_assessment.sav")
mcs5_child_assessment <- read_sav("mcs5_cm_cognitive_assessment.sav")
mcs5_child_assessment4thEd <- read_sav("mcs5_cm_assessment-4thEd.sav")
mcs5_child_derived <- read_sav("mcs5_cm_derived.sav")
mcs6_child_assessment<- read_sav("mcs6_cm_cognitive_assessment.sav")
mcs1_hh <- read_sav("mcs1_hhgrid.sav")
mcs2_hh <- read_sav("mcs2_hhgrid.sav")
mcs3_hh <- read_sav("mcs3_hhgrid.sav")
mcs5_hh <- read_sav("mcs5_hhgrid.sav")
mcs_family <- read_sav("mcs_longitudinal_family_file.sav")
mcs1_parent <- read_sav("mcs1_parent_interview.sav")
mcs1_derived <- read_sav("mcs1_parent_derived.sav")
mcs2_parent <- read_sav("mcs2_parent_interview.sav")
mcs2_derived <- read_sav("mcs2_parent_derived.sav")
mcs2_derived_family <- read_sav("mcs2_family_derived.sav")
mcs1_derived_family <- read_sav("mcs1_family_derived.sav")
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
mcs3_derived_parent = read_sav("mcs3_parent_derived.sav")
mcs3_derived_family = read_sav("mcs3_family_derived.sav")
#mcs3_derived_7thEd = read_sav("mcs3_derived_variables-7thEd.sav")
mcs3_parent <- read_sav("mcs3_parent_interview.sav")
#### Convert all to lowercase ####
names(mcs2_child_assessment) <- tolower(names(mcs2_child_assessment))
names(mcs3_child_assessment) <- tolower(names(mcs3_child_assessment))
names(mcs5_child_assessment) <- tolower(names(mcs5_child_assessment))
names(mcs6_child_assessment) <- tolower(names(mcs6_child_assessment))
names(mcs_family) <- tolower(names(mcs_family))
names(mcs5_parent) <- tolower(names(mcs5_parent))
names(mcs5_child_derived) <- tolower(names(mcs5_child_derived))
names(mcs1_parent) <- tolower(names(mcs1_parent))
names(mcs2_parent) <- tolower(names(mcs2_parent))
names(mcs2_derived) <- tolower(names(mcs2_derived))
names(mcs2_derived_family) <- tolower(names(mcs2_derived_family))
names(mcs1_derived) <- tolower(names(mcs1_derived))
names(mcs1_derived_family) <- tolower(names(mcs1_derived_family))
names(mcs1_hh) <- tolower(names(mcs1_hh))
names(mcs2_hh) <- tolower(names(mcs2_hh))
names(mcs3_hh) <- tolower(names(mcs3_hh))
names(mcs5_hh) <- tolower(names(mcs5_hh))
names(mcs5_family) <- tolower(names(mcs5_family))
names(mcs2_geography) <- tolower(names(mcs2_geography))
names(mcs1_geography) <- tolower(names(mcs1_geography))
names(mcs1_parent_12thEd) <- tolower(names(mcs1_parent_12thEd))
names(mcs2_parent_9thEd) <- tolower(names(mcs2_parent_9thEd))
names(mcs1_cm_derived) <- tolower(names(mcs1_cm_derived))
names(mcs2_cm_derived) <- tolower(names(mcs2_cm_derived))
names(mcs1_cm_parent) <- tolower(names(mcs1_cm_parent))
names(mcs2_cm_parent) <- tolower(names(mcs2_cm_parent))
names(mcs5_child_assessment4thEd) <- tolower(names(mcs5_child_assessment4thEd))
names(mcs3_derived_parent) <- tolower(names(mcs3_derived_parent))
names(mcs3_derived_family) <- tolower(names(mcs3_derived_family))
#names(mcs3_derived_7thEd) <- tolower(names(mcs3_derived_7thEd))
names(mcs3_parent) <- tolower(names(mcs3_parent))

#### Create weight variable ####

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

#attrition and sample  weight age 14 sweep 
mcs6_weight <- c("mcsid", "fovwt2")
mcs6_weight <- mcs_family[mcs6_weight]
mcs6_weight [mcs6_weight  ==-1] <- NA

#overall weight
weight2 <- merge(all=TRUE, mcs_weight, mcs6_weight,by="mcsid")
weight2$weight2 <- ifelse(!is.na(weight2$weight1), weight2$weight1, weight2$fovwt2)
mcs_weight2 <- c("mcsid", "weight2")
mcs_weight2 <- weight2[mcs_weight2]

#create sweep entry variable to identify second entry families later####
sweep_entry <- c("mcsid", "sentry")
sweep_entry <- mcs_family[sweep_entry]
sweep_entry$sentry = as.character(sweep_entry$sentry)

#### Respondent Key ####
respondent_key = c(`1` = "main",
                   `2` = "partner", 
                   `3` = "partner_proxy",
                   `4` = "not_eligible")


#### Vocabulary ####

# Age 5 - Naming Vocabulary Ability Score
age5_vocab = mcs3_child_assessment %>% select(mcsid, ccnvabil, ccnum00, ccstnv00, chcage00) %>% 
  filter(ccnum00 == 1) %>% 
  filter(!ccstnv00 == 2) %>% #filter out those who didnt start vocab test and were given incorrect score of 20 in data
  select(mcsid, ccnvabil, chcage00) %>% 
  filter(!is.na(ccnvabil)) %>% 
  rename(age5_vocab = ccnvabil) %>% 
  replace_with_na(replace = list(chcage00 = c(-8, -1, -9, 98, 99))) %>% 
  mutate(age_in_days = days(chcage00)) %>% 
  mutate(age5_years = as.period(age_in_days,unit="days")/years(1)) %>% 
  select(mcsid, age5_vocab, age5_years)

#Age 11 - Verbal Similarities T-Scores
# age information missing from latest edition of data, take this from previous edition (4th Edition).

age11_vocab = mcs5_child_derived %>% select(mcsid, evsabil, ecnum00) %>% 
  filter(ecnum00 == 1) %>% 
  select(mcsid, evsabil) %>% 
  filter(!is.na(evsabil)) %>% 
  rename(age11_vocab = evsabil) %>% 
  left_join(mcs5_child_assessment4thEd) %>% 
  filter(ecnum00 == 1) %>% 
  select(mcsid,age11_vocab, eccage00,eccdbm00,eccdby00,ecintm00,ecinty00) %>% 
  unite("cm_age_sweep5", c(eccdbm00, eccdby00), sep =" ", remove = TRUE, na.rm = TRUE) %>% 
  unite("interview_date", c(ecintm00, ecinty00), sep =" ", remove = TRUE, na.rm = TRUE) 

age11_vocab$cm_age_sweep5 = my(age11_vocab$cm_age_sweep5)
age11_vocab$interview_date = my(age11_vocab$interview_date)

age11_vocab = age11_vocab %>% 
  mutate("cm_age11" = as.period(interval(start = cm_age_sweep5, end = interview_date))$year) %>% 
  select(mcsid, age11_vocab, cm_age11)

# Age 14 Vocabulary
age14_vocab = mcs6_child_assessment %>% select(mcsid, contains("fccmcog"), fcvtypdo, fcvtcdck, fcnum00, 
                                               fccage00) %>% 
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
  rename("cm_age14" = fccage00) %>% 
  select(mcsid, age14_vocab, cm_age14)

### Socioeconomic Status Variables ####

#1. Occupational Status ####
# HIGHEST HOUSEHOLD LEVEL AT AGE 5. IF NA, HIGHEST HOUSEHOLD LEVEL AT AGE 3. IF NA, HIGHEST HOUSEHOLD LEVEL AT AGE 9 MONTHS.
#Age 5
age5_occupation = mcs3_derived_parent %>% select(mcsid, cdd05s00, celig00) %>% 
  mutate(occupation = case_when(cdd05s00 ==1 ~1, #recode to be 3 level variable. 
                                cdd05s00 ==2 ~2,  
                                cdd05s00 ==3 ~2, 
                                cdd05s00 ==4 ~3, 
                                cdd05s00 ==5 ~3))
age5_occupation$celig00 = as.factor(age5_occupation$celig00)
age5_occupation$celig00 = recode(age5_occupation$celig00, !!!respondent_key)

age5_occupation_wide = age5_occupation %>% select(!cdd05s00) %>% 
  group_by(mcsid) %>%
  pivot_wider(names_from = celig00, values_from = occupation) %>% 
  rename("main_occupation" = main, 
         "partner_occupation" = partner, 
         "proxy_partner_occupation" = partner_proxy) %>% 
  mutate(partner_occupation = case_when(!is.na(partner_occupation) ~ partner_occupation, 
                                        is.na(partner_occupation) ~ proxy_partner_occupation)) %>% 
  mutate(highest_occupation_age5 = pmin(main_occupation, partner_occupation, na.rm = TRUE)) %>% 
  select(mcsid, highest_occupation_age5)

#add unemployment as 4th category  age 5 
#age5_employment = mcs3_derived_family %>% select(mcsid, cdcwrk00)
#age5_occupation = merge(all=TRUE, age5_occupation_wide, age5_employment, by="mcsid")
#age5_occupation[is.na(age5_occupation$highest_occupation_age5) & age5_occupation$cdcwrk00 %in% c(4,6,10),]$highest_occupation_age5 = 4   #this code throws up the following error Error in `$<-.data.frame`(`*tmp*`, highest_occupation_age5, value = 4): replacement has 1 row, data has 0 - assume there are no cases that apply. 

#Age 3
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
  mutate(highest_occupation_age3 = pmin(main_occupation, partner_occupation, na.rm = TRUE)) %>% 
  select(mcsid, highest_occupation_age3)

#add unemployment as 4th category age 3
age3_employment = mcs2_derived_family %>% select(mcsid, bdcwrk00)
age3_occupation = merge(all=TRUE, age3_occupation_wide, age3_employment, by="mcsid")
age3_occupation[is.na(age3_occupation$highest_occupation_age3) & age3_occupation$bdcwrk00 %in% c(4,6,10),]$highest_occupation_age3= 4

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
  mutate(highest_occupation_9mo = pmin(main_occupation, partner_occupation, na.rm = TRUE)) %>% 
  select(mcsid, highest_occupation_9mo)

months9_employment = mcs1_derived_family %>% select(mcsid, adcwrk00)
months9_occupation = merge(all=TRUE, months9_occupation_wide, months9_employment, by="mcsid")
months9_occupation[is.na(months9_occupation$highest_occupation_9mo) & months9_occupation$adcwrk00 %in% c(4,6,10),]$highest_occupation_9mo = 4


#replace NA at age 5 with age 3 and NA at age 3 with values from 9 months
occupational_status = merge(all=TRUE, age5_occupation_wide, age3_occupation, by = "mcsid")
occupational_status = merge(all=TRUE, occupational_status,months9_occupation) %>% 
  select(mcsid, highest_occupation_age5, highest_occupation_age3, highest_occupation_9mo) %>% 
  mutate(highest_household_occupation = case_when(!is.na(highest_occupation_age5) ~ highest_occupation_age5, 
                                                  is.na(highest_occupation_age5) & 
                                                    !is.na(highest_occupation_age3) ~ highest_occupation_age3,
                                                  is.na(highest_occupation_age5) & is.na(highest_occupation_age3) ~
                                                    highest_occupation_9mo)) %>% 
  select(mcsid, highest_household_occupation) %>% 
  rec(highest_household_occupation,  rec = "1=4; 2=3; 3=2; 4=1", #reverse code variable
      as.num = TRUE, var.label = NULL, 
      val.labels = NULL, append = TRUE, suffix = "_r") %>% 
  select(mcsid, highest_household_occupation_r) %>% 
  rename("occupational_status" = highest_household_occupation_r)

# 2. Income ####

#INCOME AT AGE 11. OECD weighted quintiles
#Create OECD equivilisation for Age 11
#household grid

household_grid = mcs5_hh %>% select(
  mcsid, epnum00, ecnum00,
  bchk0000, 
  epage0000, ecrel0000, 
  ecage0000, epres0000, ecful0000) %>% 
  group_by(mcsid)
#add "person" to person number column 
household_grid$epnum00 = paste0("Person_", household_grid$epnum00)
#note: person number == 100 -> this person is the first (cnum==1) cohort member, make sure dont count them twice! 
#also case for person number ==200 -> This also refers to cohort member (cnum==2)

#add in binary variables (1 = yes; 2 = no) to indicate whether each person is a child (age <=13) and present in household, or adult and present in household - will total these later to give total number of adults and children in household. 
household_grid = household_grid %>% 
  mutate(isChild_andPresent = case_when(epage0000 <=13 & (epres0000 == 1 | bchk0000 == 1) ~ 1,
                                        epage0000 >13 ~ 0, 
                                        is.na(epage0000) ~ NA_real_, 
                                        TRUE ~ 0), .after = "epage0000") %>% #think this includes CM too?
  mutate(isAdult_andPresent = case_when(epage0000 >13 & (epres0000 == 1 | bchk0000 == 1) ~ 1,
                                        epage0000 <=13 ~ 0, 
                                        is.na(epage0000) ~ NA_real_, 
                                        TRUE ~ 0), .after = "epage0000") 
#select mcsid, person number, and created binary variavles 
household_grid = household_grid %>% select(mcsid, epnum00,  isChild_andPresent, isAdult_andPresent)

#convert CM age at last birthday to Na if = -1 (will be missing as will be replicated for each person in HH)
#household_grid$ecage0000[household_grid$ecage0000== -1] <- NA

#convert household grid variables to wide format- 1 row per mcsid, so that we can use rowSums to create total number of adults and children variables 
household_gridWide = household_grid %>% 
  group_by(mcsid, epnum00) %>% 
  mutate(row=row_number()) %>% 
  pivot_wider(names_from = epnum00, values_from = c("isChild_andPresent", "isAdult_andPresent")) %>% 
  select(-row) 

#create total number of children and total number of adults in household
household_gridWide_age11 = household_gridWide %>% 
  ungroup() %>% 
  mutate(number_ofChildren =  rowSums(.[c("isChild_andPresent_Person_1","isChild_andPresent_Person_2" , "isChild_andPresent_Person_3" , "isChild_andPresent_Person_4" , "isChild_andPresent_Person_5", "isChild_andPresent_Person_6" , "isChild_andPresent_Person_7" , "isChild_andPresent_Person_8" , "isChild_andPresent_Person_9" ,"isChild_andPresent_Person_10", "isChild_andPresent_Person_11" ,"isChild_andPresent_Person_12", "isChild_andPresent_Person_13", "isChild_andPresent_Person_14" ,"isChild_andPresent_Person_15", "isChild_andPresent_Person_16", "isChild_andPresent_Person_17", "isChild_andPresent_Person_18", "isChild_andPresent_Person_19", "isChild_andPresent_Person_20", "isChild_andPresent_Person_21", "isChild_andPresent_Person_22","isChild_andPresent_Person_100", "isChild_andPresent_Person_200" )], #person 100 and person 200 refer to cohort members.
                                      na.rm = TRUE), .after ="mcsid") %>% 
  mutate(number_ofAdults =  rowSums(.[c("isAdult_andPresent_Person_1","isAdult_andPresent_Person_2" , "isAdult_andPresent_Person_3" , "isAdult_andPresent_Person_4" , "isAdult_andPresent_Person_5", "isAdult_andPresent_Person_6" , "isAdult_andPresent_Person_7" , "isAdult_andPresent_Person_8" , "isAdult_andPresent_Person_9" ,"isAdult_andPresent_Person_10", "isAdult_andPresent_Person_11" ,"isAdult_andPresent_Person_12", "isAdult_andPresent_Person_13", "isAdult_andPresent_Person_14" ,"isAdult_andPresent_Person_15", "isAdult_andPresent_Person_16", "isAdult_andPresent_Person_17", "isAdult_andPresent_Person_18", "isAdult_andPresent_Person_19", "isAdult_andPresent_Person_20", "isAdult_andPresent_Person_21", "isAdult_andPresent_Person_22")],
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
  mutate(total_equivalenceage11 =  rowSums(.[c("childrenEquivalence", "adultEquivalence")], #create total equivalence value by summing child and adult equivalence values 
                                           na.rm = TRUE), .before = "childrenEquivalence") 


age11_equivalence = household_gridWide_age11 %>% select(mcsid, total_equivalenceage11)

#Income
#Take mid-point of each band and multiply this by the equivilisation score. 
age11_income = mcs5_parent%>% select(mcsid, epntco00,epntlp00, eresp00) %>% 
  mutate(respondent = case_when( eresp00 == 1 ~ "main", eresp00 == 2 ~ "partner")) %>% 
  select(mcsid, epntco00, epntlp00, respondent)
age11_income[,2:3] <- lapply(age11_income[,2:3], as.numeric)
age11_income = age11_income %>% 
  mutate(annual_incomeMedianJoint = case_when(
    epntco00 == 2 ~ 1500,  epntco00 == 3 ~ 5000, epntco00 == 4 ~ 8750, epntco00 == 5 ~ 11500, 
    epntco00 == 6 ~ 13250, epntco00 == 7 ~ 14500, epntco00 == 8 ~ 17250,  epntco00 == 9 ~ 21500,  epntco00 == 10 ~ 25500,
    epntco00 == 11 ~ 29000, epntco00 == 12 ~ 32500,  epntco00 == 13 ~ 37250,  epntco00 == 14 ~ 44250, epntco00 == 15 ~ 51000, 
    epntco00 == 16 ~ 58500, epntco00 == 17 ~ 73000,  epntco00 == 18 ~ 99000, epntco00 == 19 ~ 132500, epntco00 == 20 ~ 150000, 
    epntco00 == -3 | epntco00 == -1 ~ NA_real_, 
    is.na(epntco00) ~ NA_real_)) %>% 
  mutate(annual_incomeMedianLone = case_when(
    epntlp00 == 2 ~ 500, epntlp00 == 3 ~ 4000, epntlp00 == 4 ~ 8250, epntlp00 == 5 ~ 10500,
    epntlp00 == 6 ~ 11750, epntlp00 == 7 ~ 12500, epntlp00 == 8 ~ 14500, epntlp00 == 9 ~ 17500, epntlp00 == 10 ~ 20750,
    epntlp00 == 11 ~ 24250, epntlp00 == 12 ~ 27750, epntlp00 == 13 ~ 32000, epntlp00 == 14 ~ 39000, epntlp00 == 15 ~ 46000,
    epntlp00 == 16 ~ 51750, epntlp00 == 17 ~ 63500, epntlp00 == 18 ~ 88000, epntlp00 == 19 ~ 107000, epntlp00 == 20 ~ 110000,
    epntlp00 == -3 | epntlp00 == -1 ~ NA_real_, 
    is.na(epntlp00) ~ NA_real_)) 

#OECD equivilisation 
age11income_wide = age11_income %>% 
  group_by(mcsid, respondent) %>% 
  mutate(row=row_number()) %>% 
  pivot_wider(names_from = respondent, values_from = c("epntco00", "annual_incomeMedianJoint", "annual_incomeMedianLone")) %>% 
  select(-row) %>% 
  ungroup() %>% 
  mutate(annual_incomeJoint = case_when(!is.na(annual_incomeMedianJoint_main) ~ annual_incomeMedianJoint_main, 
                                        is.na(annual_incomeMedianJoint_main) ~ annual_incomeMedianJoint_partner)) %>% 
  mutate(annual_income = case_when(!is.na(annual_incomeJoint)  ~ annual_incomeJoint, 
                                   is.na(annual_incomeJoint)  ~ annual_incomeMedianLone_main)) %>% 
  select(mcsid, annual_income) %>% 
  inner_join(age11_equivalence) %>% 
  mutate(oecd_adjusted = annual_income/total_equivalenceage11) %>%  #divide income by the total_equivalence to give equivalised income 
  mutate(income_quintiles = quantcut(oecd_adjusted,5)) %>% 
  select(mcsid, income_quintiles) %>% 
  distinct(mcsid, .keep_all =  TRUE)

levels(age11income_wide$income_quintiles)[1] = "1"
levels(age11income_wide$income_quintiles)[2] = "2"
levels(age11income_wide$income_quintiles)[3] = "3"
levels(age11income_wide$income_quintiles)[4] = "4"
levels(age11income_wide$income_quintiles)[5] = "5"


# 3. Parent Education ####

#RESPONDENT VARIABLE
#parent education variable 
#first need to identify mother and father figures from main and partner respondents
#do this separately for each sweep 2 as can change between sweeps

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
respondent_identity_sweep2 = mcs2_derived %>% select(mcsid, bddres00, belig00) 
mother_respondent_main_sweep2 = respondent_identity_sweep2 %>% filter(belig00 == 1 & (bddres00 == 1 | bddres00 == 3 |bddres00 ==5|
                                                                                        bddres00==7 | bddres00 == 9 | bddres00 == 11 |
                                                                                        bddres00 == 13 |bddres00 == 15)) %>% 
  select(mcsid, bddres00) %>% 
  rename(mother_mainRespondent = "bddres00")

mother_respondent_partner_sweep2  = respondent_identity_sweep2 %>% filter((belig00 == 2 | belig00 == 3) & (bddres00 == 1 | bddres00 == 3 |bddres00 ==5|
                                                                                                             bddres00==7 | bddres00 == 9 | bddres00 == 11 |
                                                                                                             bddres00 == 13 |bddres00 == 15 | bddres00 == 21)) %>% 
  select(mcsid, bddres00) %>% 
  rename(mother_partnerRespondent = "bddres00")

father_respondent_main_sweep2  = respondent_identity_sweep2 %>% filter(belig00 == 1 & (bddres00 == 2 | bddres00 == 4 |bddres00 ==6|
                                                                                         bddres00==8 | bddres00 == 10 | bddres00 == 12 |
                                                                                         bddres00 == 14 |bddres00 == 16)) %>% 
  select(mcsid, bddres00) %>% 
  rename(father_mainRespondent = "bddres00")

father_respondent_partner_sweep2  = respondent_identity_sweep2 %>% filter((belig00 == 2 | belig00 == 3) & (bddres00 == 2 | bddres00 == 4 |bddres00 ==6|
                                                                                                             bddres00==8 | bddres00 == 10 | bddres00 == 12 |
                                                                                                             bddres00 == 14 |bddres00 == 16 | bddres00 == 22 | bddres00 == 24)) %>% 
  select(mcsid, bddres00) %>% 
  rename(father_partnerRespondent = "bddres00")

#sweep 3 respondent identity
respondent_identity_sweep3 = mcs3_derived_parent %>% select(mcsid, cddres00, celig00) 
mother_respondent_main_sweep3 = respondent_identity_sweep3 %>% filter(celig00 == 1 & (cddres00 == 1 | cddres00 == 3 |cddres00 ==5|
                                                                                        cddres00==7 | cddres00 == 9 | cddres00 == 11 |
                                                                                        cddres00 == 13 |cddres00 == 15)) %>% 
  select(mcsid, cddres00) %>% 
  rename(mother_mainRespondent = "cddres00")

mother_respondent_partner_sweep3 = respondent_identity_sweep3 %>% filter((celig00 == 2 | celig00 == 3) & (cddres00 == 1 | cddres00 == 3 |cddres00 ==5|
                                                                                                            cddres00==7 | cddres00 == 9 | cddres00 == 11 |
                                                                                                            cddres00 == 13 |cddres00 == 15 | cddres00 == 21)) %>% 
  select(mcsid, cddres00) %>% 
  rename(mother_partnerRespondent = "cddres00")

father_respondent_main_sweep3 = respondent_identity_sweep3 %>% filter(celig00 == 1 & (cddres00 == 2 | cddres00 == 4 |cddres00 ==6|
                                                                                        cddres00==8 | cddres00 == 10 | cddres00 == 12 |
                                                                                        cddres00 == 14 |cddres00 == 16)) %>% 
  select(mcsid, cddres00) %>% 
  rename(father_mainRespondent = "cddres00")

father_respondent_partner_sweep3 = respondent_identity_sweep3 %>% filter((celig00 == 2 | celig00 == 3) & (cddres00 == 2 | cddres00 == 4 |cddres00 ==6|
                                                                                                            cddres00==8 | cddres00 == 10 | cddres00 == 12 |
                                                                                                            cddres00 == 14 |cddres00 == 16 | cddres00 == 22 | cddres00 == 24)) %>% 
  select(mcsid, cddres00) %>% 
  rename(father_partnerRespondent = "cddres00")

parentEducation_sweep1 = mcs1_parent %>% select(mcsid, apacqu00, aelig00)
parentEducation_sweep1$apacqu00 = as.numeric(parentEducation_sweep1$apacqu00)
parentEducation_sweep1$apacqu00[parentEducation_sweep1$apacqu00 == 95] <- 7
parentEducation_sweep1$apacqu00[parentEducation_sweep1$apacqu00 == 96] <- 8

#mother
mainMotherEducation_sweep1 = parentEducation_sweep1 %>% 
  filter(aelig00 == 1 & (mcsid %in% mother_respondent_main$mcsid)) %>% 
  select(mcsid, apacqu00) %>% 
  rename("main_educationMother" = apacqu00)
partnerMotherEducation_sweep1 = parentEducation_sweep1 %>% 
  filter(aelig00 == 2 & (mcsid %in% mother_respondent_partner$mcsid)) %>% 
  select(mcsid, apacqu00) %>% 
  rename("partner_educationMother" = apacqu00)

motherEducation_sweep1 = full_join(mainMotherEducation_sweep1, partnerMotherEducation_sweep1) %>% 
  mutate(maternalEducation = case_when(!is.na(main_educationMother) ~ main_educationMother, 
                                       is.na(main_educationMother) ~ partner_educationMother)) %>% 
  select(mcsid, maternalEducation)

#father 
mainFatherEducation_sweep1 = parentEducation_sweep1 %>% 
  filter(aelig00 == 1 & (mcsid %in% father_respondent_main$mcsid)) %>% 
  select(mcsid, apacqu00) %>% 
  rename("main_educationFather" = apacqu00)
partnerFatherEducation_sweep1 = parentEducation_sweep1 %>% 
  filter(aelig00 == 2 & (mcsid %in% father_respondent_partner$mcsid)) %>% 
  select(mcsid, apacqu00) %>% 
  rename("partner_educationFather" = apacqu00)

fatherEducation_sweep1 = full_join(mainFatherEducation_sweep1, partnerFatherEducation_sweep1) %>% 
  mutate(paternalEducation = case_when(!is.na(main_educationFather) ~ main_educationFather, 
                                       is.na(main_educationFather) ~ partner_educationFather)) %>% 
  select(mcsid, paternalEducation)

parentEducation_s1 = full_join(motherEducation_sweep1, fatherEducation_sweep1)

#Sweep 2 - Any new qualifications? 
parentEducation_sweep2 = mcs2_parent %>% select(mcsid, belig00, bpedus00,contains("bpnacq0"), bpacqu00) %>% 
  mutate(newQuals_sweep2 = case_when(
    bpedus00 == 1 & bpnacq0a == 1 ~ 1, 
    bpedus00 == 1 & bpnacq0b == 1 ~ 2, 
    bpedus00 == 1 & bpnacq0c == 1 ~ 3, 
    bpedus00 == 1 & bpnacq0d == 1 ~ 4, 
    bpedus00 == 1 & bpnacq0e == 1 ~ 5, 
    bpedus00 == 1 & bpnacq0f == 1 ~ 6, 
    bpedus00 == 1 & bpnacq0g == 1 ~ 7,
    bpedus00 == 1 & bpnacq0h == 1 ~ 8)) %>% 
  select(mcsid, bpedus00, newQuals_sweep2, bpacqu00, belig00)

parentEducation_sweep2[,2:4] <- lapply(parentEducation_sweep2[,2:4], as.numeric)

#Mother
#Main
mainMotherEducation_sweep2 = parentEducation_sweep2 %>% 
  filter(belig00 == 1 & (mcsid %in% mother_respondent_main_sweep2$mcsid)) %>% 
  select(mcsid, bpacqu00, bpedus00, newQuals_sweep2) %>% 
  rename("bpacqu00_main" = bpacqu00, 
         "bpedus00_main" = bpedus00,
         "newQuals_sweep2_main" = newQuals_sweep2)
#Partner
partnerMotherEducation_sweep2 = parentEducation_sweep2 %>% 
  filter(belig00 == 2 & (mcsid %in% mother_respondent_partner_sweep2$mcsid)) %>% 
  select(mcsid, bpacqu00, bpedus00, newQuals_sweep2) %>% 
  rename("bpacqu00_partner" = bpacqu00, 
         "bpedus00_partner" = bpedus00,
         "newQuals_sweep2_partner" = newQuals_sweep2)
#Combine
motherEducation_sweep2 = full_join(mainMotherEducation_sweep2, partnerMotherEducation_sweep2) %>% 
  mutate(bpacqu00_mother = case_when(
    !is.na(bpacqu00_main) ~ bpacqu00_main, 
    is.na(bpacqu00_main) ~ bpacqu00_partner)) %>% 
  mutate(bpedus00_mother = case_when(
    !is.na(bpedus00_main) ~ bpedus00_main, 
    is.na(bpedus00_main) ~ bpedus00_partner)) %>% 
  mutate(newQuals_sweep2_mother = case_when(
    !is.na(newQuals_sweep2_main) ~ newQuals_sweep2_main, 
    is.na(newQuals_sweep2_main) ~ newQuals_sweep2_partner)) %>% 
  select(mcsid, bpacqu00_mother, bpedus00_mother, newQuals_sweep2_mother)
#Father
#Main
mainFatherEducation_sweep2 = parentEducation_sweep2 %>% 
  filter(belig00 == 1 & (mcsid %in% father_respondent_main_sweep2$mcsid)) %>% 
  select(mcsid, bpacqu00, bpedus00, newQuals_sweep2) %>% 
  rename("bpacqu00_main" = bpacqu00, 
         "bpedus00_main" = bpedus00,
         "newQuals_sweep2_main" = newQuals_sweep2)
#Partner
partnerFatherEducation_sweep2 = parentEducation_sweep2 %>% 
  filter(belig00 == 2 & (mcsid %in% father_respondent_partner_sweep2$mcsid)) %>% 
  select(mcsid, bpacqu00, bpedus00, newQuals_sweep2) %>% 
  rename("bpacqu00_partner" = bpacqu00, 
         "bpedus00_partner" = bpedus00,
         "newQuals_sweep2_partner" = newQuals_sweep2)
#Combine
fatherEducation_sweep2 = full_join(mainFatherEducation_sweep2, partnerFatherEducation_sweep2) %>% 
  mutate(bpacqu00_father = case_when(
    !is.na(bpacqu00_main) ~ bpacqu00_main, 
    is.na(bpacqu00_main) ~ bpacqu00_partner)) %>% 
  mutate(bpedus00_father = case_when(
    !is.na(bpedus00_main) ~ bpedus00_main, 
    is.na(bpedus00_main) ~ bpedus00_partner)) %>% 
  mutate(newQuals_sweep2_father = case_when(
    !is.na(newQuals_sweep2_main) ~ newQuals_sweep2_main, 
    is.na(newQuals_sweep2_main) ~ newQuals_sweep2_partner)) %>% 
  select(mcsid, bpacqu00_father, bpedus00_father, newQuals_sweep2_father)

parentEducation_s2 = full_join(motherEducation_sweep2, fatherEducation_sweep2)

#Age 5 - any new qualifications? 
parentEducation_sweep3 = mcs3_parent %>% select(mcsid, celig00, cpedus00, contains("cpacqu0")) %>% 
  mutate(across(c(cpacqu0a, cpacqu0b, cpacqu0c, cpacqu0d, cpacqu0e, cpacqu0f, cpacqu0g), #Recode to match levels at previous sweeps
                .fns = list(recoded = ~rec(., rec = "
                                          1, 3 = 1; 
                                          2 = 2; 
                                          4,5 = 3;
                                          6 = 4;
                                          7 = 5;
                                          8 = 6;
                                          95 = 7;
                                          96 = 8")), 
                .names = "{fn}_{col}"))

# Select minimum as 1 is the highest possible qualification, to get the highest qualification at age 5. 
parentEducation_sweep3 <- transform(parentEducation_sweep3,newQuals_sweep3  = 
                                      pmin(recoded_cpacqu0a, recoded_cpacqu0b, recoded_cpacqu0c, recoded_cpacqu0d, recoded_cpacqu0e, 
                                           recoded_cpacqu0f, recoded_cpacqu0g,  na.rm = TRUE))
parentEducation_sweep3 = parentEducation_sweep3 %>% select(mcsid, celig00, cpedus00, newQuals_sweep3)   

parentEducation_sweep3[,3:4] <- lapply(parentEducation_sweep3[,3:4], as.numeric)

#Mother
#Main
mainMotherEducation_sweep3 = parentEducation_sweep3 %>% 
  filter(celig00 == 1 & (mcsid %in% mother_respondent_main_sweep3$mcsid)) %>% 
  select(mcsid, cpedus00, newQuals_sweep3) %>% 
  rename("cpedus00_main" = cpedus00,
         "newQuals_sweep3_main" = newQuals_sweep3)
#Partner
partnerMotherEducation_sweep3 = parentEducation_sweep3 %>% 
  filter(celig00 == 2 & (mcsid %in% mother_respondent_partner_sweep3$mcsid)) %>% 
  select(mcsid,  cpedus00, newQuals_sweep3) %>% 
  rename("cpedus00_partner" = cpedus00,
         "newQuals_sweep3_partner" = newQuals_sweep3)
#Combine
motherEducation_sweep3 = full_join(mainMotherEducation_sweep3, partnerMotherEducation_sweep3) %>% 
  mutate(cpedus00_mother = case_when(
    !is.na(cpedus00_main) ~ cpedus00_main, 
    is.na(cpedus00_main) ~ cpedus00_partner)) %>% 
  mutate(newQuals_sweep3_mother = case_when(
    !is.na(newQuals_sweep3_main) ~ newQuals_sweep3_main, 
    is.na(newQuals_sweep3_main) ~ newQuals_sweep3_partner)) %>% 
  select(mcsid, cpedus00_mother, newQuals_sweep3_mother)
#Father
#Main
mainFatherEducation_sweep3 = parentEducation_sweep3 %>% 
  filter(celig00 == 1 & (mcsid %in% father_respondent_main_sweep3$mcsid)) %>% 
  select(mcsid, cpedus00, newQuals_sweep3) %>% 
  rename("cpedus00_main" = cpedus00,
         "newQuals_sweep3_main" = newQuals_sweep3)
#Partner
partnerFatherEducation_sweep3 = parentEducation_sweep3 %>% 
  filter(celig00 == 2 & (mcsid %in% father_respondent_partner_sweep3$mcsid)) %>% 
  select(mcsid, cpedus00, newQuals_sweep3) %>% 
  rename("cpedus00_partner" = cpedus00,
         "newQuals_sweep3_partner" = newQuals_sweep3)
#Combine
fatherEducation_sweep3 = full_join(mainFatherEducation_sweep3, partnerFatherEducation_sweep3) %>% 
  mutate(cpedus00_father = case_when(
    !is.na(cpedus00_main) ~ cpedus00_main, 
    is.na(cpedus00_main) ~ cpedus00_partner)) %>% 
  mutate(newQuals_sweep3_father = case_when(
    !is.na(newQuals_sweep3_main) ~ newQuals_sweep3_main, 
    is.na(newQuals_sweep3_main) ~ newQuals_sweep3_partner)) %>% 
  select(mcsid, cpedus00_father, newQuals_sweep3_father)

parentEducation_s3 = full_join(motherEducation_sweep3, fatherEducation_sweep3)

#Create parent education variable, taking into account new qualifications gained at later sweeps. 
#Will want to have the highest qualification across these three sweeps to represent the highest qualification that parent holds. 
parentEducation = parentEducation_s1 %>% 
  full_join(parentEducation_s2) %>% 
  full_join(parentEducation_s3) %>% 
  full_join(sweep_entry) %>% 
  mutate(maternal_education = case_when(
    sentry == 1 ~ pmin(maternalEducation, newQuals_sweep2_mother, newQuals_sweep3_mother, na.rm = TRUE), 
    sentry == 2 ~ pmin(bpacqu00_mother, newQuals_sweep3_mother, na.rm = TRUE))) %>% 
  mutate(paternal_education = case_when(
    sentry == 1 ~ pmin(paternalEducation, newQuals_sweep2_father, newQuals_sweep3_father, na.rm = TRUE), 
    sentry == 2 ~ pmin(bpacqu00_father, newQuals_sweep3_father, na.rm = TRUE))) %>% 
  select(mcsid,maternal_education, paternal_education) %>% 
  mutate(highest_academic_qual = pmin(maternal_education, paternal_education, na.rm = TRUE)) %>% 
  mutate(highested1 = rec(highest_academic_qual,  #collapse into 4 category measure for cross cohort comparison
                          rec = "1, 2=4; 3,4=3;  5=2; 6,7,8=1", #1 =no quals/low level quals. 2 = o levels/gcses grades a*-c. 3= post 16 education. 4=university level qualifications
                          as.num = TRUE, var.label = NULL, val.labels = NULL, append = FALSE, suffix = "_r" )) %>% 
  select(mcsid, highested1)

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
# Make binary for cross cohort comparison 
EAL[EAL == 1] <- 0
EAL[EAL == 2] <- 1
EAL[EAL == 3] <- 1

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
#make binary for cross-cohort comparison 
ethnicity[ethnicity == 1] <- 0
ethnicity[ethnicity == 2] <- 1
ethnicity[ethnicity == 3] <- 1
ethnicity[ethnicity == 4] <- 1
ethnicity[ethnicity == 5] <- 1
ethnicity[ethnicity == 6] <- 1
ethnicity[ethnicity == 7] <- 1

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
  filter(belig00 == 1 & (mcsid %in% mother_respondent_main_sweep2$mcsid)) %>% 
  select(mcsid, bddagb00) %>% 
  rename("main_birthAge" = bddagb00)


age_atBirth_sweep2_motherPartner = mcs2_derived %>% select(mcsid, bddagb00, bddres00,belig00) %>% 
  filter((belig00 == 2 |belig00 == 3) & (mcsid %in% mother_respondent_partner_sweep2$mcsid)) %>% 
  select(mcsid, bddagb00) %>% 
  rename("partner_birthAge" = bddagb00)

age_atBirth_sweep2 = merge(all=TRUE, age_atBirth_sweep2_motherMain, age_atBirth_sweep2_motherPartner, by="mcsid") %>% 
  mutate(age_atBirth_sweep2 = case_when(!is.na(main_birthAge) ~main_birthAge, 
                                        is.na(main_birthAge) ~ partner_birthAge))




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
                                 is.na(age_atBirth_sweep1) ~ age_atBirth_sweep2, 
                                 is.na(age_atBirth_sweep2) ~ age_atBirth_sweep1, 
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

#### Combine variables into analysis data ####
analysis_data <- merge(all=TRUE, sex, ethnicity,by="mcsid")
nrow(analysis_data)
analysis_data <- merge(all=TRUE, analysis_data, EAL,by="mcsid")
nrow(analysis_data)
analysis_data <- merge(all=TRUE, analysis_data, age_atBirth ,by="mcsid")
nrow(analysis_data)
analysis_data <- merge(all=TRUE, analysis_data, accommodation,by="mcsid")
nrow(analysis_data)
analysis_data <- merge(all=TRUE, analysis_data, tenure, by="mcsid")
nrow(analysis_data)
analysis_data <- merge(all=TRUE, analysis_data, parentEducation,by="mcsid")
nrow(analysis_data)
analysis_data <- merge(all=TRUE, analysis_data, breastfed, by="mcsid")
nrow(analysis_data)
analysis_data <- merge(all=TRUE, analysis_data, carers_in_hh, by="mcsid")
nrow(analysis_data)
analysis_data <- merge(all=TRUE, analysis_data, occupational_status, by="mcsid")
nrow(analysis_data)
analysis_data <- merge(all=TRUE, analysis_data, age11income_wide, by="mcsid")
nrow(analysis_data)
analysis_data <- merge(all=TRUE, analysis_data, age5_vocab, by="mcsid")
nrow(analysis_data)
analysis_data <- merge(all=TRUE, analysis_data, age11_vocab, by="mcsid")
nrow(analysis_data)
analysis_data <- merge(all=TRUE, analysis_data, age14_vocab, by="mcsid")
nrow(analysis_data)
analysis_data <- merge(all=TRUE, analysis_data,mcs_weight2, by="mcsid")
nrow(analysis_data)

#pull out those with a response to language measure ####
mcs_analysis <- analysis_data[!is.na(analysis_data$age5_vocab) |
                                !is.na(analysis_data$age11_vocab)| 
                                !is.na(analysis_data$age14_vocab),]


#save data as csv file####
write.csv(mcs_analysis, file = "mcs_ses_comparison_data.csv")


#sensitivity check with white only ethnicity - to account for the change ####
#in ethnic composition between cohorts/see if this has affected results. 

white_sensitivity <- mcs_analysis[which(mcs_analysis$ethnicity == "0"),] #binary variable where 0=white, 1=minority 
write.csv(white_sensitivity, file = "mcs_crossCohort_whiteSample.csv")

   