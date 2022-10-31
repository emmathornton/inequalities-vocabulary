#SEC Inequalities in Vocabualry - BCS data compilation code for cross-cohort comparison (with new edition data)

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

#### Load in Data ####
bcs7072a <- read_sav("bcs7072a.sav")
bcs7072b <- read_sav("bcs7072b.sav")
f699a <- read_sav("f699a.sav")
f699b <- read_sav("f699b.sav")
f699c <- read_sav("f699c.sav")
f699c_new <- read_sav("NEWf699c.sav")
age10 <- read_sav("NEWsn3723.sav")
age10_derived <- read_sav("bcs3derived.sav")
age16 <- read_sav("NEWbcs7016x.sav")
age16_derived <- read_sav("bcs4derived.sav")

#### convert data names to lowercase ####
names(bcs7072a ) <- tolower(names(bcs7072a ))
names(bcs7072b ) <- tolower(names(bcs7072b ))
names(f699a ) <- tolower(names(f699a ))
names(f699b ) <- tolower(names(f699b))
names(f699c ) <- tolower(names(f699c))
names(f699c_new ) <- tolower(names(f699c_new))
names(age10 ) <- tolower(names(age10))
names(age16 ) <- tolower(names(age16))
names(age10_derived ) <- tolower(names(age10_derived))
names(age16_derived ) <- tolower(names(age16_derived))

### getting single cohort members (0), first twin (1) and first triplet (3). so have a cohort member from each family ####

cm_codeBirth = bcs7072a %>%  select(bcsid, a0002)
cm_code5 = f699a %>% select(bcsid, d002)
cm_code10 = age10 %>% select(bcsid, tc10)
cm_code16 = age16 %>% select(bcsid, tc2)

cm_code = cm_codeBirth %>% full_join(cm_code5) %>% 
  full_join(cm_code10) %>% 
  full_join(cm_code16)

first_CM <- cm_code[cm_code$a0002 == 0 | cm_code$a0002 == 1 | cm_code$a0002 == 3 |
                      cm_code$d002 == 0 | cm_code$d002 == 1 |#Seems to be an issue with age 5 variable where it places a few  second twins as singletons instead - but at all other sweeps these are down as being twin number 2. will remove these n = 5. 
                      cm_code$tc10 == 0 | cm_code$tc10 == 1 |
                      cm_code$tc2 == 0  |cm_code$tc2 == 1 ,] 

discrepant_age5 = first_CM %>% filter(d002 == 0, a0002 == 2)
first_CM = first_CM %>% filter(!first_CM$bcsid %in% discrepant_age5$bcsid)


#### Vocabulary ####

# Age 5 - English Picture Vocabulary Test
# Compiled from code in Parsons (2014) documentation on cognition measures, done in SPSS. 
# https://cls.ucl.ac.uk/wp-content/uploads/2017/07/BCS70-Childhood-cognition-in-the-1970-British-Cohort-Study-Nov-2014-final.pdf

age5_vocab = f699c_new %>% select(bcsid, b5epvt, f112) %>% 
  rename(age5_vocab = b5epvt) %>% 
  replace_with_na(replace = list(f112 = -3 )) %>% 
  mutate(age_in_days = days(f112)) %>% 
  mutate(age5_years = round(as.period(age_in_days,unit="days")/years(1), 2)) %>% 
  select(bcsid, age5_vocab, age5_years)


# Age 10 - Word Similarities
# Compiled from code in Parsons (2014) documentation on cognition measures, done in SPSS. 
# https://cls.ucl.ac.uk/wp-content/uploads/2017/07/BCS70-Childhood-cognition-in-the-1970-British-Cohort-Study-Nov-2014-final.pdf

age10_vocab1 = age10 %>% select(bcsid, b10bass, b10sin) %>% 
  mutate(b10bass = case_when(
    b10bass == 0 & b10sin == 0 ~ NA_real_, 
                             TRUE ~ as.numeric(b10bass))) %>% 
  select(-b10sin)
age10_vocab = age10_derived %>% select(bcsid, bd3age) %>% 
  right_join(age10_vocab1) %>% 
  distinct(bcsid, .keep_all = TRUE)

# Age 16 - Vocabulary Test
# Compiled from code in Parsons documentation on cognition measures, done in SPSS. 
# https://cls.ucl.ac.uk/wp-content/uploads/2017/07/BCS70-Childhood-cognition-in-the-1970-British-Cohort-Study-Nov-2014-final.pdf
# Also a harmonised version in which items match the 20 items used in the Word Activity Task in MCS

age16_vocab1 = age16 %>% select(bcsid,b16vocab, b16vocab_harmonised) %>% 
  replace_with_na(replace = list(b16vocab = c(-1, -9, -7, -8))) %>% 
  replace_with_na(replace = list(b16vocab_harmonised = c(-1, -9, -7, -8)))

age16_vocab = age16_derived %>% select(bcsid, bd4age) %>% 
  right_join(age16_vocab1)
  
#### Socioeconomic Status Variables ####

# 1. Occupational Status ####
# Highest household level at age 5, or at birth if NA
# Students/volunteers (7) categorised as unemployed


age5_occupation = f699b %>% select(bcsid, e197, e206) %>% 
  mutate(across(c(e197, e206), 
                .fns = list(recoded = ~rec(., rec = "
                                           -1, -2, - 3, -4 = NA;
                                           1, 2 = 1; 
                                           3, 4 = 2; 
                                           5, 6 = 3; 
                                           7 = 4")), 
                .names = "{fn}_{col}")) %>% 
  mutate(highest_sec5 = pmin(recoded_e197, recoded_e206, na.rm = TRUE)) %>% 
  select(bcsid, highest_sec5)


# Highest household level at birth. 
#4th category for unemployed. 
birth_occupation = bcs7072a %>% select(bcsid, a0014, a0015, a0018, a0019) %>% 
  mutate(across(c(a0014, a0018),
                .fns = list(recoded = ~rec(., rec = "
                                           -2, 7, 8 = NA; 
                                           1, 2 = 1; 
                                           3, 4 = 2; 
                                           5, 6 = 3")))) %>% 
  mutate(recoded_a0014 = case_when(!is.na(a0014_recoded) ~ a0014_recoded, 
                                   is.na(a0014_recoded) & a0015 == 2 ~ 4)) %>% 
  mutate(recoded_a0018 = case_when(!is.na(a0018_recoded) ~ a0018_recoded, 
                                   is.na(a0018_recoded) & a0019 == 2 ~ 4)) %>% 
  mutate(highest_sec = pmin(recoded_a0014, recoded_a0018, na.rm = TRUE)) %>% 
  select(bcsid, highest_sec)

# Age 5 Occupation and then replace NA with Occupation at Birth 
occupational_status <- age5_occupation %>% full_join(birth_occupation) %>% 
  mutate(highest_ses = 
           ifelse(!is.na(highest_sec5), 
                  highest_sec5,
                  highest_sec)) %>% 
  select(bcsid, highest_ses) %>% 
  mutate(highest_occupation = rec(highest_ses, rec = "
                                  1=4; 
                                  2=3; 
                                  3=2; 
                                  4=1", 
                                  as.num = TRUE, var.label = NULL, 
                                  val.labels = NULL, append = FALSE, 
                                  suffix = "_r")) %>% 
  select(bcsid, highest_occupation)

# 2. Parent Education - Highest Household Qualification #### 
parent_education = f699b %>% select(bcsid, e190, e189a, e189b) %>% 
  mutate(across(c(e190, e189a, e189b), 
                .fns = list(recoded = ~rec(., rec = "
                                           -1, -2, - 3, -4, -8, 8 = NA;
                                           1 = 7; 2 = 6; 3 = 5; 4 = 4; 5 = 3; 6 = 2; 7 = 1", # Reverse code
                                           as.num = TRUE, var.label = NULL, 
                                           val.labels = NULL, append = FALSE, 
                                           suffix = "_r")), 
                .names = "{fn}_{col}")) %>% 
  mutate(highestEd = pmin(recoded_e189a, recoded_e189b, na.rm = TRUE)) %>% 
  mutate(across(c(recoded_e190, highestEd), # Collapse into 4 category measure for cross cohort comparison
                .fns = list(recoded = ~rec(., rec = "
                                           1, 2, 3 = 4;   
                                           4 = 3;        
                                           5 = 2;         
                                           6, 7 = 1",
                                           #1 = no quals/low level quals. 2 = o levels/gcses grades a*-c. 
                                           #3 = post 16 education. 4 = university level qualifications
                                           as.num = TRUE, var.label = NULL, 
                                           val.labels = NULL, append = FALSE, 
                                           suffix = "_r")))) %>%  
  select(bcsid,highestEd_recoded)

# 3.Income ####
# OECD equivalised
income = age10 %>% 
  select(bcsid,c9.1, c9.2, c9.3, c9.4, c9.5, c9.6, c9.7, c9.8) %>% 
  mutate(weekly_income = case_when(
    c9.1 == 1 ~ "under £35 pw", 
    c9.2 == 1 ~ "£35-£49 pw",
    c9.3 == 1 ~ "£50-£99 pw",
    c9.4 == 1 ~ "£100-£149 pw",
    c9.5 == 1 ~ "£150-£199 pw", 
    c9.6 == 1 ~ "£200-£249 pw", 
    c9.7 == 1 ~ ">£250 pw"
  ), .after= 1) %>% 
  mutate(weekly_incomeMedian = case_when(
    weekly_income == "under £35 pw" ~ 17,
    weekly_income == "£35-£49 pw" ~ 42,
    weekly_income == "£50-£99 pw" ~ 74.5,
    weekly_income == "£100-£149 pw" ~ 124.5,
    weekly_income == "£150-£199 pw" ~ 174.5,
    weekly_income == "£200-£249 pw" ~ 224.5,
    weekly_income == ">£250 pw" ~ 275, 
  ), .after = 2)


#people in household to get OECD equivalisation score
household_grid = age10 %>% select(
  bcsid,back4ap, back4bp, back4cp,a4a.5, a4a.41, a4a.42, a8.1, a4a.3, a4a.4, a4a.7, a4a.8, a4a.11, a4a.12, a4a.15, a4a.16, a4a.19, a4a.20, a4a.23, a4a.24, 
  a4a.27, a4a.28, a4a.31, a4a.32, a4a.35, a4a.36, a4a.39, a4a.40,
  a4a.5, a4a.9, a4a.13, a4a.17, a4a.21,a4a.25, a4a.29, a4a.33, a4a.37
)
#convert interview date into lubridate 
household_grid$back4ap = as.numeric(household_grid$back4ap)
#household_grid$back4ap = day(household_grid$back4ap)
household_grid$back4bp = as.numeric(household_grid$back4bp)
household_grid$back4bp = month(household_grid$back4bp)
household_grid$back4cp = as.numeric(household_grid$back4cp)
#household_grid$back4cp[is.na(household_grid$back4cp)] = 80
household_grid$back4cp = paste0("19",household_grid$back4cp)
household_grid$back4cp[household_grid$back4cp == "19NA"] <- NA
household_grid$back4cp = as.numeric(household_grid$back4cp)
household_grid = household_grid %>% unite("interview_date", c(back4ap, back4bp, back4cp), sep=" ", remove = FALSE, na.rm = TRUE)
household_grid$interview_date[household_grid$interview_date == ""] <- NA
#household_grid = household_grid %>% filter(!is.na(interview_date))
household_grid$interview_date = dmy(household_grid$interview_date)
#household_grid$interview_date = format_ISO8601(household_grid$interview_date, precision = "ym")

#convert cm dob into lubridate to get age in years
household_grid$a4a.3 = month(household_grid$a4a.3)
household_grid$a4a.4 = paste0("19",household_grid$a4a.4)
household_grid$a4a.4[household_grid$a4a.4 == "19NA"] <- NA
household_grid$a4a.4 = as.numeric(household_grid$a4a.4)
household_grid = household_grid %>% unite("cm_dob", c(a4a.3, a4a.4), sep=" ", remove = FALSE, na.rm = TRUE)
household_grid$cm_dob[household_grid$cm_dob == ""] <- NA
household_grid$cm_dob = my(household_grid$cm_dob)
#household_grid$cm_dob = format_ISO8601(household_grid$cm_dob, precision = "ym")
household_grid = household_grid %>% mutate(cm_age = as.period(interval(start = cm_dob, end = interview_date))$year, .after = "cm_dob")

#age for person 2
household_grid$a4a.7 = as.numeric(household_grid$a4a.7)
household_grid$a4a.7 = month(household_grid$a4a.7)
household_grid$a4a.8 = paste0("19",household_grid$a4a.8)
household_grid$a4a.8[household_grid$a4a.8 == "19NA"] <- NA
household_grid$a4a.8 = as.numeric(household_grid$a4a.8)
household_grid = household_grid %>% unite("p2_dob", c(a4a.7, a4a.8), sep=" ", remove = FALSE, na.rm = TRUE)
household_grid$p2_dob[household_grid$p2_dob == ""] <- NA
household_grid$p2_dob = my(household_grid$p2_dob)
#household_grid$cm_dob = format_ISO8601(household_grid$cm_dob, precision = "ym")
household_grid = household_grid %>% mutate(p2_age = as.period(interval(start = p2_dob, end = interview_date))$year, .after = "p2_dob")

#age for person 3
household_grid$a4a.11 = as.numeric(household_grid$a4a.11)
household_grid$a4a.11 = month(household_grid$a4a.11)
household_grid$a4a.12 = paste0("19",household_grid$a4a.12)
household_grid$a4a.12[household_grid$a4a.12 == "19NA"] <- NA
household_grid$a4a.12 = as.numeric(household_grid$a4a.12)
household_grid = household_grid %>% unite("p3_dob", c(a4a.11, a4a.12), sep=" ", remove = FALSE, na.rm = TRUE)
household_grid$p3_dob[household_grid$p3_dob == ""] <- NA
household_grid$p3_dob = my(household_grid$p3_dob)
#household_grid$cm_dob = format_ISO8601(household_grid$cm_dob, precision = "ym")
household_grid = household_grid %>% mutate(p3_age = as.period(interval(start = p3_dob, end = interview_date))$year, .after = "p3_dob")

#age for person 4
household_grid$a4a.15 = as.numeric(household_grid$a4a.15)
household_grid$a4a.15 = month(household_grid$a4a.15)
household_grid$a4a.16 = paste0("19",household_grid$a4a.16)
household_grid$a4a.16[household_grid$a4a.16 == "19NA"] <- NA
household_grid$a4a.16 = as.numeric(household_grid$a4a.16)
household_grid = household_grid %>% unite("p4_dob", c(a4a.15, a4a.16), sep=" ", remove = FALSE, na.rm = TRUE)
household_grid$p4_dob[household_grid$p4_dob == ""] <- NA
household_grid$p4_dob = my(household_grid$p4_dob)
#household_grid$cm_dob = format_ISO8601(household_grid$cm_dob, precision = "ym")
household_grid = household_grid %>% mutate(p4_age = as.period(interval(start = p4_dob, end = interview_date))$year, .after = "p4_dob")

#age for person 5
household_grid$a4a.19 = as.numeric(household_grid$a4a.19)
household_grid$a4a.19 = month(household_grid$a4a.19)
household_grid$a4a.20 = paste0("19",household_grid$a4a.20)
household_grid$a4a.20[household_grid$a4a.20 == "19NA"] <- NA
household_grid$a4a.20 = as.numeric(household_grid$a4a.20)
household_grid = household_grid %>% unite("p5_dob", c(a4a.19, a4a.20), sep=" ", remove =FALSE, na.rm = TRUE)
household_grid$p5_dob[household_grid$p5_dob == ""] <- NA
household_grid$p5_dob = my(household_grid$p5_dob)
#household_grid$cm_dob = format_ISO8601(household_grid$cm_dob, precision = "ym")
household_grid = household_grid %>% mutate(p5_age = as.period(interval(start = p5_dob, end = interview_date))$year, .after = "p5_dob")

#age for person 6
household_grid$a4a.23 = as.numeric(household_grid$a4a.23)
household_grid$a4a.23 = month(household_grid$a4a.23)
household_grid$a4a.24 = paste0("19",household_grid$a4a.24)
household_grid$a4a.24[household_grid$a4a.24 == "19NA"] <- NA
household_grid$a4a.24 = as.numeric(household_grid$a4a.24)
household_grid = household_grid %>% unite("p6_dob", c(a4a.23, a4a.24), sep=" ", remove = FALSE, na.rm = TRUE)
household_grid$p6_dob[household_grid$p6_dob == ""] <- NA
household_grid$p6_dob = my(household_grid$p6_dob)
#household_grid$cm_dob = format_ISO8601(household_grid$cm_dob, precision = "ym")
household_grid = household_grid %>% mutate(p6_age = as.period(interval(start = p6_dob, end = interview_date))$year, .after = "p6_dob")

#age for person 7
household_grid$a4a.27 = as.numeric(household_grid$a4a.27)
household_grid$a4a.27 = month(household_grid$a4a.27)
household_grid$a4a.28 = paste0("19",household_grid$a4a.28)
household_grid$a4a.28[household_grid$a4a.28 == "19NA"] <- NA
household_grid$a4a.28 = as.numeric(household_grid$a4a.28)
household_grid = household_grid %>% unite("p7_dob", c(a4a.27, a4a.28), sep=" ", remove = FALSE, na.rm = TRUE)
household_grid$p7_dob[household_grid$p7_dob == ""] <- NA
household_grid$p7_dob = my(household_grid$p7_dob)
#household_grid$cm_dob = format_ISO8601(household_grid$cm_dob, precision = "ym")
household_grid = household_grid %>% mutate(p7_age = as.period(interval(start = p7_dob, end = interview_date))$year, .after = "p7_dob")

#age for person 8 
household_grid$a4a.31 = as.numeric(household_grid$a4a.31)
household_grid$a4a.31 = month(household_grid$a4a.31)
household_grid$a4a.32 = paste0("19",household_grid$a4a.32)
household_grid$a4a.32[household_grid$a4a.32 == "19NA"] <- NA
household_grid$a4a.32 = as.numeric(household_grid$a4a.32)
household_grid = household_grid %>% unite("p8_dob", c(a4a.31, a4a.32), sep=" ", remove = FALSE, na.rm = TRUE)
household_grid$p8_dob[household_grid$p8_dob == ""] <- NA
household_grid$p8_dob = my(household_grid$p8_dob)
#household_grid$cm_dob = format_ISO8601(household_grid$cm_dob, precision = "ym")
household_grid = household_grid %>% mutate(p8_age = as.period(interval(start = p8_dob, end = interview_date))$year, .after = "p8_dob")

#age for person 9
household_grid$a4a.35 = as.numeric(household_grid$a4a.35)
household_grid$a4a.35 = month(household_grid$a4a.35)
household_grid$a4a.36 = paste0("19",household_grid$a4a.36)
household_grid$a4a.36[household_grid$a4a.36 == "19NA"] <- NA
household_grid$a4a.36 = as.numeric(household_grid$a4a.36)
household_grid = household_grid %>% unite("p9_dob", c(a4a.35, a4a.36), sep=" ", remove = FALSE, na.rm = TRUE)
household_grid$p9_dob[household_grid$p9_dob == ""] <- NA
household_grid$p9_dob = my(household_grid$p9_dob)
#household_grid$cm_dob = format_ISO8601(household_grid$cm_dob, precision = "ym")
household_grid = household_grid %>% mutate(p9_age = as.period(interval(start = p9_dob, end = interview_date))$year, .after = "p9_dob")

#age for person 10
household_grid$a4a.39 = as.numeric(household_grid$a4a.39)
household_grid$a4a.39 = month(household_grid$a4a.39)
household_grid$a4a.40 = paste0("19",household_grid$a4a.40)
household_grid$a4a.40[household_grid$a4a.40 == "19NA"] <- NA
household_grid$a4a.40 = as.numeric(household_grid$a4a.40)
household_grid = household_grid %>% unite("p10_dob", c(a4a.39, a4a.40), sep=" ", remove = FALSE, na.rm = TRUE)
household_grid$p10_dob[household_grid$p10_dob == ""] <- NA
household_grid$p10_dob = my(household_grid$p10_dob)
#household_grid$cm_dob = format_ISO8601(household_grid$cm_dob, precision = "ym")
household_grid = household_grid %>% mutate(p10_age = as.period(interval(start = p10_dob, end = interview_date))$year, .after = "p10_dob")

#sort household grid by number of people in the household
household_grid = household_grid %>% arrange(a4a.41)

people_in_hh = household_grid %>% select(bcsid, a4a.41, a4a.42, a8.1, contains('age'), 
                                         a4a.5, a4a.9, a4a.13, a4a.17, a4a.21,a4a.25, a4a.29, a4a.33, a4a.37)

people_in_hh = people_in_hh %>% 
  mutate(p2_isChild = case_when(p2_age <= 13 & 
                                  (a4a.5 == 11 | a4a.5 ==12 | a4a.5 == 13 | 
                                     a4a.5==14 | a4a.5 ==15 |a4a.5 ==16 |
                                     a4a.5 == 17 | a4a.5 == 18 |a4a.5 == 21 |
                                     a4a.5 ==29 | a4a.5 ==30) ~ 1,
                                is.na(p2_age) & is.na(a4a.5) ~ NA_real_,
                                TRUE ~ 0), .after = a4a.5) %>% 
  mutate(p2_isAdult = case_when(p2_age <= 13 & 
                                  (a4a.5 == 11 | a4a.5 ==12 | a4a.5 == 13 | 
                                     a4a.5==14 | a4a.5 ==15 |a4a.5 ==16 |
                                     a4a.5 == 17 | a4a.5 == 18 |a4a.5 == 21 |
                                     a4a.5 ==29 | a4a.5 ==30) ~ 0,
                                is.na(p2_age) & is.na(a4a.5) ~ NA_real_,
                                TRUE ~ 1), .after = "p2_isChild") %>% 
  mutate(p3_isChild = case_when(p3_age <= 13 & 
                                  (a4a.9 == 11 | a4a.9 ==12 | a4a.9 == 13 | 
                                     a4a.9==14 | a4a.9 ==15 |a4a.9 ==16 |
                                     a4a.9 == 17 | a4a.9 == 18 |a4a.9 == 21 |
                                     a4a.9 ==29 | a4a.9 ==30) ~ 1,
                                is.na(p3_age) & is.na(a4a.9) ~ NA_real_,
                                TRUE ~ 0), .after = a4a.9) %>% 
  mutate(p3_isAdult = case_when(p3_age <= 13 & 
                                  (a4a.9 == 11 | a4a.9 ==12 | a4a.9 == 13 | 
                                     a4a.9==14 | a4a.9 ==15 |a4a.9 ==16 |
                                     a4a.9 == 17 | a4a.9 == 18 |a4a.9 == 21 |
                                     a4a.9 ==29 | a4a.9 ==30) ~ 0,
                                is.na(p3_age) & is.na(a4a.9) ~ NA_real_,
                                TRUE ~ 1), .after = "p3_isChild") %>% 
  mutate(p4_isChild = case_when(p4_age <= 13 & 
                                  (a4a.13 == 11 | a4a.13 ==12 | a4a.13 == 13 | 
                                     a4a.13==14 | a4a.13 ==15 |a4a.13 ==16 |
                                     a4a.13 == 17 | a4a.13 == 18 |a4a.13 == 21 |
                                     a4a.13 ==29 | a4a.13 ==30) ~ 1,
                                is.na(p4_age) & is.na(a4a.13) ~ NA_real_,
                                TRUE ~ 0), .after = a4a.13) %>% 
  mutate(p4_isAdult = case_when(p4_age <= 13 & 
                                  (a4a.13 == 11 | a4a.13 ==12 | a4a.13 == 13 | 
                                     a4a.13==14 | a4a.13 ==15 |a4a.13 ==16 |
                                     a4a.13 == 17 | a4a.13 == 18 |a4a.13 == 21 |
                                     a4a.13 ==29 | a4a.13 ==30) ~ 0,
                                is.na(p4_age) & is.na(a4a.13) ~ NA_real_,
                                TRUE ~ 1), .after = "p4_isChild") %>% 
  mutate(p5_isChild = case_when(p5_age <= 13 & 
                                  (a4a.17 == 11 | a4a.17 ==12 | a4a.17 == 13 | 
                                     a4a.17==14 | a4a.17 ==15 |a4a.17 ==16 |
                                     a4a.17 == 17 | a4a.17 == 18 |a4a.17 == 21 |
                                     a4a.17 ==29 | a4a.17 ==30) ~ 1,
                                is.na(p5_age) & is.na(a4a.17) ~ NA_real_,
                                TRUE ~ 0), .after = a4a.17) %>% 
  mutate(p5_isAdult = case_when(p5_age <= 13 & 
                                  (a4a.17 == 11 | a4a.17 ==12 | a4a.17 == 13 | 
                                     a4a.17==14 | a4a.17 ==15 |a4a.17 ==16 |
                                     a4a.17 == 17 | a4a.17 == 18 |a4a.17 == 21 |
                                     a4a.17 ==29 | a4a.17 ==30) ~ 0,
                                is.na(p5_age) & is.na(a4a.17) ~ NA_real_,
                                TRUE ~ 1), .after = "p5_isChild") %>% 
  mutate(p6_isChild = case_when(p6_age <= 13 & 
                                  (a4a.21 == 11 | a4a.21 ==12 | a4a.21 == 13 | 
                                     a4a.21==14 | a4a.21 ==15 |a4a.21 ==16 |
                                     a4a.21 == 17 | a4a.21 == 18 |a4a.21 == 21 |
                                     a4a.21 ==29 | a4a.21 ==30) ~ 1,
                                is.na(p6_age) & is.na(a4a.21) ~ NA_real_,
                                TRUE ~ 0), .after = a4a.21) %>% 
  mutate(p6_isAdult = case_when(p6_age <= 13 & 
                                  (a4a.21 == 11 | a4a.21 ==12 | a4a.21 == 13 | 
                                     a4a.21==14 | a4a.21 ==15 |a4a.21 ==16 |
                                     a4a.21 == 17 | a4a.21 == 18 |a4a.21 == 21 |
                                     a4a.21 ==29 | a4a.21 ==30) ~ 0,
                                is.na(p6_age) & is.na(a4a.21) ~ NA_real_,
                                TRUE ~ 1), .after = "p6_isChild") %>% 
  mutate(p7_isChild = case_when(p7_age <= 13 & 
                                  (a4a.25 == 11 | a4a.25 ==12 | a4a.25 == 13 | 
                                     a4a.25==14 | a4a.25 ==15 |a4a.25 ==16 |
                                     a4a.25 == 17 | a4a.25 == 18 |a4a.25 == 21 |
                                     a4a.25 ==29 | a4a.25 ==30) ~ 1,
                                is.na(p7_age) & is.na(a4a.25) ~ NA_real_,
                                TRUE ~ 0), .after = a4a.25) %>% 
  mutate(p7_isAdult = case_when(p7_age <= 13 & 
                                  (a4a.25 == 11 | a4a.25 ==12 | a4a.25 == 13 | 
                                     a4a.25==14 | a4a.25 ==15 |a4a.25 ==16 |
                                     a4a.25 == 17 | a4a.25 == 18 |a4a.25 == 21 |
                                     a4a.25 ==29 | a4a.25 ==30) ~ 0,
                                is.na(p7_age) & is.na(a4a.25) ~ NA_real_,
                                TRUE ~ 1), .after = "p7_isChild") %>% 
  mutate(p8_isChild = case_when(p8_age <= 13 & 
                                  (a4a.29 == 11 | a4a.29 ==12 | a4a.29 == 13 | 
                                     a4a.29==14 | a4a.29 ==15 |a4a.29 ==16 |
                                     a4a.29 == 17 | a4a.29 == 18 |a4a.29 == 21 |
                                     a4a.29 ==29 | a4a.29 ==30) ~ 1,
                                is.na(p8_age) & is.na(a4a.29) ~ NA_real_,
                                TRUE ~ 0), .after = a4a.29) %>% 
  mutate(p8_isAdult = case_when(p8_age <= 13 & 
                                  (a4a.29 == 11 | a4a.29 ==12 | a4a.29 == 13 | 
                                     a4a.29==14 | a4a.29 ==15 |a4a.29 ==16 |
                                     a4a.29 == 17 | a4a.29 == 18 |a4a.29 == 21 |
                                     a4a.29 ==29 | a4a.29 ==30) ~ 0,
                                is.na(p8_age) & is.na(a4a.29) ~ NA_real_,
                                TRUE ~ 1), .after = "p8_isChild") %>% 
  mutate(p9_isChild = case_when(p9_age <= 13 & 
                                  (a4a.33 == 11 | a4a.33 ==12 | a4a.33 == 13 | 
                                     a4a.33==14 | a4a.33 ==15 |a4a.33 ==16 |
                                     a4a.33 == 17 | a4a.33 == 18 |a4a.33 == 21 |
                                     a4a.33 ==29 | a4a.33 ==30) ~ 1,
                                is.na(p9_age) & is.na(a4a.33) ~ NA_real_,
                                TRUE ~ 0), .after = a4a.33) %>% 
  mutate(p9_isAdult = case_when(p9_age <= 13 & 
                                  (a4a.33 == 11 | a4a.33 ==12 | a4a.33 == 13 | 
                                     a4a.33==14 | a4a.33 ==15 |a4a.33 ==16 |
                                     a4a.33 == 17 | a4a.33 == 18 |a4a.33 == 21 |
                                     a4a.33 ==29 | a4a.33 ==30) ~ 0,
                                is.na(p9_age) & is.na(a4a.33) ~ NA_real_,
                                TRUE ~ 1), .after = "p9_isChild") %>% 
  mutate(p10_isChild = case_when(p10_age <= 13 & 
                                   (a4a.37 == 11 | a4a.37 ==12 | a4a.37 == 13 | 
                                      a4a.37==14 | a4a.37 ==15 |a4a.37 ==16 |
                                      a4a.37 == 17 | a4a.37 == 18 |a4a.37 == 21 |
                                      a4a.37 ==29 | a4a.37 ==30) ~ 1,
                                 is.na(p10_age) & is.na(a4a.37) ~ NA_real_,
                                 TRUE ~ 0), .after = a4a.37) %>% 
  mutate(p10_isAdult = case_when(p10_age <= 13 & 
                                   (a4a.37 == 11 | a4a.37 ==12 | a4a.37 == 13 | 
                                      a4a.37==14 | a4a.37 ==15 |a4a.37 ==16 |
                                      a4a.37 == 17 | a4a.37 == 18 |a4a.37 == 21 |
                                      a4a.37 ==29 | a4a.37 ==30) ~ 0,
                                 is.na(p10_age) & is.na(a4a.37) ~ NA_real_,
                                 TRUE ~ 1), .after = "p10_isChild") %>% 
  mutate(cm_isChild = 1) %>% 
  mutate(number_ofChildren =  rowSums(.[c("cm_isChild", "p2_isChild", "p3_isChild", "p4_isChild", "p5_isChild",
                                          "p6_isChild", "p7_isChild", "p8_isChild", "p9_isChild", "p10_isChild")],
                                      na.rm = TRUE), .before = "cm_age") %>% 
  mutate(number_ofAdults =rowSums(.[c("p2_isAdult", "p3_isAdult", "p4_isAdult", "p5_isAdult",
                                      "p6_isAdult", "p7_isAdult", "p8_isAdult", "p9_isAdult", "p10_isAdult")],
                                  na.rm = TRUE), .before = "number_ofChildren") %>% 
  mutate(childrenEquivalence = number_ofChildren*0.3, .after = "number_ofAdults") %>% 
  mutate(adultEquivalence = case_when(number_ofAdults == 1 ~ 1, 
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
  mutate(total_equivalence =  rowSums(.[c("childrenEquivalence", "adultEquivalence")],
                                      na.rm = TRUE), .before = "childrenEquivalence") 


own_equivalence = people_in_hh %>% select(bcsid, total_equivalence)

oecd_income = merge(all=TRUE, income, own_equivalence, by ="bcsid")
oecd_income = oecd_income %>% select(bcsid, weekly_income, weekly_incomeMedian, total_equivalence) %>% 
  mutate(oecd_adjusted = weekly_incomeMedian/total_equivalence) %>% 
  mutate(oecd_quintiles = quantcut(oecd_adjusted,5)) %>% 
  select(bcsid, oecd_quintiles)

levels(oecd_income$oecd_quintiles)[1] = "1"
levels(oecd_income$oecd_quintiles)[2] = "2"
levels(oecd_income$oecd_quintiles)[3] = "3"
levels(oecd_income$oecd_quintiles)[4] = "4"
levels(oecd_income$oecd_quintiles)[5] = "5"

#### Potential Confounders ####

# 1. EAL status #### 
# Collapse into 0=english, 1 = other language present
EAL = f699b %>% select(bcsid, e248) %>% 
  mutate(EAL = rec(e248, rec = "
                   -1, -2, -3, -4 = NA; 
                   1 = 0;
                   2, 3, 4, 5, 6, 7 = 1")) %>% 
  select(bcsid, EAL)

# 2. Ethnicity ####
# Collapse into 0 = White, 1 = Minority
ethnicity = f699b %>% select(bcsid, e245) %>% 
  mutate(ethnicity = rec(e245, rec = "
                         -1, -2, -3, -4 = NA;
                         1 = 0; 
                         2, 3, 4, 5, 6, 7 = 1"))


# 3. Sex ####
sex = bcs7072a %>% select(bcsid, a0255) %>% 
  replace_with_na(replace = list(a0255 = c(-1, -2, -3)))


#### Auxiliary Variables for Imputation ####
# 1. Accommodation Type ####

accommodation_birth = bcs7072b %>% select(bcsid, b0008, b0009) %>% 
  replace_with_na(replace = list(b0008 = c(-1, -2, -3, -4, -5, -6))) %>% 
  replace_with_na(replace = list(b0009 = c(-1, -2, -3, -4, -5, -6))) %>% 
  mutate(accommodation_type = ifelse(!(b0008==4), b0008, b0009)) %>% 
  mutate(accommodation_birth = rec(accommodation_type, rec = "
                             1 = 1; 
                             2 = 2; 
                             10, 11, 12, 13, 14, 17, 
                             18, 20, 23, 25 = 3"))

accommodation_age5 = f699b %>% select(bcsid, e218) %>% 
  replace_with_na(replace = list(e218 = c(-1, -2, -3, -4))) %>% 
  mutate(accommodation_age5 = rec(e218, rec = "
                             1, 2, 3 = 1; 
                             4 = 2; 
                             5, 6 = 3"))
  
accommodation = accommodation_age5 %>% full_join(accommodation_birth) %>% 
  select(bcsid, accommodation_age5, accommodation_birth) %>% 
  mutate(accommodation = case_when(!is.na(accommodation_age5) ~ accommodation_age5,
                                   is.na(accommodation_age5) ~ accommodation_birth)) 



# 2. Mother's age at birth of CM####
age_atBirth = bcs7072a %>% select(bcsid, a0005a) %>% 
  replace_with_na(replace = list(a0005a = c(-2))) 


# 3. Tenure ####
tenure = f699b %>% select(bcsid, e220) %>% 
  replace_with_na(replace = list(e220 = c(-1, -2, -3, -4))) %>% 
  mutate(tenure = rec(e220, rec = "
                      1, 2 = 1;
                      3, 4, 5 = 2;
                      6, 7 = 3"))

#### Combine variables into analysis data ####
analysis_data <- merge(all=TRUE, sex, ethnicity,by="bcsid")
nrow(analysis_data)
analysis_data <- merge(all=TRUE, analysis_data, EAL,by="bcsid")
nrow(analysis_data)
analysis_data <- merge(all=TRUE, analysis_data,age_atBirth,by="bcsid")
nrow(analysis_data)
analysis_data <- merge(all=TRUE, analysis_data, accommodation,by="bcsid")
nrow(analysis_data)
analysis_data <- merge(all=TRUE, analysis_data, tenure,by="bcsid")
nrow(analysis_data)
analysis_data <- merge(all=TRUE, analysis_data, parent_education,by="bcsid")
nrow(analysis_data)
analysis_data <- merge(all=TRUE, analysis_data, oecd_income,by="bcsid")
nrow(analysis_data)
analysis_data <- merge(all=TRUE, analysis_data,occupational_status, by="bcsid")
nrow(analysis_data)
analysis_data <- merge(all=TRUE, analysis_data, age5_vocab, by="bcsid")
nrow(analysis_data)
analysis_data <- merge(all=TRUE, analysis_data, age10_vocab, by="bcsid")
nrow(analysis_data)
analysis_data <- merge(all=TRUE, analysis_data, age16_vocab, by="bcsid")
nrow(analysis_data)

#Select 1st cohort member in each family
bcs_cm1 <- analysis_data[analysis_data$bcsid %in% first_CM$bcsid,]
#select those with a response on any language measure ####
bcs_analysis <- bcs_cm1[!is.na(bcs_cm1$age5_vocab) | !is.na(bcs_cm1$b10bass) |!is.na(bcs_cm1$b16vocab) ,]

#create an attrition weight####
#creating the weight for age 5 

#FROM BIRTH SWEEP for response at age 5####
#CM BORN TO TEENAGE MOTHER: 
#mother's age at birth of CM####
mother_age_delivery <- c("bcsid", "a0005a")
mother_age_delivery <- bcs7072a[mother_age_delivery]
mother_age_delivery[ mother_age_delivery == -2] <- NA
teenage_mother = mother_age_delivery %>% mutate(teen_mum = a0005a <=19)
teenage_mother$teen_mum = as.numeric(teenage_mother$teen_mum)
teen_mum = c("bcsid", "teen_mum")
teen_mum=teenage_mother[teen_mum]

#parity
parity <- c("bcsid", "a0166")
parity <- bcs7072a[parity]
parity[ parity== -2] <-NA
high_parity = parity %>% mutate (high_parity = a0166 >=4)
high_parity$high_parity = as.numeric(high_parity$high_parity)
highParity=c("bcsid", "high_parity")
highParity=high_parity[highParity]

#heavy smokers
smoker <- c("bcsid", "a0043b")
smoker <- bcs7072a[smoker]
smoker[smoker==-3] <-NA
heavy_smoker = smoker %>% mutate (heavy_smoker= a0043b == 6)
heavy_smoker$heavy_smoker= as.numeric(heavy_smoker$heavy_smoker)
heavySmoker = c("bcsid", "heavy_smoker")
heavySmoker=heavy_smoker[heavySmoker]

#marital status at birth
marital_status <- c("bcsid", "a0012")
marital_status <- bcs7072a[marital_status]
#ignoring missing values
marital_status$a0012[ marital_status$a0012== -2] <-NA
marital_status$a0012[ marital_status$a0012== 1] <- 1
marital_status$a0012[ marital_status$a0012== 2] <- 0
marital_status$a0012[ marital_status$a0012== 3] <- 1
marital_status$a0012[ marital_status$a0012== 4] <- 1
marital_status$a0012[ marital_status$a0012== 5] <- 1

combined_data = bcs_analysis
#generate response variable (this will be the outcome variable in the logistic regression)
combined_data$complete <- as.integer(complete.cases(combined_data$age5_vocab))

#add birth variables to predict missingness
combined_data <- merge(all=TRUE, combined_data, teen_mum,by="bcsid")
#combined_data <- merge(all=TRUE, combined_data, occupational_status,  by="bcsid")
#combined_data <- merge(all=TRUE, combined_data, mothers_social_class,by="bcsid")
#combined_data <- merge(all=TRUE, combined_data, gender, by="bcsid")
combined_data <- merge(all=TRUE, combined_data, highParity, by="bcsid")
combined_data <- merge(all=TRUE, combined_data, heavySmoker, by="bcsid")
combined_data <- merge(all=TRUE, combined_data, marital_status, by="bcsid")

combined_data=combined_data[!is.na(combined_data$complete), ]

#single imputation for missing birth variables - use random imputation.
#in random imputation, impute random values sampled from the nonmissing values of the variable
#missing for mother's age at birth and for gender. 
#function for random imputation: 
rand.impute <- function(a) {
  missing <- is.na(a)
  n.missing <- sum(missing)
  a.obs <- a[!missing]
  imputed <- a
  imputed[missing] <- sample (a.obs, n.missing, replace=TRUE)
  return (imputed)
}

random.impute.data.frame <- function(dat, cols) {
  nms <- names(dat)
  for(col in cols) {
    name <- paste(nms[col],".imputed", sep = "")
    dat[name] <- rand.impute(dat[,col])
  }
  dat
}

combined_data <- random.impute.data.frame(combined_data, c(2, 14, 23, 24, 25, 26))
#combined_data<- merge(all=TRUE, bcs_analysis, new_birth_variables,by="bcsid")
#generate response variable (this will be the outcome variable in the logistic regression)
#combined_data$complete <- as.integer(complete.cases( combined_data$b5epvt))
#logistic regression to predict response. use: parent education, SES, mothers age at birth, gender, ethnicity etc
#then get predicted probabilities
#then weight=1/pr if complete==1 (weight is 1 divided by probability.)
#need to make sure there are no missing values in predictors in the logistic regression.
#need to create single imputation for ethnicity and parent education. 

combined_data$complete=as.factor(combined_data$complete)
combined_data$a0255.imputed=as.factor(combined_data$a0255.imputed)
combined_data$highest_occupation.imputed=as.factor(combined_data$highest_occupation.imputed)
combined_data$teen_mum.imputed=as.factor(combined_data$teen_mum.imputed)
combined_data$high_parity.imputed =as.factor(combined_data$high_parity.imputed)
combined_data$heavy_smoker.imputed=as.factor(combined_data$heavy_smoker.imputed)
combined_data$a0012.imputed=as.factor(combined_data$a0012.imputed)
#logistic regression to predict response at age 5
missing <- glm(complete ~  a0255.imputed + highest_occupation.imputed  + teen_mum.imputed + high_parity.imputed + heavy_smoker.imputed +a0012.imputed  , family=binomial(link='logit'), data=combined_data)
predicted <- plogis(predict(missing)) 
#new_predicted <- 1 / (1 + exp(-predicted)) - maybe?
predicted[combined_data$complete == 0] <- NA

combined_data$predicted_age5 <- predicted
#to get bcsid
predicted_age5_1 <- c("bcsid", "predicted_age5")
predicted_age5_1<-combined_data[predicted_age5_1]

#new_predicted<-na.omit(predicted_age5_1)

predicted_age5_1$weight=1/predicted_age5_1$predicted

#attrition weight for age 10 sweep####
#age 10 ses variables to predict response

father_ed_10 <- c("bcsid", "c1.1", "c1.2", "c1.3", "c1.4", "c1.5", "c1.6", "c1.7", "c1.8", "c1.9")
father_ed_10 <- age10[father_ed_10]
father_ed_10$c1.1[father_ed_10$c1.1==1] <- 1
father_ed_10$c1.2[father_ed_10$c1.2==1] <- 2
father_ed_10$c1.3[father_ed_10$c1.3==1] <- 3
father_ed_10$c1.4[father_ed_10$c1.4==1] <- 4
father_ed_10$c1.5[father_ed_10$c1.5==1] <- 5
father_ed_10$c1.6[father_ed_10$c1.6==1] <- 6
father_ed_10$c1.7[father_ed_10$c1.7==1] <- 7
father_ed_10$c1.8[father_ed_10$c1.7==8] <- 8
father_ed_10$c1.9[father_ed_10$c1.9==1] <- 9

father_ed_10$c1.1 = as.character(father_ed_10$c1.1)
father_ed_10$c1.2 = as.character(father_ed_10$c1.2)
father_ed_10$c1.3 = as.character(father_ed_10$c1.3)
father_ed_10$c1.4 = as.character(father_ed_10$c1.4)
father_ed_10$c1.5 = as.character(father_ed_10$c1.5)
father_ed_10$c1.6 = as.character(father_ed_10$c1.6)
father_ed_10$c1.7 = as.character(father_ed_10$c1.7)
father_ed_10$c1.8 = as.character(father_ed_10$c1.8)
father_ed_10$c1.9 = as.character(father_ed_10$c1.9)
father_ed_10 <- transform(father_ed_10, dad_ed10 = pmax(c1.1, c1.2, c1.3, c1.4, c1.5, c1.6, c1.7,c1.8, c1.9,  na.rm = TRUE))

dad_education10 <- c("bcsid", "dad_ed10")
dad_education10 <- father_ed_10[dad_education10]
dad_education10$dad_ed10[dad_education10$dad_ed10==-8] <- NA
dad_education10$dad_ed10[dad_education10$dad_ed10==-3] <- NA

mother_ed_10 <- c("bcsid", "c1.12", "c1.13", "c1.14", "c1.15", "c1.16", "c1.17", "c1.18", "c1.19", "c1.20")
mother_ed_10 <- age10[mother_ed_10]
mother_ed_10$c1.12[mother_ed_10$c1.12==1] <- 1
mother_ed_10$c1.13[mother_ed_10$c1.13==1] <- 2
mother_ed_10$c1.14[mother_ed_10$c1.14==1] <- 3
mother_ed_10$c1.15[mother_ed_10$c1.15==1] <- 4
mother_ed_10$c1.16[mother_ed_10$c1.16==1] <- 5
mother_ed_10$c1.17[mother_ed_10$c1.17==1] <- 6
mother_ed_10$c1.18[mother_ed_10$c1.18==1] <- 7
mother_ed_10$c1.20[mother_ed_10$c1.20==1] <- 9

mother_ed_10$c1.12=as.character(mother_ed_10$c1.12)
mother_ed_10$c1.13=as.character(mother_ed_10$c1.13)
mother_ed_10$c1.14=as.character(mother_ed_10$c1.14)
mother_ed_10$c1.15=as.character(mother_ed_10$c1.15)
mother_ed_10$c1.16=as.character(mother_ed_10$c1.16)
mother_ed_10$c1.17=as.character(mother_ed_10$c1.17)
mother_ed_10$c1.18=as.character(mother_ed_10$c1.18)
mother_ed_10$c1.19=as.character(mother_ed_10$c1.19)
mother_ed_10$c1.20=as.character(mother_ed_10$c1.20)

mother_ed_10 <- transform(mother_ed_10, mum_ed10 = pmax(c1.12, c1.13, c1.14, c1.15, c1.16, c1.17, c1.18,c1.19, c1.20,  na.rm = TRUE))

mum_education10 <- c("bcsid", "mum_ed10")
mum_education10 <- mother_ed_10[mum_education10]
mum_education10$mum_ed10[mum_education10$mum_ed10==-8] <- NA
mum_education10$mum_ed10[mum_education10$mum_ed10==-3] <- NA


age10_ses <- c("bcsid", "c3.4", "c3.11")
age10_ses <- age10[age10_ses]
age10_ses$c3.4[age10_ses$c3.4==-8] <- NA
age10_ses$c3.4[age10_ses$c3.4==-9] <- NA
age10_ses$c3.11[age10_ses$c3.11==-8] <- NA
age10_ses$c3.11[age10_ses$c3.11==-9] <- NA

age10_gender <- c("bcsid", "sex10")
age10_gender <- age10[age10_gender]
age10_gender$sex10[age10_gender$sex10==3] <- NA

bcsid_age10 <- c("bcsid")
bcsid_age10 <- age10[bcsid_age10]
names(bcsid_age10) <- c("bcsid10")
age10_data <- data.frame(age10,bcsid_age10)
age10_bcsid <- c("bcsid", "bcsid10")
age10_bcsid <- age10_data[age10_bcsid]

#response at age 10####
#region of parents
#mother
mother_region <- c("bcsid", "a0006a")
mother_region <- bcs7072a[mother_region]
mother_region[mother_region==-3] <- NA
mother_region[mother_region==-2] <- NA
mother_regionNotBritain = mother_region %>% mutate (mother_notBritain= a0006a >=14)
mother_regionNotBritain$mother_notBritain = as.numeric(mother_regionNotBritain$mother_notBritain)
mother_notBritain = c("bcsid", "mother_notBritain")
mother_notBritain=mother_regionNotBritain[mother_notBritain]

#father
father_region <- c("bcsid", "a0007a")
father_region <- bcs7072a[father_region]
father_region[father_region==-3] <- NA
father_region[father_region==-2] <- NA
father_regionNotBritain = father_region %>% mutate (father_notBritain= a0007a >=14)
father_regionNotBritain$father_notBritain = as.numeric(father_regionNotBritain$father_notBritain)

father_notBritain = c("bcsid", "father_notBritain")
father_notBritain=father_regionNotBritain[father_notBritain]

#age parents left full time education
#mother
leaving_ageMother= c("bcsid", "a0009")
leaving_ageMother=bcs7072a[leaving_ageMother]
leaving_ageMother[leaving_ageMother==-3] <- NA
leaving_ageMother[leaving_ageMother==-2] <- NA
leaving_ageMother[leaving_ageMother==-0] <- NA
leaving_age15Mother = leaving_ageMother %>% mutate (leaving_age15Mother = a0009 <=15)
leaving_age15Mother$leaving_age15Mother = as.numeric(leaving_age15Mother$leaving_age15Mother)

mother_left_school15 =c("bcsid", "leaving_age15Mother")
mother_left_school15=leaving_age15Mother[mother_left_school15]

#father
leaving_ageFather= c("bcsid", "a0010")
leaving_ageFather=bcs7072a[leaving_ageFather]
leaving_ageFather[leaving_ageFather==-3] <- NA
leaving_ageFather[leaving_ageFather==-2] <- NA
leaving_ageFather[leaving_ageFather==-0] <- NA
leaving_age15Father = leaving_ageFather %>% mutate (leaving_age15Father = a0010 <=15)
leaving_age15Father$leaving_age15Father = as.numeric(leaving_age15Father$leaving_age15Father)

father_left_school15 =c("bcsid", "leaving_age15Father")
father_left_school15=leaving_age15Father[father_left_school15]

#father unemployed
father_employment <- c("bcsid", "a0015")
father_employment<- bcs7072a[father_employment]
father_employment[father_employment==-3] <- NA
father_employment[father_employment==-2] <- NA
father_employment[father_employment==-1] <- NA
father_unemployed = father_employment %>% mutate (father_unemployed = a0015 == 2)
father_unemployed$father_unemployed = as.numeric(father_unemployed$father_unemployed)

fatherUnemployed=c("bcsid", "father_unemployed")
fatherUnemployed = father_unemployed[fatherUnemployed]

#twins
cm_code <- c("bcsid", "a0002")
cm_code <- bcs7072a[cm_code ]
twin = cm_code %>% mutate(twin = a0002 >0 & a0002 <3)
twin$twin=as.numeric(twin$twin)
twins = c("bcsid", "twin")
twins=twin[twins]

#from age 5 sweep
#parents with no quals
#highest household variable
householdEducation <- c("bcsid", "e190")
householdEducation <- f699b[householdEducation]
householdEducation$e190[householdEducation$e190==-4] <- NA 
householdEducation$e190[householdEducation$e190==-3] <- NA 
householdEducation$e190[householdEducation$e190==-2] <- NA 
householdEducation$e190[householdEducation$e190==-1] <- NA 
householdEducation$e190[householdEducation$e190==-8] <- NA 
noQuals = householdEducation %>% mutate(noQuals = e190 == 1)
noQuals$noQuals=as.numeric(noQuals$noQuals)
no_qualifications = c("bcsid", "noQuals")
no_qualifications=noQuals[no_qualifications]
#mother aged over 40 at birth of child
aged40 = mother_age_delivery %>% mutate(aged40 = a0005a >=40)
aged40$aged40 = as.numeric(aged40$aged40)
mother_over40 = c("bcsid", "aged40")
mother_over40= aged40[mother_over40]
#separation of mother and baby for over a month
separation=c("bcsid", "e019")
separation=f699b[separation]
separation[separation==-1] <- 0
separation[separation==-2] <- NA
separation[separation==-3] <- NA
separation[separation==-4] <- NA
separated_oneMonthPlus = separation %>% mutate(oneMonthPlus= e019 >=30)
separated_oneMonthPlus$oneMonthPlus=as.numeric(separated_oneMonthPlus$oneMonthPlus)
month_separated= c("bcsid", "oneMonthPlus")
month_separated=separated_oneMonthPlus[month_separated]
# low birthweight
birthweight <- c("bcsid", "a0278")
birthweight <- bcs7072a [birthweight]
#ignore missing
birthweight[ birthweight== -1] <-NA
birthweight[ birthweight== -2] <-NA
birthweight[ birthweight== -3] <-NA
birthweight[ birthweight== -4] <-NA
birthweight[ birthweight== 0] <-NA
low_birthweight = birthweight %>% mutate(low_birthweight = a0278 <=2267.96)
low_birthweight$low_birthweight=as.numeric(low_birthweight$low_birthweight)
lowBirthweight=c("bcsid", "low_birthweight")
lowBirthweight=low_birthweight[lowBirthweight]
#geographical mobility - moved more than 3 times = high.
family_moved = c("bcsid", "e249")
family_moved=f699b[family_moved]
family_moved[family_moved==-1]<-NA
family_moved[family_moved==-2]<-NA
family_moved[family_moved==-3]<-NA
family_moved[family_moved==-4]<-NA
highMobility= family_moved %>% mutate (high_mobility = e249 >= 3)
highMobility$high_mobility = as.numeric(highMobility$high_mobility )
mobile_family= c("bcsid", "high_mobility")
mobile_family=highMobility[mobile_family]
#crowded accommodation
#>1 person per room = crowded
person_roomRatio = c("bcsid", "e228b")
person_roomRatio=f699b[person_roomRatio]
person_roomRatio[person_roomRatio==-1] <- NA
person_roomRatio[person_roomRatio==-2] <- NA
person_roomRatio[person_roomRatio==-3] <- NA
person_roomRatio[person_roomRatio==-4] <- NA
crowded= person_roomRatio %>% mutate(crowded = e228b >1)
crowded$crowded=as.numeric(crowded$crowded)
crowded_room = c("bcsid", "crowded")
crowded_room = crowded[crowded_room]
#private rented accommodation
tenure_type = c("bcsid", "e220")
tenure_type=f699b[tenure_type]
tenure_type[tenure_type==-1] <- NA
tenure_type[tenure_type==-2] <- NA
tenure_type[tenure_type==-3] <- NA
tenure_type[tenure_type==-4] <- NA
privateRented= tenure_type %>% mutate (privateRented = e220 >3 & e220 <6 )
privateRented$privateRented=as.numeric(privateRented$privateRented)
private_rented= c("bcsid", "privateRented")
private_rented= privateRented[private_rented]
#poor neighborhood
poor_neighborhood = c("bcsid", "e267a")
poor_neighborhood = f699b[poor_neighborhood]
poor_neighborhood[poor_neighborhood==-1] <- NA
poor_neighborhood[poor_neighborhood==-2] <- NA
poor_neighborhood[poor_neighborhood==-3] <- NA
poor_neighborhood[poor_neighborhood==-4] <- NA
poorArea = poor_neighborhood %>% mutate(poorNeighborhood = e267a==1)
poorArea$poorNeighborhood = as.numeric(poorArea$poorNeighborhood)
poor_area =c("bcsid", "poorNeighborhood")
poor_area=poorArea[poor_area]

#age10_variables <- merge(all=TRUE, age10_bcsid, vocab10, by="bcsid")
#age10_variables <- merge(all=TRUE, age10_variables, dad_education10,by="bcsid")
#age10_variables <- merge(all=TRUE, age10_variables, mum_education10,by="bcsid")
#age10_variables <- merge(all=TRUE, age10_variables,age10_ses,by="bcsid")
#age10_variables <- merge(all=TRUE, age10_variables,vocab10,by="bcsid")

#names(age10_variables) <- c("bcsid", "bcsid10",  "vocab10")



#combined_data_age10<- merge(all=TRUE, age10_variables, bcs_analysis,by="bcsid")


combined_data_age10=bcs_analysis 

#generate response variable (this will be the outcome variable in the logistic regression)
combined_data_age10$complete10 <- as.integer(complete.cases(combined_data_age10$b10bass))

#combined_data_age10 <- merge(all=TRUE, combined_data_age10, gender, by="bcsid")
combined_data_age10 <- merge(all=TRUE, combined_data_age10,private_rented,by="bcsid")
combined_data_age10 <- merge(all=TRUE, combined_data_age10,poor_area,by="bcsid")
combined_data_age10 <- merge(all=TRUE, combined_data_age10,crowded_room,by="bcsid")
combined_data_age10 <- merge(all=TRUE, combined_data_age10,twins,by="bcsid")
combined_data_age10 <- merge(all=TRUE, combined_data_age10,father_notBritain,by="bcsid")
combined_data_age10 <- merge(all=TRUE, combined_data_age10,mother_notBritain,by="bcsid")
combined_data_age10 <- merge(all=TRUE, combined_data_age10,mother_left_school15,by="bcsid")
combined_data_age10 <- merge(all=TRUE, combined_data_age10,father_left_school15,by="bcsid")
combined_data_age10 <- merge(all=TRUE, combined_data_age10,teen_mum,by="bcsid")
combined_data_age10 <- merge(all=TRUE, combined_data_age10,mother_over40,by="bcsid")
#combined_data_age10 <- merge(all=TRUE, combined_data_age10,ethnicity,by="bcsid")
combined_data_age10 <- merge(all=TRUE, combined_data_age10, mobile_family,by="bcsid")
combined_data_age10 <- merge(all=TRUE, combined_data_age10,lowBirthweight,by="bcsid")
combined_data_age10 <- merge(all=TRUE, combined_data_age10,month_separated,by="bcsid")
combined_data_age10 <- merge(all=TRUE, combined_data_age10,no_qualifications,by="bcsid")
combined_data_age10 <- merge(all=TRUE, combined_data_age10,fatherUnemployed,by="bcsid")
#remove na for complete 10 so have those who responded at age 10 and who didnt from own sample
combined_data_age10=combined_data_age10[!is.na(combined_data_age10$complete10), ]

#need to make sure there are no missing values in predictors in the logistic regression.
combined_data_age10 <- random.impute.data.frame(combined_data_age10, c(2,3, 23, 24, 25, 26, 27, 28, 29, 30, 31, 32, 33, 34,35,36,37))

#combined_data_age10$complete10 <- rec(combined_data_age10, complete10,  rec = "0=1;1=2", as.num = TRUE, var.label = NULL, val.labels = NULL, append = FALSE, suffix = "_r")
combined_data_age10$complete10=as.factor(combined_data_age10$complete10)
combined_data_age10$a0255.imputed=as.factor(combined_data_age10$a0255.imputed)
combined_data_age10$e245.imputed=as.factor(combined_data_age10$e245.imputed)
combined_data_age10$privatedRented.imputed=as.factor(combined_data_age10$privateRented.imputed)
combined_data_age10$poorNeighborhood.imputed=as.factor(combined_data_age10$poorNeighborhood.imputed)
combined_data_age10$crowded.imputed=as.factor(combined_data_age10$crowded.imputed)
combined_data_age10$twin.imputed=as.factor(combined_data_age10$twin.imputed)
combined_data_age10$father_notBritain.imputed=as.factor(combined_data_age10$father_notBritain.imputed)
combined_data_age10$mother_notBritain.imputed=as.factor(combined_data_age10$mother_notBritain.imputed)
combined_data_age10$leaving_age15Mother.imputed=as.factor(combined_data_age10$leaving_age15Mother.imputed)
combined_data_age10$leaving_age15Father.imputed=as.factor(combined_data_age10$leaving_age15Father.imputed)
combined_data_age10$teen_mum.imputed=as.factor(combined_data_age10$teen_mum.imputed)
combined_data_age10$aged40.imputed=as.factor(combined_data_age10$aged40.imputed)
combined_data_age10$high_mobility.imputed=as.factor(combined_data_age10$high_mobility.imputed)
combined_data_age10$low_birthweight.imputed=as.factor(combined_data_age10$low_birthweight.imputed)
combined_data_age10$oneMonthPlus.imputed=as.factor(combined_data_age10$oneMonthPlus.imputed)
combined_data_age10$noQuals.imputed=as.factor(combined_data_age10$noQuals.imputed)
combined_data_age10$father_unemployed.imputed=as.factor(combined_data_age10$father_unemployed.imputed)
#logistic regression to predict response. use: parent education, SES, mothers age at birth, gender, ethnicity etc
#then get predicted probabilities
#then weight=1/pr if complete==1 (weight is 1 divided by probability.)
missing_age10 <- glm(complete10 ~ a0255.imputed + e245.imputed + teen_mum.imputed +aged40.imputed + noQuals.imputed +father_unemployed.imputed +
                       leaving_age15Father.imputed +leaving_age15Mother.imputed +mother_notBritain.imputed +father_notBritain.imputed +
                       twin.imputed + low_birthweight.imputed + privatedRented.imputed + poorNeighborhood.imputed + crowded.imputed +
                       high_mobility.imputed + oneMonthPlus.imputed  , family=binomial(link='logit'), data=combined_data_age10)
predicted_age10 <- plogis(predict(missing_age10)) 
#new_predicted <- 1 / (1 + exp(-predicted)) - maybe?
predicted_age10[combined_data_age10$complete10==0] <- NA
combined_data_age10$predicted_age10 <- predicted_age10

predicted_age10_1 <- c("bcsid", "predicted_age10")
predicted_age10_1<-combined_data_age10[predicted_age10_1]

#predicted_age10_2 <- na.omit(predicted_age10_1)


predicted_age10_1$weight_age10=1/predicted_age10_1$predicted_age10

#combine with age 5 weight - if age 5 weight is missing, use age 10 weight
#weight_vars <- merge(all=TRUE, predicted_age10_1,predicted_age5_1,by="bcsid")

#weight_vars$combined_weight <- ifelse(!is.na(weight_vars$weight), weight_vars$weight, weight_vars$weight_age10)
#weight <- c("bcsid", "combined_weight")
#weight <- weight_vars[weight]

#attrition weight for age 16 ####
#age 16 ses variables 
#age16_gender <- c("bcsid", "sex86")
#age16_gender <- age16[age16_gender]
#age16_gender$sex86[age16_gender$sex86==-2] <- NA
#age16_gender$sex86[age16_gender$sex86==-1] <- NA

#education
#parent_quals16 <- c("bcsid", "t6.1", "t6.2", "t6.3", "t6.4", "t6.5", "t6.6", "t6.9")
#parent_quals16 <- age16[parent_quals16]
#parent_quals16[parent_quals16==4] <- NA

#parent_quals16$t6.1[parent_quals16$t6.1==1] <- 1
#parent_quals16$t6.1[parent_quals16$t6.1==2] <- 1
#parent_quals16$t6.1[parent_quals16$t6.1==3] <- 1

#parent_quals16$t6.2[parent_quals16$t6.2==1] <- 2
#parent_quals16$t6.2[parent_quals16$t6.2==2] <- 2
#parent_quals16$t6.2[parent_quals16$t6.2==3] <- 2

#parent_quals16$t6.3[parent_quals16$t6.3==1] <- 3
#parent_quals16$t6.3[parent_quals16$t6.3==2] <- 3
#parent_quals16$t6.3[parent_quals16$t6.3==3] <- 3

#parent_quals16$t6.4[parent_quals16$t6.4==1] <- 4
#parent_quals16$t6.4[parent_quals16$t6.4==2] <- 4
#parent_quals16$t6.4[parent_quals16$t6.4==3] <- 4

#parent_quals16$t6.5[parent_quals16$t6.5==1] <- 5
#parent_quals16$t6.5[parent_quals16$t6.5==2] <- 5
#parent_quals16$t6.5[parent_quals16$t6.5==3] <- 5

#parent_quals16$t6.6[parent_quals16$t6.6==1] <- 6
#parent_quals16$t6.6[parent_quals16$t6.6==2] <- 6
#parent_quals16$t6.6[parent_quals16$t6.6==3] <- 6

#parent_quals16$t6.9[parent_quals16$t6.9==1] <- 0
#parent_quals16$t6.9[parent_quals16$t6.9==2] <- 0
#parent_quals16$t6.9[parent_quals16$t6.9==3] <- 0

#parent_quals16$t6.1=as.character(parent_quals16$t6.1)
#parent_quals16$t6.2=as.character(parent_quals16$t6.2)
#parent_quals16$t6.3=as.character(parent_quals16$t6.3)
#parent_quals16$t6.4=as.character(parent_quals16$t6.4)
#parent_quals16$t6.5=as.character(parent_quals16$t6.5)
#parent_quals16$t6.6=as.character(parent_quals16$t6.6)

#parent_quals16$t6.9=as.character(parent_quals16$t6.9)
#parent_quals16<- transform(parent_quals16, parent_ed16 = pmax(t6.1, t6.2, t6.3, t6.4, t6.5, t6.6, t6.9,  na.rm = TRUE))

#age_left_ed <- c("bcsid", "t7.1", "t7.2")
#age_left_ed <- age16[age_left_ed]
#age_left_ed[age_left_ed==-4] <-NA
#age_left_ed[age_left_ed==-2] <-NA
#age_left_ed[age_left_ed==-1] <-NA

#age16_ses <- c("bcsid", "t11.2", "t11.9")
#age16_ses <- age16[age16_ses]
#age16_ses[age16_ses==-1] <-NA
#age16_ses[age16_ses==-2] <-NA
#age16_ses[age16_ses==-4] <-NA
#age16_ses[age16_ses==7] <-NA
#age16_ses[age16_ses==8] <-NA

#for age 16 responses####
#region of parents
region80 = c("bcsid", "bd3regn")
region80=age10_derived[region80]
region80[region80==-1] <- NA
region80[region80==-2] <- NA

#age16_variables <- merge(all=TRUE,  gender,age_left_ed,  by="bcsid")
#age16_variables <- merge(all=TRUE, age16_variables, parent_quals16,by="bcsid")
#age16_variables <- merge(all=TRUE, age16_variables,age16_ses,by="bcsid")
#age16_variables <- merge(all=TRUE, age16_variables,region80,by="bcsid")

#new_age16_variables <- random.impute.data.frame(age16_variables, c(2,3,4, 12, 13, 14))

combined_data_age16=bcs_analysis
combined_data_age16$complete16 <- as.integer(complete.cases(combined_data_age16$b16vocab))
#combined_data_age16<- merge(all=TRUE, age16_variables, bcs_analysis,by="bcsid")

combined_data_age16 =merge(all=TRUE, combined_data_age16, dad_education10,by="bcsid")
combined_data_age16=merge(all=TRUE, combined_data_age16, mum_education10,by="bcsid")
combined_data_age16=merge(all=TRUE, combined_data_age16, age10_ses,by="bcsid")
combined_data_age16=merge(all=TRUE,combined_data_age16, region80,by="bcsid")
#remove na for complete 16 so have those who responded at age 16and who didnt from own sample
combined_data_age16=combined_data_age16[!is.na(combined_data_age16$complete16), ]

combined_data_age16<- random.impute.data.frame(combined_data_age16, c(2,23,24,25,26,27))


combined_data_age16$complete16=as.factor(combined_data_age16$complete16)
combined_data_age16$a0255.imputed=as.factor(combined_data_age16$a0255.imputed)
combined_data_age16$dad_ed10.imputed=as.factor(combined_data_age16$dad_ed10.imputed)
combined_data_age16$mum_ed10.imputed=as.factor(combined_data_age16$mum_ed10.imputed)
combined_data_age16$c3.4.imputed=as.factor(combined_data_age16$c3.4.imputed)
combined_data_age16$c3.11.imputed=as.factor(combined_data_age16$c3.11.imputed)
combined_data_age16$bd3regn.imputed=as.factor(combined_data_age16$bd3regn.imputed)
#logistic regression to predict missingness 
missing_age16 <- glm(complete16 ~ a0255.imputed + dad_ed10.imputed +mum_ed10.imputed +c3.4.imputed +c3.11.imputed + bd3regn.imputed , family=binomial(link='logit'), data=combined_data_age16)

predicted_age16 <- plogis(predict(missing_age16)) 
#new_predicted <- 1 / (1 + exp(-predicted)) - maybe?
predicted_age16[combined_data_age16$complete16==0] <- NA
combined_data_age16$predicted_age16 <- predicted_age16

predicted_age16_1 <- c("bcsid", "predicted_age16")
predicted_age16_1<-combined_data_age16[predicted_age16_1]

#predicted_age16_2 <- na.omit(predicted_age16_1)

predicted_age16_1$weight_age16=1/predicted_age16_1$predicted_age16

#weight16_2 <- predicted_age16_2$weight_age16/1.38

#combine with combined ages 5 and 10 weight to replace missing with age 16 weight
weight_vars2 <- merge(all=TRUE, predicted_age5_1, predicted_age10_1,by="bcsid")
weight_vars2= merge(all=TRUE, weight_vars2, predicted_age16_1)

weight_vars2$combined_weight2 <- ifelse(!is.na(weight_vars2$weight), weight_vars2$weight, weight_vars2$weight_age10)
weight_vars2$combined_weight3 <- ifelse(!is.na(weight_vars2$combined_weight2), weight_vars2$combined_weight2, weight_vars2$weight_age16)
weight2 <- c("bcsid", "combined_weight3")
weight2 <- weight_vars2[weight2]

#add weight variable to the bcs analysis data and apply a constant####
weight2$weight <- weight2$combined_weight3/1.38

weight2 = weight2 %>% select(bcsid, weight) %>% 
  distinct(bcsid, .keep_all =  TRUE)

bcs_analysis$weight = weight2$weight


# Write csv####
write.csv(bcs_analysis, "bcs_ses_comparison_data.csv")

# White Sensitivity check  ####
white_sensitivity <- bcs_analysis[which(bcs_analysis$e245 == "0"),] #binary variable where 0=white, 1=minority 
write.csv(white_sensitivity, file = "bcs_crossCohort_whiteSample.csv")

