#AGE 14 SES PREDICTORS - SENSITIVITY CHECK

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

#Load in Data
mcs2_child_assessment <- read_sav("mcs2_cm_cognitive_assessment.sav")
mcs3_child_assessment <- read_sav("mcs3_cm_cognitive_assessment.sav")
mcs5_child_assessment <- read_sav("mcs5_cm_cognitive_assessment.sav")
mcs5_child_derived <- read_sav("mcs5_cm_derived.sav")
mcs6_child_assessment<- read_sav("mcs6_cm_cognitive_assessment.sav")
mcs1_hh <- read_sav("mcs1_hhgrid.sav")
mcs2_hh <- read_sav("mcs2_hhgrid.sav")
mcs3_hh <- read_sav("mcs3_hhgrid.sav")
mcs5_hh <- read_sav("mcs5_hhgrid.sav")
mcs6_hh <- read_sav("mcs6_hhgrid.sav")
mcs_family <- read_sav("mcs_longitudinal_family_file.sav")
mcs1_parent <- read_sav("mcs1_parent_interview.sav")
mcs1_derived <- read_sav("mcs1_parent_derived.sav")
mcs2_parent <- read_sav("mcs2_parent_interview.sav")
mcs2_derived <- read_sav("mcs2_parent_derived.sav")
mcs2_derived_family <- read_sav("mcs2_family_derived.sav")
mcs1_derived_family <- read_sav("mcs1_family_derived.sav")
mcs5_parent<- read_sav("mcs5_parent_interview.sav")
mcs6_imd_eng <- read_stata("mcs_sweep6_imd_e_2004.dta")
mcs6_imd_ni <- read_stata("mcs_sweep6_imd_n_2004.dta")
mcs6_imd_sc <- read_stata("mcs_sweep6_imd_s_2004.dta")
mcs6_imd_w <-  read_stata("mcs_sweep6_imd_w_2004.dta")
mcs6_derived_parent <- read_sav("mcs6_parent_derived.sav")
mcs6_derived_family <- read_sav("mcs6_family_derived.sav")
mcs6_parent <- read_sav("mcs6_parent_interview.sav")
mcs6_parent_cm <- read_sav("mcs6_parent_cm_interview.sav")
mcs5_derived_family <- read_sav("mcs5_family_derived.sav")
mcs5_parent <- read_sav("mcs5_parent_interview.sav")
mcs4_parent <- read_sav("mcs4_parent_interview.sav")
mcs3_parent <- read_sav("mcs3_parent_interview.sav")
mcs5_derived_parent <- read_sav("mcs5_parent_derived.sav")
mcs1_parent_12thEd <- read_sav("mcs1_parent_interview_12thEd.sav")
mcs2_parent_9thEd <- read_sav("mcs2_parent_interview_9thEd.sav")
mcs1_cm_derived = read_sav("mcs1_cm_derived.sav")
mcs2_cm_derived = read_sav("mcs2_cm_derived.sav")
mcs1_cm_parent = read_sav("mcs1_parent_cm_interview.sav")
mcs2_cm_parent = read_sav("mcs2_parent_cm_interview.sav")



#convert all to lowercase
names(mcs2_child_assessment) <- tolower(names(mcs2_child_assessment))
names(mcs3_child_assessment) <- tolower(names(mcs3_child_assessment))
names(mcs5_child_assessment) <- tolower(names(mcs5_child_assessment))
names(mcs6_child_assessment) <- tolower(names(mcs6_child_assessment))
names(mcs_family) <- tolower(names(mcs_family))
names(mcs5_parent) <- tolower(names(mcs5_parent))
names(mcs1_parent) <- tolower(names(mcs1_parent))
names(mcs2_parent) <- tolower(names(mcs2_parent))
names(mcs2_derived) <- tolower(names(mcs2_derived))
names(mcs1_derived) <- tolower(names(mcs1_derived))
names(mcs1_hh) <- tolower(names(mcs1_hh))
names(mcs2_hh) <- tolower(names(mcs2_hh))
#names(mcs2_geography) <- tolower(names(mcs2_geography))
#names(mcs1_geography) <- tolower(names(mcs1_geography))
names(mcs6_derived_family) <- tolower(names(mcs6_derived_family))
names(mcs6_derived_parent) <- tolower(names(mcs6_derived_parent))
names(mcs6_parent)<- tolower(names(mcs6_parent))
names(mcs6_parent_cm)<- tolower(names(mcs6_parent_cm))
names(mcs5_parent)<- tolower(names(mcs5_parent))
names(mcs5_derived_family) <- tolower(names(mcs5_derived_family))
names(mcs6_imd_eng) <- tolower(names(mcs6_imd_eng))
names(mcs6_imd_ni) <- tolower(names(mcs6_imd_ni))
names(mcs6_imd_w) <- tolower(names(mcs6_imd_w))
names(mcs6_imd_sc) <- tolower(names(mcs6_imd_sc))
names(mcs4_parent)<- tolower(names(mcs4_parent))
names(mcs3_parent)<- tolower(names(mcs3_parent))
names(mcs5_derived_parent) <- tolower(names(mcs5_derived_parent))
names(mcs1_hh) <- tolower(names(mcs1_hh))
names(mcs2_hh) <- tolower(names(mcs2_hh))
names(mcs6_hh) <- tolower(names(mcs6_hh))
names(mcs1_parent_12thEd) <- tolower(names(mcs1_parent_12thEd))
names(mcs2_parent_9thEd) <- tolower(names(mcs2_parent_9thEd))
names(mcs1_cm_derived) <- tolower(names(mcs1_cm_derived))
names(mcs2_cm_derived) <- tolower(names(mcs2_cm_derived))
names(mcs1_cm_parent) <- tolower(names(mcs1_cm_parent))
names(mcs2_cm_parent) <- tolower(names(mcs2_cm_parent))




#Age 14 cohort member number
mcsid_age14 <- c("mcsid", "fcnum00")
mcsid_age14 <- mcs6_parent_cm[mcsid_age14]


#weight
weights = mcs_family %>% select(mcsid, fovwt2, eovwt2)
weights [weights  == -1] <- NA

weights$weight1 <- ifelse(!is.na(weights$fovwt2), weights$fovwt2, weights$eovwt2)
weights = weights %>% select(mcsid, weight1)

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
  #filter(acnum00 == 1 | bcnum00 == 1) %>% 
  #mutate(cm_number = case_when(
  #  sentry == 1 ~ acnum00, 
   # sentry == 2 ~ bcnum00, 
   # sentry == -1 ~ NA_real_
 # )) %>% 
 # select(mcsid, cm_number)




#Age 14 - Word Activity Scores - create own total score ####
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
 # left_join(mcs_cm1) %>% 
 # filter(cm_number == 1)

#SEC Predictors

#Parent Education ####
#Respondent variable 
#Respondent variable sweep 6
respondent_number <- c("mcsid", "fresp00")
respondent_number <- mcs6_derived_parent[respondent_number]
respondent_number$fresp00 = as.character(respondent_number$fresp00)
main_interview_sweep6 <-  respondent_number[which(respondent_number$fresp00 == "1"),]
parnter_interview_sweep6 <-  respondent_number[which(respondent_number$fresp00 == "2"),]
proxy_interview_sweep6 <-  respondent_number[which(respondent_number$fresp00 == "3"),]

respondent_sweep6 <- c("mcsid", "fdres00")
respondent_sweep6<-mcs6_derived_parent[respondent_sweep6]
main_respondent_sweep6 <- respondent_sweep6 [which(respondent_number$fresp00=="1"),]
partner_respondent_sweep6 <- respondent_sweep6 [which(respondent_number$fresp00=="2"),]
proxy_respondent_sweep6 <- respondent_sweep6 [which(respondent_number$fresp00=="3"),]

#Mother as main 
mother_sweep6_main <- main_respondent_sweep6[main_respondent_sweep6$fdres00 == 1 | main_respondent_sweep6$fdres00== 3 | main_respondent_sweep6$fdres00 == 5 | main_respondent_sweep6$fdres00 == 7 |main_respondent_sweep6$fdres00 == 9 |main_respondent_sweep6$fdres00 == 11 | main_respondent_sweep6$fdres00 == 13|main_respondent_sweep6$fdres00 == 15 ,]
#Mother as partner
mother_sweep6_partner <- partner_respondent_sweep6[partner_respondent_sweep6$fdres00 == 1 | partner_respondent_sweep6$fdres00== 3 | partner_respondent_sweep6$fdres00 == 5 |partner_respondent_sweep6$fdres00 == 7 |partner_respondent_sweep6$fdres00 == 9 |partner_respondent_sweep6$fdres00 == 11 | partner_respondent_sweep6$fdres00 == 13|partner_respondent_sweep6$fdres00 == 15 ,]
#Mother Proxy
mother_sweep6_proxy <- proxy_respondent_sweep6[proxy_respondent_sweep6$fdres00 == 1 | proxy_respondent_sweep6$fdres00== 3 | proxy_respondent_sweep6$fdres00 == 5 | proxy_respondent_sweep6$fdres00 == 7 |proxy_respondent_sweep6$fdres00 == 9 |proxy_respondent_sweep6$fdres00 == 11 | proxy_respondent_sweep6$fdres00 == 13|proxy_respondent_sweep6$fdres00 == 15 ,]

#Father main
father_sweep6_main <- main_respondent_sweep6[main_respondent_sweep6$fdres00 == 2 | main_respondent_sweep6$fdres00== 4 | main_respondent_sweep6$fdres00 == 6 |main_respondent_sweep6$fdres00 == 8 |main_respondent_sweep6$fdres00 == 10 |main_respondent_sweep6$fdres00 == 12 | main_respondent_sweep6$fdres00 == 14|main_respondent_sweep6$fdres00 == 16 |main_respondent_sweep6$fdres00 == 22 | main_respondent_sweep6$fdres00 == 24 ,]
#Father Partner
father_sweep6_partner <- partner_respondent_sweep6[partner_respondent_sweep6$fdres00 == 2 | partner_respondent_sweep6$fdres00== 4 | partner_respondent_sweep6$fdres00 == 6 |partner_respondent_sweep6$fdres00 == 8 |partner_respondent_sweep6$fdres00 == 10 |partner_respondent_sweep6$fdres00 == 12 | partner_respondent_sweep6$fdres00 == 14|partner_respondent_sweep6$fdres00 == 16 |partner_respondent_sweep6$fdres00 == 22 |partner_respondent_sweep6$fdres00 == 24 ,]
#Father proxy 
father_sweep6_proxy <- proxy_respondent_sweep6[proxy_respondent_sweep6$fdres00 == 2 | proxy_respondent_sweep6$fdres00== 4 | proxy_respondent_sweep6$fdres00 == 6 |proxy_respondent_sweep6$fdres00 == 8 |proxy_respondent_sweep6$fdres00 == 10 |proxy_respondent_sweep6$fdres00 == 12 | proxy_respondent_sweep6$fdres00 == 14|proxy_respondent_sweep6$fdres00 == 16 |proxy_respondent_sweep6$fdres00 == 22 |proxy_respondent_sweep6$fdres00 == 24 ,]

#Sweep 6 NVQ
sweep6_NVQ <- c("mcsid", "fdnvq00")
sweep6_NVQ <- mcs6_derived_parent[sweep6_NVQ]
sweep6_NVQ$fdnvq00[sweep6_NVQ$fdnvq00==-1] <- NA
sweep6_NVQ$fdnvq00[sweep6_NVQ$fdnvq00==-9] <- NA
sweep6_NVQ$fdnvq00[sweep6_NVQ$fdnvq00==-8] <- NA
sweep6_NVQ$fdnvq00[sweep6_NVQ$fdnvq00==95] <- 0 #overseas qual only - convert into an NVQ level 0
sweep6_NVQ$fdnvq00[sweep6_NVQ$fdnvq00==96] <- 0 #none of these - convert into an NVQ level 0

main_nvq<-sweep6_NVQ[which(respondent_number$fresp00=="1"),]
partner_nvq<-sweep6_NVQ[which(respondent_number$fresp00=="2"),]
proxy_nvq<-sweep6_NVQ[which(respondent_number$fresp00=="3"),]
NVQ6 <- merge(all=TRUE, main_nvq, partner_nvq,by="mcsid")
NVQ6 <- merge(all=TRUE, NVQ6, proxy_nvq,by="mcsid")

NVQ6_mother_main <- main_nvq[main_nvq$mcsid %in% mother_sweep6_main$mcsid,]
NVQ6_mother_partner <- partner_nvq[partner_nvq$mcsid %in% mother_sweep6_partner$mcsid,]
NVQ6_mother_proxy <- proxy_nvq[proxy_nvq$mcsid %in% mother_sweep6_proxy$mcsid,]
NVQ6_mother <- merge(all=TRUE, NVQ6_mother_main, NVQ6_mother_partner,by="mcsid")
NVQ6_mother <- merge(all=TRUE, NVQ6_mother, NVQ6_mother_proxy,by="mcsid")
NVQ6_mother$mother_nvq6 <- ifelse(!is.na(NVQ6_mother$fdnvq00.x), NVQ6_mother$fdnvq00.x, NVQ6_mother$fdnvq00.y)
NVQ6_mother$mother_nvq6_1 <- ifelse(!is.na(NVQ6_mother$mother_nvq6), NVQ6_mother$mother_nvq6, NVQ6_mother$fdnvq00)                               

NVQ6_father_main <- main_nvq[main_nvq$mcsid %in% father_sweep6_main$mcsid,]
NVQ6_father_partner <- partner_nvq[partner_nvq$mcsid %in% father_sweep6_partner$mcsid,]
NVQ6_father_proxy <- proxy_nvq[proxy_nvq$mcsid %in% father_sweep6_proxy$mcsid,]
NVQ6_father <- merge(all=TRUE, NVQ6_father_main, NVQ6_father_partner,by="mcsid")
NVQ6_father <- merge(all=TRUE, NVQ6_father, NVQ6_father_proxy,by="mcsid")
NVQ6_father$father_nvq6 <- ifelse(!is.na(NVQ6_father$fdnvq00.x), NVQ6_father$fdnvq00.x, NVQ6_father$fdnvq00.y)
NVQ6_father$father_nvq6_1 <- ifelse(!is.na(NVQ6_father$father_nvq6), NVQ6_father$father_nvq6, NVQ6_father$fdnvq00)    

parent_NVQ_6 <- merge(all=TRUE, NVQ6_mother, NVQ6_father,by="mcsid")
sweep6_parent_nvq<- c("mcsid", "mother_nvq6_1", "father_nvq6_1")
sweep6_parent_nvq<- parent_NVQ_6[sweep6_parent_nvq]
sweep6_parent_nvq  <- transform(sweep6_parent_nvq, highestNVQ_sweep6 = pmax(mother_nvq6_1, father_nvq6_1, na.rm=TRUE))

#highest_NVQ1 <- data.frame(parent_NVQ, highest_NVQ)
highest_parent_NVQ6 <- c("mcsid", "highestNVQ_sweep6")
highest_parent_NVQ6<- sweep6_parent_nvq[highest_parent_NVQ6]

#Occupational Status ####
sweep6_occupation <- c("mcsid", "fd05s00")
sweep6_occupation <- mcs6_derived_parent[sweep6_occupation]
sweep6_occupation$fd05s00[sweep6_occupation$fd05s00==-1] <- NA

main_occupation<-sweep6_occupation[which(respondent_number$fresp00=="1"),]
partner_occupation<-sweep6_occupation[which(respondent_number$fresp00=="2"),]
proxy_occupation<-sweep6_occupation[which(respondent_number$fresp00=="3"),]
occupation6 <- merge(all=TRUE, main_occupation, partner_occupation,by="mcsid")
occupation6 <- merge(all=TRUE, occupation6, proxy_occupation,by="mcsid")

occupation6_mother_main <- main_occupation[main_occupation$mcsid %in% mother_sweep6_main$mcsid,]
occupation6_mother_partner <- partner_occupation[partner_occupation$mcsid %in% mother_sweep6_partner$mcsid,]
occupation6_mother_proxy <- proxy_occupation[proxy_occupation$mcsid %in% mother_sweep6_proxy$mcsid,]
occupation6_mother <- merge(all=TRUE, occupation6_mother_main, occupation6_mother_partner,by="mcsid")
occupation6_mother <- merge(all=TRUE, occupation6_mother, occupation6_mother_proxy,by="mcsid")
occupation6_mother$mother_occupation6 <- ifelse(!is.na(occupation6_mother$fd05s00.x), occupation6_mother$fd05s00.x, occupation6_mother$fd05s00.y)
occupation6_mother$mother_occupation6_1 <- ifelse(!is.na(occupation6_mother$mother_occupation6),occupation6_mother$mother_occupation6, occupation6_mother$fd05s00)     


occupation6_mother$mother_occupation6_1[occupation6_mother$mother_occupation6_1==1] <- 1
occupation6_mother$mother_occupation6_1[occupation6_mother$mother_occupation6_1==2] <- 2
occupation6_mother$mother_occupation6_1[occupation6_mother$mother_occupation6_1==3] <- 2
occupation6_mother$mother_occupation6_1[occupation6_mother$mother_occupation6_1==4] <- 3
occupation6_mother$mother_occupation6_1[occupation6_mother$mother_occupation6_1==5] <- 3

occupation6_father_main <- main_occupation[main_occupation$mcsid %in% father_sweep6_main$mcsid,]
occupation6_father_partner <- partner_occupation[partner_occupation$mcsid %in% father_sweep6_partner$mcsid,]
occupation6_father_proxy <- proxy_occupation[proxy_occupation$mcsid %in% father_sweep6_proxy$mcsid,]
occupation6_father <- merge(all=TRUE, occupation6_father_main, occupation6_father_partner,by="mcsid")
occupation6_father <- merge(all=TRUE, occupation6_father, occupation6_father_proxy,by="mcsid")
occupation6_father$father_occupation6 <- ifelse(!is.na(occupation6_father$fd05s00.x), occupation6_father$fd05s00.x, occupation6_father$fd05s00.y)
occupation6_father$father_occupation6_1 <- ifelse(!is.na(occupation6_father$father_occupation6),occupation6_father$father_occupation6, occupation6_father$fd05s00)     


occupation6_father$father_occupation6_1[occupation6_father$father_occupation6_1==1] <- 1
occupation6_father$father_occupation6_1[occupation6_father$father_occupation6_1==2] <- 2
occupation6_father$father_occupation6_1[occupation6_father$father_occupation6_1==3] <- 2
occupation6_father$father_occupation6_1[occupation6_father$father_occupation6_1==4] <- 3
occupation6_father$father_occupation6_1[occupation6_father$father_occupation6_1==5] <- 3


parent_occupation_6 <- merge(all=TRUE, occupation6_mother, occupation6_father,by="mcsid")
sweep6_parent_occupation<- c("mcsid", "mother_occupation6_1", "father_occupation6_1")
sweep6_parent_occupation<- parent_occupation_6[sweep6_parent_occupation]


#labour market status
sweep6_labour <- c("mcsid", "fdwrk00")
sweep6_labour <- mcs6_derived_parent[sweep6_labour]
sweep6_labour$fdwrk00[sweep6_labour$fdwrk00==-1] <- NA

main_labour<-sweep6_labour[which(respondent_number$fresp00=="1"),]
partner_labour<-sweep6_labour[which(respondent_number$fresp00=="2"),]
proxy_labour<-sweep6_labour[which(respondent_number$fresp00=="3"),]
labour6 <- merge(all=TRUE, main_labour, partner_labour,by="mcsid")
labour6 <- merge(all=TRUE, labour6, proxy_labour,by="mcsid")

labour6_mother_main <- main_labour[main_labour$mcsid %in% mother_sweep6_main$mcsid,]
labour6_mother_partner <- partner_labour[partner_labour$mcsid %in% mother_sweep6_partner$mcsid,]
labour6_mother_proxy <- proxy_labour[proxy_labour$mcsid %in% mother_sweep6_proxy$mcsid,]
labour6_mother <- merge(all=TRUE, labour6_mother_main, labour6_mother_partner,by="mcsid")
labour6_mother <- merge(all=TRUE, labour6_mother, labour6_mother_proxy,by="mcsid")
labour6_mother$mother_labour6 <- ifelse(!is.na(labour6_mother$fdwrk00.x), labour6_mother$fdwrk00.x, labour6_mother$fdwrk00.y)
labour6_mother$mother_labour6_1 <- ifelse(!is.na(labour6_mother$mother_labour6),labour6_mother$mother_labour6, labour6_mother$fdwrk00)     

labour6_father_main <- main_labour[main_labour$mcsid %in% father_sweep6_main$mcsid,]
labour6_father_partner <- partner_labour[partner_labour$mcsid %in% father_sweep6_partner$mcsid,]
labour6_father_proxy <- proxy_labour[proxy_labour$mcsid %in% father_sweep6_proxy$mcsid,]
labour6_father <- merge(all=TRUE, labour6_father_main, labour6_father_partner,by="mcsid")
labour6_father <- merge(all=TRUE, labour6_father, labour6_father_proxy,by="mcsid")
labour6_father$father_labour6 <- ifelse(!is.na(labour6_father$fdwrk00.x), labour6_father$fdwrk00.x, labour6_father$fdwrk00.y)
labour6_father$father_labour6_1 <- ifelse(!is.na(labour6_father$father_labour6),labour6_father$father_labour6, labour6_father$fdwrk00)     

parent_labour_6 <- merge(all=TRUE, labour6_mother, labour6_father,by="mcsid")
sweep6_parent_labour<- c("mcsid", "mother_labour6_1", "father_labour6_1")
sweep6_parent_labour<- parent_labour_6[sweep6_parent_labour]

sweep6_mother_occupation <- c("mcsid", "mother_occupation6_1")
sweep6_mother_occupation <- occupation6_mother[sweep6_mother_occupation]
sweep6_father_occupation <- c("mcsid", "father_occupation6_1")
sweep6_father_occupation <- occupation6_father[sweep6_father_occupation]

sweep6_mother_labour <- c("mcsid", "mother_labour6_1")
sweep6_mother_labour <- labour6_mother[sweep6_mother_labour]
sweep6_father_labour <- c("mcsid", "father_labour6_1")
sweep6_father_labour <- labour6_father[sweep6_father_labour]


occupation_4cat_mother <- merge(all=TRUE, sweep6_mother_occupation , sweep6_mother_labour, by="mcsid")
occupation_4cat_mother[is.na(occupation_4cat_mother$mother_occupation6_1) & occupation_4cat_mother$mother_labour6_1 %in% c(2),]$mother_occupation6_1 = 4

occupation_4cat_father <- merge(all=TRUE, sweep6_father_occupation , sweep6_father_labour, by="mcsid")
occupation_4cat_father[is.na(occupation_4cat_father$father_occupation6_1) & occupation_4cat_father$father_labour6_1 %in% c(2),]$father_occupation6_1 = 4

occupation_4cat <- merge (all=TRUE, occupation_4cat_mother, occupation_4cat_father,by="mcsid")
occupation_4cat<-transform(occupation_4cat, highest_occupation_sweep6 = pmax(mother_occupation6_1, father_occupation6_1, na.rm=TRUE))

#highest_NVQ1 <- data.frame(parent_NVQ, highest_NVQ)
highest_parent_occupation6 <- c("mcsid", "highest_occupation_sweep6")
highest_parent_occupation6<- occupation_4cat[highest_parent_occupation6]

#Income ####
household_grid_age14 = mcs6_hh %>% select(
  mcsid, fpnum00, fcnum00,fhcrel00,fhcage00,
  fhpage00, fhcrel00, fhpres00) %>% 
  group_by(mcsid) %>% 
  mutate(fpnum00 = case_when(
    (is.na(fpnum00) & fhcrel00 == 96) ~ 0, #cohort member
    TRUE ~ as.numeric(fpnum00)))
#add "person" to person number column 
household_grid_age14$fpnum00 = paste0("Person_", household_grid_age14$fpnum00)

#add in binary variables (1 = yes; 2 = no) to indicate whether each person is a child (age <=13) and present in household, or adult and present in household - will total these later to give total number of adults and children in household. 
household_grid_age14 = household_grid_age14 %>% 
  mutate(isChild_andPresent = case_when(fhpage00 <=13 & (fhpres00 == 1 ) ~ 1,
                                        fhcage00 <= 13 ~ 1,
                                        fhpage00 >13 | fhcage00 >13 ~ 0, 
                                        is.na(fhpage00) ~ NA_real_, 
                                        is.na(fhcage00) ~ NA_real_,
                                        TRUE ~ 0), .after = "fhpage00") %>% 
  mutate(isAdult_andPresent = case_when(fhpage00 >13 & (fhpres00 == 1 ) ~ 1,
                                        fhcage00 >13 ~ 1, 
                                        fhpage00 <=13 | fhcage00 <= 13 ~ 0, 
                                        is.na(fhpage00) ~ NA_real_, 
                                        is.na(fhcage00) ~ NA_real_,
                                        TRUE ~ 0), .after = "fhpage00") 
#select mcsid, person number, and created binary variavles 
household_grid_age14 = household_grid_age14 %>% select(mcsid, fpnum00,  isChild_andPresent, isAdult_andPresent)

#convert household grid variables to wide format- 1 row per mcsid, so that we can use rowSums to create total number of adults and children variables 
household_gridWide_age14 = household_grid_age14 %>% 
  group_by(mcsid, fpnum00) %>% 
  mutate(row=row_number()) %>% 
  pivot_wider(names_from = fpnum00, values_from = c("isChild_andPresent", "isAdult_andPresent")) %>% 
  select(-row) 

#create total number of children and total number of adults in household
household_gridWide_age14 = household_gridWide_age14 %>% 
  ungroup() %>% 
  mutate(number_ofChildren =  rowSums(.[c("isChild_andPresent_Person_1","isChild_andPresent_Person_2" , "isChild_andPresent_Person_3" , "isChild_andPresent_Person_4" , "isChild_andPresent_Person_5", "isChild_andPresent_Person_6" , "isChild_andPresent_Person_7" , "isChild_andPresent_Person_8" , "isChild_andPresent_Person_9" ,"isChild_andPresent_Person_10", "isChild_andPresent_Person_11" ,"isChild_andPresent_Person_12", "isChild_andPresent_Person_13", "isChild_andPresent_Person_14" ,"isChild_andPresent_Person_15", "isChild_andPresent_Person_16", "isChild_andPresent_Person_17","isChild_andPresent_Person_18", "isChild_andPresent_Person_19","isChild_andPresent_Person_0"  )], #person 0 refers to cohort members.
                                      na.rm = TRUE), .after ="mcsid") %>% 
  mutate(number_ofAdults =  rowSums(.[c("isAdult_andPresent_Person_1","isAdult_andPresent_Person_2" , "isAdult_andPresent_Person_3" , "isAdult_andPresent_Person_4" , "isAdult_andPresent_Person_5", "isAdult_andPresent_Person_6" , "isAdult_andPresent_Person_7" , "isAdult_andPresent_Person_8" , "isAdult_andPresent_Person_9" ,"isAdult_andPresent_Person_10", "isAdult_andPresent_Person_11" ,"isAdult_andPresent_Person_12", "isAdult_andPresent_Person_13", "isAdult_andPresent_Person_14" ,"isAdult_andPresent_Person_15", "isAdult_andPresent_Person_16", "isAdult_andPresent_Person_17","isAdult_andPresent_Person_18","isAdult_andPresent_Person_19", "isAdult_andPresent_Person_0" )],
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
  mutate(total_equivalenceage14 =  rowSums(.[c("childrenEquivalence", "adultEquivalence")], #create total equivalence value by summing child and adult equivalence values 
                                           na.rm = TRUE), .before = "childrenEquivalence") 
age14_equivalence = household_gridWide_age14 %>% select(mcsid, total_equivalenceage14)

#Take mid-point of each band and multiply this by the equivilisation score. 
age14_income = mcs6_parent%>% select(mcsid, fpntco00,fpntlp00, fresp00) %>% 
  mutate(respondent = case_when( fresp00 == 1 ~ "main", fresp00 == 2 ~ "partner")) %>% 
  select(mcsid, fpntco00, fpntlp00, respondent)
age14_income[,2:3] <- lapply(age14_income[,2:3], as.numeric)
age14_income = age14_income %>% 
  mutate(annual_incomeMedianJoint = case_when(
    fpntco00 == 2 ~ median(as.numeric(c("0","4999"))), 
    fpntco00 == 3 ~ median(as.numeric(c("5000","10999"))),
    fpntco00 == 4 ~ median(as.numeric(c("11000","14499"))), 
    fpntco00 == 5 ~ median(as.numeric(c("14500","15999"))), 
    fpntco00 == 6 ~ median(as.numeric(c("16000","17999"))),  
    fpntco00 == 7 ~ median(as.numeric(c("18000","18999"))), 
    fpntco00 == 8 ~ median(as.numeric(c("19000","22999"))),  
    fpntco00 == 9 ~ median(as.numeric(c("23000","25999"))), 
    fpntco00 == 10 ~ median(as.numeric(c("26000","29999"))), 
    fpntco00 == 11 ~ median(as.numeric(c("30000","32999"))), 
    fpntco00 == 12 ~ median(as.numeric(c("33000","37499"))), 
    fpntco00 == 13 ~ median(as.numeric(c("37500","42999"))), 
    fpntco00 == 14 ~ median(as.numeric(c("43000","50499"))), 
    fpntco00 == 15 ~ median(as.numeric(c("50500","57499"))), 
    fpntco00 == 16 ~ median(as.numeric(c("57500","65999"))), 
    fpntco00 == 17 ~ median(as.numeric(c("66000","82999"))), 
    fpntco00 == 18 ~ median(as.numeric(c("83000","109999"))), 
    fpntco00 == 19 ~ median(as.numeric(c("110000","139999"))), 
    fpntco00 == 20 ~ 140000, 
    fpntco00 == -3 | fpntco00 == -1 | fpntco00 == -9 | fpntco00 == -8 ~ NA_real_, 
    is.na(fpntco00) ~ NA_real_))  %>% 
  mutate(annual_incomeMedianLone = case_when(
    fpntlp00 == 2 ~ median(as.numeric(c("0","2999"))), 
    fpntlp00 == 3 ~ median(as.numeric(c("3000","6999"))),
    fpntlp00 == 4 ~ median(as.numeric(c("7000","8999"))), 
    fpntlp00 == 5 ~ median(as.numeric(c("9000","10499"))), 
    fpntlp00 == 6 ~ median(as.numeric(c("10500","10999"))),  
    fpntlp00 == 7 ~ median(as.numeric(c("11000","11999"))), 
    fpntlp00 == 8 ~ median(as.numeric(c("12000","14999"))),  
    fpntlp00 == 9 ~ median(as.numeric(c("15000","16999"))), 
    fpntlp00 == 10 ~ median(as.numeric(c("17000","17999"))), 
    fpntlp00 == 11 ~ median(as.numeric(c("18000","19999"))), 
    fpntlp00 == 12 ~ median(as.numeric(c("20000","21999"))), 
    fpntlp00 == 13 ~ median(as.numeric(c("22000","23999"))), 
    fpntlp00 == 14 ~ median(as.numeric(c("24000","26999"))), 
    fpntlp00 == 15 ~ median(as.numeric(c("27000","28999"))), 
    fpntlp00 == 16 ~ median(as.numeric(c("29000","30999"))), 
    fpntlp00 == 17 ~ median(as.numeric(c("31000","36999"))), 
    fpntlp00 == 18 ~ median(as.numeric(c("37000","44999"))), 
    fpntlp00 == 19 ~ median(as.numeric(c("45000","55999"))), 
    fpntlp00 == 20 ~ 56000, 
    fpntlp00 == -3 | fpntlp00 == -1 | fpntlp00 == -9 | fpntlp00 == -8 ~ NA_real_, 
    is.na(fpntlp00) ~ NA_real_))

#OECD equivilisation 
age14income_wide = age14_income %>% 
  group_by(mcsid, respondent) %>% 
  mutate(row=row_number()) %>% 
  pivot_wider(names_from = respondent, values_from = c("fpntco00", "annual_incomeMedianJoint", "annual_incomeMedianLone")) %>% 
  select(-row) %>% 
  ungroup() %>% 
  mutate(annual_incomeJoint = case_when(!is.na(annual_incomeMedianJoint_main) ~ annual_incomeMedianJoint_main, 
                                        is.na(annual_incomeMedianJoint_main) ~ annual_incomeMedianJoint_partner)) %>% 
  mutate(annual_income = case_when(!is.na(annual_incomeJoint)  ~ annual_incomeJoint, 
                                   is.na(annual_incomeJoint)  ~ annual_incomeMedianLone_main)) %>% 
  select(mcsid, annual_income) %>% 
  inner_join(age14_equivalence) %>% 
  mutate(oecd_adjusted = annual_income/total_equivalenceage14) %>%  #divide income by the total_equivalence to give equivalised income 
  mutate(income_quintiles = quantcut(oecd_adjusted,5)) %>% 
  select(mcsid, income_quintiles) %>% 
  distinct(mcsid, .keep_all =  TRUE)

levels(age14income_wide$income_quintiles)[1] = "1"
levels(age14income_wide$income_quintiles)[2] = "2"
levels(age14income_wide$income_quintiles)[3] = "3"
levels(age14income_wide$income_quintiles)[4] = "4"
levels(age14income_wide$income_quintiles)[5] = "5"

#Wealth ####
wealth_variables <- mcs6_parent %>%  select(mcsid, fresp00, fpmopa00, fphval00, fpinvt00, fpdeba00, felig00) %>% 
  filter(felig00 == 1) %>% 
  select(mcsid, fpmopa00, fphval00, fpinvt00, fpdeba00) %>% 
  rename("mortgage" = fpmopa00,
         "houseValue" = fphval00, 
         "savings" = fpinvt00, 
         "debt" = fpdeba00)

tenure = mcs6_derived_family %>% select(mcsid, fdroow00)


new_wealth = merge(all=TRUE, wealth_variables, tenure, by = "mcsid")
#if mortgage missing and tenure = not a home owner --> mortgage = 0 

housing_wealthVars = new_wealth %>% 
  mutate(new_mortgage = case_when(
    !is.na(mortgage) ~ as.numeric(mortgage),
    is.na(mortgage) & fdroow00 == 2 | fdroow00 == 3 ~ NA_real_,
    (fdroow00 != 2 | fdroow00 != 3) & (is.na(mortgage))   ~ 0 
  )) %>% 
  mutate(new_value = case_when(
    !is.na(houseValue) ~ as.numeric(houseValue), 
    is.na(mortgage) & fdroow00 == 1 | fdroow00 == 2 | fdroow00 == 3 ~ NA_real_,
    (fdroow00 != 1 | fdroow00 != 2 | fdroow00 != 3) & (is.na(mortgage))   ~ 0
  )) %>% 
  select(mcsid, mortgage, new_mortgage, houseValue, new_value)


#savings variables
savings = mcs6_parent %>% select(mcsid, fpinvt00, contains("fpsavi0"), felig00) %>% 
  filter(felig00 == 1) %>% 
  mutate(new_savings = case_when(
    !is.na(fpinvt00) ~ as.numeric(fpinvt00), 
    is.na(fpinvt00) & fpsavi0a == 1 |fpsavi0b == 1 |
      fpsavi0c == 1 | fpsavi0d == 1 | fpsavi0e == 1 |
      fpsavi0f == 1 | fpsavi0g == 1 | fpsavi0h == 1 ~ NA_real_, 
    (fpsavi0a == 0 |fpsavi0b == 0 |
       fpsavi0c == 0 | fpsavi0d == 0 | fpsavi0e == 0 |
       fpsavi0f == 0 | fpsavi0g == 0 | fpsavi0h == 0) & is.na(fpinvt00)  ~ 0
  )) %>% 
  select(mcsid, fpinvt00, new_savings)

#debt variables 
debt = mcs6_parent %>% select(mcsid, fpdeba00, contains("fpdebt0"), felig00) %>% 
  filter(felig00 == 1) %>% 
  select(-felig00) %>% 
  mutate(new_debt = case_when(
    !is.na(fpdeba00) ~ as.numeric(fpdeba00), 
    is.na(fpdeba00) & fpdebt0a == 1 |fpdebt0b == 1 |
      fpdebt0c == 1 | fpdebt0d == 1 | fpdebt0e == 1 |
      fpdebt0f == 1 | fpdebt0g == 1 | fpdebt0h == 1 |
      fpdebt0i == 1 ~ NA_real_, 
    (fpdebt0a == 0 |fpdebt0b == 0 |
       fpdebt0c == 0 | fpdebt0d == 0 | 
       fpdebt0e == 0 | fpdebt0f == 0 | 
       fpdebt0g == 0 | fpdebt0h == 0 | fpdebt0i == 0) & is.na(fpdeba00)  ~ 0
  )) %>% 
  select(mcsid, fpdeba00, new_debt)

wealth_vars = merge(all = TRUE, housing_wealthVars, savings, by = "mcsid")
wealth_vars = merge(all= TRUE, wealth_vars, debt, by = "mcsid")

wealth = wealth_vars %>% select(mcsid, new_mortgage, new_value, new_savings, new_debt)

#IMD ####
imd = mcs6_imd_eng %>% 
  full_join(mcs6_imd_ni) %>% 
  full_join(mcs6_imd_sc) %>% 
  full_join(mcs6_imd_w) %>% 
  select(mcsid, factry00, contains("fimdsco"),fisimdsc,fiwimdsc) %>% 
  mutate(imd = case_when(factry00 == 1 ~ fimdscoe,
                         factry00 == 2 ~ fiwimdsc,
                         factry00 == 3 ~ fisimdsc,
                         factry00 == 4 ~ fimdscon, 
                         TRUE ~ NA_real_)) %>% 
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

#2. Housing tenure at Age 11 ####
tenure_sweep5 = mcs5_derived_family %>% select(mcsid, eroow00) 


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

#3. Accommodation type at age 11####
accommodation_sweep5 = mcs5_parent %>% select(mcsid, epmoty00, epmotx00, eelig00) %>% 
  filter (eelig00 == 1)
#Age 7
accommodation_sweep4 = mcs4_parent %>% select(mcsid,  dpmtyz00, delig00) %>% 
  filter (delig00 == 1) %>% 
  full_join(accommodation_sweep5) 
accommodation_sweep4$accommodation =  
  ifelse(!is.na(accommodation_sweep4$epmoty00),
         accommodation_sweep4$epmoty00,
         accommodation_sweep4$dpmtyz00) 
accommodation_sweep4 = accommodation_sweep4 %>% 
  select(mcsid, accommodation)
#Age 5
accommodation_sweep3 = mcs3_parent %>% select(mcsid,  cpmotz00, celig00) %>% 
  filter (celig00 == 1) %>% 
  full_join(accommodation_sweep4)
accommodation_sweep3$accommodation1 =  
  ifelse(!is.na(accommodation_sweep3$accommodation),
         accommodation_sweep3$accommodation,
         accommodation_sweep3$cpmotz00) 

accommodation_sweep2 = mcs2_parent %>% select(mcsid, bpmotm00, belig00) %>% 
  filter (belig00 == 1)

accommodation_sweep1 = mcs1_parent %>% select(mcsid, apmoty00, aelig00) %>% 
  filter (aelig00 == 1) %>% 
  merge(all=TRUE, accommodation_sweep2, by="mcsid") %>% 
  merge(all=TRUE, accommodation_sweep3, by="mcsid") 
accommodation_sweep1$accommodation_2 = 
  ifelse(!is.na(accommodation_sweep1$apmoty00),accommodation_sweep1$apmoty00, accommodation_sweep1$bpmotm00)  
accommodation_sweep1$accommodation_type = 
  ifelse(!is.na(accommodation_sweep1$accommodation1),accommodation_sweep1$accommodation1, accommodation_sweep1$accommodation_2)

accommodation = accommodation_sweep1 %>% select(mcsid, accommodation_type)

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
accommodation[accommodation==51] <-3

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


#5. Number of parents present in household at age 11 ####
carers_in_hh = mcs5_derived_family %>% select(mcsid,ehtys00) %>% 
  rename("carers_in_hh" = ehtys00)


#Create analysis data 
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
analysis_data = merge(all=TRUE, analysis_data, highest_parent_NVQ6, by = "mcsid")
nrow(analysis_data)
analysis_data = merge(all= TRUE, analysis_data, breastfed, by = "mcsid")
nrow(analysis_data)
analysis_data = merge(all=TRUE, analysis_data, carers_in_hh, by = "mcsid")
nrow(analysis_data)
analysis_data = merge(all=TRUE, analysis_data, age14income_wide, by = "mcsid")
nrow(analysis_data)
analysis_data = merge(all=TRUE, analysis_data, imd, by = "mcsid")
nrow(analysis_data)
analysis_data = merge(all=TRUE, analysis_data, highest_parent_occupation6, by = "mcsid")
nrow(analysis_data)
analysis_data = merge(all=TRUE, analysis_data, wealth, by = "mcsid")
nrow(analysis_data)
analysis_data = merge(all=TRUE, analysis_data, age14_vocab, by = "mcsid")
nrow(analysis_data)
analysis_data = merge(all=TRUE, analysis_data, weights, by = "mcsid")
nrow(analysis_data)
analysis_data = analysis_data %>% left_join(mcs_cm1)
nrow(analysis_data) 
mcs_analysis <- analysis_data %>% filter(cm_number == 1) %>% 
  filter(!is.na(age14_vocab)) %>% 
  select(-acnum00, -bcnum00, -sentry, -cm_number)


write.csv(mcs_analysis, file = "age14_SES_data.csv")

