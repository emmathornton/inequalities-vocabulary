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
#mcs3_hh <- read_sav("mcs3_hhgrid.sav")
#mcs5_hh <- read_sav("mcs5_hhgrid.sav")
#mcs6_hh <- read_sav("mcs6_hhgrid.sav")
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

