#AGE 14 SES PREDICTORS - SENSITIVITY CHECK
library(haven)
require(swfscMisc)
require(sjmisc)
require(Hmisc)
require(psych)
#load in data
mcs2_child_assessment <- read_sav("mcs2_cm_cognitive_assessment.sav")
mcs3_child_assessment <- read_sav("mcs3_cm_cognitive_assessment.sav")
mcs5_child_assessment <- read_sav("mcs5_cm_cognitive_assessment.sav")
mcs5_child_derived <- read_sav("mcs5_cm_derived.sav")
mcs6_child_assessment<- read_sav("mcs6_cm_cognitive_assessment.sav")
mcs1_hh <- read_sav("mcs1_hhgrid.sav")
mcs2_hh <- read_sav("mcs2_hhgrid.sav")
mcs6_hh <- read_sav("mcs6_hhgrid.sav")
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
names(mcs6_hh) <- tolower(names(mcs6_hh))
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

sweep_entry <- c("mcsid", "sentry")
sweep_entry <- mcs_family[sweep_entry]
sweep_entry$sentry = as.character(sweep_entry$sentry)

mcsid_age14 <- c("mcsid", "fcnum00")
mcsid_age14 <- mcs6_parent_cm[mcsid_age14]
mcsid_age14$fcnum00 = as.character(mcsid_age14$fcnum00)
mcsid_number_age9mo <- c("mcsid", "ahcnuma0")
mcsid_number_age9mo <- mcs1_parent[mcsid_number_age9mo]
mcsid_number_age9mo$ahcnuma0 = as.character(mcsid_number_age9mo$ahcnuma0)

mcsid_number_age3 <- c("mcsid", "bhcnuma0")
mcsid_number_age3 <- mcs2_parent[mcsid_number_age3]
mcsid_number_age3$bhcnuma0 = as.character(mcsid_number_age3$bhcnuma0)

#weight
#attrition and sample weight age 3 sweep 
mcs6_weight <- c("mcsid", "fovwt2")
mcs6_weight <- mcs_family[mcs6_weight]
mcs6_weight [mcs6_weight  ==-1] <- NA

mcs5_weight <- c("mcsid", "eovwt2")
mcs5_weight <- mcs_family[mcs5_weight]
mcs5_weight [mcs5_weight  ==-1] <- NA

weight <- merge(all=TRUE, mcs6_weight, mcs5_weight,by="mcsid")
weight$weight1 <- ifelse(!is.na(weight$fovwt2), weight$fovwt2, weight$eovwt2)
mcs_weight <- c("mcsid", "weight1")
mcs_weight <- weight[mcs_weight]

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

#Respondent variable sweep 6
respondent_number <- c("mcsid", "fresp00")
respondent_number <- mcs6_derived_parent[respondent_number]
respondent_number$fresp00 = as.character(respondent_number$fresp00)
main_interview_sweep6 <-  respondent_number[which(respondent_number$fresp00 == "1"),]
parnter_interview_sweep6 <-  respondent_number[which(respondent_number$fresp00 == "2"),]
proxy_interview_sweep6 <-  respondent_number[which(respondent_number$fresp00 == "3"),]

#sweep 1 entry families at 9 months
respondent_sweep6 <- c("mcsid", "fdres00")
respondent_sweep6<-mcs6_derived_parent[respondent_sweep6]
main_respondent_sweep6 <- respondent_sweep6 [which(respondent_number$fresp00=="1"),]
partner_respondent_sweep6 <- respondent_sweep6 [which(respondent_number$fresp00=="2"),]
proxy_respondent_sweep6 <- respondent_sweep6 [which(respondent_number$fresp00=="3"),]

#mother 
mother_sweep6_main <- main_respondent_sweep6[main_respondent_sweep6$fdres00 == 1 | main_respondent_sweep6$fdres00== 3 | main_respondent_sweep6$fdres00 == 5 | main_respondent_sweep6$fdres00 == 7 |main_respondent_sweep6$fdres00 == 9 |main_respondent_sweep6$fdres00 == 11 | main_respondent_sweep6$fdres00 == 13|main_respondent_sweep6$fdres00 == 15 ,]
mother_sweep6_partner <- partner_respondent_sweep6[partner_respondent_sweep6$fdres00 == 1 | partner_respondent_sweep6$fdres00== 3 | partner_respondent_sweep6$fdres00 == 5 |partner_respondent_sweep6$fdres00 == 7 |partner_respondent_sweep6$fdres00 == 9 |partner_respondent_sweep6$fdres00 == 11 | partner_respondent_sweep6$fdres00 == 13|partner_respondent_sweep6$fdres00 == 15 ,]
mother_sweep6_proxy <- proxy_respondent_sweep6[proxy_respondent_sweep6$fdres00 == 1 | proxy_respondent_sweep6$fdres00== 3 | proxy_respondent_sweep6$fdres00 == 5 | proxy_respondent_sweep6$fdres00 == 7 |proxy_respondent_sweep6$fdres00 == 9 |proxy_respondent_sweep6$fdres00 == 11 | proxy_respondent_sweep6$fdres00 == 13|proxy_respondent_sweep6$fdres00 == 15 ,]

#mother_sweep6_entry <- merge (all=TRUE, mother_sweep6, sweep_entry, by="mcsid")
#mother_sweep6_entry1_1<- mother_sweep6[which(mother_sweep6_entry$sentry == "1"),]
#sweep6_mother_entry1 <- c("mcsid", "fdres00")
#sweep6_mother_entry1 <-mother_sweep6_entry1_1[sweep6_mother_entry1]
#mother_sweep6_entry2<- mother_sweep6_entry[which(mother_sweep6_entry$sentry == "2"),]
#sweep6_mother_entry2 <- c("mcsid", "fdres00")
#sweep6_mother_entry2 <-mother_sweep6_entry2[sweep6_mother_entry2]
#sweep6_mother_combined_entry <- merge(all=TRUE, sweep6_mother_entry1, sweep6_mother_entry2, by="mcsid")
#sweep6_mother_combined_entry$S6mother_combined_entry <- ifelse(!is.na(sweep6_mother_combined_entry$fdres00.x), sweep6_mother_combined_entry$fdres00.x,sweep6_mother_combined_entry$fdres00.y)
#subset data so just have 1 standard score and mcsid for the variable
#sweep6_mother<- c("mcsid", "S6mother_combined_entry")
#sweep6_mother <- sweep6_mother_combined_entry[sweep6_mother]

#father
father_sweep6_main <- main_respondent_sweep6[main_respondent_sweep6$fdres00 == 2 | main_respondent_sweep6$fdres00== 4 | main_respondent_sweep6$fdres00 == 6 |main_respondent_sweep6$fdres00 == 8 |main_respondent_sweep6$fdres00 == 10 |main_respondent_sweep6$fdres00 == 12 | main_respondent_sweep6$fdres00 == 14|main_respondent_sweep6$fdres00 == 16 |main_respondent_sweep6$fdres00 == 22 | main_respondent_sweep6$fdres00 == 24 ,]
father_sweep6_partner <- partner_respondent_sweep6[partner_respondent_sweep6$fdres00 == 2 | partner_respondent_sweep6$fdres00== 4 | partner_respondent_sweep6$fdres00 == 6 |partner_respondent_sweep6$fdres00 == 8 |partner_respondent_sweep6$fdres00 == 10 |partner_respondent_sweep6$fdres00 == 12 | partner_respondent_sweep6$fdres00 == 14|partner_respondent_sweep6$fdres00 == 16 |partner_respondent_sweep6$fdres00 == 22 |partner_respondent_sweep6$fdres00 == 24 ,]
father_sweep6_proxy <- proxy_respondent_sweep6[proxy_respondent_sweep6$fdres00 == 2 | proxy_respondent_sweep6$fdres00== 4 | proxy_respondent_sweep6$fdres00 == 6 |proxy_respondent_sweep6$fdres00 == 8 |proxy_respondent_sweep6$fdres00 == 10 |proxy_respondent_sweep6$fdres00 == 12 | proxy_respondent_sweep6$fdres00 == 14|proxy_respondent_sweep6$fdres00 == 16 |proxy_respondent_sweep6$fdres00 == 22 |proxy_respondent_sweep6$fdres00 == 24 ,]

#father_sweep6_entry <- merge (all=TRUE, father_sweep6, sweep_entry, by="mcsid")
#father_sweep6_entry1_1<- father_sweep6[which(father_sweep6_entry$sentry == "1"),]
#sweep6_father_entry1 <- c("mcsid", "fdres00")
#sweep6_father_entry1 <-father_sweep6_entry1_1[sweep6_father_entry1]
#father_sweep6_entry2<- father_sweep6_entry[which(father_sweep6_entry$sentry == "2"),]
#sweep6_father_entry2 <- c("mcsid", "fdres00")
#sweep6_father_entry2 <-father_sweep6_entry2[sweep6_father_entry2]
#sweep6_father_combined_entry <- merge(all=TRUE, sweep6_father_entry1, sweep6_father_entry2, by="mcsid")
#sweep6_father_combined_entry$S6father_combined_entry <- ifelse(!is.na(sweep6_father_combined_entry$fdres00.x), sweep6_father_combined_entry$fdres00.x,sweep6_father_combined_entry$fdres00.y)
#subset data so just have 1 standard score and mcsid for the variable
#sweep6_father<- c("mcsid", "S6father_combined_entry")
#sweep6_father <- sweep6_father_combined_entry[sweep6_father]

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

#SWEEP 6 OCCUPATIONAL STATUS
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
#highest_parent_occupation6<- highest_parent_occupation6[which(mcsid_number_age14$fcnum00=="1"),]

#mcs6 income
oecd_sweep6 <- c("mcsid", "foecduk0")
oecd_sweep6 <- mcs6_derived_family[oecd_sweep6]
oecd_sweep6[oecd_sweep6==-1]<- NA
#cm1 only - shouldnt matter as is parent info
#oecd_sweep6<- oecd_sweep6[which(mcsid_age14$fcnum00=="1"),]


#IMD variables
#INDICES OF MULTIPLE DEPRIVATION (DECILES) (AGE 3, 9 MONTHS IF MISSING)
imd_variables <- merge(all=TRUE, mcs6_imd_eng, mcs6_imd_ni, by="mcsid")
imd_variables <- merge(all=TRUE, imd_variables, mcs6_imd_sc, by="mcsid")
imd_variables <- merge(all=TRUE, imd_variables, mcs6_imd_w, by="mcsid")
imd_variables_sweep6 <- c("mcsid",  "fimdscoe", "fiwimdsc", "fisimdsc", "fimdscon")
imd_variables_sweep6 <- imd_variables[imd_variables_sweep6]
names(imd_variables_sweep6) <- c("mcsid", "imd_eng", "imd_w", "imd_sc", "imd_ni")


imd_sweep6_1 <- transform(imd_variables_sweep6, imd6_1 = pmax(imd_eng, imd_w, imd_sc, imd_ni,  na.rm = TRUE))
IMD_sweep6<- c("mcsid",  "imd6_1")
IMD_sweep6 <- imd_sweep6_1[IMD_sweep6]



#AGE 14 language - word activity test; scores out of 20. 
mcsid_number_age14 <- c("mcsid", "fcnum00")
mcsid_number_age14 <- mcs6_child_assessment[mcsid_number_age14]
mcsid_number_age14$fcnum00 = as.character(mcsid_number_age14$fcnum00)
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
#age14_lang_number <- merge(all=TRUE, new_vocab_age14, mcsid_number_age14, by="mcsid")
#age14_language1 <- age14_lang_number[age14_lang_number$fcnum00 == "1", ]
#age14_language <- c("mcsid", "word_act_age14")
#age14_language <- age14_language1[age14_language]
#age14_language$age <- rep(14, nrow(age14_language))
#names(age14_language) <- c("mcsid", "language", "age")
#age14_language$language <- scale(age14_language$language, center=TRUE, scale=TRUE)

#language spoken at home
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

#home_language <- c("mcsid", "new_langcombine")
#home_language <- language_used_home[home_language]

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

#ethnicity_cm <- c("mcsid", "ethnic_combine")
#ethnicity_cm <- cm_ethnicity1[ethnicity_cm]

#gender
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

#auxiliary variables for imputation
#mother's age at birth of CM
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


#housing tenure at age 14
housing_tenure <- c("mcsid", "edroow00")
housing_tenure <- mcs5_derived_family[housing_tenure]
housing_tenure[housing_tenure==-9] <- NA
housing_tenure[housing_tenure==-8] <- NA
housing_tenure[housing_tenure==-1] <- NA

housing_tenure$edroow00[housing_tenure$edroow00==1] <-1
housing_tenure$edroow00[housing_tenure$edroow00==2] <-1
housing_tenure$edroow00[housing_tenure$edroow00==3] <-2
housing_tenure$edroow00[housing_tenure$edroow00==4] <-2
housing_tenure$edroow00[housing_tenure$edroow00==5] <-2
housing_tenure$edroow00[housing_tenure$edroow00==6] <-2
housing_tenure$edroow00[housing_tenure$edroow00==7] <-3
housing_tenure$edroow00[housing_tenure$edroow00==8] <-4
housing_tenure$edroow00[housing_tenure$edroow00==9] <-5
housing_tenure$edroow00[housing_tenure$edroow00==10] <-5

#accommodation type at age 11
#Respondent variable sweep 5
respondent_number_5 <- c("mcsid", "eresp00")
respondent_number_5 <- mcs5_derived_parent[respondent_number_5]
respondent_number_5$eresp00 = as.character(respondent_number_5$eresp00)
main_interview_sweep5 <-  respondent_number_5[which(respondent_number_5$eresp00 == "1"),]
parnter_interview_sweep5 <-  respondent_number_5[which(respondent_number_5$eresp00 == "2"),]
proxy_interview_sweep5 <-  respondent_number_5[which(respondent_number_5$eresp00 == "3"),]

#select interview respondents . separate out interview respondent into main, partner and proxy 
respondent_sweep5 <- c("mcsid", "eddres00")
respondent_sweep5<-mcs5_derived_parent[respondent_sweep5]
respondent_sweep5$eddres00 = as.character(respondent_sweep5$eddres00)
main_respondent_sweep5 <- respondent_sweep5 [which(respondent_number_5$eresp00=="1"),]
partner_respondent_sweep5 <- respondent_sweep5 [which(respondent_number_5$eresp00=="2"),]
proxy_respondent_sweep5 <- respondent_sweep5 [which(respondent_number_5$eresp00=="3"),]


#mother main/ partner/ proxy 
mother_sweep5_main <- main_respondent_sweep5[main_respondent_sweep5$eddres00 == 1 | main_respondent_sweep5$eddres00== 3 | 
                                               main_respondent_sweep5$eddres00 == 5 | main_respondent_sweep5$eddres00 == 7 |
                                               main_respondent_sweep5$eddres00 == 9 |main_respondent_sweep5$eddres00 == 11 | 
                                               main_respondent_sweep5$eddres00 == 13|main_respondent_sweep5$eddres00 == 15 ,]
mother_sweep5_partner <- partner_respondent_sweep5[partner_respondent_sweep5$eddres00 == 1 | partner_respondent_sweep5$eddres00== 3 | 
                                                     partner_respondent_sweep5$eddres00 == 5 |partner_respondent_sweep5$eddres00 == 7 |
                                                     partner_respondent_sweep5$eddres00 == 9 |partner_respondent_sweep5$eddres00 == 11 | 
                                                     partner_respondent_sweep5$eddres00 == 13|partner_respondent_sweep5$eddres00 == 15 ,]
mother_sweep5_proxy <- proxy_respondent_sweep5[proxy_respondent_sweep5$eddres00 == 1 | proxy_respondent_sweep5$eddres00== 3 | 
                                                 proxy_respondent_sweep5$eddres00 == 5 | proxy_respondent_sweep5$eddres00 == 7 |proxy_respondent_sweep5$eddres00 == 9 |
                                                 proxy_respondent_sweep5$eddres00 == 11 | proxy_respondent_sweep5$eddres00 == 13|proxy_respondent_sweep5$eddres00 == 15 ,]

#father main/ partner/ proxy 
father_sweep5_main <- main_respondent_sweep5[main_respondent_sweep5$eddres00 == 2 | main_respondent_sweep5$eddres00== 4 | 
                                               main_respondent_sweep5$eddres00 == 6 |main_respondent_sweep5$eddres00 == 8 |
                                               main_respondent_sweep5$eddres00 == 10 |main_respondent_sweep5$eddres00 == 12 | 
                                               main_respondent_sweep5$eddres00 == 14|main_respondent_sweep5$eddres00 == 16 |
                                               main_respondent_sweep5$eddres00 == 22 | main_respondent_sweep5$eddres00 == 24 ,]
father_sweep5_partner <- partner_respondent_sweep5[partner_respondent_sweep5$eddres00 == 2 | partner_respondent_sweep5$eddres00== 4 | 
                                                     partner_respondent_sweep5$eddres00 == 6 |partner_respondent_sweep5$eddres00 == 8 |
                                                     partner_respondent_sweep5$eddres00 == 10 |partner_respondent_sweep5$eddres00 == 12 | 
                                                     partner_respondent_sweep5$eddres00 == 14|partner_respondent_sweep5$eddres00 == 16 |
                                                     partner_respondent_sweep5$eddres00 == 22 |partner_respondent_sweep5$eddres00 == 24 ,]
father_sweep5_proxy <- proxy_respondent_sweep5[proxy_respondent_sweep5$eddres00 == 2 | proxy_respondent_sweep5$eddres00== 4 | proxy_respondent_sweep5$eddres00 == 6 |
                                                 proxy_respondent_sweep5$eddres00 == 8 |proxy_respondent_sweep5$eddres00 == 10 |proxy_respondent_sweep5$eddres00 == 12 | 
                                                 proxy_respondent_sweep5$eddres00 == 14|proxy_respondent_sweep5$eddres00 == 16 |proxy_respondent_sweep5$eddres00 == 22 |
                                                 proxy_respondent_sweep5$eddres00 == 24 ,]





sweep5_accommodation <-c ("mcsid", "epmoty00")
sweep5_accommodation <- mcs5_parent[sweep5_accommodation]
sweep5_accommodation[sweep5_accommodation ==-1:-9] <- NA
sweep5_accommodation$epmoty00[sweep5_accommodation$epmoty00 ==98] <- NA
sweep5_accommodation$epmoty00[sweep5_accommodation$epmoty00 ==99] <- NA
sweep5_accommodation$epmoty00[sweep5_accommodation$epmoty00 ==95] <- NA

#get main, partner and proxy interview responses for accommodation question
main_accommodation<-sweep5_accommodation[sweep5_accommodation$mcsid %in% main_interview_sweep5$mcsid,]
partner_accommodation<-sweep5_accommodation[sweep5_accommodation$mcsid %in% parnter_interview_sweep5$mcsid,]
proxy_accommodation <- sweep5_accommodation[ sweep5_accommodation$mcsid %in% proxy_interview_sweep5$mcsid,]

#accomm5 <- merge(all=TRUE, main_accommodation, partner_accommodation,by="mcsid")
#accomm5 <- merge(all=TRUE, accomm5, proxy_accommodation,by="mcsid")

accommodation5_mother_main <- main_accommodation[main_accommodation$mcsid %in% mother_sweep5_main$mcsid,]
accommodation5_mother_partner <- partner_accommodation[partner_accommodation$mcsid %in% mother_sweep5_partner$mcsid,]
accommodation5_mother_proxy <- proxy_accommodation[proxy_accommodation$mcsid %in% mother_sweep5_proxy$mcsid,]

accommodation5_mother <- merge(all=TRUE, accommodation5_mother_main, accommodation5_mother_partner,by="mcsid")
accommodation5_mother <- merge(all=TRUE, accommodation5_mother, accommodation5_mother_proxy,by="mcsid")
accommodation5_mother$mother_accommodation5 <- ifelse(!is.na(accommodation5_mother$epmoty00.x), accommodation5_mother$epmoty00.x, accommodation5_mother$epmoty00.y)
accommodation5_mother$mother_accommodation5_1 <- ifelse(!is.na(accommodation5_mother$mother_accommodation5), accommodation5_mother$mother_accommodation5, accommodation5_mother$epmoty00)                               

accommodation5_father_main <- main_accommodation[main_accommodation$mcsid %in% father_sweep5_main$mcsid,]
accommodation5_father_partner <- partner_accommodation[partner_accommodation$mcsid %in% father_sweep5_partner$mcsid,]
accommodation5_father_proxy <- proxy_accommodation[proxy_accommodation$mcsid %in% father_sweep5_proxy$mcsid,]
accommodation5_father <- merge(all=TRUE, accommodation5_father_main, accommodation5_father_partner,by="mcsid")
accommodation5_father <- merge(all=TRUE, accommodation5_father, accommodation5_father_proxy,by="mcsid")
accommodation5_father$father_accommodation5 <- ifelse(!is.na(accommodation5_father$epmoty00.x), accommodation5_father$epmoty00.x, accommodation5_father$epmoty00.y)
accommodation5_father$father_accommodation5_1 <- ifelse(!is.na(accommodation5_father$father_accommodation5), accommodation5_father$father_accommodation5, accommodation5_father$epmoty00)    

parent_accommodation_5 <- merge(all=TRUE, accommodation5_mother, accommodation5_father,by="mcsid")
sweep5_parent_accommodation<- c("mcsid", "mother_accommodation5_1", "father_accommodation5_1")
sweep5_parent_accommodation<- parent_accommodation_5[sweep5_parent_accommodation]

#remove duplicate mcsids
parent_accommodation_sweep5 <- sweep5_parent_accommodation[!duplicated(sweep5_parent_accommodation$mcsid), ]
parent_accommodation_sweep5$ACCOMMODATION <- ifelse(!is.na(parent_accommodation_sweep5$mother_accommodation5_1), parent_accommodation_sweep5$mother_accommodation5_1, parent_accommodation_sweep5$father_accommodation5_1)    

#accommodation at age 7
sweep4_accommodation <-c ("mcsid", "dmmoty00")
sweep4_accommodation <- mcs4_parent[sweep4_accommodation]
sweep4_accommodation[sweep4_accommodation ==-1:-9] <- NA
sweep4_accommodation$dmmoty00[sweep4_accommodation$dmmoty00 ==98] <- NA
sweep4_accommodation$dmmoty00[sweep4_accommodation$dmmoty00 ==99] <- NA
sweep4_accommodation$dmmoty00[sweep4_accommodation$dmmoty00 ==95] <- NA

#replace NA at age 11 with age 7 values
accommodation <- merge(all=TRUE, parent_accommodation_sweep5, sweep4_accommodation, by="mcsid")
accommodation$accommodation_combine1 <- ifelse(!is.na(accommodation$ACCOMMODATION),accommodation$ACCOMMODATION, accommodation$dmmoty00)  


ACCOMMODATION1 <- c("mcsid", "accommodation_combine1")
ACCOMMODATION1<- accommodation[ACCOMMODATION1]

ACCOMMODATION1[ACCOMMODATION1==1] <-1
ACCOMMODATION1[ACCOMMODATION1==2] <-2
ACCOMMODATION1[ACCOMMODATION1==3] <-2
ACCOMMODATION1[ACCOMMODATION1==4] <-3

#whether CM breastfed
breastfed <-c("mcsid", "ambfeva0")
breastfed <- mcs1_parent[breastfed]
breastfed[breastfed == -1:-9]<- NA
breastfed <- breastfed[which(mcsid_number_age9mo$ahcnuma0=="1"),]


#number of parents present in household at age 3
household <-c ("mcsid", "bdhtys00")
household <- mcs2_derived[household]
household[household ==-1:-9] <- NA
household <-household[which(mcsid_number_age3$bhcnuma0=="1"),]
new_household <- merge (all=TRUE,household, sweep_entry, by="mcsid")
new_household1<- new_household[which(new_household$sentry == "1"),]

household2 <-c ("mcsid", "bdhtys00")
household2 <- mcs2_derived[household2]
household2[household2 ==-1:-9] <- NA
household2 <-household2[which(mcsid_number_age3$bhcnuma0=="1"),]
new_household2 <- merge (all=TRUE,household2, sweep_entry, by="mcsid")
new_household_2<- new_household2[which(new_household2$sentry == "2"),]


house <- merge(all=TRUE, new_household1, new_household_2,by="mcsid")
house_combine <- ifelse(!is.na(house$bdhtys00.x), house$bdhtys00.x, house$bdhtys00.y)
#create dataframe so also have mcsid 
new_household<- data.frame(house_combine,house)
#subset data so just have 1 standard score and mcsid for the variable
parents_in_hh <- c("mcsid", "house_combine")
parents_in_hh <- new_household[parents_in_hh ]

#replace NA with sweep1 responses
household_sweep1 <-c ("mcsid", "adhtys00")
household_sweep1 <- mcs1_derived[household_sweep1]
household_sweep1[household_sweep1 ==-1:-9] <- NA
household_sweep1<-household_sweep1[which(mcsid_number_age9mo$ahcnuma0=="1"),]
parents_home <- merge(all=TRUE, household_sweep1, parents_in_hh,by="mcsid")
parents_home$parents_in_household <- ifelse(!is.na(parents_home$house_combine), parents_home$house_combine, parents_home$adhtys00)
PARENTS_IN_HH <- c("mcsid", "parents_in_household")
PARENTS_IN_HH <- parents_home[PARENTS_IN_HH]

#4. Wealth ####
wealth_variables <- mcs6_parent %>%  select(mcsid, fresp00, fpmopa00, fphval00, fpinvt00, fpdeba00, felig00) %>% 
  filter(felig00 == 1) %>% 
  select(mcsid, fpmopa00, fphval00, fpinvt00, fpdeba00) %>% 
  rename("mortgage" = fpmopa00,
         "houseValue" = fphval00, 
         "savings" = fpinvt00, 
         "debt" = fpdeba00)

tenure = mcs6_family %>% select(mcsid, fdroow00)


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


analysis_data <- merge(all=TRUE, cm_sex, cm_ethnicity,by="mcsid")
nrow(analysis_data)
analysis_data <- merge(all=TRUE, analysis_data, language_used,by="mcsid")
nrow(analysis_data)
analysis_data <- merge(all=TRUE, analysis_data, mother_birth_age,by="mcsid")
nrow(analysis_data)
analysis_data <- merge(all=TRUE, analysis_data, housing_tenure,by="mcsid")
nrow(analysis_data)
analysis_data <- merge(all=TRUE, analysis_data, ACCOMMODATION1,by="mcsid")
nrow(analysis_data)
#analysis_data <- merge(all=TRUE, analysis_data, parent_academic_quals, by="mcsid")
#nrow(analysis_data)
#analysis_data <- merge(all=TRUE, analysis_data, highest_parent_qualification, by="mcsid")
#nrow(analysis_data)
#analysis_data <- merge(all=TRUE, analysis_data, parent_NVQ,by="mcsid")
#nrow(analysis_data)
analysis_data <- merge(all=TRUE, analysis_data, highest_parent_NVQ6,by="mcsid")
nrow(analysis_data)
analysis_data <- merge(all=TRUE, analysis_data, IMD_sweep6,by="mcsid")
nrow(analysis_data)
analysis_data <- merge(all=TRUE, analysis_data, breastfed,by="mcsid")
nrow(analysis_data)
analysis_data <- merge(all=TRUE, analysis_data, PARENTS_IN_HH,by="mcsid")
nrow(analysis_data)
analysis_data <- merge(all=TRUE, analysis_data, oecd_sweep6,by="mcsid")
nrow(analysis_data)
analysis_data <- merge(all=TRUE, analysis_data, highest_parent_occupation6,by="mcsid")
nrow(analysis_data)
analysis_data <- merge(all=TRUE, analysis_data, wealth,by="mcsid")
nrow(analysis_data)
analysis_data <- merge(all=TRUE, analysis_data, age14_language,by="mcsid")
nrow(analysis_data)
analysis_data <- merge(all=TRUE, analysis_data, mcs_weight,by="mcsid")
nrow(analysis_data)


mcs_analysis = analysis_data[!is.na(analysis_data$word_act_age14) ,]

names(mcs_analysis) <- c("mcsid", "gender", "ethnicity", "language_used_at_home", "mothers_age", "housing_tenure", "accommodation_type", "highest_NVQ", "imd", "cm_breastfed", "parents_in_house",  "income_quintiles" , "occupational_status",   "mortgage", "house_value", "total_debt", "savings" ,  "vocabulary_age14", "mcs2_weight")#if do this before merge together, issue with duplicate mcsids. 

write.csv(mcs_analysis, file = "age14_SES_data.csv")
