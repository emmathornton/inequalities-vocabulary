#DISTRIBUTIONS 
#LOAD IN DATA and libraries####
library(mice)
library(ggplot2)
library(ggpubr)
library(tidyr)
library(tidyverse)
load("~/Documents/PhD/MCS DATASETS/SES_inequalities_language/inequalities-language-ability/Datasets/final datasets/2021-02-26_mcs_ses_crossCohort.Rdata")
mcs_imputation = mi.res
rm(mi.res)
load("~/Documents/PhD/MCS DATASETS/SES_inequalities_language/inequalities-language-ability/Datasets/final datasets/2021-02-26_bcs_ses_cross_cohort.Rdata")
bcs_imputation = mi.res
rm(mi.res)
imputed_mcs_1 <- complete(mcs_imputation,1)
imputed_bcs_1 <- complete(bcs_imputation,1)

#Language by SES indiactor density plots ####
#most deprived vs least deprived####
#get lowest and highest SES groups and combine to one variable
#density plot for highest and lowest SES group at each age
#create data with highest and lowest SES group 
#MCSeducation
MCSnvq_lowest_mcsid = imputed_mcs_1$id[imputed_mcs_1$highest_household_education==1]
MCSnvq_lowest = imputed_mcs_1$highest_household_education[imputed_mcs_1$highest_household_education==1]
MCSage5_lowest = imputed_mcs_1$standardised_vocab5[imputed_mcs_1$highest_household_education==1]
MCSage11_lowest = imputed_mcs_1$standardised_vocab11[imputed_mcs_1$highest_household_education==1]
MCSage14_lowest = imputed_mcs_1$standardised_vocab14[imputed_mcs_1$highest_household_education==1]
MCSnvq_lowest= data.frame(MCSnvq_lowest_mcsid, MCSnvq_lowest,  MCSage5_lowest, MCSage11_lowest,MCSage14_lowest)
names(MCSnvq_lowest) <- c("mcsid", "nvq_lowest",  "age5_lowest", "age11_lowest", "age14_lowest")

MCSnvq_highest_mcsid = imputed_mcs_1$id[imputed_mcs_1$highest_household_education==4]
MCSnvq_highest = imputed_mcs_1$highest_household_education[imputed_mcs_1$highest_household_education==4]
MCSage5_highest = imputed_mcs_1$standardised_vocab5[imputed_mcs_1$highest_household_education==4]
MCSage11_highest = imputed_mcs_1$standardised_vocab11[imputed_mcs_1$highest_household_education==4]
MCSage14_highest = imputed_mcs_1$standardised_vocab14[imputed_mcs_1$highest_household_education==4]
MCSnvq_highest= data.frame(MCSnvq_highest_mcsid, MCSnvq_highest,  MCSage5_highest, MCSage11_highest, MCSage14_highest)
names(MCSnvq_highest) <- c("mcsid", "nvq_highest","age5_highest", "age11_highest", "age14_highest")

MCSnvq_highest_lowest <- merge(all=TRUE, MCSnvq_lowest, MCSnvq_highest,by="mcsid")

MCSnvq_highest_lowest$nvq <- ifelse(!is.na(MCSnvq_highest_lowest$nvq_lowest), MCSnvq_highest_lowest$nvq_lowest, MCSnvq_highest_lowest$nvq_highest)
MCSnvq_highest_lowest$age5_vocab <- ifelse(!is.na(MCSnvq_highest_lowest$age5_lowest), MCSnvq_highest_lowest$age5_lowest, MCSnvq_highest_lowest$age5_highest)
MCSnvq_highest_lowest$age11_vocab <- ifelse(!is.na(MCSnvq_highest_lowest$age11_lowest), MCSnvq_highest_lowest$age11_lowest, MCSnvq_highest_lowest$age11_highest)
MCSnvq_highest_lowest$age14_vocab <- ifelse(!is.na(MCSnvq_highest_lowest$age14_lowest), MCSnvq_highest_lowest$age14_lowest, MCSnvq_highest_lowest$age14_highest)
MCSnvq_highest_lowest$nvq=as.factor(MCSnvq_highest_lowest$nvq)

#BCS education
BCSnvq_lowest_bcsid = imputed_bcs_1$id[imputed_bcs_1$highest_household_education==1]
BCSnvq_lowest = imputed_bcs_1$highest_household_education[imputed_bcs_1$highest_household_education==1]
BCSage5_lowest = imputed_bcs_1$standardised_vocab5[imputed_bcs_1$highest_household_education==1]
BCSage11_lowest = imputed_bcs_1$standardised_vocab10[imputed_bcs_1$highest_household_education==1]
BCSage14_lowest = imputed_bcs_1$standardised_vocab16[imputed_bcs_1$highest_household_education==1]
BCSnvq_lowest= data.frame(BCSnvq_lowest_bcsid, BCSnvq_lowest,  BCSage5_lowest, BCSage11_lowest,BCSage14_lowest)
names(BCSnvq_lowest) <- c("bcsid", "nvq_lowest",  "age5_lowest", "age11_lowest", "age14_lowest")

BCSnvq_highest_bcsid = imputed_bcs_1$id[imputed_bcs_1$highest_household_education==4]
BCSnvq_highest = imputed_bcs_1$highest_household_education[imputed_bcs_1$highest_household_education==4]
BCSage5_highest = imputed_bcs_1$standardised_vocab5[imputed_bcs_1$highest_household_education==4]
BCSage11_highest = imputed_bcs_1$standardised_vocab10[imputed_bcs_1$highest_household_education==4]
BCSage14_highest = imputed_bcs_1$standardised_vocab16[imputed_bcs_1$highest_household_education==4]
BCSnvq_highest= data.frame(BCSnvq_highest_bcsid, BCSnvq_highest,  BCSage5_highest, BCSage11_highest, BCSage14_highest)
names(BCSnvq_highest) <- c("bcsid", "nvq_highest","age5_highest", "age11_highest", "age14_highest")

BCSnvq_highest_lowest <- merge(all=TRUE, BCSnvq_lowest, BCSnvq_highest,by="bcsid")

BCSnvq_highest_lowest$nvq <- ifelse(!is.na(BCSnvq_highest_lowest$nvq_lowest), BCSnvq_highest_lowest$nvq_lowest, BCSnvq_highest_lowest$nvq_highest)
BCSnvq_highest_lowest$age5_vocab <- ifelse(!is.na(BCSnvq_highest_lowest$age5_lowest), BCSnvq_highest_lowest$age5_lowest, BCSnvq_highest_lowest$age5_highest)
BCSnvq_highest_lowest$age11_vocab <- ifelse(!is.na(BCSnvq_highest_lowest$age11_lowest), BCSnvq_highest_lowest$age11_lowest, BCSnvq_highest_lowest$age11_highest)
BCSnvq_highest_lowest$age14_vocab <- ifelse(!is.na(BCSnvq_highest_lowest$age14_lowest), BCSnvq_highest_lowest$age14_lowest, BCSnvq_highest_lowest$age14_highest)
BCSnvq_highest_lowest$nvq=as.factor(BCSnvq_highest_lowest$nvq)

#BCS occupation
BCSoccupation_lowest_bcsid = imputed_bcs_1$id[imputed_bcs_1$occupational_status==1]
BCSoccupation_lowest = imputed_bcs_1$occupational_status[imputed_bcs_1$occupational_status==1]
BCSage5_lowest = imputed_bcs_1$standardised_vocab5[imputed_bcs_1$occupational_status==1]
BCSage11_lowest = imputed_bcs_1$standardised_vocab10[imputed_bcs_1$occupational_status==1]
BCSage14_lowest = imputed_bcs_1$standardised_vocab16[imputed_bcs_1$occupational_status==1]
BCSoccupation_lowest= data.frame(BCSoccupation_lowest_bcsid, BCSoccupation_lowest,  BCSage5_lowest, BCSage11_lowest,BCSage14_lowest)
names(BCSoccupation_lowest) <- c("bcsid", "occupation_lowest",  "age5_lowest", "age11_lowest", "age14_lowest")

BCSoccupation_highest_bcsid = imputed_bcs_1$id[imputed_bcs_1$occupational_status==4]
BCSoccupation_highest = imputed_bcs_1$occupational_status[imputed_bcs_1$occupational_status==4]
BCSage5_highest = imputed_bcs_1$standardised_vocab5[imputed_bcs_1$occupational_status==4]
BCSage11_highest = imputed_bcs_1$standardised_vocab10[imputed_bcs_1$occupational_status==4]
BCSage14_highest = imputed_bcs_1$standardised_vocab16[imputed_bcs_1$occupational_status==4]
BCSoccupation_highest= data.frame(BCSoccupation_highest_bcsid, BCSoccupation_highest,  BCSage5_highest, BCSage11_highest, BCSage14_highest)
names(BCSoccupation_highest) <- c("bcsid", "occupation_highest","age5_highest", "age11_highest", "age14_highest")

BCSoccupation_highest_lowest <- merge(all=TRUE, BCSoccupation_lowest, BCSoccupation_highest,by="bcsid")

BCSoccupation_highest_lowest$occupation <- ifelse(!is.na(BCSoccupation_highest_lowest$occupation_lowest), BCSoccupation_highest_lowest$occupation_lowest, BCSoccupation_highest_lowest$occupation_highest)
BCSoccupation_highest_lowest$age5_vocab <- ifelse(!is.na(BCSoccupation_highest_lowest$age5_lowest), BCSoccupation_highest_lowest$age5_lowest, BCSoccupation_highest_lowest$age5_highest)
BCSoccupation_highest_lowest$age11_vocab <- ifelse(!is.na(BCSoccupation_highest_lowest$age11_lowest), BCSoccupation_highest_lowest$age11_lowest, BCSoccupation_highest_lowest$age11_highest)
BCSoccupation_highest_lowest$age14_vocab <- ifelse(!is.na(BCSoccupation_highest_lowest$age14_lowest), BCSoccupation_highest_lowest$age14_lowest, BCSoccupation_highest_lowest$age14_highest)
BCSoccupation_highest_lowest$occupation=as.factor(BCSoccupation_highest_lowest$occupation)

#MCS occupation
MCSoccupation_lowest_mcsid = imputed_mcs_1$id[imputed_mcs_1$occupational_status==1]
MCSoccupation_lowest = imputed_mcs_1$occupational_status[imputed_mcs_1$occupational_status==1]
MCSage5_lowest = imputed_mcs_1$standardised_vocab5[imputed_mcs_1$occupational_status==1]
MCSage11_lowest = imputed_mcs_1$standardised_vocab11[imputed_mcs_1$occupational_status==1]
MCSage14_lowest = imputed_mcs_1$standardised_vocab14[imputed_mcs_1$occupational_status==1]
MCSoccupation_lowest= data.frame(MCSoccupation_lowest_mcsid, MCSoccupation_lowest,  MCSage5_lowest, MCSage11_lowest,MCSage14_lowest)
names(MCSoccupation_lowest) <- c("mcsid", "occupation_lowest",  "age5_lowest", "age11_lowest", "age14_lowest")

MCSoccupation_highest_mcsid = imputed_mcs_1$id[imputed_mcs_1$occupational_status==4]
MCSoccupation_highest = imputed_mcs_1$occupational_status[imputed_mcs_1$occupational_status==4]
MCSage5_highest = imputed_mcs_1$standardised_vocab5[imputed_mcs_1$occupational_status==4]
MCSage11_highest = imputed_mcs_1$standardised_vocab11[imputed_mcs_1$occupational_status==4]
MCSage14_highest = imputed_mcs_1$standardised_vocab14[imputed_mcs_1$occupational_status==4]
MCSoccupation_highest= data.frame(MCSoccupation_highest_mcsid, MCSoccupation_highest,  MCSage5_highest, MCSage11_highest, MCSage14_highest)
names(MCSoccupation_highest) <- c("mcsid", "occupation_highest","age5_highest", "age11_highest", "age14_highest")

MCSoccupation_highest_lowest <- merge(all=TRUE, MCSoccupation_lowest, MCSoccupation_highest,by="mcsid")

MCSoccupation_highest_lowest$occupation <- ifelse(!is.na(MCSoccupation_highest_lowest$occupation_lowest), MCSoccupation_highest_lowest$occupation_lowest, MCSoccupation_highest_lowest$occupation_highest)
MCSoccupation_highest_lowest$age5_vocab <- ifelse(!is.na(MCSoccupation_highest_lowest$age5_lowest), MCSoccupation_highest_lowest$age5_lowest, MCSoccupation_highest_lowest$age5_highest)
MCSoccupation_highest_lowest$age11_vocab <- ifelse(!is.na(MCSoccupation_highest_lowest$age11_lowest), MCSoccupation_highest_lowest$age11_lowest, MCSoccupation_highest_lowest$age11_highest)
MCSoccupation_highest_lowest$age14_vocab <- ifelse(!is.na(MCSoccupation_highest_lowest$age14_lowest), MCSoccupation_highest_lowest$age14_lowest, MCSoccupation_highest_lowest$age14_highest)
MCSoccupation_highest_lowest$occupation=as.factor(MCSoccupation_highest_lowest$occupation)

#DENSITY PLOTS ####
MCSage5_nvq <- ggplot(MCSnvq_highest_lowest, aes(x=age5_vocab, colour=nvq)) +   geom_density(size=0.5) +
  scale_colour_manual(name="Socioeconomic Circumstance", values=c( "#E66101", "#5E3C99"), labels=c("Most Deprived", "Least Deprived"))+
  theme_classic() +
  labs( x = "Vocabulary Score \n (standardised)", y = "Density") +
  geom_vline(data=MCSnvq_highest_lowest, aes(xintercept=mean(age5_vocab)),
             linetype="dashed")+
  theme(panel.background = element_rect(fill = "#FDCDAC",
                                        colour = "#FDCDAC",
                                        size = 0.5, linetype = "dashed"),
        panel.grid.major = element_line(size = 0.5, linetype = 'solid',
                                        colour = "white"), 
        panel.grid.minor = element_line(size = 0.25, linetype = 'solid',
                                        colour = "white")) + theme(axis.text = element_text(size=18, family = "Times", colour = "black"), axis.title = element_text(size=18, family = "Times"))+
  theme(legend.text = element_text(size=15, family="Times"), legend.title = element_text(size=18, family="Times")) +
  ggtitle("MCS2001: Parent Education") +
  theme(plot.title = element_text(face="bold", size=20, family ="Times",hjust = 0.5))

MCSage11_nvq <- ggplot(MCSnvq_highest_lowest, aes(x=age11_vocab, colour=nvq)) +   geom_density(size=0.5) +
  scale_colour_manual(name="Socioeconomic Circumstance", values=c( "#E66101", "#5E3C99"), labels=c("Most Deprived", "Least Deprived"))+
  theme_classic() +
  labs( x = "Vocabulary Score \n (standardised)", y = "Density") +
  geom_vline(data=MCSnvq_highest_lowest, aes(xintercept=mean(age11_vocab)),
             linetype="dashed")+
  theme(panel.background = element_rect(fill = "#CBD5E8",
                                        colour = "#CBD5E8",
                                        size = 0.5, linetype = "dashed"),
        panel.grid.major = element_line(size = 0.5, linetype = 'solid',
                                        colour = "white"), 
        panel.grid.minor = element_line(size = 0.25, linetype = 'solid',
                                        colour = "white")) + theme(axis.text = element_text(size=18, family = "Times", colour = "black"), axis.title = element_text(size=18, family = "Times"))+
  theme(legend.text = element_text(size=15, family="Times"), legend.title = element_text(size=18, family="Times")) +
  ggtitle("MCS2001: Parent Education") +
  theme(plot.title = element_text(face="bold", size=20, family ="Times",hjust = 0.5))

MCSage14_nvq <- ggplot(MCSnvq_highest_lowest, aes(x=age14_vocab, colour=nvq)) +   geom_density(size=0.5) +
  scale_colour_manual(name="Socioeconomic Circumstance", values=c( "#E66101", "#5E3C99"), labels=c("Most Deprived", "Least Deprived"))+
  theme_classic() +
  labs( x = "Vocabulary Score \n (standardised)", y = "Density") +
  geom_vline(data=MCSnvq_highest_lowest, aes(xintercept=mean(age14_vocab)),
             linetype="dashed")+
  theme(panel.background = element_rect(fill = "#F4CAE4",
                                        colour = "#F4CAE4",
                                        size = 0.5, linetype = "dashed"),
        panel.grid.major = element_line(size = 0.5, linetype = 'solid',
                                        colour = "white"), 
        panel.grid.minor = element_line(size = 0.25, linetype = 'solid',
                                        colour = "white")) + theme(axis.text = element_text(size=18, family = "Times", colour = "black"), axis.title = element_text(size=18, family = "Times"))+
  theme(legend.text = element_text(size=15, family="Times"), legend.title = element_text(size=18, family="Times")) +
  ggtitle("MCS2001: Parent Education") +
  theme(plot.title = element_text(face="bold", size=20, family ="Times",hjust = 0.5))

BCSage5_nvq <- ggplot(BCSnvq_highest_lowest, aes(x=age5_vocab, colour=nvq)) +   geom_density(size=0.5) +
  scale_colour_manual(name="Socioeconomic Circumstance", values=c( "#E66101", "#5E3C99"), labels=c("Most Deprived", "Least Deprived"))+
  theme_classic() +
  labs( x = "Vocabulary Score \n (standardised)", y = "Density") +
  geom_vline(data=MCSnvq_highest_lowest, aes(xintercept=mean(age5_vocab)),
             linetype="dashed")+
  theme(panel.background = element_rect(fill = "#FDCDAC",
                                        colour = "#FDCDAC",
                                        size = 0.5, linetype = "dashed"),
        panel.grid.major = element_line(size = 0.5, linetype = 'solid',
                                        colour = "white"), 
        panel.grid.minor = element_line(size = 0.25, linetype = 'solid',
                                        colour = "white")) + theme(axis.text = element_text(size=18, family = "Times", colour = "black"), axis.title = element_text(size=18, family = "Times"))+
  theme(legend.text = element_text(size=15, family="Times"), legend.title = element_text(size=18, family="Times")) +
  ggtitle("BCS1970: Parent Education") +
  theme(plot.title = element_text(face="bold", size=20, family ="Times",hjust = 0.5))

BCSage11_nvq <- ggplot(MCSnvq_highest_lowest, aes(x=age11_vocab, colour=nvq)) +   geom_density(size=0.5) +
  scale_colour_manual(name="Socioeconomic Circumstance", values=c( "#E66101", "#5E3C99"), labels=c("Most Deprived", "Least Deprived"))+
  theme_classic() +
  labs( x = "Vocabulary Score \n (standardised)", y = "Density") +
  geom_vline(data=MCSnvq_highest_lowest, aes(xintercept=mean(age11_vocab)),
             linetype="dashed")+
  theme(panel.background = element_rect(fill = "#CBD5E8",
                                        colour = "#CBD5E8",
                                        size = 0.5, linetype = "dashed"),
        panel.grid.major = element_line(size = 0.5, linetype = 'solid',
                                        colour = "white"), 
        panel.grid.minor = element_line(size = 0.25, linetype = 'solid',
                                        colour = "white")) + theme(axis.text = element_text(size=18, family = "Times", colour = "black"), axis.title = element_text(size=18, family = "Times"))+
  theme(legend.text = element_text(size=15, family="Times"), legend.title = element_text(size=18, family="Times")) +
  ggtitle("BCS1970: Parent Education") +
  theme(plot.title = element_text(face="bold", size=20, family ="Times",hjust = 0.5))

BCSage14_nvq <- ggplot(MCSnvq_highest_lowest, aes(x=age14_vocab, colour=nvq)) +   geom_density(size=0.5) +
  scale_colour_manual(name="Socioeconomic Circumstance", values=c( "#E66101", "#5E3C99"), labels=c("Most Deprived", "Least Deprived"))+
  theme_classic() +
  labs( x = "Vocabulary Score \n (standardised)", y = "Density") +
  geom_vline(data=MCSnvq_highest_lowest, aes(xintercept=mean(age14_vocab)),
             linetype="dashed")+
  theme(panel.background = element_rect(fill = "#F4CAE4",
                                        colour = "#F4CAE4",
                                        size = 0.5, linetype = "dashed"),
        panel.grid.major = element_line(size = 0.5, linetype = 'solid',
                                        colour = "white"), 
        panel.grid.minor = element_line(size = 0.25, linetype = 'solid',
                                        colour = "white")) + theme(axis.text = element_text(size=18, family = "Times", colour = "black"), axis.title = element_text(size=18, family = "Times"))+
  theme(legend.text = element_text(size=15, family="Times"), legend.title = element_text(size=18, family="Times")) +
  ggtitle("BCS1970: Parent Education") +
  theme(plot.title = element_text(face="bold", size=20, family ="Times",hjust = 0.5))

#occupational status
MCSage5_occupation <- ggplot(MCSoccupation_highest_lowest, aes(x=age5_vocab, colour=occupation)) +   geom_density(size=0.5) +
  scale_colour_manual(name="Socioeconomic Circumstance", values=c( "#E66101", "#5E3C99"), labels=c("Most Deprived", "Least Deprived"))+
  theme_classic() +
  labs( x = "Vocabulary Score \n (standardised)", y = "Density") +
  geom_vline(data=MCSoccupation_highest_lowest, aes(xintercept=mean(age5_vocab)),
             linetype="dashed")+
  theme(panel.background = element_rect(fill = "#FDCDAC",
                                        colour = "#FDCDAC",
                                        size = 0.5, linetype = "dashed"),
        panel.grid.major = element_line(size = 0.5, linetype = 'solid',
                                        colour = "white"), 
        panel.grid.minor = element_line(size = 0.25, linetype = 'solid',
                                        colour = "white")) + theme(axis.text = element_text(size=18, family = "Times", colour = "black"), axis.title = element_text(size=18, family = "Times"))+
  theme(legend.text = element_text(size=15, family="Times"), legend.title = element_text(size=18, family="Times")) +
  ggtitle("MCS2001: Occupational Status ") +
  theme(plot.title = element_text(face="bold", size=20, family ="Times",hjust = 0.5))

MCSage11_occupation <- ggplot(MCSoccupation_highest_lowest, aes(x=age11_vocab, colour=occupation)) +   geom_density(size=0.5) +
  scale_colour_manual(name="Socioeconomic Circumstance", values=c( "#E66101", "#5E3C99"), labels=c("Most Deprived", "Least Deprived"))+
  theme_classic() +
  labs( x = "Vocabulary Score\n (standardised)", y = "Density") +
  geom_vline(data=MCSoccupation_highest_lowest, aes(xintercept=mean(age11_vocab)),
             linetype="dashed")+
  theme(panel.background = element_rect(fill = "#CBD5E8",
                                        colour = "#CBD5E8",
                                        size = 0.5, linetype = "dashed"),
        panel.grid.major = element_line(size = 0.5, linetype = 'solid',
                                        colour = "white"), 
        panel.grid.minor = element_line(size = 0.25, linetype = 'solid',
                                        colour = "white")) + theme(axis.text = element_text(size=18, family = "Times", colour = "black"), axis.title = element_text(size=18, family = "Times"))+
  theme(legend.text = element_text(size=15, family="Times"), legend.title = element_text(size=18, family="Times")) +
  ggtitle("MCS2001: Occupational Status ") +
  theme(plot.title = element_text(face="bold", size=20, family ="Times",hjust = 0.5))
 

MCSage14_occupation <- ggplot(MCSoccupation_highest_lowest, aes(x=age14_vocab, colour=occupation)) +   geom_density(size=0.5) +
  scale_colour_manual(name="Socioeconomic Circumstance", values=c( "#E66101", "#5E3C99"), labels=c("Most Deprived", "Least Deprived"))+
  theme_classic() +
  labs( x = "Vocabulary Score \n (standardised)", y = "Density") +
  geom_vline(data=MCSoccupation_highest_lowest, aes(xintercept=mean(age14_vocab)),
             linetype="dashed")+
  theme(panel.background = element_rect(fill = "#F4CAE4",
                                        colour = "#F4CAE4",
                                        size = 0.5, linetype = "dashed"),
        panel.grid.major = element_line(size = 0.5, linetype = 'solid',
                                        colour = "white"), 
        panel.grid.minor = element_line(size = 0.25, linetype = 'solid',
                                        colour = "white")) + theme(axis.text = element_text(size=18, family = "Times", colour = "black"), axis.title = element_text(size=18, family = "Times"))+
  theme(legend.text = element_text(size=15, family="Times"), legend.title = element_text(size=18, family="Times")) +
  ggtitle("MCS2001: Occupational Status ") +
  theme(plot.title = element_text(face="bold", size=20, family ="Times",hjust = 0.5))


BCSage5_occupation <- ggplot(BCSoccupation_highest_lowest, aes(x=age5_vocab, colour=occupation)) +   geom_density(size=0.5) +
  scale_colour_manual(name="Socioeconomic Circumstance", values=c( "#E66101", "#5E3C99"), labels=c("Most Deprived", "Least Deprived"))+
  theme_classic() +
  labs( x = "Vocabulary Score \n (standardised)", y = "Density") +
  geom_vline(data=BCSoccupation_highest_lowest, aes(xintercept=mean(age5_vocab)),
             linetype="dashed")+
  theme(panel.background = element_rect(fill = "#FDCDAC",
                                        colour = "#FDCDAC",
                                        size = 0.5, linetype = "dashed"),
        panel.grid.major = element_line(size = 0.5, linetype = 'solid',
                                        colour = "white"), 
        panel.grid.minor = element_line(size = 0.25, linetype = 'solid',
                                        colour = "white")) + theme(axis.text = element_text(size=18, family = "Times", colour = "black"), axis.title = element_text(size=18, family = "Times"))+
  theme(legend.text = element_text(size=15, family="Times"), legend.title = element_text(size=18, family="Times")) +
  ggtitle("BCS1970: Occupational Status ") +
  theme(plot.title = element_text(face="bold", size=20, family ="Times",hjust = 0.5))

BCSage11_occupation <- ggplot(BCSoccupation_highest_lowest, aes(x=age11_vocab, colour=occupation)) +   geom_density(size=0.5) +
  scale_colour_manual(name="Socioeconomic Circumstance", values=c( "#E66101", "#5E3C99"), labels=c("Most Deprived", "Least Deprived"))+
  theme_classic() +
  labs( x = "Vocabulary Score\n (standardised)", y = "Density") +
  geom_vline(data=BCSoccupation_highest_lowest, aes(xintercept=mean(age11_vocab)),
             linetype="dashed")+
  theme(panel.background = element_rect(fill = "#CBD5E8",
                                        colour = "#CBD5E8",
                                        size = 0.5, linetype = "dashed"),
        panel.grid.major = element_line(size = 0.5, linetype = 'solid',
                                        colour = "white"), 
        panel.grid.minor = element_line(size = 0.25, linetype = 'solid',
                                        colour = "white")) + theme(axis.text = element_text(size=18, family = "Times", colour = "black"), axis.title = element_text(size=18, family = "Times"))+
  theme(legend.text = element_text(size=15, family="Times"), legend.title = element_text(size=18, family="Times")) +
  ggtitle("BCS1970: Occupational Status ") +
  theme(plot.title = element_text(face="bold", size=20, family ="Times",hjust = 0.5))


BCSage14_occupation <- ggplot(BCSoccupation_highest_lowest, aes(x=age14_vocab, colour=occupation)) +   geom_density(size=0.5) +
  scale_colour_manual(name="Socioeconomic Circumstance", values=c( "#E66101", "#5E3C99"), labels=c("Most Deprived", "Least Deprived"))+
  theme_classic() +
  labs( x = "Vocabulary Score \n (standardised)", y = "Density") +
  geom_vline(data=BCSoccupation_highest_lowest, aes(xintercept=mean(age14_vocab)),
             linetype="dashed")+
  theme(panel.background = element_rect(fill = "#F4CAE4",
                                        colour = "#F4CAE4",
                                        size = 0.5, linetype = "dashed"),
        panel.grid.major = element_line(size = 0.5, linetype = 'solid',
                                        colour = "white"), 
        panel.grid.minor = element_line(size = 0.25, linetype = 'solid',
                                        colour = "white")) + theme(axis.text = element_text(size=18, family = "Times", colour = "black"), axis.title = element_text(size=18, family = "Times"))+
  theme(legend.text = element_text(size=15, family="Times"), legend.title = element_text(size=18, family="Times")) +
  ggtitle("BCS1970: Occupational Status ") +
  theme(plot.title = element_text(face="bold", size=20, family ="Times",hjust = 0.5))


#create plot to get legends for ggarranged plots
load("~/Documents/PhD/MCS DATASETS/SES_inequalities_language/inequalities-language-ability/Datasets/final datasets/2021-03-03_mcs_ses_imputedRQ1.Rdata")
imputed_mcs2=mi.res
imputed_mcs2_1 <- complete(imputed_mcs2,1)
legend_plot =  ggplot(imputed_mcs2_1, aes(x=age3_standardised, y=language_used_at_home, fill=language_used_at_home)) +  
  geom_bar(stat="identity") +
  geom_line(aes(color = gender), size=1) +
  scale_fill_manual(name="Age", values=c("#FDCDAC", "#CBD5E8", "#F4CAE4"), labels=c("Early Childhood Vocabulary", "Late Childhood Vocabulary", "Adolescent Vocabulary"), aesthetics="fill")+
  scale_colour_manual(name="SEC", values=c( "#E66101", "#5E3C99"), labels=c("Most Deprived", "Least Deprived"))+
  theme_classic() +
  theme(legend.text = element_text(size=15, family="Times"), legend.title = element_text(size=15, family="Times"))

density_plots = ggarrange(BCSage5_nvq, BCSage5_occupation, MCSage5_nvq, MCSage5_occupation, 
                          BCSage11_nvq, BCSage11_occupation,MCSage11_nvq, MCSage11_occupation,
                           BCSage14_nvq, BCSage14_occupation,MCSage14_nvq, MCSage14_occupation,
                          common.legend = FALSE, legend="right" , 
                          ncol =2, nrow=6,  align = c( "hv"),
                          legend.grob = get_legend(legend_plot))

p = density_plots + ggtitle("Figure 3.4: Density plots showing the cross-cohort distribution of vocabulary scores \n in the most deprived and least deprived SEC groups") +
  theme(plot.title = element_text(face="italic", size=25, family ="Times",hjust = 0.05))

ggsave("cross-cohort-density.jpeg", p, width=15, height=15, units= "in" ,dpi=300)


