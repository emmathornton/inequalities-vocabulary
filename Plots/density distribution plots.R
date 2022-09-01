#DISTRIBUTIONS 
#LOAD IN DATA and libraries####
library(mice)
library(ggplot2)
library(ggpubr)
library(tidyr)
library(tidyverse)
load("~/Documents/PhD/MCS DATASETS/SES_inequalities_language/inequalities-language-ability/Datasets/final datasets/2021-03-03_mcs_ses_imputedRQ1.Rdata")
imputed_mcs2=mi.res
imputed_mcs2_1 <- complete(imputed_mcs2,1)


#density plots####
#language scores distributions ####
age3 <- ggplot(imputed_mcs2_1, aes(x=age3_standardised)) + geom_density(alpha=.3) +
  labs( x = "Age 3 Language \n (standardised)", y = "Density")
age5 <- ggplot(imputed_mcs2_1, aes(x=age5_standardised)) + geom_density(alpha=.3)+
  labs( x = "Age 5 Language \n (standardised)", y = "Density")
age11<- ggplot(imputed_mcs2_1, aes(x=age11_standardised)) + geom_density(alpha=.3)+
  labs( x = "Age 11 Language \n (standardised)", y = "Density")
age14 <- ggplot(imputed_mcs2_1, aes(x=age14_standardised)) + geom_density(alpha=.3)+
  labs( x = "Age 14 Language \n (standardised)", y = "Density")
age3_1 <- age3+theme_classic()+theme(axis.text = element_text(size=9, family = "Times", colour = "black"), axis.title = element_text(size=12, family = "Times"))+
  theme(legend.text = element_text(size=15, family="Times"), legend.title = element_text(size=15, family="Times"))
age5_1 <- age5+theme_classic()+theme(axis.text = element_text(size=9, family = "Times", colour = "black"), axis.title = element_text(size=12, family = "Times"))+
  theme(legend.text = element_text(size=15, family="Times"), legend.title = element_text(size=15, family="Times"))
age11_1 <- age11+theme_classic()+theme(axis.text = element_text(size=9, family = "Times", colour = "black"), axis.title = element_text(size=12, family = "Times"))+
  theme(legend.text = element_text(size=15, family="Times"), legend.title = element_text(size=15, family="Times"))
age14_1 <- age14+theme_classic() +theme(axis.text = element_text(size=9, family = "Times", colour = "black"), axis.title = element_text(size=12, family = "Times"))+
  theme(legend.text = element_text(size=15, family="Times"), legend.title = element_text(size=15, family="Times"))
language_density = ggarrange(age3_1, age5_1, age11_1, age14_1, 
          # labels = c("3", "5", "11", "14"),
          #  label.x=0, label.y=0,
          #  font.label = list(size = 10, color = "black", face="plain" ),
          ncol =2, nrow=2,  align = c( "hv"),
          common.legend = TRUE, legend = "right")

language_distribution = language_density + ggtitle("Figure 3: Density plots showing the distribution of language scores at ages 3, 5, 11 & 14")
languageDistribution= language_distribution+ theme(plot.title = element_text(face="italic", size=12, family ="Times",hjust = 0.05))

#Language by SES indiactor density plots ####
#most deprived vs least deprived####
#get lowest and highest SES groups and combine to one variable
#density plot for highest and lowest SES group at each age
#create data with highest and lowest SES group 
nvq_lowest_mcsid = imputed_mcs2_1$mcsid[imputed_mcs2_1$highest_NVQ==1]
nvq_lowest = imputed_mcs2_1$highest_NVQ[imputed_mcs2_1$highest_NVQ==1]
age3_lowest = imputed_mcs2_1$age3_standardised[imputed_mcs2_1$highest_NVQ==1]
age5_lowest = imputed_mcs2_1$age5_standardised[imputed_mcs2_1$highest_NVQ==1]
age11_lowest = imputed_mcs2_1$age11_standardised[imputed_mcs2_1$highest_NVQ==1]
age14_lowest = imputed_mcs2_1$age14_standardised[imputed_mcs2_1$highest_NVQ==1]
nvq_lowest= data.frame(nvq_lowest_mcsid, nvq_lowest, age3_lowest, age5_lowest, age11_lowest, age14_lowest)
names(nvq_lowest) <- c("mcsid", "nvq_lowest", "age3_lowest", "age5_lowest", "age11_lowest", "age14_lowest")

nvq_highest_mcsid = imputed_mcs2_1$mcsid[imputed_mcs2_1$highest_NVQ==5]
nvq_highest = imputed_mcs2_1$highest_NVQ[imputed_mcs2_1$highest_NVQ==5]
age3_highest= imputed_mcs2_1$age3_standardised[imputed_mcs2_1$highest_NVQ==5]
age5_highest = imputed_mcs2_1$age5_standardised[imputed_mcs2_1$highest_NVQ==5]
age11_highest = imputed_mcs2_1$age11_standardised[imputed_mcs2_1$highest_NVQ==5]
age14_highest = imputed_mcs2_1$age14_standardised[imputed_mcs2_1$highest_NVQ==5]
nvq_highest= data.frame(nvq_highest_mcsid, nvq_highest, age3_highest,age5_highest, age11_highest, age14_highest)
names(nvq_highest) <- c("mcsid", "nvq_highest", "age3_highest","age5_highest", "age11_highest", "age14_highest")

nvq_highest_lowest <- merge(all=TRUE, nvq_lowest, nvq_highest,by="mcsid")

nvq_highest_lowest$nvq <- ifelse(!is.na(nvq_highest_lowest$nvq_lowest), nvq_highest_lowest$nvq_lowest, nvq_highest_lowest$nvq_highest)
nvq_highest_lowest$age3_vocab <- ifelse(!is.na(nvq_highest_lowest$age3_lowest), nvq_highest_lowest$age3_lowest, nvq_highest_lowest$age3_highest)
nvq_highest_lowest$age5_vocab <- ifelse(!is.na(nvq_highest_lowest$age5_lowest), nvq_highest_lowest$age5_lowest, nvq_highest_lowest$age5_highest)
nvq_highest_lowest$age11_vocab <- ifelse(!is.na(nvq_highest_lowest$age11_lowest), nvq_highest_lowest$age11_lowest, nvq_highest_lowest$age11_highest)
nvq_highest_lowest$age14_vocab <- ifelse(!is.na(nvq_highest_lowest$age14_lowest), nvq_highest_lowest$age14_lowest, nvq_highest_lowest$age14_highest)
nvq_highest_lowest$nvq=as.factor(nvq_highest_lowest$nvq)

income_lowest_mcsid = imputed_mcs2_1$mcsid[imputed_mcs2_1$income_quintiles==1]
income_lowest = imputed_mcs2_1$income_quintiles[imputed_mcs2_1$income_quintiles==1]
age3_lowest_income = imputed_mcs2_1$age3_standardised[imputed_mcs2_1$income_quintiles==1]
age5_lowest_income = imputed_mcs2_1$age5_standardised[imputed_mcs2_1$income_quintiles==1]
age11_lowest_income = imputed_mcs2_1$age11_standardised[imputed_mcs2_1$income_quintiles==1]
age14_lowest_income = imputed_mcs2_1$age14_standardised[imputed_mcs2_1$income_quintiles==1]
income_lowest= data.frame(income_lowest_mcsid, income_lowest, age3_lowest_income, age5_lowest_income, age11_lowest_income, age14_lowest_income)
names(income_lowest) <- c("mcsid", "income_lowest", "age3_lowest", "age5_lowest", "age11_lowest", "age14_lowest")

income_highest_mcsid = imputed_mcs2_1$mcsid[imputed_mcs2_1$income_quintiles==5]
income_highest = imputed_mcs2_1$income_quintiles[imputed_mcs2_1$income_quintiles==5]
age3_highest_income= imputed_mcs2_1$age3_standardised[imputed_mcs2_1$income_quintiles==5]
age5_highest_income= imputed_mcs2_1$age5_standardised[imputed_mcs2_1$income_quintiles==5]
age11_highest_income = imputed_mcs2_1$age11_standardised[imputed_mcs2_1$income_quintiles==5]
age14_highest_income = imputed_mcs2_1$age14_standardised[imputed_mcs2_1$income_quintiles==5]
income_highest= data.frame(income_highest_mcsid, income_highest, age3_highest_income,age5_highest_income, age11_highest_income, age14_highest_income)
names(income_highest) <- c("mcsid", "income_highest", "age3_highest","age5_highest", "age11_highest", "age14_highest")

income_highest_lowest <- merge(all=TRUE, income_lowest, income_highest,by="mcsid")

income_highest_lowest$income <- ifelse(!is.na(income_highest_lowest$income_lowest), income_highest_lowest$income_lowest, income_highest_lowest$income_highest)
income_highest_lowest$age3_vocab <- ifelse(!is.na(income_highest_lowest$age3_lowest), income_highest_lowest$age3_lowest, income_highest_lowest$age3_highest)
income_highest_lowest$age5_vocab <- ifelse(!is.na(income_highest_lowest$age5_lowest), income_highest_lowest$age5_lowest, income_highest_lowest$age5_highest)
income_highest_lowest$age11_vocab <- ifelse(!is.na(income_highest_lowest$age11_lowest), income_highest_lowest$age11_lowest, income_highest_lowest$age11_highest)
income_highest_lowest$age14_vocab <- ifelse(!is.na(income_highest_lowest$age14_lowest), income_highest_lowest$age14_lowest, income_highest_lowest$age14_highest)
income_highest_lowest$income=as.factor(income_highest_lowest$income)

wealth_lowest_mcsid = imputed_mcs2_1$mcsid[imputed_mcs2_1$wealth_quintiles==1]
wealth_lowest = imputed_mcs2_1$wealth_quintiles[imputed_mcs2_1$wealth_quintiles==1]
age3_lowest_wealth = imputed_mcs2_1$age3_standardised[imputed_mcs2_1$wealth_quintiles==1]
age5_lowest_wealth = imputed_mcs2_1$age5_standardised[imputed_mcs2_1$wealth_quintiles==1]
age11_lowest_wealth = imputed_mcs2_1$age11_standardised[imputed_mcs2_1$wealth_quintiles==1]
age14_lowest_wealth = imputed_mcs2_1$age14_standardised[imputed_mcs2_1$wealth_quintiles==1]
wealth_lowest= data.frame(wealth_lowest_mcsid, wealth_lowest, age3_lowest_wealth, age5_lowest_wealth, age11_lowest_wealth, age14_lowest_wealth)
names(wealth_lowest) <- c("mcsid", "wealth_lowest", "age3_lowest", "age5_lowest", "age11_lowest", "age14_lowest")

wealth_highest_mcsid = imputed_mcs2_1$mcsid[imputed_mcs2_1$wealth_quintiles==5]
wealth_highest = imputed_mcs2_1$wealth_quintiles[imputed_mcs2_1$wealth_quintiles==5]
age3_highest_wealth= imputed_mcs2_1$age3_standardised[imputed_mcs2_1$wealth_quintiles==5]
age5_highest_wealth= imputed_mcs2_1$age5_standardised[imputed_mcs2_1$wealth_quintiles==5]
age11_highest_wealth = imputed_mcs2_1$age11_standardised[imputed_mcs2_1$wealth_quintiles==5]
age14_highest_wealth = imputed_mcs2_1$age14_standardised[imputed_mcs2_1$wealth_quintiles==5]
wealth_highest= data.frame(wealth_highest_mcsid, wealth_highest, age3_highest_wealth,age5_highest_wealth, age11_highest_wealth, age14_highest_wealth)
names(wealth_highest) <- c("mcsid", "wealth_highest", "age3_highest","age5_highest", "age11_highest", "age14_highest")

wealth_highest_lowest <- merge(all=TRUE, wealth_lowest, wealth_highest,by="mcsid")

wealth_highest_lowest$wealth <- ifelse(!is.na(wealth_highest_lowest$wealth_lowest), wealth_highest_lowest$wealth_lowest, wealth_highest_lowest$wealth_highest)
wealth_highest_lowest$age3_vocab <- ifelse(!is.na(wealth_highest_lowest$age3_lowest), wealth_highest_lowest$age3_lowest, wealth_highest_lowest$age3_highest)
wealth_highest_lowest$age5_vocab <- ifelse(!is.na(wealth_highest_lowest$age5_lowest), wealth_highest_lowest$age5_lowest, wealth_highest_lowest$age5_highest)
wealth_highest_lowest$age11_vocab <- ifelse(!is.na(wealth_highest_lowest$age11_lowest), wealth_highest_lowest$age11_lowest, wealth_highest_lowest$age11_highest)
wealth_highest_lowest$age14_vocab <- ifelse(!is.na(wealth_highest_lowest$age14_lowest), wealth_highest_lowest$age14_lowest, wealth_highest_lowest$age14_highest)
wealth_highest_lowest$wealth=as.factor(wealth_highest_lowest$wealth)

occupation_lowest_mcsid = imputed_mcs2_1$mcsid[imputed_mcs2_1$occupational_status==1]
occupation_lowest = imputed_mcs2_1$occupational_status[imputed_mcs2_1$occupational_status==1]
age3_lowest_occupation = imputed_mcs2_1$age3_standardised[imputed_mcs2_1$occupational_status==1]
age5_lowest_occupation = imputed_mcs2_1$age5_standardised[imputed_mcs2_1$occupational_status==1]
age11_lowest_occupation = imputed_mcs2_1$age11_standardised[imputed_mcs2_1$occupational_status==1]
age14_lowest_occupation = imputed_mcs2_1$age14_standardised[imputed_mcs2_1$occupational_status==1]
occupation_lowest= data.frame(occupation_lowest_mcsid, occupation_lowest, age3_lowest_occupation, age5_lowest_occupation, age11_lowest_occupation, age14_lowest_occupation)
names(occupation_lowest) <- c("mcsid", "occupation_lowest", "age3_lowest", "age5_lowest", "age11_lowest", "age14_lowest")

occupation_highest_mcsid = imputed_mcs2_1$mcsid[imputed_mcs2_1$occupational_status==4]
occupation_highest = imputed_mcs2_1$occupational_status[imputed_mcs2_1$occupational_status==4]
age3_highest_occupation= imputed_mcs2_1$age3_standardised[imputed_mcs2_1$occupational_status==4]
age5_highest_occupation= imputed_mcs2_1$age5_standardised[imputed_mcs2_1$occupational_status==4]
age11_highest_occupation = imputed_mcs2_1$age11_standardised[imputed_mcs2_1$occupational_status==4]
age14_highest_occupation = imputed_mcs2_1$age14_standardised[imputed_mcs2_1$occupational_status==4]
occupation_highest= data.frame(occupation_highest_mcsid, occupation_highest, age3_highest_occupation,age5_highest_occupation, age11_highest_occupation, age14_highest_occupation)
names(occupation_highest) <- c("mcsid", "occupation_highest", "age3_highest","age5_highest", "age11_highest", "age14_highest")

occupation_highest_lowest <- merge(all=TRUE, occupation_lowest, occupation_highest,by="mcsid")

occupation_highest_lowest$occupation <- ifelse(!is.na(occupation_highest_lowest$occupation_lowest), occupation_highest_lowest$occupation_lowest, occupation_highest_lowest$occupation_highest)
occupation_highest_lowest$age3_vocab <- ifelse(!is.na(occupation_highest_lowest$age3_lowest), occupation_highest_lowest$age3_lowest, occupation_highest_lowest$age3_highest)
occupation_highest_lowest$age5_vocab <- ifelse(!is.na(occupation_highest_lowest$age5_lowest), occupation_highest_lowest$age5_lowest, occupation_highest_lowest$age5_highest)
occupation_highest_lowest$age11_vocab <- ifelse(!is.na(occupation_highest_lowest$age11_lowest), occupation_highest_lowest$age11_lowest, occupation_highest_lowest$age11_highest)
occupation_highest_lowest$age14_vocab <- ifelse(!is.na(occupation_highest_lowest$age14_lowest), occupation_highest_lowest$age14_lowest, occupation_highest_lowest$age14_highest)
occupation_highest_lowest$occupation=as.factor(occupation_highest_lowest$occupation)

imd_lowest_mcsid = imputed_mcs2_1$mcsid[imputed_mcs2_1$imd==1]
imd_lowest = imputed_mcs2_1$imd[imputed_mcs2_1$imd==1]
age3_lowest_imd = imputed_mcs2_1$age3_standardised[imputed_mcs2_1$imd==1]
age5_lowest_imd = imputed_mcs2_1$age5_standardised[imputed_mcs2_1$imd==1]
age11_lowest_imd = imputed_mcs2_1$age11_standardised[imputed_mcs2_1$imd==1]
age14_lowest_imd = imputed_mcs2_1$age14_standardised[imputed_mcs2_1$imd==1]
imd_lowest= data.frame(imd_lowest_mcsid, imd_lowest, age3_lowest_imd, age5_lowest_imd, age11_lowest_imd, age14_lowest_imd)
names(imd_lowest) <- c("mcsid", "imd_lowest", "age3_lowest", "age5_lowest", "age11_lowest", "age14_lowest")

imd_highest_mcsid = imputed_mcs2_1$mcsid[imputed_mcs2_1$imd==10]
imd_highest = imputed_mcs2_1$imd[imputed_mcs2_1$imd==10]
age3_highest_imd= imputed_mcs2_1$age3_standardised[imputed_mcs2_1$imd==10]
age5_highest_imd= imputed_mcs2_1$age5_standardised[imputed_mcs2_1$imd==10]
age11_highest_imd = imputed_mcs2_1$age11_standardised[imputed_mcs2_1$imd==10]
age14_highest_imd = imputed_mcs2_1$age14_standardised[imputed_mcs2_1$imd==10]
imd_highest= data.frame(imd_highest_mcsid, imd_highest, age3_highest_imd,age5_highest_imd, age11_highest_imd, age14_highest_imd)
names(imd_highest) <- c("mcsid", "imd_highest", "age3_highest","age5_highest", "age11_highest", "age14_highest")

imd_highest_lowest <- merge(all=TRUE, imd_lowest, imd_highest,by="mcsid")

imd_highest_lowest$imd <- ifelse(!is.na(imd_highest_lowest$imd_lowest), imd_highest_lowest$imd_lowest, imd_highest_lowest$imd_highest)
imd_highest_lowest$age3_vocab <- ifelse(!is.na(imd_highest_lowest$age3_lowest), imd_highest_lowest$age3_lowest, imd_highest_lowest$age3_highest)
imd_highest_lowest$age5_vocab <- ifelse(!is.na(imd_highest_lowest$age5_lowest), imd_highest_lowest$age5_lowest, imd_highest_lowest$age5_highest)
imd_highest_lowest$age11_vocab <- ifelse(!is.na(imd_highest_lowest$age11_lowest), imd_highest_lowest$age11_lowest, imd_highest_lowest$age11_highest)
imd_highest_lowest$age14_vocab <- ifelse(!is.na(imd_highest_lowest$age14_lowest), imd_highest_lowest$age14_lowest, imd_highest_lowest$age14_highest)
imd_highest_lowest$imd=as.factor(imd_highest_lowest$imd)

#density plots ####
#parent education
age3_nvq <- ggplot(nvq_highest_lowest, aes(x=age3_vocab, colour=nvq)) +   geom_density(size=0.5)+
  scale_colour_manual(name="Socioeconomic Circumstances", values=c( "#E66101", "#5E3C99"), labels=c("Most Deprived", "Least Deprived"))+
  theme_classic() +
  labs( x = "Vocabulary Score \n (standardised)", y = "Density") +
  geom_vline(data=nvq_highest_lowest, aes(xintercept=mean(age3_vocab)),
             linetype="dashed")+
 theme(panel.background = element_rect(fill = "#B3E2CD",
                                    colour = "#B3E2CD",
                                    size = 0.5, linetype = "dashed"),
    panel.grid.major = element_line(size = 0.5, linetype = 'solid',
                                    colour = "white"), 
    panel.grid.minor = element_line(size = 0.25, linetype = 'solid',
                                    colour = "white")) + theme(axis.text = element_text(size=18, family = "Times", colour = "black"), axis.title = element_text(size=18, family = "Times"))+
  theme(legend.text = element_text(size=15, family="Times"), legend.title = element_text(size=18, family="Times")) +
  ggtitle("Parent Education") +
  theme(plot.title = element_text(face="bold", size=20, family ="Times",hjust = 0.5)) +
  xlim(-5, 5)
  
age5_nvq <- ggplot(nvq_highest_lowest, aes(x=age5_vocab, colour=nvq)) +   geom_density(size=0.5) +
  scale_colour_manual(name="Socioeconomic Circumstances", values=c( "#E66101", "#5E3C99"), labels=c("Most Deprived", "Least Deprived"))+
  theme_classic() +
  labs( x = "Vocabulary Score \n (standardised)", y = "Density") +
  geom_vline(data=nvq_highest_lowest, aes(xintercept=mean(age5_vocab)),
             linetype="dashed")+
  theme(panel.background = element_rect(fill = "#FDCDAC",
                                        colour = "#FDCDAC",
                                        size = 0.5, linetype = "dashed"),
        panel.grid.major = element_line(size = 0.5, linetype = 'solid',
                                        colour = "white"), 
        panel.grid.minor = element_line(size = 0.25, linetype = 'solid',
                                        colour = "white")) + theme(axis.text = element_text(size=18, family = "Times", colour = "black"), axis.title = element_text(size=18, family = "Times"))+
  theme(legend.text = element_text(size=15, family="Times"), legend.title = element_text(size=18, family="Times")) +
  ggtitle("Parent Education") +
  theme(plot.title = element_text(face="bold", size=20, family ="Times",hjust = 0.5))+
  xlim(-5, 5)

age11_nvq <- ggplot(nvq_highest_lowest, aes(x=age11_vocab, colour=nvq)) +   geom_density(size=0.5) +
  scale_colour_manual(name="Socioeconomic Circumstances", values=c( "#E66101", "#5E3C99"), labels=c("Most Deprived", "Least Deprived"))+
  theme_classic() +
  labs( x = "Vocabulary Score \n (standardised)", y = "Density") +
  geom_vline(data=nvq_highest_lowest, aes(xintercept=mean(age11_vocab)),
             linetype="dashed")+
  theme(panel.background = element_rect(fill = "#CBD5E8",
                                        colour = "#CBD5E8",
                                        size = 0.5, linetype = "dashed"),
        panel.grid.major = element_line(size = 0.5, linetype = 'solid',
                                        colour = "white"), 
        panel.grid.minor = element_line(size = 0.25, linetype = 'solid',
                                        colour = "white")) + theme(axis.text = element_text(size=18, family = "Times", colour = "black"), axis.title = element_text(size=18, family = "Times"))+
  theme(legend.text = element_text(size=15, family="Times"), legend.title = element_text(size=18, family="Times")) +
  ggtitle("Parent Education") +
  theme(plot.title = element_text(face="bold", size=20, family ="Times",hjust = 0.5))+
  xlim(-5, 5)

age14_nvq <- ggplot(nvq_highest_lowest, aes(x=age14_vocab, colour=nvq)) +   geom_density(size=0.5) +
  scale_colour_manual(name="Socioeconomic Circumstances", values=c( "#E66101", "#5E3C99"), labels=c("Most Deprived", "Least Deprived"))+
  theme_classic() +
  labs( x = "Vocabulary Score \n (standardised)", y = "Density") +
  geom_vline(data=nvq_highest_lowest, aes(xintercept=mean(age14_vocab)),
             linetype="dashed")+
  theme(panel.background = element_rect(fill = "#F4CAE4",
                                        colour = "#F4CAE4",
                                        size = 0.5, linetype = "dashed"),
        panel.grid.major = element_line(size = 0.5, linetype = 'solid',
                                        colour = "white"), 
        panel.grid.minor = element_line(size = 0.25, linetype = 'solid',
                                        colour = "white")) + theme(axis.text = element_text(size=18, family = "Times", colour = "black"), axis.title = element_text(size=18, family = "Times"))+
  theme(legend.text = element_text(size=15, family="Times"), legend.title = element_text(size=18, family="Times")) +
  ggtitle("Parent Education") +
  theme(plot.title = element_text(face="bold", size=20, family ="Times",hjust = 0.5))+
  xlim(-5, 5)
#income
age3_income <- ggplot(income_highest_lowest, aes(x=age3_vocab, colour=income)) +   geom_density(size=0.5) +
  scale_colour_manual(name="Socioeconomic Circumstances", values=c( "#E66101", "#5E3C99"), labels=c("Most Deprived", "Least Deprived"))+
  theme_classic() +
  labs( x = "Vocabulary Score \n (standardised)", y = "Density") +
  geom_vline(data=income_highest_lowest, aes(xintercept=mean(age3_vocab)),
             linetype="dashed")+
  theme(panel.background = element_rect(fill = "#B3E2CD",
                                        colour = "#B3E2CD",
                                        size = 0.5, linetype = "dashed"),
        panel.grid.major = element_line(size = 0.5, linetype = 'solid',
                                        colour = "white"), 
        panel.grid.minor = element_line(size = 0.25, linetype = 'solid',
                                        colour = "white")) + theme(axis.text = element_text(size=18, family = "Times", colour = "black"), axis.title = element_text(size=18, family = "Times"))+
  theme(legend.text = element_text(size=15, family="Times"), legend.title = element_text(size=18, family="Times")) +
  ggtitle("Household Income") +
  theme(plot.title = element_text(face="bold", size=20, family ="Times",hjust = 0.5))+
  xlim(-5, 5)

age5_income <- ggplot(income_highest_lowest, aes(x=age5_vocab, colour=income)) +   geom_density(size=0.5) +
  scale_colour_manual(name="Socioeconomic Circumstances", values=c( "#E66101", "#5E3C99"), labels=c("Most Deprived", "Least Deprived"))+
  theme_classic() +
  labs( x = "Vocabulary Score \n (standardised)", y = "Density") +
  geom_vline(data=income_highest_lowest, aes(xintercept=mean(age5_vocab)),
             linetype="dashed")+
  theme(panel.background = element_rect(fill = "#FDCDAC",
                                        colour = "#FDCDAC",
                                        size = 0.5, linetype = "dashed"),
        panel.grid.major = element_line(size = 0.5, linetype = 'solid',
                                        colour = "white"), 
        panel.grid.minor = element_line(size = 0.25, linetype = 'solid',
                                        colour = "white")) + theme(axis.text = element_text(size=18, family = "Times", colour = "black"), axis.title = element_text(size=18, family = "Times"))+
  theme(legend.text = element_text(size=15, family="Times"), legend.title = element_text(size=18, family="Times")) +
  ggtitle("Household Income") +
  theme(plot.title = element_text(face="bold", size=20, family ="Times",hjust = 0.5))+
  xlim(-5, 5)

age11_income <- ggplot(income_highest_lowest, aes(x=age11_vocab, colour=income)) +   geom_density(size=0.5) +
  scale_colour_manual(name="Socioeconomic Circumstances", values=c( "#E66101", "#5E3C99"), labels=c("Most Deprived", "Least Deprived"))+
  theme_classic() +
  labs( x = "Vocabulary Score \n (standardised)", y = "Density") +
  geom_vline(data=income_highest_lowest, aes(xintercept=mean(age11_vocab)),
             linetype="dashed")+
  theme(panel.background = element_rect(fill = "#CBD5E8",
                                        colour = "#CBD5E8",
                                        size = 0.5, linetype = "dashed"),
        panel.grid.major = element_line(size = 0.5, linetype = 'solid',
                                        colour = "white"), 
        panel.grid.minor = element_line(size = 0.25, linetype = 'solid',
                                        colour = "white")) + theme(axis.text = element_text(size=18, family = "Times", colour = "black"), axis.title = element_text(size=18, family = "Times"))+
  theme(legend.text = element_text(size=15, family="Times"), legend.title = element_text(size=18, family="Times")) +
  ggtitle("Household Income") +
  theme(plot.title = element_text(face="bold", size=20, family ="Times",hjust = 0.5))+
  xlim(-5, 5)

age14_income <- ggplot(income_highest_lowest, aes(x=age14_vocab, colour=income)) +   geom_density(size=0.5) +
  scale_colour_manual(name="Socioeconomic Circumstances", values=c( "#E66101", "#5E3C99"), labels=c("Most Deprived", "Least Deprived"))+
  theme_classic() +
  labs( x = "Vocabulary Score \n (standardised)", y = "Density") +
  geom_vline(data=income_highest_lowest, aes(xintercept=mean(age14_vocab)),
             linetype="dashed")+
  theme(panel.background = element_rect(fill = "#F4CAE4",
                                        colour = "#F4CAE4",
                                        size = 0.5, linetype = "dashed"),
        panel.grid.major = element_line(size = 0.5, linetype = 'solid',
                                        colour = "white"), 
        panel.grid.minor = element_line(size = 0.25, linetype = 'solid',
                                        colour = "white")) + theme(axis.text = element_text(size=18, family = "Times", colour = "black"), axis.title = element_text(size=18, family = "Times"))+
  theme(legend.text = element_text(size=15, family="Times"), legend.title = element_text(size=18, family="Times")) +
  ggtitle("Household Income") +
  theme(plot.title = element_text(face="bold", size=20, family ="Times",hjust = 0.5))+
  xlim(-5, 5)

#wealth
age3_wealth <- ggplot(wealth_highest_lowest, aes(x=age3_vocab, colour=wealth)) +   geom_density(size=0.5) +
  scale_colour_manual(name="Socioeconomic Circumstances", values=c( "#E66101", "#5E3C99"), labels=c("Most Deprived", "Least Deprived"))+
  theme_classic() +
  labs( x = "Vocabulary Score \n (standardised)", y = "Density") +
  geom_vline(data=wealth_highest_lowest, aes(xintercept=mean(age3_vocab)),
             linetype="dashed")+
  theme(panel.background = element_rect(fill = "#B3E2CD",
                                        colour = "#B3E2CD",
                                        size = 0.5, linetype = "dashed"),
        panel.grid.major = element_line(size = 0.5, linetype = 'solid',
                                        colour = "white"), 
        panel.grid.minor = element_line(size = 0.25, linetype = 'solid',
                                        colour = "white")) + theme(axis.text = element_text(size=18, family = "Times", colour = "black"), axis.title = element_text(size=18, family = "Times"))+
  theme(legend.text = element_text(size=15, family="Times"), legend.title = element_text(size=18, family="Times")) +
  ggtitle("Family Wealth") +
  theme(plot.title = element_text(face="bold", size=20, family ="Times",hjust = 0.5))+
  xlim(-5, 5)

age5_wealth <- ggplot(wealth_highest_lowest, aes(x=age5_vocab, colour=wealth)) +   geom_density(size=0.5) +
  scale_colour_manual(name="Socioeconomic Circumstances", values=c( "#E66101", "#5E3C99"), labels=c("Most Deprived", "Least Deprived"))+
  theme_classic() +
  labs( x = "Vocabulary Score \n (standardised)", y = "Density") +
  geom_vline(data=wealth_highest_lowest, aes(xintercept=mean(age5_vocab)),
             linetype="dashed")+
  theme(panel.background = element_rect(fill = "#FDCDAC",
                                        colour = "#FDCDAC",
                                        size = 0.5, linetype = "dashed"),
        panel.grid.major = element_line(size = 0.5, linetype = 'solid',
                                        colour = "white"), 
        panel.grid.minor = element_line(size = 0.25, linetype = 'solid',
                                        colour = "white")) + theme(axis.text = element_text(size=18, family = "Times", colour = "black"), axis.title = element_text(size=18, family = "Times"))+
  theme(legend.text = element_text(size=15, family="Times"), legend.title = element_text(size=18, family="Times")) +
  ggtitle("Family Wealth") +
  theme(plot.title = element_text(face="bold", size=20, family ="Times",hjust = 0.5))+ 
  xlim(-5, 5)

age11_wealth <- ggplot(wealth_highest_lowest, aes(x=age11_vocab, colour=wealth)) +   geom_density(size=0.5) +
  scale_colour_manual(name="Socioeconomic Circumstances", values=c( "#E66101", "#5E3C99"), labels=c("Most Deprived", "Least Deprived"))+
  theme_classic() +
  labs( x = "Vocabulary Score \n (standardised)", y = "Density") +
  geom_vline(data=wealth_highest_lowest, aes(xintercept=mean(age11_vocab)),
             linetype="dashed")+
  theme(panel.background = element_rect(fill = "#CBD5E8",
                                        colour = "#CBD5E8",
                                        size = 0.5, linetype = "dashed"),
        panel.grid.major = element_line(size = 0.5, linetype = 'solid',
                                        colour = "white"), 
        panel.grid.minor = element_line(size = 0.25, linetype = 'solid',
                                        colour = "white")) + theme(axis.text = element_text(size=18, family = "Times", colour = "black"), axis.title = element_text(size=18, family = "Times"))+
  theme(legend.text = element_text(size=15, family="Times"), legend.title = element_text(size=18, family="Times")) +
  ggtitle("Family Wealth") +
  theme(plot.title = element_text(face="bold", size=20, family ="Times",hjust = 0.5))+
  xlim(-5, 5)

age14_wealth <- ggplot(wealth_highest_lowest, aes(x=age14_vocab, colour=wealth)) +   geom_density(size=0.5) +
  scale_colour_manual(name="Socioeconomic Circumstances", values=c( "#E66101", "#5E3C99"), labels=c("Most Deprived", "Least Deprived"))+
  theme_classic() +
  labs( x = "Vocabulary Score \n (standardised)", y = "Density") +
  geom_vline(data=wealth_highest_lowest, aes(xintercept=mean(age14_vocab)),
             linetype="dashed")+
  theme(panel.background = element_rect(fill = "#F4CAE4",
                                        colour = "#F4CAE4",
                                        size = 0.5, linetype = "dashed"),
        panel.grid.major = element_line(size = 0.5, linetype = 'solid',
                                        colour = "white"), 
        panel.grid.minor = element_line(size = 0.25, linetype = 'solid',
                                        colour = "white")) + theme(axis.text = element_text(size=18, family = "Times", colour = "black"), axis.title = element_text(size=18, family = "Times"))+
  theme(legend.text = element_text(size=15, family="Times"), legend.title = element_text(size=18, family="Times")) +
  ggtitle("Family Wealth") +
  theme(plot.title = element_text(face="bold", size=20, family ="Times",hjust = 0.5))+
  xlim(-5, 5)


#occupational status
age3_occupation <- ggplot(occupation_highest_lowest, aes(x=age3_vocab, colour=occupation)) +   geom_density(size=0.5) +
  scale_colour_manual(name="Socioeconomic Circumstances", values=c( "#E66101", "#5E3C99"), labels=c("Most Deprived", "Least Deprived"))+
  theme_classic() +
  labs( x = "Vocabulary Score \n (standardised)", y = "Density") +
  geom_vline(data=occupation_highest_lowest, aes(xintercept=mean(age3_vocab)),
             linetype="dashed")+
  theme(panel.background = element_rect(fill = "#B3E2CD",
                                        colour = "#B3E2CD",
                                        size = 0.5, linetype = "dashed"),
        panel.grid.major = element_line(size = 0.5, linetype = 'solid',
                                        colour = "white"), 
        panel.grid.minor = element_line(size = 0.25, linetype = 'solid',
                                        colour = "white")) + theme(axis.text = element_text(size=18, family = "Times", colour = "black"), axis.title = element_text(size=18, family = "Times"))+
  theme(legend.text = element_text(size=15, family="Times"), legend.title = element_text(size=18, family="Times")) +
  ggtitle("Occupational Status ") +
  theme(plot.title = element_text(face="bold", size=20, family ="Times",hjust = 0.5))+
  xlim(-5, 5)

age5_occupation <- ggplot(occupation_highest_lowest, aes(x=age5_vocab, colour=occupation)) +   geom_density(size=0.5) +
  scale_colour_manual(name="Socioeconomic Circumstances", values=c( "#E66101", "#5E3C99"), labels=c("Most Deprived", "Least Deprived"))+
  theme_classic() +
  labs( x = "Vocabulary Score \n (standardised)", y = "Density") +
  geom_vline(data=occupation_highest_lowest, aes(xintercept=mean(age5_vocab)),
             linetype="dashed")+
  theme(panel.background = element_rect(fill = "#FDCDAC",
                                        colour = "#FDCDAC",
                                        size = 0.5, linetype = "dashed"),
        panel.grid.major = element_line(size = 0.5, linetype = 'solid',
                                        colour = "white"), 
        panel.grid.minor = element_line(size = 0.25, linetype = 'solid',
                                        colour = "white")) + theme(axis.text = element_text(size=18, family = "Times", colour = "black"), axis.title = element_text(size=18, family = "Times"))+
  theme(legend.text = element_text(size=15, family="Times"), legend.title = element_text(size=18, family="Times")) +
  ggtitle("Occupational Status ") +
  theme(plot.title = element_text(face="bold", size=20, family ="Times",hjust = 0.5))+
  xlim(-5, 5)

age11_occupation <- ggplot(occupation_highest_lowest, aes(x=age11_vocab, colour=occupation)) +   geom_density(size=0.5) +
  scale_colour_manual(name="Socioeconomic Circumstances", values=c( "#E66101", "#5E3C99"), labels=c("Most Deprived", "Least Deprived"))+
  theme_classic() +
  labs( x = "Language Score\n (standardised)", y = "Density") +
  geom_vline(data=occupation_highest_lowest, aes(xintercept=mean(age11_vocab)),
             linetype="dashed")+
  theme(panel.background = element_rect(fill = "#CBD5E8",
                                        colour = "#CBD5E8",
                                        size = 0.5, linetype = "dashed"),
        panel.grid.major = element_line(size = 0.5, linetype = 'solid',
                                        colour = "white"), 
        panel.grid.minor = element_line(size = 0.25, linetype = 'solid',
                                        colour = "white")) + theme(axis.text = element_text(size=18, family = "Times", colour = "black"), axis.title = element_text(size=18, family = "Times"))+
  theme(legend.text = element_text(size=15, family="Times"), legend.title = element_text(size=18, family="Times")) +
  ggtitle("Occupational Status ") +
  theme(plot.title = element_text(face="bold", size=20, family ="Times",hjust = 0.5))+
  xlim(-5, 5)

age14_occupation <- ggplot(occupation_highest_lowest, aes(x=age14_vocab, colour=occupation)) +   geom_density(size=0.5) +
  scale_colour_manual(name="Socioeconomic Circumstances", values=c( "#E66101", "#5E3C99"), labels=c("Most Deprived", "Least Deprived"))+
  theme_classic() +
  labs( x = "Vocabulary Score \n (standardised)", y = "Density") +
  geom_vline(data=occupation_highest_lowest, aes(xintercept=mean(age14_vocab)),
             linetype="dashed")+
  theme(panel.background = element_rect(fill = "#F4CAE4",
                                        colour = "#F4CAE4",
                                        size = 0.5, linetype = "dashed"),
        panel.grid.major = element_line(size = 0.5, linetype = 'solid',
                                        colour = "white"), 
        panel.grid.minor = element_line(size = 0.25, linetype = 'solid',
                                        colour = "white")) + theme(axis.text = element_text(size=18, family = "Times", colour = "black"), axis.title = element_text(size=18, family = "Times"))+
  theme(legend.text = element_text(size=15, family="Times"), legend.title = element_text(size=18, family="Times")) +
  ggtitle("Occupational Status ") +
  theme(plot.title = element_text(face="bold", size=20, family ="Times",hjust = 0.5))+
  xlim(-5, 5)

#imd
age3_imd <- ggplot(imd_highest_lowest, aes(x=age3_vocab, colour=imd)) +   geom_density(size=0.5) +
  scale_colour_manual(name="Socioeconomic Circumstances", values=c( "#E66101", "#5E3C99"), labels=c("Most Deprived", "Least Deprived"))+
  theme_classic() +
  labs( x = "Vocabulary Score \n (standardised)", y = "Density") +
  geom_vline(data=imd_highest_lowest, aes(xintercept=mean(age3_vocab)),
             linetype="dashed")+
  theme(panel.background = element_rect(fill = "#B3E2CD",
                                        colour = "#B3E2CD",
                                        size = 0.5, linetype = "dashed"),
        panel.grid.major = element_line(size = 0.5, linetype = 'solid',
                                        colour = "white"), 
        panel.grid.minor = element_line(size = 0.25, linetype = 'solid',
                                        colour = "white")) + theme(axis.text = element_text(size=18, family = "Times", colour = "black"), axis.title = element_text(size=18, family = "Times"))+
  theme(legend.text = element_text(size=15, family="Times"), legend.title = element_text(size=18, family="Times")) +
  ggtitle("Relative Neighbourhood Deprivation") +
  theme(plot.title = element_text(face="bold", size=20, family ="Times",hjust = 0.5))+
  xlim(-5, 5)

age5_imd <- ggplot(imd_highest_lowest, aes(x=age5_vocab, colour=imd)) +   geom_density(size=0.5) +
  scale_colour_manual(name="Socioeconomic Circumstances", values=c( "#E66101", "#5E3C99"), labels=c("Most Deprived", "Least Deprived"))+
  theme_classic() +
  labs( x = "Vocabulary Score \n (standardised)", y = "Density") +
  geom_vline(data=imd_highest_lowest, aes(xintercept=mean(age5_vocab)),
             linetype="dashed")+
  theme(panel.background = element_rect(fill = "#FDCDAC",
                                        colour = "#FDCDAC",
                                        size = 0.5, linetype = "dashed"),
        panel.grid.major = element_line(size = 0.5, linetype = 'solid',
                                        colour = "white"), 
        panel.grid.minor = element_line(size = 0.25, linetype = 'solid',
                                        colour = "white")) + theme(axis.text = element_text(size=18, family = "Times", colour = "black"), axis.title = element_text(size=18, family = "Times"))+
  theme(legend.text = element_text(size=15, family="Times"), legend.title = element_text(size=18, family="Times")) +
  ggtitle("Relative Neighbourhood Deprivation") +
  theme(plot.title = element_text(face="bold", size=20, family ="Times",hjust = 0.5))+
  xlim(-5, 5)

age11_imd <- ggplot(imd_highest_lowest, aes(x=age11_vocab, colour=imd)) +   geom_density(size=0.5) +
  scale_colour_manual(name="Socioeconomic Circumstances", values=c( "#E66101", "#5E3C99"), labels=c("Most Deprived", "Least Deprived"))+
  theme_classic() +
  labs( x = "Vocabulary Score \n (standardised)", y = "Density") +
  geom_vline(data=imd_highest_lowest, aes(xintercept=mean(age11_vocab)),
             linetype="dashed")+
  theme(panel.background = element_rect(fill = "#CBD5E8",
                                        colour = "#CBD5E8",
                                        size = 0.5, linetype = "dashed"),
        panel.grid.major = element_line(size = 0.5, linetype = 'solid',
                                        colour = "white"), 
        panel.grid.minor = element_line(size = 0.25, linetype = 'solid',
                                        colour = "white")) + theme(axis.text = element_text(size=18, family = "Times", colour = "black"), axis.title = element_text(size=18, family = "Times"))+
  theme(legend.text = element_text(size=15, family="Times"), legend.title = element_text(size=18, family="Times")) +
  ggtitle("Relative Neighbourhood Deprivation") +
  theme(plot.title = element_text(face="bold", size=20, family ="Times",hjust = 0.5))+
  xlim(-5, 5)

age14_imd <- ggplot(imd_highest_lowest, aes(x=age14_vocab, colour=imd)) +   geom_density(size=0.5) +
  scale_colour_manual(name="Socioeconomic Circumstances", values=c( "#E66101", "#5E3C99"), labels=c("Most Deprived", "Least Deprived"))+
  theme_classic() +
  labs( x = "Vocabulary Score \n (standardised)", y = "Density") +
  geom_vline(data=imd_highest_lowest, aes(xintercept=mean(age14_vocab)),
             linetype="dashed")+
  theme(panel.background = element_rect(fill = "#F4CAE4",
                                        colour = "#F4CAE4",
                                        size = 0.5, linetype = "dashed"),
        panel.grid.major = element_line(size = 0.5, linetype = 'solid',
                                        colour = "white"), 
        panel.grid.minor = element_line(size = 0.25, linetype = 'solid',
                                        colour = "white")) + theme(axis.text = element_text(size=18, family = "Times", colour = "black"), axis.title = element_text(size=18, family = "Times"))+
  theme(legend.text = element_text(size=15, family="Times"), legend.title = element_text(size=18, family="Times")) +
  ggtitle("Relative Neighbourhood Deprivation") +
  theme(plot.title = element_text(face="bold", size=20, family ="Times",hjust = 0.5))+
  xlim(-5, 5)

#create plot to get legends for ggarranged plots

legend_plot =  ggplot(imputed_mcs2_1, aes(x=age3_standardised, y=occupational_status, fill=occupational_status)) +  
  geom_bar(stat="identity") +
  geom_line(aes(color = gender), size=1) +
  scale_fill_manual(name="Age", values=c( "#B3E2CD", "#FDCDAC", "#CBD5E8", "#F4CAE4"), labels=c("Age 3", "Age 5", "Age 11", "Age 14"), aesthetics="fill")+
  scale_colour_manual(name="SEC", values=c( "#E66101", "#5E3C99"), labels=c("Most Deprived", "Least Deprived"))+
  theme_classic() +
  theme(legend.text = element_text(size=25, family="Times"), legend.title = element_text(size=25, family="Times"))



  
density_plots = ggarrange(age3_nvq, age3_income, age3_wealth, age3_occupation, age3_imd, 
                          age5_nvq, age5_income, age5_wealth, age5_occupation, age5_imd,
                          age11_nvq, age11_income, age11_wealth, age11_occupation, age11_imd,
                          age14_nvq, age14_income, age14_wealth, age14_occupation, age14_imd, 
                          common.legend = FALSE, legend="right" , 
                          ncol =5, nrow=4,  align = c( "hv"),
                          legend.grob = get_legend(legend_plot))
                                                   
p = density_plots + ggtitle("Figure 3.1: Density plots showing the distribution of language scores in the most deprived and least deprived SEC groups") +
                          theme(plot.title = element_text(face="italic", size=30, family ="Times",hjust = 0.05))
                          
ggsave("mcs-density.jpeg", p, width=28, height=20, units= "in" ,dpi=300, limitsize = FALSE)

