#PLOTS BY SES INDICATOR 
library(ggplot2)
library(ggpubr)
library(RColorBrewer)
#NVQ
NVQ_plot <- data.frame(
  parent_NVQ = factor(c("NVQ1" ,"NVQ2", "NVQ3", "NVQ4", "NVQ5"), levels=c("NVQ1","NVQ2", "NVQ3", "NVQ4", "NVQ5")),
  language = c(0,  0.1940105, 0.3382691 ,  0.5795223 ,0.7340364, 
               0,0.23441782, 0.36245677,  0.66245483,0.89752718, 
               0,0.17481718, 0.32804643 , 0.56224915 , 0.81414513, 
              0,0.147957584,0.257006020, 0.553040993,0.929754771 ),
  lower_ci_nvq = c(0,0.1323571 , 0.2736658, 0.5194296 , 0.6590732 , 
                  0, 0.16763072 ,0.29190198 ,  0.59783208 ,0.81620206 , 
                  0, 0.09476466, 0.23872321,0.48111003 ,0.71787095 ,
                  0, 0.05758660,  0.15649630,0.46170447 ,  0.82079878),
  upper_ci_nvq = c(0,0.25566394, 0.40287244, 0.63961509, 0.80899963,
                   0,0.30120493, 0.43301157,  0.72707757,  0.97885230,
                  0,0.25486970,0.41736966, 0.64338827,0.91041931,
                  0, 0.23832857, 0.35751574,0.64437752,1.03871076
                   ),
  age = factor(c("Age 3","Age 3", "Age 3", "Age 3", "Age 3", 
                 "Age 5", "Age 5", "Age 5", "Age 5", "Age 5", 
                 "Age 11",  "Age 11", "Age 11", "Age 11", "Age 11", 
                 "Age 14", "Age 14", "Age 14", "Age 14", "Age 14"
                 )
  ))


nvq_indicator <- ggplot(data=NVQ_plot, aes(x=parent_NVQ, y=language, group=age)) +
  geom_line(aes(color=age)) +
  geom_ribbon(data=NVQ_plot,aes(ymin=lower_ci_nvq,ymax=upper_ci_nvq, fill=age), alpha=0.1) + 
 geom_point(aes(color=age)) +
scale_color_manual(name="Age", values=c("#d7191c", "#fdae61","#31A354", "#2b83ba"), 
                   limits=c("Age 3", "Age 5", "Age 11", "Age 14"))+
  scale_fill_manual(name="Age", values=c("#d7191c", "#fdae61","#31A354", "#2b83ba"), 
                    limits=c("Age 3", "Age 5", "Age 11", "Age 14"))+

  theme_classic()+
  theme(axis.text = element_text(size=9, 
                                 family = "Times", 
                                 colour = "black"), 
        axis.title = element_text(size=12, 
                                  family = "Times"))+
  theme(legend.text = element_text(size=20, 
                                   family="Times"), 
        legend.title = element_text(size=20, 
                                    family="Times"))


print(nvq_indicator)


#INCOME

income_plot <- data.frame(
  Income_Quintiles = factor(c("Lowest \n Quintile", "2", "3", "4", "Highest \n Quintile"), levels=c("Lowest \n Quintile", "2", "3", "4", "Highest \n Quintile")),
  language = c(0, 0.1634444, 0.4012661, 0.5658124, 0.6690263,
               0,  0.1635556, 0.4256756, 0.5727573 ,0.7683683,
               0,  0.1265009, 0.3179149, 0.4581217, 0.6689242 ,
              0,   0.1390911, 0.2798513, 0.4420979, 0.6891986 ),
  lower_ci_income = c(0,  0.1193042, 0.3577083, 0.5222084, 0.6248146,
                      0,  0.1203904, 0.3784683,0.5280206, 0.7224439 ,
                     0,  0.07400522 ,0.26411234, 0.40520168, 0.61508755, 
                    0, 0.08690498, 0.22535879, 0.37853604, 0.63382532 ),
  upper_ci_income = c(0, 0.2075846, 0.4448240, 0.6094163, 0.7132381,
                      0, 0.2067208, 0.4728829, 0.6174940, 0.8142926,
                      0, 0.1789965, 0.3717174, 0.5110417 ,0.7227609,
                     0, 0.1912772, 0.3343438, 0.5056598, 0.7445719),
  age = factor(c("Age 3", "Age 3", "Age 3", "Age 3", "Age 3", 
                 "Age 5", "Age 5", "Age 5", "Age 5", "Age 5",
                 "Age 11", "Age 11", "Age 11", "Age 11", "Age 11",
                 "Age 14", "Age 14", "Age 14", "Age 14", "Age 14"
                 )
  ))


income_indicator <- ggplot(data=income_plot, aes(x=Income_Quintiles, y=language, group=age)) +
  geom_line(aes(color=age)) +
  geom_ribbon(data=income_plot,aes(ymin=lower_ci_income,ymax=upper_ci_income, fill=age), alpha=0.1) + 
  geom_point(aes(color=age)) +
  scale_color_manual(name="Age", values=c("#d7191c", "#fdae61","#31A354", "#2b83ba"), limits=c("Age 3", "Age 5", "Age 11", "Age 14"))+
  scale_fill_manual(name="Age", values=c("#d7191c", "#fdae61","#31A354", "#2b83ba"), limits=c("Age 3", "Age 5", "Age 11", "Age 14"))+
  theme_classic()+
  theme(axis.text = element_text(size=9, family = "Times",colour = "black"), axis.title = element_text(size=12, family = "Times"))+
  theme(legend.text = element_text(size=20, family="Times"), legend.title = element_text(size=20, family="Times"))



print(income_indicator)


#WEALTH
wealth_plot <- data.frame(
  wealth = factor(c("Lowest \n Quintile", "2", "3", "4", "Highest \n Quintile"), levels=c("Lowest \n Quintile", "2", "3", "4", "Highest \n Quintile")),
  language = c(0, 0.03438853 ,0.09467303, 0.19619418, 0.35355892 ,
               0, 0.02031765 ,0.09569745 ,0.21144834, 0.41812666, 
               0, 0.02945516 ,0.09888874, 0.19274520, 0.38636224, 
               0,  0.02000656, 0.10296388, 0.18206503, 0.42347581
               
               
  ),
  lower_ci_wealth = c(0, -0.03548665,  0.02209831,  0.12693043 , 0.29656497,
                      0, -0.0507022,  0.0323370,  0.1475750,  0.3649633, 
                      0,  -0.05183129 , 0.02522663,  0.13422574,  0.31994320, 
                      0, -0.04890041,  0.02927762,  0.10381353 , 0.35794141
                      
  ),
  upper_ci_wealth = c(0, 0.1042637, 0.1672478, 0.2654579, 0.4105529, 
                      0,0.0913375, 0.1590579, 0.2753217, 0.4712901, 
                      0,0.1107416, 0.1725508, 0.2512647, 0.4527813, 
                     0, 0.08891352, 0.17665013, 0.26031653, 0.48901021
                      
  ),
  age = factor(c("Age 3", "Age 3", "Age 3", "Age 3", "Age 3", 
                 "Age 5", "Age 5", "Age 5", "Age 5", "Age 5",
                 "Age 11", "Age 11", "Age 11", "Age 11", "Age 11",
                 "Age 14", "Age 14", "Age 14", "Age 14", "Age 14"
                 )
  ))


wealth_indicator <- ggplot(data=wealth_plot, aes(x=wealth, y=language, group=age)) +
  geom_line(aes(color=age)) +
  geom_ribbon(data=wealth_plot,aes(ymin=lower_ci_wealth,ymax=upper_ci_wealth, fill=age), alpha=0.1) + 
  geom_point(aes(color=age)) +
  scale_color_manual(name="Age", values=c("#d7191c", "#fdae61","#31A354", "#2b83ba"), limits=c("Age 3", "Age 5", "Age 11", "Age 14"))+
  scale_fill_manual(name="Age", values=c("#d7191c", "#fdae61","#31A354", "#2b83ba"), limits=c("Age 3", "Age 5", "Age 11", "Age 14"))+
  theme_classic()+
  theme(axis.text = element_text(size=9, family = "Times",colour = "black"), axis.title = element_text(size=12, family = "Times"))+
  theme(legend.text = element_text(size=20, family="Times"), legend.title = element_text(size=20, family="Times"))




#OCCUPATIONAL STATUS
#occupation plot

#standardised plot 
occupation_plot <- data.frame(
  occupational_status = factor(c("Routine", "Intermediate", "Higher managerial"), levels=c("Routine", "Intermediate", "Higher managerial")),
  language = c(0,  0.2141938,  0.3936560 ,
               0,  0.1993054 ,0.4805509  , 
               0,   0.1856263, 0.4302986 ,  
               0, 0.1147465, 0.4390198),
  lower_ci_occupation = c( 0, 0.1717306, 0.3567541  , 
                           0, 0.1556178, 0.4432185 , 
                           0,  0.1330063, 0.3863348  , 
                           0, 0.05524026, 0.39026929),
  upper_ci_occupation = c(0,0.2566571 ,0.4305579,
                          0, 0.2429929, 0.5178832, 
                          0, 0.2382463, 0.4742624,
                          0, 0.1742527, 0.4877704
                          ),
  age = factor(c("Age 3", "Age 3", "Age 3", 
                 "Age 5", "Age 5", "Age 5", 
                 "Age 11", "Age 11", "Age 11",  
                 "Age 14", "Age 14", "Age 14" 
                 )
  ))


occupation_indicator <- ggplot(data=occupation_plot, aes(x=occupational_status, y=language, group=age)) +
  geom_line(aes(color=age)) +
   geom_ribbon(data=occupation_plot,aes(ymin=lower_ci_occupation,ymax=upper_ci_occupation, fill=age),  alpha=0.1 ) + 
  geom_point(aes(color=age)) +
  scale_color_manual(name="Age", values=c("#d7191c", "#fdae61","#31A354", "#2b83ba"), limits=c("Age 3", "Age 5", "Age 11", "Age 14")) +
  scale_fill_manual(name="Age", values=c("#d7191c", "#fdae61","#31A354", "#2b83ba"), limits=c("Age 3", "Age 5", "Age 11", "Age 14"))+
  theme_classic()+
  theme(axis.text = element_text(size=9, family = "Times",colour = "black"), axis.title = element_text(size=12, family = "Times"))+
  theme(legend.text = element_text(size=20, family="Times"), legend.title = element_text(size=20, family="Times"))


print(occupation_indicator)


#IMD
#IMD plot

#standardised plot 
imd_plot <- data.frame(
  imd_decile = factor(c("Most \n Deprived", "2", "3", "4", "5", 
                        "6", "7", "8", "9", "Least \n Deprived"), 
                      levels=c("Most \n Deprived", "2", "3", "4", "5", 
                               "6", "7", "8", "9", "Least \n Deprived")),
  language = c(0,  0.1132371, 0.1841993, 0.2762036, 0.3001065, 0.3616200, 0.4427029 ,0.4924276, 0.4911546, 0.6047419, 
              0,  0.1041518, 0.2295768, 0.2822575, 0.3305889, 0.4086185, 0.4683842, 0.5686366, 0.5688710 ,0.6882437, 
              0, 0.1991699, 0.2512543, 0.3252985, 0.3297667, 0.3779710, 0.4986561 ,0.4954096, 0.5081629, 0.6348611, 
              0,  0.1133297, 0.1765709, 0.2268711, 0.2587086, 0.2895145, 0.3678400, 0.4709770, 0.4676964, 0.5667327 ),
  lower_ci_imd = c(0,  0.05398134, 0.12367851, 0.21416056, 0.23703196, 0.30054232, 0.37962261, 0.42975293, 0.42882871 ,0.54383569,
                   0,  0.04406578, 0.16841665, 0.22086769, 0.26868183, 0.34739325, 0.40509601, 0.50658266, 0.50660331 ,0.62613007, 
                   0,  0.1266371, 0.1802909, 0.2523942 ,0.2561774 ,0.3055035, 0.4241683, 0.4269556, 0.4378561, 0.5676631, 
                   0,  0.04109172, 0.09342304, 0.15160476, 0.17369421, 0.20319564, 0.28565461, 0.38833025, 0.38901416 ,0.48714593 
                   ),
  upper_ci_imd = c(0,0.1724929, 0.2447202, 0.3382467, 0.3631811 ,0.4226976, 0.5057831, 0.5551023, 0.5534805, 0.6656481,
                   0, 0.1642379, 0.2907370, 0.3436473, 0.3924959, 0.4698438, 0.5316723, 0.6306906 ,0.6311387 ,0.7503574,
                  0, 0.2717028, 0.3222177, 0.3982029, 0.4033560, 0.4504386, 0.5731439, 0.5638637, 0.5784696, 0.7020591, 
                 0,  0.1855677, 0.2597188, 0.3021375, 0.3437229, 0.3758334, 0.4500253, 0.5536237, 0.5463785 ,0.6463195),
  age = factor(c("Age 3", "Age 3", "Age 3", "Age 3", "Age 3", "Age 3", "Age 3", "Age 3", "Age 3", "Age 3",
                 "Age 5", "Age 5", "Age 5", "Age 5",  "Age 5", "Age 5", "Age 5", "Age 5", "Age 5", "Age 5",
                 "Age 11", "Age 11", "Age 11", "Age 11", "Age 11", "Age 11", "Age 11", "Age 11", "Age 11", "Age 11", 
                 "Age 14", "Age 14", "Age 14", "Age 14", "Age 14", "Age 14", "Age 14", "Age 14", "Age 14", "Age 14"
                 )
  ))


imd_indicator <- ggplot(data=imd_plot, aes(x=imd_decile, y=language, group=age)) +
  geom_line(aes(color=age)) +
  geom_ribbon(data=imd_plot,aes(ymin=lower_ci_imd,ymax=upper_ci_imd,fill=age),  alpha=0.1) + 
  geom_point(aes(color=age)) +
  scale_color_manual(name="Age", values=c("#d7191c", "#fdae61","#31A354", "#2b83ba"), limits=c("Age 3", "Age 5", "Age 11", "Age 14")) +
  scale_fill_manual(name="Age", values=c("#d7191c", "#fdae61","#31A354", "#2b83ba"), limits=c("Age 3", "Age 5", "Age 11", "Age 14"))+
  theme_classic()+
  theme(axis.text = element_text(size=9, family = "Times",colour = "black"), axis.title = element_text(size=12, family = "Times"))+
  theme(legend.text = element_text(size=20, family="Times"), legend.title = element_text(size=20, family="Times"))


print(imd_indicator)

#ggarrange(nvq_indicator, income_indicator, wealth_indicator, occupation_indicator, imd_indicator,
         # labels = c("NVQ", "Income", "Wealth", "Occupational Status", "IMD"), 
         # label.x=0, label.y=0,
          #font.label = list(size = 10, color = "black", face="plain" ),
          #ncol =2, nrow=3,  align = c( "hv"),
          #common.legend = TRUE, legend = "right") 

#composite
composite_plot <- data.frame(
  composite = factor(c("Lowest \n Quintile", "2", "3", "4", "Highest \n Quintile"), levels=c("Lowest \n Quintile", "2", "3", "4", "Highest \n Quintile")),
  language = c(0,  0.1683939 ,0.4170481, 0.6412497, 0.7688166,
               0,0.1926735 ,0.4377638, 0.6464484, 0.8989176 ,
               0, 0.1680478, 0.3596994, 0.5351457, 0.7842432,
               0, 0.1180322, 0.3095027, 0.4748474, 0.7891679
               
               
  ),
  lower_ci_composite = c(0, 0.1151101 ,0.3659581, 0.5904861, 0.7201624,
                         0, 0.1327952 ,0.3873621 ,0.5918534, 0.8512898,
                         0,   0.1053096, 0.3017302, 0.4743512, 0.7262912,
                         0, 0.05148587 ,0.24629767, 0.41043346, 0.72768231
  ),
  upper_ci_composite = c(0, 0.2216776, 0.4681381, 0.6920133, 0.8174709,
                         0, 0.2525518, 0.4881655, 0.7010435, 0.9465454,
                         0, 0.2307860, 0.4176685, 0.5959401, 0.8421952,
                         0,0.1845786, 0.3727078, 0.5392614, 0.8506534
                         
  ),
  age = factor(c("Age 3", "Age 3", "Age 3", "Age 3", "Age 3", 
                 "Age 5", "Age 5", "Age 5", "Age 5", "Age 5",
                 "Age 11", "Age 11", "Age 11", "Age 11", "Age 11",
                 "Age 14", "Age 14", "Age 14", "Age 14", "Age 14"
  )
  ))


composite_indicator <- ggplot(data=composite_plot, aes(x=composite, y=language, group=age)) +
  geom_line(aes(color=age)) +
  geom_ribbon(data=composite_plot,aes(ymin=lower_ci_composite,ymax=upper_ci_composite, fill=age), alpha=0.1) + 
  geom_point(aes(color=age)) +
  scale_color_manual(name="Age", values=c("#d7191c", "#fdae61","#31A354", "#2b83ba"), limits=c("Age 3", "Age 5", "Age 11", "Age 14"))+
  scale_fill_manual(name="Age", values=c("#d7191c", "#fdae61","#31A354", "#2b83ba"), limits=c("Age 3", "Age 5", "Age 11", "Age 14"))+
  theme_classic()+
  theme(axis.text = element_text(size=9, family = "Times",colour = "black"), axis.title = element_text(size=14, family = "Times"))+
  theme(legend.text = element_text(size=20, family="Times"), legend.title = element_text(size=20, family="Times"))




nvq_indicator1<- nvq_indicator+ labs( x = "Highest Household Parent Educational Attainment \n (NVQ level)", y = "Language Ability \n (standardised)") + ylim(-0.055, 1.05)
income_indicator1<-income_indicator+ labs( x = "Income \n(quintiles)", y = "Language Ability \n (standardised)") + ylim(-0.055,1.05)
wealth_indicator1<-wealth_indicator+ labs( x = "Wealth \n(quintiles)", y = "Language Ability\n (standardised)") + ylim(-0.055, 1.05)
occupation_indicator1<-occupation_indicator+ labs( x = "Highest Household Occupational Status", y = "Language Ability\n (standardised)") + ylim(-0.055,1.05)
imd_indicator1<-imd_indicator+ labs( x = "Relative Neighbourhood Deprivation\n (deciles)", y = "Language Ability\n (standardised)") + ylim(-0.055, 1.05)
composite_indicator1<-composite_indicator+ labs( x = "Composite SES factor\n (quintiles)", y = "Language Ability\n (standardised)") + ylim(-0.055, 1.05)

plot=ggarrange(nvq_indicator1, income_indicator1, wealth_indicator1, occupation_indicator1, imd_indicator1,composite_indicator1,
          #labels = c("NVQ", "Income", "Wealth", "Occupational Status", "IMD"), 
          #label.x=0, label.y=0,
          #font.label = list(size = 10, color = "black", face="plain" ),
          ncol =2, nrow=3,  align = c( "hv"),
          common.legend = TRUE, legend = "right") 
p = plot + ggtitle("Figure 2: Relationships between SES indicators and language ability at ages 3, 5, 11 and 14")
p1= p+ theme(plot.title = element_text(face="italic", size=20, family ="Times",hjust = 0.05))

#annotate_figure(combined_plot, top = text_grob("Early Childhood Language", face="bold", size=12))

ggsave("mcs-results.png", p1, width=12.8, height=14.3, units= "in" ,dpi=300)


print(nvq_indicator)
print(income_indicator)
print(wealth_indicator)
print(occupation_indicator)
print(imd_indicator)









