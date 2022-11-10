#age 14 ses measures sensitivity plot
#age 14 language and age 3 SEC measures plotted with
#age 14 language and age 14 ses measures
#for each indicator 
library(ggplot2)
library(tidyverse)
library(ggpubr)
#parent education ####
NVQ_plot <- data.frame(
  parent_NVQ = factor(c("NVQ1" ,"NVQ2", "NVQ3", "NVQ4", "NVQ5"), 
                      levels=c("NVQ1","NVQ2", "NVQ3", "NVQ4", "NVQ5")),
  language = c(0, 0.15, 0.26, 0.55, 0.93, 
               0, 0.18, 0.29, 0.51, 0.68),
  lower_ci_nvq = c(0,0.06 , 0.17, 0.47 , 0.82, 
                   0, 0.10, 0.20, 0.43, 0.59),
  upper_ci_nvq = c(0, 0.24, 0.35, 0.64, 1.03,
                   0,0.27, 0.38, 0.59, 0.77),
  ses_age = factor(c("Age 3","Age 3", "Age 3", "Age 3", "Age 3", 
                 "Age 14", "Age 14", "Age 14", "Age 14", "Age 14"))
  )


nvq_indicator <- ggplot(data=NVQ_plot, aes(x=parent_NVQ, y=language, group=ses_age)) +
  geom_line(aes(color=ses_age)) +
  #geom_ribbon(data=NVQ_plot,aes(ymin=lower_ci_nvq,ymax=upper_ci_nvq, fill=age), alpha=-0.1) + 
  geom_point(aes(color=ses_age)) +
  scale_color_manual(name="Age of SEC Measure", values=c( "#B8E186",  "#F1B6DA"), 
                     limits=c("Age 3",  "Age 14"))+
  scale_fill_manual(name="Age of SEC Measure", values=c( "#B8E186",  "#F1B6DA"), 
                    limits=c("Age 3",  "Age 14"))+
  
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

#income ####

income_plot <- data.frame(
  Income_Quintiles = factor(c("Lowest \n Quintile", "2", "3", "4", "Highest \n Quintile"), 
                            levels=c("Lowest \n Quintile", "2", "3", "4", "Highest \n Quintile")),
  language = c(0, 0.15, 0.30, 0.47, 0.65,

               0, 0.15, 0.24, 0.42, 0.63),
  lower_ci_income = c(0,  0.09, 0.24, 0.40, 0.57,
                      0, 0.09, 0.18, 0.36, 0.57
                    ),
  upper_ci_income = c(0, 0.21, 0.36, 0.53, 0.72,
                      0, 0.21, 0.30, 0.48, 0.70),
  age = factor(c("Age 3", "Age 3", "Age 3", "Age 3", "Age 3", 
  
                 "Age 14", "Age 14", "Age 14", "Age 14", "Age 14"
  )
  ))


income_indicator <- ggplot(data=income_plot, aes(x=Income_Quintiles, y=language, group=age)) +
  geom_line(aes(color=age)) +
  #geom_ribbon(data=income_plot,aes(ymin=lower_ci_income,ymax=upper_ci_income, fill=age), alpha=0.1) + 
  geom_point(aes(color=age)) +
  scale_color_manual(name="Age of SEC Measure", values=c("#B8E186",  "#F1B6DA"), limits=c("Age 3", "Age 14"))+
  scale_fill_manual(name="Age of SEC Measure", values=c("#B8E186",  "#F1B6DA"), limits=c("Age 3",  "Age 14"))+
  theme_classic()+
  theme(axis.text = element_text(size=9, family = "Times",colour = "black"), axis.title = element_text(size=12, family = "Times"))+
  theme(legend.text = element_text(size=20, family="Times"), legend.title = element_text(size=20, family="Times"))





#wealth ####
wealth_plot <- data.frame(
  wealth = factor(c("Lowest \n Quintile", "2", "3", "4", "Highest \n Quintile"), 
                  levels=c("Lowest \n Quintile", "2", "3", "4", "Highest \n Quintile")),
  language = c(0, -0.01, 0.16, 0.25, 0.48,
               
               0,  0.15, 0.26, 0.39, 0.60
               
               
  ),
  lower_ci_wealth = c(0, -0.08, 0.10, 0.18, 0.41,
                      
                      0, 0.09, 0.19 , 0.33, 0.54
                      
  ),
  upper_ci_wealth = c(0, 0.06, 0.22, 0.32, 0.55, 
                      
                      0, 0.21, 0.26, 0.39, 0.66
                      
  ),
  age = factor(c("Age 3", "Age 3", "Age 3", "Age 3", "Age 3", 
                
                 "Age 14", "Age 14", "Age 14", "Age 14", "Age 14"
  )
  ))


wealth_indicator <- ggplot(data=wealth_plot, aes(x=wealth, y=language, group=age)) +
  geom_line(aes(color=age)) +
  #geom_ribbon(data=wealth_plot,aes(ymin=lower_ci_wealth,ymax=upper_ci_wealth, fill=age), alpha=0.1) + 
  geom_point(aes(color=age)) +
  scale_color_manual(name="Age of SEC Measure", values=c("#B8E186",  "#F1B6DA"), limits=c("Age 3", "Age 14"))+
  scale_fill_manual(name="Age of SEC Measure", values=c("#B8E186",  "#F1B6DA"), limits=c("Age 3", "Age 14"))+
  theme_classic()+
  theme(axis.text = element_text(size=9, family = "Times",colour = "black"), axis.title = element_text(size=12, family = "Times"))+
  theme(legend.text = element_text(size=20, family="Times"), legend.title = element_text(size=20, family="Times"))


#occupation ####
occupation_plot <- data.frame(
  occupational_status = factor(c("Routine", "Intermediate", "Higher managerial"), 
                               levels=c("Routine", "Intermediate", "Higher managerial")),
  language = c(0,  0.12, 0.44,
              
               0, 0.18, 0.41),
  lower_ci_occupation = c( 0, 0.06, 0.39 , 
                           
                           0, 0.13, 0.36),
  upper_ci_occupation = c(0, 0.17, 0.49,
                          
                          0, 0.24, 0.47
  ),
  age = factor(c("Age 3", "Age 3", "Age 3", 
                 "Age 14", "Age 14", "Age 14" 
  )
  ))


occupation_indicator <- ggplot(data=occupation_plot, aes(x=occupational_status, y=language, group=age)) +
  geom_line(aes(color=age)) +
  #geom_ribbon(data=occupation_plot,aes(ymin=lower_ci_occupation,ymax=upper_ci_occupation, fill=age),  alpha=0.1 ) + 
  geom_point(aes(color=age)) +
  scale_color_manual(name="Age of SEC Measure", values=c("#B8E186",  "#F1B6DA"), limits=c("Age 3",  "Age 14")) +
  scale_fill_manual(name="Age of SEC Measure", values=c("#B8E186",  "#F1B6DA"), limits=c("Age 3", "Age 14"))+
  theme_classic()+
  theme(axis.text = element_text(size=9, family = "Times",colour = "black"), axis.title = element_text(size=12, family = "Times"))+
  theme(legend.text = element_text(size=20, family="Times"), legend.title = element_text(size=20, family="Times"))


print(occupation_indicator)


#imd ####
imd_plot <- data.frame(
  imd_decile = factor(c("Most \n Deprived", "2", "3", "4", "5", 
                        "6", "7", "8", "9", "Least \n Deprived"), 
                      levels=c("Most \n Deprived", "2", "3", "4", "5", 
                               "6", "7", "8", "9", "Least \n Deprived")),
  language = c(0, 0.12, 0.17, 0.22, 0.25, 0.28, 0.36, 0.46, 0.45, 0.55, 
               0, 0.10, 0.14, 0.23, 0.29, 0.36, 0.45, 0.43, 0.50, 0.66),
  lower_ci_imd = c(0, 0.04, 0.09, 0.14, 0.17, 0.20, 0.28, 0.38, 0.37, 0.48,
                   0, 0.02, 0.06, 0.16, 0.21, 0.28, 0.37, 0.35, 0.42, 0.58 
  ),
  upper_ci_imd = c(0, 0.19, 0.24, 0.30, 0.32, 0.36, 0.44, 0.54, 0.53, 0.63,
                   0, 0.17, 0.22, 0.31, 0.36, 0.44, 0.53, 0.51, 0.58, 0.74),
  age = factor(c("Age 3", "Age 3", "Age 3", "Age 3", "Age 3", "Age 3", "Age 3", "Age 3", "Age 3", "Age 3",
                 "Age 14", "Age 14", "Age 14", "Age 14", "Age 14", "Age 14", "Age 14", "Age 14", "Age 14", "Age 14"
  )
  ))





imd_indicator <- ggplot(data=imd_plot, aes(x=imd_decile, y=language, group=age)) +
  geom_line(aes(color=age)) +
  #geom_ribbon(data=imd_plot,aes(ymin=lower_ci_imd,ymax=upper_ci_imd,fill=age),  alpha=0.1) + 
  geom_point(aes(color=age)) +
  scale_color_manual(name="Age of SEC Measure", values=c("#B8E186",  "#F1B6DA"), 
                     limits=c("Age 3", "Age 14")) +
  scale_fill_manual(name="Age of SEC Measure", values=c("#B8E186",  "#F1B6DA"), 
                    limits=c("Age 3",  "Age 14"))+
  theme_classic()+
  theme(axis.text = element_text(size=9, family = "Times",colour = "black"), axis.title = element_text(size=12, family = "Times"))+
  theme(legend.text = element_text(size=20, family="Times"), legend.title = element_text(size=20, family="Times"))


print(imd_indicator)






#composite

composite_plot <- data.frame(
  composite = factor(c("Lowest \n Quintile", "2", "3", "4", "Highest \n Quintile"), levels=c("Lowest \n Quintile", "2", "3", "4", "Highest \n Quintile")),
  language = c(0,  0.11, 0.31, 0.48, 0.78,
               0, 0.19, 0.34, 0.52, 0.78
       
  ),
  lower_ci_composite = c(0, 0.04, 0.25, 0.41, 0.73,
                         0, 0.13, 0.28, 0.45, 0.72
  ),
  upper_ci_composite = c(0, 0.17, 0.36, 0.54, 0.84,
                         0, 0.26, 0.40, 0.58, 0.85
                         
  ),
  age = factor(c("Age 3", "Age 3", "Age 3", "Age 3", "Age 3", 
                 
                 "Age 14", "Age 14", "Age 14", "Age 14", "Age 14"
  )
  ))


composite_indicator <- ggplot(data=composite_plot, aes(x=composite, y=language, group=age)) +
  geom_line(aes(color=age)) +
  #geom_ribbon(data=composite_plot,aes(ymin=lower_ci_composite,ymax=upper_ci_composite, fill=age), alpha=0.1) + 
  geom_point(aes(color=age)) +
  scale_color_manual(name="Age of SEC Measure", values=c("#B8E186",  "#F1B6DA"), limits=c("Age 3",  "Age 14"))+
  scale_fill_manual(name="Age of SEC Measure", values=c("#B8E186",  "#F1B6DA"), limits=c("Age 3",  "Age 14"))+
  theme_classic()+
  theme(axis.text = element_text(size=9, family = "Times",colour = "black"), axis.title = element_text(size=14, family = "Times"))+
  theme(legend.text = element_text(size=20, family="Times"), legend.title = element_text(size=20, family="Times"))


#combine
nvq_indicator1<- nvq_indicator+ labs( x = "Highest Household Parent Educational Attainment \n (NVQ level)", y = "Language Ability \n (standardised)") + ylim(-0.055, 1.05)
income_indicator1<-income_indicator+ labs( x = "Income \n(quintiles)", y = "Language Ability \n (standardised)") + ylim(-0.055,1.05)
wealth_indicator1<-wealth_indicator+ labs( x = "Wealth \n(quintiles)", y = "Language Ability\n (standardised)") + ylim(-0.055, 1.05)
occupation_indicator1<-occupation_indicator+ labs( x = "Highest Household Occupational Status", y = "Language Ability\n (standardised)") + ylim(-0.055,1.05)
imd_indicator1<-imd_indicator+ labs( x = "Relative Neighbourhood Deprivation\n (deciles)", y = "Language Ability\n (standardised)") + ylim(-0.055, 1.05)
composite_indicator1<-composite_indicator+ labs( x = "Composite SEC factor\n (quintiles)", y = "Language Ability\n (standardised)") + ylim(-0.055, 1.05)

plot=ggarrange(nvq_indicator1, income_indicator1, wealth_indicator1, occupation_indicator1, imd_indicator1,composite_indicator1,
               #labels = c("NVQ", "Income", "Wealth", "Occupational Status", "IMD"), 
               #label.x=0, label.y=0,
               #font.label = list(size = 10, color = "black", face="plain" ),
               ncol =2, nrow=3,  align = c( "hv"),
               common.legend = TRUE, legend = "right") 
p = plot + ggtitle("Figure 7: Relationships between SEC indicators (Ages 3 & 14) and Vocabulary (Age 14)")
p1= p+ theme(plot.title = element_text(face="italic", size=20, family ="Times",hjust = 0.05))

ggsave("mcs-age14_sensitivity.png", p1, width=12.8, height=14.3, units= "in" ,dpi=300)



#r squared comparison ####

r_squared_plot1 <- data.frame(
  age = factor(c("Age 3",  "Age 14"), 
               levels=c("Age 3",  "Age 14")),
  r_squared = c(7.1,  5.5,
               4.3, 4.4,
               3.4, 4.6,
                5.3, 3.1,
                2.6, 3.9,
                7.4, 7.7
              
  ),
  indicator = factor(c("Education", "Education", 
                       "Income", "Income",
                       "Wealth", "Wealth", 
                       "Occupational Status", "Occupational Status", 
                       "Neighbourhood Deprivation",  "Neighbourhood Deprivation", 
                       "Composite Factor", "Composite Factor"
  )
  ))


#plot 
r2_plot1 <- r_squared_plot1 %>% 
  ggplot() +
  geom_point(mapping=aes(x=age, y=r_squared,  group= indicator, color=indicator, fill=indicator), 
             size=5, shape=20, position = position_dodge(width=0.4)) +
  scale_color_manual(name="Indicator", values=c( "#66C2A5", "#FC8D62", "#8DA0CB", "#E78AC3", "#A6D854", "black"), limits=c("Education", "Income", "Wealth", "Occupational Status", "Neighbourhood Deprivation", "Composite Factor"))+
  scale_fill_manual(name="Indicator", values=c( "#66C2A5", "#FC8D62", "#8DA0CB", "#E78AC3", "#A6D854", "black" ), limits=c("Education", "Income", "Wealth", "Occupational Status", "Neighbourhood Deprivation", "Composite Factor"))+
  theme_classic2()+
  xlab(label="Age of SEC Measure") +
  ylab(label="Partial R squared (%)") +
  theme(axis.text = element_text(size=15, family = "Times", colour = "black"), 
        axis.title = element_text(size=15, family = "Times"))+
  theme(legend.text = element_text(size=15, family="Times"), 
        legend.title = element_text(size=15, family="Times"))+
  ylim(0,12)+ 
  geom_vline(xintercept = 1.5:3.5, colour="#E0E0E0") +
  theme(legend.position = "top") +
  
  ggtitle("Figure 9: Partial" ~ R^2 ~ "values for SEC indicators (Age 3 vs Age 14) for Age 14 Vocabulary in ~2001 born cohort ") +
  theme(plot.title = element_text(face="italic", size=18, family ="Times",hjust = 0.5)) +
  
  geom_text(aes(x=age, y=r_squared, label=round(r_squared,1), color=factor(indicator), hjust=-0.7),
            family = "Times New Roman",
            position = position_dodge(width=0.4))
ggsave("age14_r2_sensitivity.png",r2_plot1, width=12, height=14, units= "in" ,dpi=300)

#age 14 SEC with main indicator 

#NVQ
NVQ_plot <- data.frame(
  parent_NVQ = factor(c("NVQ1" ,"NVQ2", "NVQ3", "NVQ4", "NVQ5"), levels=c("NVQ1","NVQ2", "NVQ3", "NVQ4", "NVQ5")),
  language = c(0, 0.20, 0.34, 0.58, 0.74, 
               0, 0.24, 0.36, 0.66, 0.90, 
               0, 0.18, 0.32, 0.55, 0.80, 
               0, 0.15, 0.26, 0.55, 0.93, 
               0, 0.18, 0.29, 0.51, 0.68),
  lower_ci_nvq = c(0,0.13, 0.28, 0.52, 0.66, 
                   0, 0.17, 0.29, 0.60, 0.82,
                   0, 0.10, 0.24, 0.48, 0.71,
                   0, 0.06, 0.17, 0.47, 0.82,
                   0, 0.10, 0.20, 0.43, 0.59
  ),
  upper_ci_nvq = c(0, 0.26, 0.41, 0.65, 0.82, 
                   0, 0.30, 0.44, 0.73, 0.97, 
                   0, 0.26, 0.41, 0.63, 0.89,
                   0, 0.24, 0.35, 0.64, 1.03,
                   0,0.27, 0.38, 0.59, 0.77
  ),
  age = factor(c("Age 3","Age 3", "Age 3", "Age 3", "Age 3", 
                 "Age 5", "Age 5", "Age 5", "Age 5", "Age 5", 
                 "Age 11",  "Age 11", "Age 11", "Age 11", "Age 11", 
                 "Age 14", "Age 14", "Age 14", "Age 14", "Age 14",
                 "Age 14 SEC", "Age 14 SEC", "Age 14 SEC", "Age 14 SEC", "Age 14 SEC"
  )
  ))

nvq_indicator <- ggplot(data=NVQ_plot, aes(x=parent_NVQ, y=language, group=age)) +
  geom_line(aes(color=age)) +
  #geom_ribbon(data=NVQ_plot,aes(ymin=lower_ci_nvq,ymax=upper_ci_nvq, fill=age), alpha=-0.1) + 
  geom_point(aes(color=age)) +
  scale_color_manual(name="Age", values=c("#d7191c", "#fdae61","#abdda4", "#2b83ba", "black"), 
                     limits=c("Age 3", "Age 5", "Age 11", "Age 14", "Age 14 SEC"))+
  scale_fill_manual(name="Age", values=c("#d7191c", "#fdae61","#abdda4", "#2b83ba", "black"), 
                    limits=c("Age 3", "Age 5", "Age 11", "Age 14", "Age 14 SEC"))+
  
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



#Income

income_plot <- data.frame(
  Income_Quintiles = factor(c("Lowest \n Quintile", "2", "3", "4", "Highest \n Quintile"),
                            levels=c("Lowest \n Quintile", "2", "3", "4", "Highest \n Quintile")),
  language = c(0, 0.17, 0.43, 0.55, 0.64,
               0, 0.16, 0.43, 0.56, 0.74, 
               0, 0.13, 0.32, 0.46, 0.66,
               0, 0.15, 0.30, 0.47, 0.65, 
               0, 0.15, 0.24, 0.42, 0.63 
  ),
  lower_ci_income = c(0, 0.13, 0.39, 0.50, 0.59, 
                      0, 0.11, 0.38, 0.51, 0.68, 
                      0, 0.07, 0.26, 0.39, 0.60, 
                      0, 0.09, 0.24, 0.40, 0.57, 
                      0, 0.09, 0.18, 0.36, 0.57),
  upper_ci_income = c(0, 0.21, 0.48, 0.59, 0.70, 
                      0, 0.21, 0.48, 0.60, 0.79, 
                      0, 0.18, 0.36, 0.49, 0.72,
                      0, 0.21, 0.36, 0.53, 0.72,
                      0, 0.21, 0.30, 0.48, 0.70),
  age = factor(c("Age 3", "Age 3", "Age 3", "Age 3", "Age 3", 
                 "Age 5", "Age 5", "Age 5", "Age 5", "Age 5",
                 "Age 11", "Age 11", "Age 11", "Age 11", "Age 11",
                 "Age 14", "Age 14", "Age 14", "Age 14", "Age 14", 
                 "Age 14 SEC", "Age 14 SEC", "Age 14 SEC", "Age 14 SEC", "Age 14 SEC"
  )
  ))

income_indicator <- ggplot(data=income_plot, aes(x=Income_Quintiles, y=language, group=age)) +
  geom_line(aes(color=age)) +
  #geom_ribbon(data=income_plot,aes(ymin=lower_ci_income,ymax=upper_ci_income, fill=age), alpha=0.1) + 
  geom_point(aes(color=age)) +
  scale_color_manual(name="Age", values=c("#d7191c", "#fdae61","#abdda4", "#2b83ba", "black"), 
                     limits=c("Age 3", "Age 5", "Age 11", "Age 14", "Age 14 SEC"))+
  scale_fill_manual(name="Age", values=c("#d7191c", "#fdae61","#abdda4", "#2b83ba", "black"),
                    limits=c("Age 3", "Age 5", "Age 11", "Age 14", "Age 14 SEC"))+
  theme_classic()+
  theme(axis.text = element_text(size=9, family = "Times",colour = "black"), axis.title = element_text(size=12, family = "Times"))+
  theme(legend.text = element_text(size=20, family="Times"), legend.title = element_text(size=20, family="Times"))



print(income_indicator)


#WEALTH

wealth_plot <- data.frame(
  wealth = factor(c("Lowest \n Quintile", "2", "3", "4", "Highest \n Quintile"), 
                  levels=c("Lowest \n Quintile", "2", "3", "4", "Highest \n Quintile")),
  language = c( 0, 0.04, 0.26, 0.35, 0.48,
                0,  0.02,  0.24,  0.35,  0.54,
                0,  0.05,  0.26,  0.35,  0.52,
                0, -0.01,  0.16,  0.25,  0.48,
                0,  0.15, 0.26, 0.39, 0.60
               
               
  ),
  lower_ci_wealth = c(0, -0.03,  0.21,  0.31,  0.43,
                      0, -0.05,  0.19,  0.30,  0.49,
                      0, -0.02,  0.21,  0.29,  0.47,
                      0, -0.08,  0.10,  0.18,  0.41, 
                      0, 0.09, 0.19 , 0.33, 0.54
                      
  ),
  upper_ci_wealth = c(0, 0.12, 0.32, 0.40, 0.53,
                      0, 0.09, 0.29, 0.40, 0.58,
                      0, 0.13, 0.32, 0.41, 0.58,
                      0, 0.06, 0.22, 0.32, 0.55,
                      0, 0.21, 0.26, 0.39, 0.66
                      
  ),
  age = factor(c("Age 3", "Age 3", "Age 3", "Age 3", "Age 3", 
                 "Age 5", "Age 5", "Age 5", "Age 5", "Age 5",
                 "Age 11", "Age 11", "Age 11", "Age 11", "Age 11",
                 "Age 14", "Age 14", "Age 14", "Age 14", "Age 14", 
                 "Age 14 SEC", "Age 14 SEC", "Age 14 SEC", "Age 14 SEC", "Age 14 SEC"
                 
  )
  ))

wealth_indicator <- ggplot(data=wealth_plot, aes(x=wealth, y=language, group=age)) +
  geom_line(aes(color=age)) +
  #geom_ribbon(data=wealth_plot,aes(ymin=lower_ci_wealth,ymax=upper_ci_wealth, fill=age), alpha=0.1) + 
  geom_point(aes(color=age)) +
  scale_color_manual(name="Age", values=c("#d7191c", "#fdae61","#abdda4", "#2b83ba", "black"),
                     limits=c("Age 3", "Age 5", "Age 11", "Age 14", "Age 14 SEC"))+
  scale_fill_manual(name="Age", values=c("#d7191c", "#fdae61","#abdda4", "#2b83ba", "black"), 
                    limits=c("Age 3", "Age 5", "Age 11", "Age 14", "Age 14 SEC"))+
  theme_classic()+
  theme(axis.text = element_text(size=9, family = "Times",colour = "black"), axis.title = element_text(size=12, family = "Times"))+
  theme(legend.text = element_text(size=20, family="Times"), legend.title = element_text(size=20, family="Times"))

#OCCUPATION
occupation_plot <- data.frame(
  occupational_status = factor(c("Routine", "Intermediate", "Higher managerial"), levels=c("Routine", "Intermediate", "Higher managerial")),
  language = c(0, 0.22, 0.39,
               0,  0.20 ,0.47  , 
               0,   0.18, 0.42 ,  
               0, 0.12, 0.44, 
               0, 0.18, 0.41),
  lower_ci_occupation = c( 0, 0.17 , 0.36,
                           0,  0.15, 0.44, 
                           0,  0.13, 0.38, 
                           0,  0.06, 0.39, 
                           0, 0.13, 0.36),
  upper_ci_occupation = c(0, 0.26, 0.43,
                          0, 0.24, 0.51,
                          0, 0.23, 0.46,
                          0, 0.17, 0.49,
                          0, 0.24, 0.47
                          
  ),
  age = factor(c("Age 3", "Age 3", "Age 3", 
                 "Age 5", "Age 5", "Age 5", 
                 "Age 11", "Age 11", "Age 11",  
                 "Age 14", "Age 14", "Age 14" , 
                 "Age 14 SEC", "Age 14 SEC", "Age 14 SEC"
  )
  ))


occupation_indicator <- ggplot(data=occupation_plot, aes(x=occupational_status, y=language, group=age)) +
  geom_line(aes(color=age)) +
  #geom_ribbon(data=occupation_plot,aes(ymin=lower_ci_occupation,ymax=upper_ci_occupation, fill=age),  alpha=0.1 ) + 
  geom_point(aes(color=age)) +
  scale_color_manual(name="Age", values=c("#d7191c", "#fdae61","#abdda4", "#2b83ba", "black"), 
                     limits=c("Age 3", "Age 5", "Age 11", "Age 14", "Age 14 SEC")) +
  scale_fill_manual(name="Age", values=c("#d7191c", "#fdae61","#abdda4", "#2b83ba", "black"), 
                    limits=c("Age 3", "Age 5", "Age 11", "Age 14", "Age 14 SEC"))+
  theme_classic()+
  theme(axis.text = element_text(size=9, family = "Times",colour = "black"), axis.title = element_text(size=12, family = "Times"))+
  theme(legend.text = element_text(size=20, family="Times"), legend.title = element_text(size=20, family="Times"))


#IMD 



imd_plot <- data.frame(
  imd_decile = factor(c("Most \n Deprived", "2", "3", "4", "5", 
                        "6", "7", "8", "9", "Least \n Deprived"), 
                      levels=c("Most \n Deprived", "2", "3", "4", "5", 
                               "6", "7", "8", "9", "Least \n Deprived")),
  language = c(0, 0.11, 0.18, 0.27, 0.29, 0.36, 0.43, 0.49, 0.48, 0.60,
               0, 0.10, 0.23, 0.28, 0.33, 0.41, 0.47, 0.57, 0.56, 0.68,
               0, 0.19, 0.24, 0.31, 0.31, 0.36, 0.49, 0.49, 0.49, 0.62,
               0, 0.12, 0.17, 0.22, 0.25, 0.28, 0.36, 0.46, 0.45, 0.55, 
               0, 0.10, 0.14, 0.23, 0.29, 0.36, 0.45, 0.43, 0.50, 0.66
  ),
  lower_ci_imd = c(0, 0.05, 0.12, 0.21, 0.23, 0.30, 0.37, 0.42, 0.42, 0.54,
                   0, 0.04, 0.16, 0.22, 0.26, 0.34, 0.40, 0.50, 0.50, 0.62,
                   0, 0.12, 0.16, 0.24, 0.24, 0.29, 0.41, 0.41, 0.41, 0.54,
                   0, 0.04, 0.09, 0.14, 0.17, 0.20, 0.28, 0.38, 0.37, 0.48, 
                   0, 0.02, 0.06, 0.16, 0.21, 0.28, 0.37, 0.35, 0.42, 0.58 
                   
                   
                   
  ),
  upper_ci_imd = c(0, 0.16, 0.24, 0.34, 0.36, 0.42, 0.50, 0.55, 0.55, 0.66,
                   0, 0.16, 0.29, 0.34, 0.39, 0.47, 0.53, 0.63, 0.63, 0.75,
                   0, 0.26, 0.31, 0.38, 0.39, 0.44, 0.56, 0.56, 0.56, 0.69,
                   0, 0.19, 0.24, 0.30, 0.32, 0.36, 0.44, 0.54, 0.53, 0.63,
                   0, 0.17, 0.22, 0.31, 0.36, 0.44, 0.53, 0.51, 0.58, 0.74
                   
  ),
  age = factor(c("Age 3", "Age 3", "Age 3", "Age 3", "Age 3", "Age 3", "Age 3", "Age 3", "Age 3", "Age 3",
                 "Age 5", "Age 5", "Age 5", "Age 5",  "Age 5", "Age 5", "Age 5", "Age 5", "Age 5", "Age 5",
                 "Age 11", "Age 11", "Age 11", "Age 11", "Age 11", "Age 11", "Age 11", "Age 11", "Age 11", "Age 11", 
                 "Age 14", "Age 14", "Age 14", "Age 14", "Age 14", "Age 14", "Age 14", "Age 14", "Age 14", "Age 14", 
                 "Age 14 SEC", "Age 14 SEC", "Age 14 SEC", "Age 14 SEC", "Age 14 SEC", "Age 14 SEC", "Age 14 SEC", "Age 14 SEC", "Age 14 SEC", "Age 14 SEC"
  )
  ))

imd_indicator <- ggplot(data=imd_plot, aes(x=imd_decile, y=language, group=age)) +
  geom_line(aes(color=age)) +
  #geom_ribbon(data=imd_plot,aes(ymin=lower_ci_imd,ymax=upper_ci_imd,fill=age),  alpha=0.1) + 
  geom_point(aes(color=age)) +
  scale_color_manual(name="Age", values=c("#d7191c", "#fdae61","#abdda4", "#2b83ba", "black "), 
                     limits=c("Age 3", "Age 5", "Age 11", "Age 14", "Age 14 SEC")) +
  scale_fill_manual(name="Age", values=c("#d7191c", "#fdae61","#abdda4", "#2b83ba", "black "), 
                    limits=c("Age 3", "Age 5", "Age 11", "Age 14", "Age 14 SEC"))+
  theme_classic()+
  theme(axis.text = element_text(size=9, family = "Times",colour = "black"), axis.title = element_text(size=12, family = "Times"))+
  theme(legend.text = element_text(size=20, family="Times"), legend.title = element_text(size=20, family="Times"))

#composite
composite_plot <- data.frame(
  composite = factor(c("Lowest \n Quintile", "2", "3", "4", "Highest \n Quintile"), 
                     levels=c("Lowest \n Quintile", "2", "3", "4", "Highest \n Quintile")),
  language = c( 0, 0.20, 0.44, 0.65, 0.80,
                0, 0.17, 0.43, 0.64, 0.88,
                0, 0.18, 0.37, 0.54, 0.78,
                0, 0.14, 0.28, 0.47,0.77, 
                0, 0.19, 0.34, 0.52, 0.78
               
               
  ),
  lower_ci_composite = c(0, 0.15, 0.39, 0.60, 0.75,
                         0, 0.12, 0.38, 0.59, 0.83,
                         0, 0.12, 0.30, 0.49, 0.73,
                         0, 0.07, 0.22, 0.41, 0.71, 
                         0, 0.13, 0.28, 0.45, 0.72
  ),
  upper_ci_composite = c(0, 0.26, 0.49, 0.70, 0.84,
                         0, 0.22, 0.48, 0.68, 0.93,
                         0, 0.24, 0.43, 0.59, 0.83,
                         0, 0.20, 0.34, 0.54,0.83,
                         0, 0.26, 0.40, 0.58, 0.85
                         
  ),
  age = factor(c("Age 3", "Age 3", "Age 3", "Age 3", "Age 3", 
                 "Age 5", "Age 5", "Age 5", "Age 5", "Age 5",
                 "Age 11", "Age 11", "Age 11", "Age 11", "Age 11",
                 "Age 14", "Age 14", "Age 14", "Age 14", "Age 14", 
                 "Age 14 SEC", "Age 14 SEC", "Age 14 SEC", "Age 14 SEC", "Age 14 SEC"
  )
  ))


composite_indicator <- ggplot(data=composite_plot, aes(x=composite, y=language, group=age)) +
  geom_line(aes(color=age)) +
  #geom_ribbon(data=composite_plot,aes(ymin=lower_ci_composite,ymax=upper_ci_composite, fill=age), alpha=0.1) + 
  geom_point(aes(color=age)) +
  scale_color_manual(name="Age", values=c("#d7191c", "#fdae61","#abdda4", "#2b83ba", "black"), 
                     limits=c("Age 3", "Age 5", "Age 11", "Age 14", "Age 14 SEC"))+
  scale_fill_manual(name="Age", values=c("#d7191c", "#fdae61","#abdda4", "#2b83ba", "black"),
                    limits=c("Age 3", "Age 5", "Age 11", "Age 14", "Age 14 SEC"))+
  theme_classic()+
  theme(axis.text = element_text(size=9, family = "Times",colour = "black"), axis.title = element_text(size=14, family = "Times"))+
  theme(legend.text = element_text(size=20, family="Times"), legend.title = element_text(size=20, family="Times"))

nvq_indicator1<- nvq_indicator+ labs( x = "Highest Household Parent Educational Attainment \n (NVQ level)", y = "Language Ability \n (standardised)") + ylim(-0.055, 1.05)
income_indicator1<-income_indicator+ labs( x = "Income \n(quintiles)", y = "Language Ability \n (standardised)") + ylim(-0.055,1.05)
wealth_indicator1<-wealth_indicator+ labs( x = "Wealth \n(quintiles)", y = "Language Ability\n (standardised)") + ylim(-0.055, 1.05)
occupation_indicator1<-occupation_indicator+ labs( x = "Highest Household Occupational Status", y = "Language Ability\n (standardised)") + ylim(-0.055,1.05)
imd_indicator1<-imd_indicator+ labs( x = "Relative Neighbourhood Deprivation\n (deciles)", y = "Language Ability\n (standardised)") + ylim(-0.055, 1.05)
composite_indicator1<-composite_indicator+ labs( x = "Composite SEC factor\n (quintiles)", y = "Language Ability\n (standardised)") + ylim(-0.055, 1.05)

plot=ggarrange(nvq_indicator1, income_indicator1, wealth_indicator1, occupation_indicator1, imd_indicator1,composite_indicator1,
               #labels = c("NVQ", "Income", "Wealth", "Occupational Status", "IMD"), 
               #label.x=0, label.y=0,
               #font.label = list(size = 10, color = "black", face="plain" ),
               ncol =2, nrow=3,  align = c( "hv"),
               common.legend = TRUE, legend = "right") 
p = plot + ggtitle("Figure 8: Relationships between SEC indicators and language ability at ages 3, 5, 11 and 14 (Age 14 vocab = SEC at ages 3 & 14)")
p1= p+ theme(plot.title = element_text(face="italic", size=18, family ="Times",hjust = 0.05))


ggsave("age14_sensitivity_plot2.png",p1, width=14, height=16, units= "in" ,dpi=300)


