#cross cohort plots for each age.
library(ggplot2)
library(ggpubr)
#education

#age 5
education_age5 <- data.frame(
  educational_attainment = factor(c("No/Low level (Reference)", "O-levels", "Post-16 ", "University level "), 
                                  levels=c("No/Low level (Reference)", "O-levels", "Post-16 ", "University level ")),
  language = c(0,  0.36, 0.48, 0.64,
               0,   0.31, 0.54, 0.84
  ),
  lower_ci_education = c( 0, 0.31, 0.41, 0.58, 
                          0, 0.27, 0.49 , 0.80
  ),
  upper_ci_education = c(0, 0.40, 0.55, 0.70 ,
                         0,  0.35, 0.58,  0.88
  ),
  cohort = factor(c(
    "1970 BCS", "1970 BCS", "1970 BCS", "1970 BCS",
    "~2001 MCS", "~2001 MCS" ,"~2001 MCS", "~2001 MCS"
    
  )
  ))


education_age5_plot <- ggplot(data=education_age5, aes(x=educational_attainment, y=language, group=cohort)) +
  geom_line(aes(color=cohort)) +
  geom_ribbon(data=education_age5,aes(ymin=lower_ci_education,ymax=upper_ci_education, fill=cohort),  alpha=0.1 ) + 
  geom_point(aes(color=cohort)) +
  scale_color_manual(name="Cohort", values=c( "#B2ABD2", "#FDB863"), limits=c( "1970 BCS", "~2001 MCS")) +
  scale_fill_manual(name="Cohort", values=c( "#B2ABD2", "#FDB863"), limits=c( "1970 BCS", "~2001 MCS"))+
  labs( x = "Highest Household Parent Qualification", y = "Language Ability \n (standardised)", size = 7) + ylim(0, 1) +
  theme_classic()+
  theme(axis.text = element_text(size=9, family = "Times", colour = "black"), axis.title = element_text(size=12, family = "Times"))+
  theme(legend.text = element_text(size=15, family="Times"), legend.title = element_text(size=15, family="Times"))

  #annotate("text", x=1.9, y=.38, label=".36", colour= "#B2ABD2", size=2.5) +
  #annotate("text", x=3, y=.43, label=".48", colour= "#B2ABD2", size=2.5)+
  #annotate("text", x=4.1, y=.67, label=".65", colour= "#B2ABD2", size=2.5)+
  #annotate("text", x=2, y=.26, label=".30", colour= "#FDB863", size=2.5) +
  #annotate("text", x=2.9, y=.55, label=".53", colour= "#FDB863", size=2.5)+
  #annotate("text", x=4.1, y=.85, label=".83", colour= "#FDB863", size=2.5)



print(education_age5_plot)


#cross cohort plots for each age.

#education

#age 10/11
education_age10 <- data.frame(
  educational_attainment = factor(c("No/Low level (Reference)", "O-levels", "Post-16 ", "University level "), 
                                  levels=c("No/Low level (Reference)", "O-levels", "Post-16 ", "University level ")),
  language = c( 0,  0.36,0.54, 0.79 ,
                0,   0.25, 0.49 ,0.75   
  ),
  lower_ci_education = c( 0, 0.31, 0.48, 0.73, 
                          0,0.20  , 0.44  , 0.70
  ),
  upper_ci_education = c(0,  0.40, 0.61, 0.84,  
                         0,   0.30, 0.55  , 0.80
  ),
  cohort = factor(c(
    "1970 BCS", "1970 BCS", "1970 BCS", "1970 BCS",
    "~2001 MCS", "~2001 MCS" ,"~2001 MCS", "~2001 MCS"
    
  )
  ))


education_age10_plot <- ggplot(data=education_age10, aes(x=educational_attainment, y=language, group=cohort)) +
  geom_line(aes(color=cohort)) +
  geom_ribbon(data=education_age10,aes(ymin=lower_ci_education,ymax=upper_ci_education, fill=cohort),  alpha=0.1 ) + 
  geom_point(aes(color=cohort)) +
  scale_color_manual(name="Cohort", values=c(  "#B2ABD2", "#FDB863"), limits=c( "1970 BCS", "~2001 MCS")) +
  scale_fill_manual(name="Cohort", values=c( "#B2ABD2", "#FDB863"), limits=c( "1970 BCS", "~2001 MCS"))+
  labs( x = "Highest Household Parent Qualification", y = "Language Ability \n (standardised)", size = 7) + ylim(0, 1) +
  theme_classic()+
  theme(axis.text = element_text(size=9, family = "Times", colour = "black"), axis.title = element_text(size=12, family = "Times"))+
  theme(legend.text = element_text(size=15, family="Times"), legend.title = element_text(size=15, family="Times"))

  #annotate("text", x=1.9, y=.38, label=".36", colour= "#B2ABD2", size=2.5) +
  #annotate("text", x=2.9, y=.57, label=".55", colour= "#B2ABD2", size=2.5)+
  #annotate("text", x=4.1, y=.81, label=".79", colour= "#B2ABD2", size=2.5)+
  #annotate("text", x=1.9, y=.26, label=".24", colour= "#FDB863", size=2.5) +
  #annotate("text", x=2.9, y=.50, label=".48", colour= "#FDB863", size=2.5)+
  #annotate("text", x=4.1, y=.75, label=".75", colour= "#FDB863", size=2.5)



print(education_age10_plot)



#age 16/14
education_age16 <- data.frame(
  educational_attainment = factor(c("No/Low level (Reference)", "O-levels", "Post-16 ", "University level "), levels=c("No/Low level (Reference)", "O-levels", "Post-16 ", "University level ")),
  language = c( 0, 0.27, 0.43, 0.70,
                0, 0.18 , 0.38 ,  0.84  
  ),
  lower_ci_education = c( 0,  0.20, 0.35, 0.63 ,
                          0, 0.13 , 0.33 ,  0.78 
  ),
  upper_ci_education = c(0, 0.33, 0.51, 0.77,
                         0, 0.24, 0.43, 0.89
  ),
  cohort = factor(c(
    "1970 BCS", "1970 BCS", "1970 BCS", "1970 BCS",
    "~2001 MCS", "~2001 MCS" ,"~2001 MCS", "~2001 MCS"
    
  )
  ))


education_age16_plot <- ggplot(data=education_age16, aes(x=educational_attainment, y=language, group=cohort)) +
  geom_line(aes(color=cohort)) +
  geom_ribbon(data=education_age16,aes(ymin=lower_ci_education,ymax=upper_ci_education, fill=cohort),  alpha=0.1 ) + 
  geom_point(aes(color=cohort)) +
  scale_color_manual(name="Cohort", values=c( "#B2ABD2", "#FDB863"), limits=c( "1970 BCS", "~2001 MCS")) +
  scale_fill_manual(name="Cohort", values=c( "#B2ABD2", "#FDB863"), limits=c( "1970 BCS", "~2001 MCS"))+
  labs( x = "Highest Household Parent Qualification", y = "Language Ability \n (standardised)", size = 7) + ylim(0, 1) +
  theme_classic() +
  theme(axis.text = element_text(size=9, family = "Times", colour = "black"), axis.title = element_text(size=12, family = "Times"))+
  theme(legend.text = element_text(size=15, family="Times"), legend.title = element_text(size=15, family="Times"))

  #annotate("text", x=1.9, y=.28, label=".27", colour= "#B2ABD2", size=2.5) +
  #annotate("text", x=2.9, y=.45, label=".44", colour= "#B2ABD2", size=2.5)+
  #annotate("text", x=4.1, y=.72, label=".71", colour= "#B2ABD2", size=2.5)+
  #annotate("text", x=1.9, y=.19, label=".18", colour= "#FDB863", size=2.5) +
  #annotate("text", x=2.9, y=.39, label=".38", colour= "#FDB863", size=2.5)+
  #annotate("text", x=4.1, y=.85, label=".83", colour= "#FDB863", size=2.5)


education_plots <- ggarrange(education_age5_plot, education_age10_plot, education_age16_plot,
                             labels = c("Early Childhood", "Late Childhood", "Adolescence"), 
                             #label.x=0, label.y=0,
                             font.label = list(size = 13, color = "black", face="italic", family="Times"),
                             hjust= -1,
                             vjust=1.8,
          ncol =1, nrow=3,  align = c( "hv"),
          common.legend = TRUE, legend = "right") 



#occupation plots
#Age 5
occupation_age5 <- data.frame(
  occupational_status = factor(c("Routine (Reference)", "Intermediate", "Higher Managerial"), levels=c("Routine (Reference)", "Intermediate", "Higher Managerial")),
  language = c(0, 0.30, 0.66, 
               0, 0.24,   0.51
               
  ),
  lower_ci_occupation = c(  0, 0.25, 0.61 ,
                            0, 0.20,0.47
                            
  ),
  upper_ci_occupation = c(0, 0.35, 0.71, 
                          0, 0.28, 0.54
                          
                          
  ),
  cohort = factor(c(
    "1970 BCS", "1970 BCS", "1970 BCS", 
    "~2001 MCS", "~2001 MCS" ,"~2001 MCS"
    
  )
  ))

occupation_age5_plot <- ggplot(data=occupation_age5, aes(x=occupational_status, y=language, group=cohort)) +
  geom_line(aes(color=cohort)) +
  geom_ribbon(data=occupation_age5,aes(ymin=lower_ci_occupation,ymax=upper_ci_occupation, fill=cohort),  alpha=0.1 ) + 
  geom_point(aes(color=cohort)) +
  scale_color_manual(name="Cohort", values=c( "#B2ABD2", "#FDB863"), limits=c( "1970 BCS","~2001 MCS")) +
  scale_fill_manual(name="Cohort", values=c( "#B2ABD2", "#FDB863"), limits=c( "1970 BCS", "~2001 MCS"))+
  labs( x = "Highest Household Occupational Status", y = "Language Ability \n (standardised)", size = 7) + ylim(0, 1) +
  theme_classic() +
  theme(axis.text = element_text(size=9, family = "Times", colour = "black"), axis.title = element_text(size=12, family = "Times"))+
  theme(legend.text = element_text(size=15, family="Times"), legend.title = element_text(size=15, family="Times"))

  #annotate("text", x=2, y=.36, label=".32", colour= "#B2ABD2", size=2.5) +
  #annotate("text", x=3.1, y=.75, label=".70", colour= "#B2ABD2", size=2.5)+
  #annotate("text", x=2, y=.20, label=".24", colour= "#FDB863", size=2.5) +
  #annotate("text", x=3.1, y=.49, label=".51", colour= "#FDB863", size=2.5)
  

#age 10/11
occupation_age10 <- data.frame(
  occupational_status = factor(c("Routine (Reference)", "Intermediate", "Higher Managerial"), levels=c("Routine (Reference)", "Intermediate", "Higher Managerial")),
  language = c(0, 0.32 ,0.78,
               0, 0.22,  0.48
                
  ),
  lower_ci_occupation = c(   0,  0.27, 0.72,
                             0, 0.17,  0.44
                            
  ),
  upper_ci_occupation = c(0, 0.37 ,0.84,  
                          0,   0.27, 0.52
                         
                          
  ),
  cohort = factor(c(
    "1970 BCS", "1970 BCS", "1970 BCS", 
    "~2001 MCS", "~2001 MCS" ,"~2001 MCS"
    
  )
  ))

occupation_age10_plot <- ggplot(data=occupation_age10, aes(x=occupational_status, y=language, group=cohort)) +
  geom_line(aes(color=cohort)) +
  geom_ribbon(data=occupation_age10,aes(ymin=lower_ci_occupation,ymax=upper_ci_occupation, fill=cohort),  alpha=0.1 ) + 
  geom_point(aes(color=cohort)) +
  scale_color_manual(name="Cohort", values=c( "#B2ABD2", "#FDB863"), limits=c( "1970 BCS", "~2001 MCS")) +
  scale_fill_manual(name="Cohort", values=c( "#B2ABD2", "#FDB863"), limits=c( "1970 BCS", "~2001 MCS"))+
  labs( x = "Highest Household Occupational Status", y = "Language Ability \n (standardised)", size = 7) + ylim(0, 1) +
  theme_classic()+
  theme(axis.text = element_text(size=9, family = "Times", colour = "black"), axis.title = element_text(size=12, family = "Times"))+
  theme(legend.text = element_text(size=15, family="Times"), legend.title = element_text(size=15, family="Times"))

  #annotate("text", x=2, y=.37, label=".32", colour= "#B2ABD2", size=2.5) +
  #annotate("text", x=3.1, y=.81, label=".79", colour= "#B2ABD2", size=2.5)+
  #annotate("text", x=2, y=.18, label=".22", colour= "#FDB863", size=2.5) +
  #annotate("text", x=3.1, y=.49, label=".48", colour= "#FDB863", size=2.5)


#Age 14/16
occupation_age16 <- data.frame(
  occupational_status = factor(c("Routine (Reference)", "Intermediate", "Higher Managerial"), levels=c("Routine (Reference)", "Intermediate", "Higher Managerial")),
  language = c( 0,0.24, 0.64, 
                0, 0.14 , 0.49  
               
  ),
  lower_ci_occupation = c( 0, 0.18, 0.57 ,
                           0, 0.09 ,0.44
                            
  ),
  upper_ci_occupation = c(0, 0.30, 0.72, 
                          0, 0.19, 0.54 
                          
                          
  ),
  cohort = factor(c(
    "1970 BCS", "1970 BCS", "1970 BCS", 
    "~2001 MCS", "~2001 MCS" ,"~2001 MCS"
    
  )
  ))

occupation_age16_plot <- ggplot(data=occupation_age16, aes(x=occupational_status, y=language, group=cohort)) +
  geom_line(aes(color=cohort)) +
  geom_ribbon(data=occupation_age16,aes(ymin=lower_ci_occupation,ymax=upper_ci_occupation, fill=cohort),  alpha=0.1 ) + 
  geom_point(aes(color=cohort)) +
  scale_color_manual(name="Cohort", values=c( "#B2ABD2", "#FDB863"), limits=c( "1970 BCS", "~2001 MCS")) +
  scale_fill_manual(name="Cohort", values=c( "#B2ABD2", "#FDB863"), limits=c( "1970 BCS", "~2001 MCS"))+
  labs( x = "Highest Household Occupational Status", y = "Language Ability \n (standardised)", size = 7) + ylim(0, 1) +
  theme_classic()+
  theme(axis.text = element_text(size=9, family = "Times", colour = "black"), axis.title = element_text(size=12, family = "Times"))+
  theme(legend.text = element_text(size=15, family="Times"), legend.title = element_text(size=15, family="Times"))

  #annotate("text", x=2, y=.29, label=".25", colour= "#B2ABD2", size=2.5) +
  #annotate("text", x=3.1, y=.65, label=".64", colour= "#B2ABD2", size=2.5)+
  #annotate("text", x=2, y=.11, label=".15", colour= "#FDB863", size=2.5) +
  #annotate("text", x=3.1, y=.50, label=".50", colour= "#FDB863", size=2.5)


occupation_plots <- ggarrange(occupation_age5_plot, occupation_age10_plot, occupation_age16_plot,
                             labels = c("Early Childhood", "Late Childhood", "Adolescence"), 
                             #label.x=0, label.y=0,
                             font.label = list(size = 13, color = "black", face="italic", family="Times"),
                             hjust= -1,
                             vjust=1.8,
                             ncol =1, nrow=3,  align = c( "hv"),
                             common.legend = TRUE, legend = "right") 


early_childhood = ggarrange(occupation_age5_plot, education_age5_plot, 
                             #labels = c("Early Childhood"), 
                             #hjust = -3, 
                             #font.label = list(size = 7, color = "black", face="bold" ),
                             ncol =2, nrow=1,  align = c( "hv"),
                             common.legend = TRUE, legend = "none") 
#earlyChildhood = early_childhood + rremove("legend")

early_childhood1 = annotate_figure(early_childhood,
                top = text_grob("Early Childhood", color="black", face="bold", size=14, family="Times"))



late_childhood = ggarrange(occupation_age10_plot, education_age10_plot, 
                            #labels = c("Early Childhood"), 
                            #hjust = -3, 
                            #font.label = list(size = 7, color = "black", face="bold" ),
                            ncol =2, nrow=1,  align = c( "hv"),
                            common.legend = TRUE, legend = "none")
late_childhood1= annotate_figure(late_childhood,
                top = text_grob("Late Childhood", color="black", face="bold", size=14, family="Times"))

adolescence= ggarrange(occupation_age16_plot, education_age16_plot, 
                           #labels = c("Early Childhood"), 
                           #hjust = -3, 
                           #font.label = list(size = 7, color = "black", face="bold" ),
                           ncol =2, nrow=1,  align = c( "hv"),
                           common.legend = TRUE, legend = "none")
adolescence1= annotate_figure(adolescence,
                top = text_grob("Adolescence", color="black", face="bold", size=14, family="Times"))

combined_plot= ggarrange(early_childhood1, late_childhood1, adolescence1, 
          #labels = c("Early Childhood", "", "Late Childhood","", "Adolescence", ""), 
          #hjust = -7, 
          #font.label = list(size = 7, color = "black", face="bold" ),
          ncol =1, nrow=3,  align = c( "hv"),
          legend.grob = get_legend(education_age5_plot), legend="right")
          


p =combined_plot + ggtitle("Figure 3: Cross-cohort relatonships between SES and language ability")

p1= p+ theme(plot.title = element_text(face="italic", size=20, family ="Times",hjust = 0.05))

ggsave("cross-cohort.png", p1, width=12.8, height=14.3, units= "in" ,dpi=300)
#annotate_figure(combined_plot, top = text_grob("Early Childhood Language", face="bold", size=12))
                




