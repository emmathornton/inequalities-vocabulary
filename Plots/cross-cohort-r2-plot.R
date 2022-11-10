#r2 point estimates for cross cohort comparison 
#first, plot for main analysis only
#then plot ridit score r2 and combine in a panel with main analysis one
#for visual comparison 


library("ggplot2")
library("ggpubr")
library("tidyverse")

r_squared_plot1 <- data.frame(
  age = factor(c("Early Childhood", "Early Childhood", "Early Childhood", 
                 "Early Childhood", "Early Childhood", "Early Childhood", 
                 "Late Childhood", "Late Childhood" , "Late Childhood" , 
                 "Late Childhood", "Late Childhood" , "Late Childhood" , 
                 "Adolescence", "Adolescence","Adolescence",
                 "Adolescence", "Adolescence","Adolescence")), 
  cohort= factor(c("BCS1970 \n Highest Household \n Education", 
                   "MCS2001 \n Highest Household \n Education",
                   "BCS1970 \n Highest Household \n Occupation",
                   "MCS2001 \n Highest Household \n Occupation",
                   "BCS1970 \n Income Quintiles",
                   "MCS2001 \n Income Quintiles" )), 
  indicator = factor(c("BCS1970 \n Highest Household \n Education", 
                       "MCS2001 \n Highest Household \n Education",
                       "BCS1970 \n Highest Household \n Occupation",
                       "MCS2001 \n Highest Household \n Occupation",
                       "BCS1970 \n Income Quintiles",
                       "MCS2001 \n Income Quintiles" )),
  r_squared = c(6.5, 9.6,4.4,8.3,5.1,6,
                8.7, 7.9, 6, 6.2,6.5, 5.1,7, 9.4, 4.3, 5.7, 4.3, 4
             ))



#plot

r2_plot1 <- r_squared_plot1 %>% 
  mutate(age = fct_relevel(age, 
                            "Early Childhood", "Late Childhood", "Adolescence")) %>% #reorder age for x axis 
  ggplot() +
  geom_point(mapping=aes(x=age, y=r_squared,  group= cohort, color=cohort, fill=cohort, shape = indicator), 
             size=5, position = position_dodge(width=0.5)) +
  scale_color_manual(name="Cohort and Indicator", values=c("#B2ABD2","#FDB863",
                                                           "#B2ABD2","#FDB863",
                                                           "#B2ABD2", "#FDB863"), 
                     limits=c("BCS1970 \n Highest Household \n Education", 
                              "MCS2001 \n Highest Household \n Education",
                              "BCS1970 \n Highest Household \n Occupation",
                              "MCS2001 \n Highest Household \n Occupation",
                              "BCS1970 \n Income Quintiles",
                              "MCS2001 \n Income Quintiles" )) +
  scale_fill_manual(name="Cohort and Indicator", values=c("#B2ABD2","#FDB863",
                                                          "#B2ABD2","#FDB863",
                                                          "#B2ABD2", "#FDB863"), 
                    limits=c("BCS1970 \n Highest Household \n Education", 
                             "MCS2001 \n Highest Household \n Education",
                             "BCS1970 \n Highest Household \n Occupation",
                             "MCS2001 \n Highest Household \n Occupation",
                             "BCS1970 \n Income Quintiles",
                             "MCS2001 \n Income Quintiles" )) +
  scale_shape_manual(name= "Cohort and Indicator", values =c(15, 15,
                                                             16, 16,
                                                             17, 17), 
                     limits = c ("BCS1970 \n Highest Household \n Education", 
                                 "MCS2001 \n Highest Household \n Education",
                                 "BCS1970 \n Highest Household \n Occupation",
                                 "MCS2001 \n Highest Household \n Occupation",
                                 "BCS1970 \n Income Quintiles",
                                 "MCS2001 \n Income Quintiles" )) +
  theme_classic2()+
  xlab(label="\n Vocabulary") +
  ylab(label="Partial R squared (%)") +
  theme(axis.text = element_text(size=15, family = "Times", colour = "black"), 
        axis.title = element_text(size=16, family = "Times"))+
  theme(legend.text = element_text(size=17, family="Times"), 
        legend.title = element_text(size=18, family="Times"))+
  ylim(0,12)+ 
  geom_vline(xintercept = 1.5:3.5, colour="#E0E0E0") +
  theme(legend.position = "top") +
  
 ggtitle("Main Analysis") +
 theme(plot.title = element_text(face="italic", size=15, family ="Times",hjust = 0.5)) +
  
  
  geom_text(aes(x=age, y=r_squared, label=round(r_squared,1), color=factor(cohort), hjust=-1.4),
            family = "Times New Roman",
            vjust = -1.5,
            hjust = 0.5,
            #check_overlap = TRUE,
            position = position_dodge(0.7))
  #main = annotate_figure(r2_plot1, top = text_grob("Main Analysis", 
       #                           color="black", face="bold", 
                  #                size=14, family="Times"))



r2_plot1
ggsave("cross-cohort-r2-plots.png",r2_plot1, width=13, height=12.5, units= "in" ,dpi=300)




#comparison with Ridit score r2

r_squared_plotRidit <- data.frame(
  age = factor(c("Early Childhood", "Early Childhood", "Early Childhood", 
                 "Early Childhood", "Early Childhood", "Early Childhood", 
                 "Late Childhood", "Late Childhood" , "Late Childhood" , 
                 "Late Childhood", "Late Childhood" , "Late Childhood" , 
                 "Adolescence", "Adolescence","Adolescence",
                 "Adolescence", "Adolescence","Adolescence")), 
  cohort= factor(c("BCS1970 \n Highest Household \n Education", 
                   "MCS2001 \n Highest Household \n Education",
                   "BCS1970 \n Highest Household \n Occupation",
                   "MCS2001 \n Highest Household \n Occupation",
                   "BCS1970 \n Income Quintiles",
                   "MCS2001 \n Income Quintiles" )), 
  indicator = factor(c("BCS1970 \n Highest Household \n Education", 
                       "MCS2001 \n Highest Household \n Education",
                       "BCS1970 \n Highest Household \n Occupation",
                       "MCS2001 \n Highest Household \n Occupation",
                       "BCS1970 \n Income Quintiles",
                       "MCS2001 \n Income Quintiles" )),
  r_squared = c(6.5,10.6,4.4, 5, 4.8,3.8,
                8.6,7.2,6,4.2, 6.2, 3.6,
                6.7,8.7,4.3, 4, 4.1, 2.5))

#plot

r2_plotRidit <- r_squared_plotRidit %>% 
  mutate(age = fct_relevel(age, 
                           "Early Childhood", "Late Childhood", "Adolescence")) %>% #reorder age for x axis 
  ggplot() +
  geom_point(mapping=aes(x=age, y=r_squared,  group= cohort, color=cohort, fill=cohort, shape = indicator), 
             size=5, position = position_dodge(width=0.5)) +
  scale_color_manual(name="Cohort and Indicator", values=c("#B2ABD2","#FDB863",
                                                           "#B2ABD2","#FDB863",
                                                           "#B2ABD2", "#FDB863"), 
                     limits=c("BCS1970 \n Highest Household \n Education", 
                              "MCS2001 \n Highest Household \n Education",
                              "BCS1970 \n Highest Household \n Occupation",
                              "MCS2001 \n Highest Household \n Occupation",
                              "BCS1970 \n Income Quintiles",
                              "MCS2001 \n Income Quintiles" )) +
  scale_fill_manual(name="Cohort and Indicator", values=c("#B2ABD2","#FDB863",
                                                          "#B2ABD2","#FDB863",
                                                          "#B2ABD2", "#FDB863"), 
                    limits=c("BCS1970 \n Highest Household \n Education", 
                             "MCS2001 \n Highest Household \n Education",
                             "BCS1970 \n Highest Household \n Occupation",
                             "MCS2001 \n Highest Household \n Occupation",
                             "BCS1970 \n Income Quintiles",
                             "MCS2001 \n Income Quintiles" )) +
  scale_shape_manual(name= "Cohort and Indicator", values =c(15, 15,
                                                             16, 16,
                                                             17, 17), 
                     limits = c ("BCS1970 \n Highest Household \n Education", 
                                 "MCS2001 \n Highest Household \n Education",
                                 "BCS1970 \n Highest Household \n Occupation",
                                 "MCS2001 \n Highest Household \n Occupation",
                                 "BCS1970 \n Income Quintiles",
                                 "MCS2001 \n Income Quintiles" )) +
  theme_classic2()+
  xlab(label="Vocabulary") +
  ylab(label="Partial R squared (%)") +
  theme(axis.text = element_text(size=15, family = "Times", colour = "black"), 
        axis.title = element_text(size=15, family = "Times"))+
  theme(legend.text = element_text(size=15, family="Times"), 
        legend.title = element_text(size=15, family="Times"))+
  ylim(0,12)+ 
  geom_vline(xintercept = 1.5:3.5, colour="#E0E0E0") +
  theme(legend.position = "bottom") +
  
  ggtitle("Ridit Score Analysis") +
  theme(plot.title = element_text(face="italic", size=15, family ="Times",hjust = 0.5)) +
  
  geom_text(aes(x=age, y=r_squared, label=round(r_squared,1), color=factor(cohort), hjust=-1.4),
            family = "Times New Roman",
            vjust = -1.5,
            hjust = 0.5,
            #check_overlap = TRUE,
            position = position_dodge(0.7))
#ridit= annotate_figure(r2_plotRidit, top = text_grob("Ridit Score Analysis", 
            #                     color="black", face="bold", 
             #                    size=14, family="Times"))


r2_plotRidit


#combine plots
combined_plot= ggarrange(r2_plot1, r2_plotRidit, 
                        #labels = c("Main Analysis ", 
                         #        "Ridit Score Analysis"), 
                       # hjust =-2,vjust = 0.2 ,
                       #  font.label = list(size = 13, color = "black", face="italic", family="Times"),
                         ncol =1, nrow=2,  align = c( "hv"), 
                      common.legend = TRUE, 
                      legend = "bottom") +
                         # legend = NULL)
                       #legend.grob = get_legend(ridit), legend="bottom")
ggtitle("Figure 1: Partial" ~ R^2 ~ "values for SEP indicators in each cohort: Comparison of Main and Ridit Score Analyses") +
theme(plot.title = element_text(face="italic", size=18, family ="Times",
                                       hjust = 0.5, vjust = 1))

ggsave("main-ridit-r2-plots.png",combined_plot, width=11, height=11.5, units= "in" ,dpi=300)
