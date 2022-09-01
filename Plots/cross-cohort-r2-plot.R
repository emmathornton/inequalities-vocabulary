#r2 point estimates for cross cohort comparison 
#first, plot for main analysis only
#then plot ridit score r2 and combine in a panel with main analysis one
#for visual comparison 


library("ggplot2")
library("ggpubr")
library("tidyverse")


r_squared_plot1 <- data.frame(
  age = factor(c("Early Childhood", "Early Childhood", 
                 "Early Childhood", "Early Childhood", 
                 "Late Childhood", "Late Childhood" , 
                 "Late Childhood", "Late Childhood" , 
                 "Adolescence", "Adolescence",
                 "Adolescence", "Adolescence")), 
  cohort= factor(c("1970 born \n Highest Household \n Education ", "1970 born \n Highest Household \n Occupation",
                   "~ 2001 born \n Highest Household \n Education ", "~ 2001 born \n Highest Household \n Occupation", 
                   "1970 born \n Highest Household \n Education ", "1970 born \n Highest Household \n Occupation",
                   "~ 2001 born \n Highest Household \n Education ", "~ 2001 born \n Highest Household \n Occupation",
                   "1970 born \n Highest Household \n Education ", "1970 born \n Highest Household \n Occupation",
                   "~ 2001 born \n Highest Household \n Education ", "~ 2001 born \n Highest Household \n Occupation")), 
  indicator = factor(c("1970 born \n Highest Household \n Education ", "1970 born \n Highest Household \n Occupation",
                       "~ 2001 born \n Highest Household \n Education ", "~ 2001 born \n Highest Household \n Occupation", 
                     "1970 born \n Highest Household \n Education ", "1970 born \n Highest Household \n Occupation",
                     "~ 2001 born \n Highest Household \n Education ", "~ 2001 born \n Highest Household \n Occupation",
                      "1970 born \n Highest Household \n Education ", "1970 born \n Highest Household \n Occupation",
                       "~ 2001 born \n Highest Household \n Education ", "~ 2001 born \n Highest Household \n Occupation")),
  r_squared = c(6.3, 4.8,
               9.4, 7.9,
               8.9, 7, 
               7.8, 6, 
               6.7, 4.9, 
               9.3, 5.8))

#plot

r2_plot1 <- r_squared_plot1 %>% 
  mutate(age = fct_relevel(age, 
                            "Early Childhood", "Late Childhood", "Adolescence")) %>% #reorder age for x axis 
  ggplot() +
  geom_point(mapping=aes(x=age, y=r_squared,  group= cohort, color=cohort, fill=cohort, shape = indicator), 
             size=5, position = position_dodge(width=0.5)) +
  scale_color_manual(name="Cohort and Indicator", values=c( "#B2ABD2","#B2ABD2", "#FDB863", "#FDB863"), 
                     limits=c( "1970 born \n Highest Household \n Education ", "1970 born \n Highest Household \n Occupation",
                               "~ 2001 born \n Highest Household \n Education ", "~ 2001 born \n Highest Household \n Occupation")) +
  scale_fill_manual(name="Cohort and Indicator", values=c( "#B2ABD2","#B2ABD2", "#FDB863", "#FDB863"), 
                    limits=c("1970 born \n Highest Household \n Education ", "1970 born \n Highest Household \n Occupation",
                             "~ 2001 born \n Highest Household \n Education ", "~ 2001 born \n Highest Household \n Occupation")) +
scale_shape_manual(name= "Cohort and Indicator", values =c(15, 16, 15,  16), 
                    limits = c ("1970 born \n Highest Household \n Education ", "1970 born \n Highest Household \n Occupation",
                                "~ 2001 born \n Highest Household \n Education ", "~ 2001 born \n Highest Household \n Occupation")) +
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
  
 #ggtitle("Main Analysis") +
 #theme(plot.title = element_text(face="italic", size=15, family ="Times",hjust = 0.5)) +
  
  geom_text(aes(x=age, y=r_squared, label=round(r_squared,1), color=factor(cohort), hjust=-1.4),
            family = "Times New Roman",
            position = position_dodge(width=0.5)) 
  #main = annotate_figure(r2_plot1, top = text_grob("Main Analysis", 
       #                           color="black", face="bold", 
                  #                size=14, family="Times"))

r2_plot1
ggsave("cross-cohort-r2-plots.png",r2_plot1, width=13, height=12.5, units= "in" ,dpi=300)




#comparison with Ridit score r2

r_squared_plotRidit <- data.frame(
  age = factor(c("Early Childhood", "Early Childhood", 
                 "Early Childhood", "Early Childhood", 
                 "Late Childhood", "Late Childhood" , 
                 "Late Childhood", "Late Childhood" , 
                 "Adolescence", "Adolescence",
                 "Adolescence", "Adolescence")), 
  cohort= factor(c("1970 born \n Highest Household \n Education ", "1970 born \n Highest Household \n Occupation",
                   "~ 2001 born \n Highest Household \n Education ", "~ 2001 born \n Highest Household \n Occupation", 
                   "1970 born \n Highest Household \n Education ", "1970 born \n Highest Household \n Occupation",
                   "~ 2001 born \n Highest Household \n Education ", "~ 2001 born \n Highest Household \n Occupation",
                   "1970 born \n Highest Household \n Education ", "1970 born \n Highest Household \n Occupation",
                   "~ 2001 born \n Highest Household \n Education ", "~ 2001 born \n Highest Household \n Occupation")), 
  indicator = factor(c("1970 born \n Highest Household \n Education ", "1970 born \n Highest Household \n Occupation",
                       "~ 2001 born \n Highest Household \n Education ", "~ 2001 born \n Highest Household \n Occupation", 
                       "1970 born \n Highest Household \n Education ", "1970 born \n Highest Household \n Occupation",
                       "~ 2001 born \n Highest Household \n Education ", "~ 2001 born \n Highest Household \n Occupation",
                       "1970 born \n Highest Household \n Education ", "1970 born \n Highest Household \n Occupation",
                       "~ 2001 born \n Highest Household \n Education ", "~ 2001 born \n Highest Household \n Occupation")),
  r_squared = c(6.3, 4.8,
                10.3, 5.4,
                8.7, 7, 
                7.7, 4.7, 
                6.4, 4.8, 
                8.6, 4.6))

#plot

r2_plotRidit <- r_squared_plotRidit %>% 
  mutate(age = fct_relevel(age, 
                           "Early Childhood", "Late Childhood", "Adolescence")) %>% #reorder age for x axis 
  ggplot() +
  geom_point(mapping=aes(x=age, y=r_squared,  group= cohort, color=cohort, fill=cohort, shape = indicator), 
             size=5, position = position_dodge(width=0.5)) +
  scale_color_manual(name="Cohort and Indicator", values=c( "#B2ABD2","#B2ABD2", "#FDB863", "#FDB863"), 
                     limits=c( "1970 born \n Highest Household \n Education ", "1970 born \n Highest Household \n Occupation",
                               "~ 2001 born \n Highest Household \n Education ", "~ 2001 born \n Highest Household \n Occupation")) +
  scale_fill_manual(name="Cohort and Indicator", values=c( "#B2ABD2","#B2ABD2", "#FDB863", "#FDB863"), 
                    limits=c("1970 born \n Highest Household \n Education ", "1970 born \n Highest Household \n Occupation",
                             "~ 2001 born \n Highest Household \n Education ", "~ 2001 born \n Highest Household \n Occupation")) +
  scale_shape_manual(name= "Cohort and Indicator", values =c(15, 16, 15,  16), 
                     limits = c ("1970 born \n Highest Household \n Education ", "1970 born \n Highest Household \n Occupation",
                                 "~ 2001 born \n Highest Household \n Education ", "~ 2001 born \n Highest Household \n Occupation")) +
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
            position = position_dodge(width=0.5)) 
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
