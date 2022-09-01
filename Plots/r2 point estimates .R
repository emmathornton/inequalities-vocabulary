library("ggplot2")
library("ggpubr")
library("tidyverse")


#r_squared_plot <- data.frame(
  #indicator = factor(c("Education", "Income", "Wealth", "Occupational Status", "Neighbourhood Deprivation"), levels=c("Education", "Income", "Wealth", "Occupational Status", "Neighbourhood Deprivation")),
 # r_squared = c(0.09721664,0.1006617,0.02342018,0.0825417,0.07456588,
              #  0.1138999,0.1139609,0.03237281,0.1049802,0.08534748,
               # 0.07149673,0.06241546,0.02385651,0.06227837,0.03842341,
              #  0.07549792,0.05778899,0.0260961,0.05550665,0.03088782)*100,
 # lower_ci_ = c(0.0884592,0.09183302,0.01817604,0.07444475,0.06677952,
             #   0.1041588,0.1043737,0.02661373,0.09554743,0.07711463,
              #  0.06263545,0.05471721,0.01825448,0.05418452,0.03234287, 
               # 0.0656567,0.04973233, 0.01897393,0.04733762,0.02464781)*100,
 # upper_ci_ = c(0.1062952,0.1098017,0.0292927,0.0909783,0.08270924,
            #    0.1239608,0.1238576,0.03865454,0.1147491,0.0939168,
             #   0.08084899,0.0705491,0.03016619,0.07085629,0.04498242,
             #   0.08590768,0.06637139,0.03428181,0.06424411,0.03778192
             #    )*100,
  #age = factor(c("Age 3","Age 3", "Age 3", "Age 3",  "Age 3",
             #    "Age 5", "Age 5", "Age 5", "Age 5", "Age 5",
              #   "Age 11",  "Age 11", "Age 11", "Age 11", "Age 11",
             #    "Age 14", "Age 14", "Age 14", "Age 14", "Age 14"
 # )
 # ))



#r2_plot <- ggplot() +
  #geom_pointrange(data=r_squared_plot, mapping=aes(x=indicator, y=r_squared, ymin=lower_ci_, ymax=upper_ci_, group= age, color=age, fill=age), size=1, shape=20) +
  #scale_color_manual(name="Age", values=c( "#D7191C", "#FDAE61", "#ABDDA4", "#2B83BA"), limits=c("Age 3", "Age 5", "Age 11", "Age 14"))+
  #scale_fill_manual(name="Age", values=c(  "#D7191C", "#FDAE61", "#ABDDA4", "#2B83BA"), limits=c("Age 3", "Age 5", "Age 11", "Age 14"))+
  #theme_classic() +
  #xlab(label="SEP indicator") +
  #ylab(label="Partial R squared (%)")
  
#r sqaured for RQ 1-3.  

r_squared_plot1 <- data.frame(
  age = factor(c("Age 3", "Age 5", "Age 11", "Age 14"), 
               levels=c("Age 3", "Age 5", "Age 11", "Age 14")),
  r_squared = c(6.72, 8.55, 6.54, 7.25, 
                6.27, 7.68, 5.64, 5.45, 
                1.83, 2.62, 2.15, 2.47, 
                6.02, 8.08, 5.78, 5.27, 
                3.43, 4.49, 3.29, 2.79, 
                8.07, 10.09, 7.53, 7.42
                ),
  lower_ci_ = c(6.62, 8.35, 5.96, 6.46, 
                6.18, 7.51, 5.17, 4.83, 
                1.76, 2.51, 1.85, 1.94,
                5.95,7.9, 5.26, 4.64,
                3.4, 4.4, 2.97, 2.35,
                7.94, 9.88, 6.91, 6.65 ),
  upper_ci_ = c(6.83, 8.75, 7.13,8.04,
                6.35, 7.84, 6.12, 6.06,
                1.9, 2.72, 2.48, 3.03,
                6.09, 8.25, 6.3, 5.9,
                3.46, 4.58, 3.61, 3.22,
                8.18, 10.28, 8.14, 8.18
                
                
  ),
  indicator = factor(c("Education", "Education", "Education", "Education",
                 "Income", "Income", "Income", "Income",
                 "Wealth", "Wealth", "Wealth", "Wealth", 
                 "Occupational Status", "Occupational Status", "Occupational Status","Occupational Status", 
                 "Neighbourhood Deprivation",  "Neighbourhood Deprivation",  "Neighbourhood Deprivation",  "Neighbourhood Deprivation",
                 "Composite Factor", "Composite Factor", "Composite Factor", "Composite Factor"
  )
  ))


r2_plot1 <- r_squared_plot1 %>% 
  ggplot() +
  geom_point(mapping=aes(x=age, y=r_squared,  group= indicator, color=indicator, fill=indicator), 
             size=5, shape=20, position = position_dodge(width=0.4)) +
  scale_color_manual(name="Indicator", values=c( "#66C2A5", "#FC8D62", "#8DA0CB", "#E78AC3", "#A6D854", "black"), limits=c("Education", "Income", "Wealth", "Occupational Status", "Neighbourhood Deprivation", "Composite Factor"))+
  scale_fill_manual(name="Indicator", values=c( "#66C2A5", "#FC8D62", "#8DA0CB", "#E78AC3", "#A6D854", "black" ), limits=c("Education", "Income", "Wealth", "Occupational Status", "Neighbourhood Deprivation", "Composite Factor"))+
  theme_classic2()+
  xlab(label="Age of Language Test") +
  ylab(label="Partial R squared (%)") +
  theme(axis.text = element_text(size=15, family = "Times", colour = "black"), 
        axis.title = element_text(size=15, family = "Times"))+
  theme(legend.text = element_text(size=15, family="Times"), 
        legend.title = element_text(size=15, family="Times"))+
  ylim(0,12)+ 
  geom_vline(xintercept = 1.5:3.5, colour="#E0E0E0") +
  theme(legend.position = "top") +
  
  ggtitle("Figure 1: Partial" ~ R^2 ~ "values for SEP indicators in ~2001 born cohort") +
  theme(plot.title = element_text(face="italic", size=18, family ="Times",hjust = 0.5)) +
  
  geom_text(aes(x=age, y=r_squared, label=round(r_squared,1), color=factor(indicator), hjust=-0.7),
            family = "Times New Roman",
            position = position_dodge(width=0.4))



ggsave("mcs-r2.png",r2_plot1, width=9, height=10, units= "in" ,dpi=300)

#cross cohort comparison ####
r_squared_plotComparison <- data.frame(

 age= factor(c("Early Childhood Language", "Late Childhood Language", "Adolescent Language"), 
             levels=c("Early Childhood Language", "Late Childhood Language", "Adolescent Language")),
 
  r_squared = c(5.33, 7.05, 4.9, 
                6.53, 9.15, 6.94,
                7.99, 5.84, 5.98,
                9.37, 7.64, 9.26),
  #lower_ci_ = c(5.25, 6.6, 4.27, 
              #  6.26, 8.49,6.14,
              #  9.02, 5.19, 5.22, 
              #  9.93, 6.78,8.31),
  #upper_ci_ = c(7.05, 8.42, 6.33,
              #  8.11, 10.59, 8.95,
               # 10.88, 6.72, 6.34, 
               # 11.87, 8.59, 10.32
  #),
 #cohort =factor(c("1970 born" ,"1970 born", "1970 born",  
                  #"1970 born" ,"1970 born", "1970 born",
                  #"~2001 born" ,"~2001 born", "~2001 born",  
                  #"~2001 born" ,"~2001 born", "~2001 born"
                 # )),
  indicator = factor(c(
    "1970 born Cohort:\n Parent Occupation" ,"1970 born Cohort:\n Parent Occupation", "1970 born Cohort:\n Parent Occupation",  
    "1970 born Cohort:\n Parent Education" ,"1970 born Cohort:\n Parent Education", "1970 born Cohort:\n Parent Education",
    "~2001 born Cohort:\n Parent Occupation" ,"~2001 born Cohort:\n Parent Occupation", "~2001 born Cohort:\n Parent Occupation",  
    "~2001 born Cohort:\n Parent Education" ,"~2001 born Cohort:\n Parent Education", "~2001 born Cohort:\n Parent Education"
  )
  
  ))

r2_plotComparison <- r_squared_plotComparison %>% 
ggplot() +
  geom_point(mapping=aes(x=indicator, y=r_squared,  group= age, color=age, fill=age), size=5,position = position_dodge(width=0.4) ) +
  scale_color_manual(name="Age", values=c( "#377EB8","#FF7F00","#F781BF"), limits=c( "Early Childhood Language", "Late Childhood Language", "Adolescent Language"))+
  scale_fill_manual(name="Age", values=c(   "#377EB8","#FF7F00","#F781BF"), limits=c( "Early Childhood Language", "Late Childhood Language", "Adolescent Language"))+
  scale_x_discrete(limits=c("1970 born Cohort:\n Parent Occupation", "1970 born Cohort:\n Parent Education", "~2001 born Cohort:\n Parent Occupation", "~2001 born Cohort:\n Parent Education")) +
  #scale_shape_manual(name="Age & Indicator", values= c(16,15, 16, 15), limits= c("1970 born Occupation", "1970 born Education",  "~2001 born Occupation", "~2001 born Education")) +
  theme_classic() +
  xlab(label="Cohort & Indicator") +
  ylab(label="Partial R squared (%)") +
  theme(axis.text = element_text(size=10, family = "Times", colour = "black"), 
        axis.title = element_text(size=13, family = "Times"))+
  theme(legend.text = element_text(size=15, family="Times"), 
        legend.title = element_text(size=15, family="Times"))+
  ggtitle("Figure 4: Partial" ~ R^2 ~ "values for SEP indicators in both cohorts (RQ4)") +
 theme(plot.title = element_text( size=20, family ="Times",hjust = 0.05, face = "italic")) +
  geom_vline(xintercept = 1.5:3.5, colour="#E0E0E0") +
  theme(legend.position = "top") +
  ylim(0,12) +
  annotate("text", x=1.25, y=4.9, label= "4.9", colour= "#F781BF", family="serif") +  #bcs occupation adolescent
  annotate("text", x=1.12, y=7.02, label= "7", colour= "#FF7F00", family="serif") +  #bcs occupation late childhood
  annotate("text", x=0.99, y=5.33, label= "5.3", colour= "#377EB8", family="serif")+   #bcs occupation early childhood
  annotate("text", x=2.25, y=6.94, label= "6.9", colour= "#F781BF", family="serif") +  #bcs education adolescent
  annotate("text", x=2.15, y=9.15, label= "9.1", colour= "#FF7F00", family="serif") +  #bcs education late childhood
  annotate("text", x=1.99, y=6.53, label= "6.5", colour= "#377EB8", family="serif") +  #bcs education early childhood
  
  annotate("text", x=3.25, y=5.98, label= "6", colour= "#F781BF", family="serif") +  #mcs occupation adolescent
  annotate("text", x=3.15, y=5.78, label= "5.8", colour= "#FF7F00", family="serif") +  #mcs occupation late childhood
  annotate("text", x=2.99, y=7.99, label= "8", colour= "#377EB8", family="serif")+   #mcs occupation early childhood
  annotate("text", x=4.25, y=9.26, label= "9.3", colour= "#F781BF", family="serif") +  #mcs education adolescent
  annotate("text", x=4.15, y=7.64, label= "7.6", colour= "#FF7F00", family="serif") +  #mcs education late childhood
  annotate("text", x=3.99, y=9.37, label= "9.4", colour= "#377EB8", family="serif")   #mcs education early childhood
#plots= ggarrange (r2_plot1, r2_plotComparison, 
                 # ncol =1, nrow=2,  align = c( "hv"),
                 # common.legend = FALSE, legend = "right") 


ggsave("r2-comparison.png",r2_plotComparison, width=8.82, height=9.52, units= "in" ,dpi=300)

